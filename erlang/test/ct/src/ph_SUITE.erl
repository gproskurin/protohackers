-module(ph_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ph.hrl").


-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,

    test_0_tcp_echo/1,
    test_1_prime_time/1,
    test_2_means_to_an_end/1,
    test_3_chat/1,
    test_3_chat_user_cleanup/1,
    test_4_unusual_database/1
]).


all() -> [
    {group, group_services}
].


groups() -> [
    {group_services, [parallel], [
        test_0_tcp_echo,
        test_1_prime_time,
        test_2_means_to_an_end,
        {group, group_3_chat},
        test_4_unusual_database
    ]},
    {group_3_chat, [], [
        test_3_chat,
        test_3_chat_user_cleanup
    ]}
].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ph),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(ph).


test_0_tcp_echo(_Config) ->
    wait_clients(spawn_clients(100, fun tcp_echo_client/1)),
    ok.


test_1_prime_time(_Config) ->
    wait_clients(spawn_clients(100, fun prime_client/1)),
    ok.


test_2_means_to_an_end(_Config) ->
    wait_clients(spawn_clients(100, fun means_client/1)),
    ok.


test_3_chat(_Config) ->
    Sa = connect(50003),
    ?assertMatch({<<"Welcome", _/binary>>, <<>>}, tcp_readline(Sa, <<>>)),
    tcp_sendline(Sa, <<"Alice">>),
    ?assertMatch({<<"* Users online: ">>, <<>>}, tcp_readline(Sa, <<>>)),

    Sb = connect(50003),
    ?assertMatch({<<"Welcome", _/binary>>, <<>>}, tcp_readline(Sb, <<>>)),
    tcp_sendline(Sb, <<"Bob">>),
    ?assertMatch({<<"* Users online: Alice">>, <<>>}, tcp_readline(Sb, <<>>)),

    ?assertMatch({<<"* User Bob joined">>, <<>>}, tcp_readline(Sa, <<>>)),

    tcp_sendline(Sb, <<"Msg from Bob">>),
    ?assertMatch({<<"[Bob] Msg from Bob">>, <<>>}, tcp_readline(Sa, <<>>)),

    tcp_sendline(Sa, <<"msg from alice">>),
    ?assertMatch({<<"[Alice] msg from alice">>, <<>>}, tcp_readline(Sb, <<>>)),

    ok = gen_tcp:close(Sa),
    ?assertMatch({<<"* Alice has left the room">>, <<>>}, tcp_readline(Sb, <<>>)),

    ok = gen_tcp:close(Sb),
    ok.


test_3_chat_user_cleanup(_Config) ->
    Session = fun() ->
        S = connect(50003),
        ?assertMatch({<<"Welcome", _/binary>>, <<>>}, tcp_readline(S, <<>>)),
        tcp_sendline(S, <<"Charlie">>),
        ?assertMatch({<<"* Users online:", _/binary>>, <<>>}, tcp_readline(S, <<>>)),
        ok = gen_tcp:close(S)
    end,

    % connect and disconnect the same user
    Session(),
    Session(),
    Session(),
    ok.


test_4_unusual_database(_Config) ->
    Timeout = 100,
    Addr = {127,0,0,1},
    Port = 50004,
    {ok, S} = gen_udp:open(0, [binary, {active, false}]),
    ok = gen_udp:connect(S, #{family => inet, addr => Addr, port => Port}),

    lists:foreach(
        fun({Req, ExpResp}) ->
            ok = gen_udp:send(S, Req),
            case ExpResp of
                timeout ->
                    ?assertEqual({error, timeout}, gen_udp:recv(S, 0, Timeout));
                _ ->
                    {ok, {Addr, Port, Resp}} = gen_udp:recv(S, 0, Timeout),
                    ?assertEqual(ExpResp, Resp)
            end
        end,
        [
            {<<"version">>, <<"version=Unusual Database v1.0">>},
            {<<"k1">>, timeout},
            {<<"k1=v1">>, <<"k1=v1">>},
            {<<"k1">>, <<"k1=v1">>},
            {<<"k2">>, timeout},
            {<<"k2==v2">>, <<"k2==v2">>}, % value starts with "="
            {<<"k2">>, <<"k2==v2">>},
            {<<"k1=v1">>, <<"k1=v1">>},
            {<<"">>, timeout}, % key is empty string
            {<<"=v">>, <<"=v">>},
            {<<"">>, <<"=v">>},
            {<<"e">>, timeout},
            {<<"e=">>, <<"e=">>},
            {<<"e">>, <<"e=">>},
            {<<"version=new">>, timeout},
            {<<"version">>, <<"version=Unusual Database v1.0">>}
        ]
    ),
    ok.

%%%

tcp_echo_client(Ppid) ->
    Datas = [
        <<"qwe">>,
        <<"ASDF">>,
        << <<"A">> || _ <- lists:seq(1, 1*1024*1024) >>
    ],
    S = connect(50000),
    lists:foreach(
        fun(D) ->
            ok = gen_tcp:send(S, D),
            {ok, R} = gen_tcp:recv(S, erlang:byte_size(D)),
            ?assertEqual(D, R),
            timer:sleep(100)
        end,
        Datas
    ),
    ok = gen_tcp:close(S),
    respond_result(Ppid, ok),
    ok.


prime_client(Ppid) ->
    S = connect(50001),

    Send = fun(Map) ->
        tcp_sendline(S, ph_utils:json_encode(Map))
    end,

    Send(#{<<"method">> => <<"isPrime">>, <<"number">> => 23}),
    {L1, <<>>} = tcp_readline(S, <<>>),
    ?assertMatch(
        #{<<"method">> := <<"isPrime">>, <<"prime">> := true},
        ph_utils:json_decode(L1)
    ),

    timer:sleep(100), % keep a bunch of sockets connected

    Send(#{<<"method">> => <<"isPrime">>, <<"number">> => <<"123">>}),
    {L2, <<>>} = tcp_readline(S, <<>>),
    ?assertMatch(
        #{<<"prime">> := <<"error">>},
        ph_utils:json_decode(L2)
    ),

    timer:sleep(100),

    Send(#{<<"method">> => <<"isPrime">>, <<"number">> => 24}),
    {L3, <<>>} = tcp_readline(S, <<>>),
    ?assertMatch(
        #{<<"method">> := <<"isPrime">>, <<"prime">> := false},
        ph_utils:json_decode(L3)
    ),

    timer:sleep(100),
    ok = gen_tcp:close(S),
    respond_result(Ppid, ok),
    ok.


means_client(Ppid) ->
    S = connect(50002),

    Send = fun(Cmd, A, B) ->
        ok = gen_tcp:send(S, <<Cmd/binary, A:4/big-signed-integer-unit:8, B:4/big-signed-integer-unit:8>>)
    end,
    SendI = fun(A, B) -> Send(<<"I">>, A, B) end,
    SendQ = fun(A, B) -> Send(<<"Q">>, A, B) end,
    Recv = fun() ->
        {ok, <<R:4/big-signed-integer-unit:8>>} = gen_tcp:recv(S, 4),
        R
    end,

    SendI(12345, 101),
    SendI(12346, 102),
    SendI(12347, 100),
    SendI(40960, 5),
    SendQ(12288, 16384),
    R = Recv(),
    ?assertEqual(101, R),

    timer:sleep(100),
    ok = gen_tcp:close(S),
    respond_result(Ppid, ok),
    ok.


connect(Port) ->
    {ok, S} = gen_tcp:connect(
        #{family => inet, port => Port, addr => {127,0,0,1}},
        [{active, false}, binary]
    ),
    S.


spawn_clients(N, Fun) ->
    Self = self(),
    lists:map(
        fun(_) -> spawn_link(fun() -> Fun(Self) end) end,
        lists:seq(1, N)
    ).


wait_clients(Pids) ->
    lists:foreach(
        fun(Pid) ->
            ok = receive
                {ph_ct_result, Pid, Result} -> Result
            after timer:seconds(5) ->
                error
            end
        end,
        Pids
    ),
    ok.


tcp_readline(S, Buf) ->
    case ph_utils:split_newline(Buf) of
        {_Line, _Rest} = R ->
            R;
        nomatch ->
            {ok, NewData} = gen_tcp:recv(S, 0, timer:seconds(1)),
            tcp_readline(S, <<Buf/binary, NewData/binary>>)
    end.


tcp_sendline(S, Msg) ->
    ok = gen_tcp:send(S, [Msg, $\n]).


respond_result(Ppid, Result) ->
    Ppid ! {ph_ct_result, self(), Result}.

