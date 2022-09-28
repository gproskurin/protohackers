-module(ph_handler_1_prime_time).

-include_lib("kernel/include/logger.hrl").

-export([handle_data/2]).

handle_data(S, Data) ->
    case split_newline(Data) of
        {Line, Rest} ->
            process(S, Line),
            handle_data(S, Rest);
        nomatch ->
            Data
    end.


process(S, Line) ->
    ErrMsg = #{<<"prime">> => <<"error">>},
    Resp = try
        J = jiffy:decode(Line, [return_maps]),
        case J of
            #{<<"method">> := <<"isPrime">> = M, <<"number">> := N} when is_number(N) ->
                ?LOG_NOTICE("NUM: ~p", [N]),
                #{<<"method">> => M, <<"prime">> => is_prime(N)};
            _ ->
                ErrMsg
        end
    catch
        error:{_Num,_Reason} ->
            ErrMsg
    end,
    RespData = [jiffy:encode(Resp), $\n],
    ok = gen_tcp:send(S, RespData).


is_prime(N) when is_float(N) ->
    Ni = erlang:floor(N),
    case (N - Ni) == 0.0 of
        true -> is_prime(Ni);
        false -> false
    end;
is_prime(N) when N =< 1; N=:=4 ->
    false;
is_prime(N) when N=:=2; N=:=3 ->
    true;
is_prime(N) when N >= 5 ->
    Lim = erlang:floor(math:sqrt(N)),
    case div_by(N, 2) orelse div_by(N,3) of
        true -> false;
        false -> is_prime(N, 5, Lim)
    end.


is_prime(_N, I, Lim) when I > Lim ->
    true;
is_prime(N, I, Lim) when I >= 5 ->
    case div_by(N, I) of
        true -> false;
        false ->
            case div_by(N, I+2) of
                true -> false;
                false -> is_prime(N, I+6, Lim)
            end
    end.


div_by(N, D) ->
    (N rem D) =:= 0.


split_newline(Str) ->
    split_by(Str, <<"\n">>).

split_by(Str, Pattern) ->
    case string:find(Str, Pattern) of
        nomatch ->
            nomatch;
        End ->
            LineLen = string:length(Str) - string:length(End),
            Line = string:slice(Str, 0, LineLen),
            Rest = string:slice(End, string:length(Pattern)),
            {Line, Rest}
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_newline_test() ->
    ?assertEqual(nomatch, split_by(<<"qwerty">>, <<"Q">>)),
    ?assertEqual(
        {<<"qwer">>, <<"asdf">>},
        split_by(<<"qwerZXasdf">>, <<"ZX">>)
    ),
    ?assertEqual(
        {<<"qwer">>, <<"asdf\n">>},
        split_newline(<<"qwer\nasdf\n">>)
    ).

is_prime_test() ->
    ?assert(is_prime(7.0)),
    ?assertNot(is_prime(7.1)),

    ?assertNot(is_prime(-100)),
    ?assertNot(is_prime(-1)),
    ?assertNot(is_prime(0)),
    ?assertNot(is_prime(1)),
    ?assert(is_prime(2)),
    ?assert(is_prime(3)),
    ?assertNot(is_prime(4)),
    ?assert(is_prime(5)),
    ?assertNot(is_prime(6)),
    ?assertNot(is_prime(10000)),
    ?assert(is_prime(27644437)),
    ?assertNot(is_prime(26970586767893351002133248949616879383518092079315091251644540)),
    ?assert(is_prime(1000000000039)).

-endif.

