-module(ph_3_chat_srv).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ph_chat_srv).

-export([
    connect/0,
    new_user/1,
    msg/1
]).

-export([
    start_link/0,

    init/1,
    handle_call/3,
    handle_info/2
]).


-record(user_info, {
    pid,
    user_name,
    monitor_ref
}).

-record(state, {
    users_by_name = #{},
    users_by_pid = #{}
}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, undefined, []).


connect() ->
    gen_server:call(?SERVER, {connect, self()}).


new_user(UserName) ->
    gen_server:call(?SERVER, {new_user, UserName, self()}).


msg(Msg) ->
    gen_server:call(?SERVER, {msg, Msg, self()}).


init(_) ->
    username_valid(<<>>), % compile regex for faster execution later
    {ok, #state{}}.


handle_call({connect, Pid}, _From, State) ->
    ?LOG_NOTICE("CHAT_SRV: connect: pid=~p", [Pid]),
    ph_handler_3_budget_chat:msg(Pid, <<"Welcome to chat!">>),
    {reply, ok, State};

handle_call({new_user, UserName, Pid}, _From, State) ->
    case username_valid(UserName) of
        false ->
            ?LOG_ERROR("CHAT_SRV: invalid username: pid=~p", [Pid]),
            ph_handler_3_budget_chat:disconnect(Pid),
            {reply, ok, State};
        true ->
            case uinfo_new_user(UserName, Pid, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, uinfo_conflict} ->
                    {reply, {error, conflict}, State}
            end
    end;

handle_call({msg, Msg, FromPid}, _From, State) ->
    case maps:find(FromPid, State#state.users_by_pid) of
        error ->
            {reply, {error, pid_not_found}, State};
        {ok, #user_info{user_name = FromName}} ->
            M = <<"[",FromName/binary,"] ",Msg/binary>>,
            uinfo_bcast_except(M, FromPid, State),
            {reply, ok, State}
    end;

handle_call(Req, From, _State) ->
    ?LOG_ERROR("CHAT_SRV: unknown call: request=~p from=~p", [Req, From]).


handle_info({'DOWN', _Mref, process, Pid, Info}, State) ->
    ?LOG_WARNING("User disconnected: pid=~p info=~p", [Pid, Info]),
    {noreply, uinfo_remove_by_pid(State, Pid)};

handle_info(Data, State) ->
    ?LOG_ERROR("CHAT_SRV: info: data=~p", [Data]),
    {noreply, State}.


uinfo_remove_by_pid(State, Pid) ->
    case maps:take(Pid, State#state.users_by_pid) of
        {#user_info{user_name = N, monitor_ref = Mr}, NewByPid} ->
            erlang:demonitor(Mr, [flush]),
            NewState = State#state{
                users_by_name = maps:remove(N, State#state.users_by_name),
                users_by_pid = NewByPid
            },
            uinfo_bcast_except(<<"* ",N/binary," has left the room">>, undefined, NewState),
            NewState;
        error ->
            State
    end.


uinfo_new_user(UserName, Pid, State) ->
    ByName = State#state.users_by_name,
    case maps:is_key(UserName, ByName) of
        true ->
            {error, uinfo_conflict};
        false ->
            ByPid = State#state.users_by_pid,
            case maps:is_key(Pid, ByPid) of
                true ->
                    {error, uinfo_conflict};
                false ->
                    Ui = #user_info{
                        user_name = UserName,
                        pid = Pid,
                        monitor_ref = erlang:monitor(process, Pid)
                    },
                    NewState = State#state{
                        users_by_name = ByName#{UserName => Ui},
                        users_by_pid = ByPid#{Pid => Ui}
                    },
                    %% greet user
                    %ph_handler_3_budget_chat:msg(Pid, <<"* Hello ",UserName/binary>>),

                    % send list of all other users
                    Users = iolist_to_binary(lists:join($,, maps:keys(State#state.users_by_name))),
                    ph_handler_3_budget_chat:msg(Pid, <<"* Users online: ",Users/binary>>),

                    % notify other users
                    uinfo_bcast_except(<<"* User ",UserName/binary," joined">>, Pid, NewState),
                    {ok, NewState}
            end
    end.


uinfo_bcast_except(Msg, SkipPid, State) ->
    maps:foreach(
        fun
            (_, #user_info{pid = P}) when P =:= SkipPid ->
                ok;
            (_, #user_info{pid = P}) ->
                ph_handler_3_budget_chat:msg(P, Msg)
        end,
        State#state.users_by_pid
    ).


username_valid(N) ->
    PtKey = ph_chat_srv_re,
    Re = case persistent_term:get(PtKey, undefined) of
        undefined ->
            {ok, Mp} = re:compile(<<"^[[:alnum:]]+$">>),
            persistent_term:put(PtKey, Mp),
            Mp;
        Mp ->
            Mp
    end,
    case re:run(N, Re) of
        {match, _} -> true;
        nomatch -> false
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

username_valid_test() ->
    ?assertNot(username_valid(<<>>)),
    ?assertNot(username_valid(<<" ">>)),
    ?assert(username_valid(<<"qwe">>)).

-endif.

