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
    {ok, #state{}}.


handle_call({connect, Pid}, _From, State) ->
    ?LOG_NOTICE("CHAT_SRV: connect: pid=~p", [Pid]),
    ph_handler_3_budget_chat:msg(Pid, <<"Welcome to chat!">>),
    {reply, ok, State};

handle_call({new_user, UserName, Pid}, _From, State) ->
    case uinfo_new_user(UserName, Pid, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, uinfo_conflict} ->
            {reply, {error, conflict}, State}
    end;

handle_call({msg, Msg, FromPid}, _From, State) ->
    case maps:find(FromPid, State#state.users_by_pid) of
        error ->
            {reply, {error, pid_not_found}, State};
        {ok, #user_info{user_name = FromName}} ->
            M = <<"[",FromName/binary,"] ",Msg/binary>>,
            maps:fold( % TODO foreach
                fun
                    (_, #user_info{pid = P}, Acc) when P =:= FromPid ->
                        Acc;
                    (_, #user_info{pid = P}, Acc) ->
                        ph_handler_3_budget_chat:msg(P, M),
                        Acc
                end,
                undefined,
                State#state.users_by_pid
            ),
            {reply, ok, State}
    end;

handle_call(Req, From, _State) ->
    ?LOG_ERROR("CHAT_SRV: unknown call: request=~p from=~p", [Req, From]).


handle_info({'DOWN', _Mref, process, Pid, Info}, State) ->
    ?LOG_WARNING("User disconnected: pid=~p info=~p", [Pid, Info]),
    {noreply, uinfo_remove_by_pid(State, Pid)};

handle_info(Data, State) ->
    ?LOG_NOTICE("CHAT_SRV: info: data=~p", [Data]),
    {noreply, State}.


uinfo_remove_by_pid(State, Pid) ->
    case maps:take(Pid, State#state.users_by_pid) of
        {#user_info{user_name = N, monitor_ref = Mr}, NewByPid} ->
            erlang:demonitor(Mr, [flush]),
            State#state{
                users_by_name = maps:remove(N, State#state.users_by_name),
                users_by_pid = NewByPid
            };
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
                    ph_handler_3_budget_chat:msg(Pid, <<"* Hello ",UserName/binary>>),
                    {ok, NewState}
            end
    end.

