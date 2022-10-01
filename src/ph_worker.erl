-module(ph_worker).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/2,

    init/1,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    socket,
    handler_info,
    buffer = <<>>,
    handler_state = undefined
}).

start_link(Socket, Hinfo) ->
    gen_server:start_link(?MODULE, {Socket, Hinfo}, []).


init({Socket, Hinfo}) ->
    State = #state{
        socket = Socket,
        handler_info = Hinfo
    },
    ?LOG_NOTICE("worker INIT: state=~p handler_info=~p", [State, Hinfo]),
    {ok, State}.


handle_cast(Msg, #state{socket = S, handler_info = Hinfo, handler_state = Hs} = State) ->
    Mod = maps:get(module, Hinfo),
    NewHs = Mod:handle_cast(S, Msg, Hs),
    {noreply, State#state{handler_state = NewHs}}.


handle_info(start_recv, #state{socket = S, handler_info = Hinfo} = State) ->
    active_once(State#state.socket),

    Mod = maps:get(module, Hinfo),
    code:load_file(Mod),
    NewState = case erlang:function_exported(Mod, handle_connect, 2) of
        true ->
            ?LOG_NOTICE("WORKER: start_recv -> connect", []),
            NewHs = Mod:handle_connect(S, State#state.handler_state),
            State#state{handler_state = NewHs};
        false ->
            ?LOG_NOTICE("WORKER: start_recv -> no_connect", []),
            State
    end,
    {noreply, NewState};

handle_info({tcp, S, Data}, #state{socket=S, handler_info=Hinfo, buffer=Buffer} = State) ->
    NewState = case proplists:get_bool(readline, maps:get(options, Hinfo)) of
        true ->
            process_readline(S, State#state{buffer = <<Buffer/binary,Data/binary>>});
        false ->
            Mod = maps:get(module, Hinfo),
            {NewBuffer, NewHandlerState} = Mod:handle_data(
                S,
                <<Buffer/binary, Data/binary>>,
                State#state.handler_state
            ),
            State#state{buffer = NewBuffer, handler_state = NewHandlerState}
    end,
    active_once(S),
    {noreply, NewState};

handle_info({tcp_closed, S}, #state{socket = S} = State) ->
    ?LOG_NOTICE("worker CLOSE: state=~p", [State#state{handler_state = <<"...handler_state...">>}]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?LOG_ERROR("worker INFO: info=~p state=~p", [Info, State]),
    {noreply, State}.


process_readline(S, #state{buffer = Buffer} = State) ->
    case ph_utils:split_newline(Buffer) of
        {Line, Rest} ->
            Mod = maps:get(module, State#state.handler_info),
            {<<>>, NewHandlerState} = Mod:handle_data(S, Line, State#state.handler_state),
            process_readline(S, State#state{buffer = Rest, handler_state = NewHandlerState});
        nomatch ->
            State
    end.

active_once(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).

