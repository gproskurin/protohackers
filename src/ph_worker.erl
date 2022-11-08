-module(ph_worker).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("include/ph.hrl").

-export([
    start_link/2,

    init/1,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    socket,
    service_info,
    buffer = <<>>,
    handler_state = undefined
}).

start_link(Socket, Si) ->
    gen_server:start_link(?MODULE, {Socket, Si}, []).


init({Socket, Si}) ->
    State = #state{
        socket = Socket,
        service_info = Si
    },
    ?LOG_NOTICE("worker finish INIT: self=~p socket=~p service_info=~p", [self(), Socket, Si]),
    {ok, State}.


handle_cast(Msg, #state{socket = S, service_info = #ph_service_info{module = Mod}, handler_state = Hs} = State) ->
    case Mod:handle_cast(S, Msg, Hs) of
        stop ->
            {stop, normal, State};
        NewHs ->
            {noreply, State#state{handler_state = NewHs}}
    end.


handle_info(start, State) ->
    self() ! recv_once,
    NewState = process_connect(State),
    {noreply, NewState};

handle_info(recv_once, State) ->
    recv_once(State);

handle_info({'$socket', S, select, _SelectInfo}, #state{socket=S} = State) ->
    recv_once(State);

handle_info(Info, State) ->
    ?LOG_ERROR("worker INFO: self=~p info=~p state=~p", [self(), Info, State]),
    {noreply, State}.


%%

process_connect(#state{service_info = #ph_service_info{module = Mod}} = State) ->
    case erlang:function_exported(Mod, handle_connect, 2) of
        true ->
            ?LOG_NOTICE("WORKER: self=~p process_connect -> handle", [self()]),
            NewHs = Mod:handle_connect(State#state.socket, State#state.handler_state),
            State#state{handler_state = NewHs};
        false ->
            ?LOG_NOTICE("WORKER: self=~p process_connect -> skip", [self()]),
            State
    end.


process_data(#state{service_info = #ph_service_info{module = Mod, options = Opts}} = State) ->
    case proplists:get_value(readline, Opts, false) of
        true ->
            process_readline(State);
        false ->
            #state{socket = S, buffer = Buffer, handler_state = Hs} = State,
            {NewBuffer, NewHs} = Mod:handle_data(S, Buffer, Hs),
            State#state{buffer = NewBuffer, handler_state = NewHs}
    end.


process_readline(#state{buffer = Buffer} = State) ->
    case ph_utils:split_newline(Buffer) of
        {Line, Rest} ->
            #state{service_info = #ph_service_info{module = Mod}, socket = S, handler_state = Hs} = State,
            {<<>>, NewHs} = Mod:handle_data(S, Line, Hs),
            process_readline(State#state{buffer = Rest, handler_state = NewHs});
        nomatch ->
            State
    end.


recv_once(State) ->
    NewState = case socket:recv(State#state.socket, [], nowait) of
        {ok, Data} ->
            ?LOG_NOTICE("WORKER: recv->data self=~p", [self()]),
            self() ! recv_once,
            process_data(State#state{buffer = <<(State#state.buffer)/binary, Data/binary>>});
        {select, {_SelectInfo, Data}} ->
            ?LOG_NOTICE("WORKER: recv->select+data self=~p", [self()]),
            process_data(State#state{buffer = <<(State#state.buffer)/binary, Data/binary>>});
        {select, _SelectInfo} ->
            ?LOG_NOTICE("WORKER: recv->select self=~p", [self()]),
            State;
        {error, closed} ->
            ?LOG_NOTICE("WORKER: recv->closed self=~p", [self()]),
            stop
    end,
    case NewState of
        stop ->
            {stop, normal, State};
        _ ->
            {noreply, NewState}
    end.

