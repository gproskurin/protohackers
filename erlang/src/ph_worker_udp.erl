-module(ph_worker_udp).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("include/ph.hrl").

-export([
    start_link/1,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    socket,
    service_info,
    handler_state = undefined
}).

start_link(#ph_service_info_udp{} = Si) ->
    gen_server:start_link(?MODULE, Si, []).


init(#ph_service_info_udp{port = Port} = Si) ->
    {ok, ListenSocket} = socket:open(inet, dgram, udp),
    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, reuseport}, true),
    ok = socket:bind(ListenSocket, #{family => inet, port => Port}),
    State = #state{
        socket = ListenSocket,
        service_info = Si
    },
    self() ! recv_once,
    ?LOG_NOTICE("worker/UDP finish INIT: self=~p listen_socket=~p service_info=~p", [self(), ListenSocket, Si]),
    {ok, State}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(recv_once, State) ->
    recv_once(State);

handle_info({'$socket', S, select, _SelectInfo}, #state{socket=S} = State) ->
    recv_once(State);

handle_info(Info, State) ->
    ?LOG_ERROR("WORKER_UDP unhandled info: self=~p info=~p state=~p", [self(), Info, State]),
    {noreply, State}.


%%

process_data_udp(Peer, Data, State) ->
    #state{
        socket = S,
        service_info = #ph_service_info_udp{module = Mod},
        handler_state = Hs
    } = State,
    NewHs = Mod:handle_data({S,Peer}, Data, Hs),
    State#state{handler_state = NewHs}.


recv_once(State) ->
    NewState = case socket:recvfrom(State#state.socket, [], nowait) of
        {select, _SelectInfo} ->
            State;
        {select_read, {_SelectInfo, {Peer, Data}}} ->
            process_data_udp(Peer, Data, State);
        {ok, {Peer, Data}} ->
            self() ! recv_once,
            process_data_udp(Peer, Data, State)
    end,
    {noreply, NewState}.

