-module(ph_acceptor_tcp).

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
    service_info,
    listen_socket
}).


start_link(#ph_service_info_tcp{} = Si) ->
    gen_server:start_link(?MODULE, Si, []).


init(#ph_service_info_tcp{port = Port} = Si) ->
    {ok, ListenSocket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, reuseport}, true),
    ok = socket:bind(ListenSocket, #{family => inet, port => Port}),
    ok = socket:listen(ListenSocket, 1024),
    State = #state{
        service_info = Si,
        listen_socket = ListenSocket
    },
    self() ! accept_once,
    ?LOG_NOTICE("ACCEPTOR_TCP: init done: state=~p", [State]),
    {ok, State}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(accept_once, State) ->
    accept_once(State),
    {noreply, State};

handle_info({'$socket', _S, select, _SelectInfo}, State) ->
    accept_once(State),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_NOTICE("ACCEPTOR_TCP: inhandled info: info=~p state=~p", [Info, State]),
    {noreply, State}.


accept_once(#state{listen_socket = ListenSocket, service_info = Si}) ->
    case socket:accept(ListenSocket, nowait) of
        {ok, S} ->
            ok = start_worker(S, Si),
            self() ! accept_once;
        {select, _} ->
            ok
    end.


start_worker(Socket, #ph_service_info_tcp{workers_sup = WorkersSup} = Si) ->
    ?LOG_NOTICE("ACCEPTOR_TCP: starting_worker: socket=~p ph_service_info=~p", [Socket, Si]),
    {ok, Wpid} = ph_workers_sup:start_worker(WorkersSup, Socket, Si),
    ok = socket:setopt(Socket, {otp, controlling_process}, Wpid),
    Wpid ! start,
    ok.

