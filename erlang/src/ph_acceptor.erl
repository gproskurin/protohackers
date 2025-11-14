-module(ph_acceptor).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("include/ph.hrl").

-export([
    start_link/1,

    init/1,
    handle_info/2
]).


-record(state, {
    service_info,
    listen_socket
}).


start_link(Si) ->
    gen_server:start_link(?MODULE, Si, []).


init(#ph_service_info{port = Port, proto = Proto} = Si) ->
    {ok, ListenSocket} = case Proto of
        tcp -> socket:open(inet, stream, tcp);
        udp -> socket:open(inet, dgram, udp)
    end,
    ok = socket:setopt(ListenSocket, {socket, reuseaddr}, true),
    ok = socket:setopt(ListenSocket, {socket, reuseport}, true),
    ok = socket:bind(ListenSocket, #{family => inet, port => Port}),
    case Proto of
        tcp ->
            ok = socket:listen(ListenSocket, 1024),
            self() ! accept_once;
        udp ->
            % for udp, worker listens socket
            ok = start_worker(ListenSocket, Si)
    end,
    State = #state{
        service_info = Si,
        listen_socket = ListenSocket
    },
    ?LOG_NOTICE("ACCEPTOR: init done: state=~p", [State]),
    {ok, State}.


handle_info(accept_once, State) ->
    accept_once(State),
    {noreply, State};

handle_info({'$socket', _S, select, _SelectInfo}, State) ->
    accept_once(State),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_NOTICE("acceptor INFO: info=~p state=~p", [Info, State]),
    {noreply, State}.


accept_once(#state{listen_socket = ListenSocket, service_info = #ph_service_info{proto = tcp} = Si}) ->
    case socket:accept(ListenSocket, nowait) of
        {ok, S} ->
            ok = start_worker(S, Si),
            self() ! accept_once;
        {select, _} ->
            ok
    end.


start_worker(Socket, #ph_service_info{workers_sup = WorkersSup} = Si) ->
    ?LOG_NOTICE("acceptor - STARTING_WORKER: socket=~p ph_service_info=~p", [Socket, Si]),
    {ok, Wpid} = ph_workers_sup:start_worker(WorkersSup, Socket, Si),
    ok = socket:setopt(Socket, {otp, controlling_process}, Wpid),
    Wpid ! start,
    ok.

