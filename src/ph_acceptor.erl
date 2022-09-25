-module(ph_acceptor).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/0,

    init/1,
    handle_info/2
]).

-record(state, {
    listen_socket
}).

-define(ACCEPT_POLL_INTERVAL, 1000).
-define(LISTEN_PORT, 50000).
-define(MOD_HANDLER, ph_handler_0_tcp_echo).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(_) ->
    ?LOG_NOTICE("STARTING..."),
    {ok, Sl} = gen_tcp:listen(50000, [binary, {active, false}]),
    self() ! accept,
    {ok, #state{listen_socket = Sl}}.


handle_info(accept, #state{listen_socket = Sl} = State) ->
    case gen_tcp:accept(Sl, ?ACCEPT_POLL_INTERVAL) of
        {ok, S} ->
            ok = start_worker(S);
        {error, timeout} ->
            ok
    end,
    self() ! accept,
    {noreply, State};

handle_info(Data, State) ->
    ?LOG_NOTICE("acceptor INFO: data=~p state=~p", [Data, State]),
    {noreply, State}.


start_worker(S) ->
    ?LOG_NOTICE("acceptor - STARTING_WORKER: socket=~p", [S]),
    {ok, Wpid} = ph_workers_sup:start_worker(S, ?MOD_HANDLER),
    ok = gen_tcp:controlling_process(S, Wpid),
    Wpid ! start_recv,
    ok.

