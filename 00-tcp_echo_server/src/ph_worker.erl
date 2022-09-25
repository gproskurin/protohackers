-module(ph_worker).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/1,

    init/1,
    handle_info/2
]).

-record(state, {
    socket
}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).


init(Socket) ->
    ?LOG_NOTICE("worker INIT: socket=~p", [Socket]),
    {ok, #state{socket = Socket}}.


handle_info(start_recv, State) ->
    active_once(State#state.socket),
    {noreply, State};

handle_info({tcp, S, Data}, #state{socket = S} = State) ->
    ok = gen_tcp:send(S, Data),
    active_once(S),
    {noreply, State};

handle_info({tcp_closed, S}, #state{socket = S} = State) ->
    ?LOG_NOTICE("worker CLOSE: state=~p", [State]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?LOG_ERROR("worker INFO: info=~p state=~p", [Info, State]),
    {noreply, State}.


active_once(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).

