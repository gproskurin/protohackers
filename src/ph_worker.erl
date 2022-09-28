-module(ph_worker).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/2,

    init/1,
    handle_info/2
]).

-record(state, {
    socket,
    module,
    buffer
}).

start_link(Socket, Mod) ->
    gen_server:start_link(?MODULE, {Socket, Mod}, []).


init({Socket, Mod}) ->
    State = #state{
        socket = Socket,
        module = Mod,
        buffer = <<>>
    },
    ?LOG_NOTICE("worker INIT: state=~p", [State]),
    {ok, State}.


handle_info(start_recv, State) ->
    active_once(State#state.socket),
    {noreply, State};

handle_info({tcp, S, Data}, #state{socket=S, module=Mod, buffer=Buffer} = State) ->
    NewBuffer = Mod:handle_data(S, <<Buffer/binary, Data/binary>>),
    active_once(S),
    {noreply, State#state{buffer = NewBuffer}};

handle_info({tcp_closed, S}, #state{socket = S} = State) ->
    ?LOG_NOTICE("worker CLOSE: state=~p", [State]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?LOG_ERROR("worker INFO: info=~p state=~p", [Info, State]),
    {noreply, State}.


active_once(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).

