-module(ph_acceptor).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/0,

    init/1,
    handle_info/2
]).

-record(state, {
    socket_info = []
}).

-define(ACCEPT_POLL_INTERVAL, 300).

-define(HANDLERS_INFO, [
    #{
        port => 50000,
        module => ph_handler_0_tcp_echo,
        options => []
    },
    #{
        port => 50001,
        module => ph_handler_1_prime_time,
        options => [{readline, true}]
    },
    #{
        port => 50002,
        module => ph_handler_2_means_to_an_end,
        options => []
    },
    #{
        port => 50003,
        module => ph_handler_3_budget_chat,
        options => [{readline, true}]
    }
]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


init(_) ->
    Sh = lists:map(
        fun (H) ->
            {ok, S} = gen_tcp:listen(maps:get(port, H), [binary, {reuseaddr,true}, {active,false}]),
            {S, H}
        end,
        ?HANDLERS_INFO
    ),
    self() ! accept,
    {ok, #state{socket_info = Sh}}.


handle_info(accept, State) ->
    lists:foreach(
        fun ({S, Hinfo}) ->
            case gen_tcp:accept(S, ?ACCEPT_POLL_INTERVAL) of
                {ok, Sa} ->
                    ok = start_worker(Sa, Hinfo);
                {error, timeout} ->
                    ok
            end
        end,
        State#state.socket_info
    ),
    self() ! accept,
    {noreply, State};

handle_info(Data, State) ->
    ?LOG_NOTICE("acceptor INFO: data=~p state=~p", [Data, State]),
    {noreply, State}.


start_worker(S, Hinfo) ->
    ?LOG_NOTICE("acceptor - STARTING_WORKER: socket=~p handler_info=~p", [S, Hinfo]),
    {ok, Wpid} = ph_workers_sup:start_worker(S, Hinfo),
    ok = gen_tcp:controlling_process(S, Wpid),
    Wpid ! start_recv,
    ok.

