-module(ph_service_sup).

-behaviour(supervisor).

-include("include/ph.hrl").

-export([start_link/1]).
-export([init/1]).


start_link(Si) ->
    supervisor:start_link(?MODULE, Si).


init(Si) ->
    Children0 = case Si of
        #ph_service_info_tcp{} ->
            % For TCP, start supervisor for workers and a few acceptors
            WorkersSup = #{
                id => workers_sup,
                type => supervisor,
                start => {ph_workers_sup, start_link, [Si]}
            },
            Acceptors = [
                #{
                    id => {Si#ph_service_info_tcp.module, N},
                    start => {ph_acceptor_tcp, start_link, [Si]}
                }
                || N <- lists:seq(1, 5)
            ],
            [WorkersSup | Acceptors];
        #ph_service_info_udp{} ->
            % For UDP, we don't use acceptors, just start worker which handles
            % all socket operations and requests.
            % Use one worker to keep the same state for all requests.
            [
                #{
                    id => worker,
                    start => {ph_worker_udp, start_link, [Si]}
                }
            ]
    end,

    % Get additional children if required by service
    Mod = case Si of
        #ph_service_info_tcp{module = M} -> M;
        #ph_service_info_udp{module = M} -> M
    end,
    {module, _} = code:ensure_loaded(Mod),
    Children = case erlang:function_exported(Mod, get_children, 1) of
        true -> Mod:get_children(Children0);
        false -> Children0
    end,

    SupFlags = #{
        strategy => one_for_one
    },
    {ok, {SupFlags, Children}}.

