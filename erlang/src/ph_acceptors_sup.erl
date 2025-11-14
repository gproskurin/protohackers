-module(ph_acceptors_sup).

-behaviour(supervisor).

-include("include/ph.hrl").

-export([start_link/1]).
-export([init/1]).


start_link(Si) ->
    supervisor:start_link(?MODULE, Si).


init(Si) ->
    NumAcceptors = case Si#ph_service_info.proto of
        tcp -> 5;
        % for UDP, acceptor starts worker immediately, and worker handles all requests
        % one worker to keep the same state for all requests
        udp -> 1
    end,
    Acceptors = [
        #{
            id => {Si#ph_service_info.module, N},
            start => {ph_acceptor, start_link, [Si]}
        }
        || N <- lists:seq(1, NumAcceptors)
    ],
    SupFlags = #{
        strategy => one_for_one % TODO simple_one_for_one
    },
    {ok, {SupFlags, Acceptors}}.

