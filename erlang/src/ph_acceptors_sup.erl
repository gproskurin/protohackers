-module(ph_acceptors_sup).

-behaviour(supervisor).

-include("include/ph.hrl").

-export([start_link/1]).
-export([init/1]).


start_link(Si) ->
    supervisor:start_link(?MODULE, Si).


init(Si) ->
    Acceptors = [
        #{
            id => {Si#ph_service_info.module, N},
            start => {ph_acceptor, start_link, [Si]}
        }
        || N <- lists:seq(1, 5)
    ],
    SupFlags = #{
        strategy => one_for_one % TODO simple_one_for_one
    },
    {ok, {SupFlags, Acceptors}}.

