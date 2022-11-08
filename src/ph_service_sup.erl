-module(ph_service_sup).

-behaviour(supervisor).

-include("include/ph.hrl").

-export([start_link/1]).
-export([init/1]).


start_link(Si) ->
    supervisor:start_link(?MODULE, Si).


init(Si) ->
    Children0 = [
        #{
            id => workers_sup,
            type => supervisor,
            start => {ph_workers_sup, start_link, [Si]}
        },
        #{
            id => acceptors_sup,
            type => supervisor,
            start => {ph_acceptors_sup, start_link, [Si]}
        }
    ],
    Mod = Si#ph_service_info.module,
    {module, _} = code:ensure_loaded(Mod),
    Children = case erlang:function_exported(Mod, get_children, 1) of
        true -> Mod:get_children(Children0);
        false -> Children0
    end,
    SupFlags = #{
        strategy => one_for_one
    },
    {ok, {SupFlags, Children}}.

