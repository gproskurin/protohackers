-module(ph_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).


init(_) ->
    Children = [
        #{
            id => ph_services_sup,
            type => supervisor,
            start => {ph_services_sup, start_link, []}
        }
    ],
    SupFlags = #{
        strategy => one_for_one
    },
    {ok, {SupFlags, Children}}.

