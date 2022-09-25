-module(ph_workers_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
    start_link/0,
    start_worker/2,

    init/1
]).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, undefined).


start_worker(Socket, Mod) ->
    ChildSpec = #{
        id => Socket,
        start => {ph_worker, start_link, [Socket, Mod]},
        restart => temporary
    },
    supervisor:start_child(?SERVER, ChildSpec).


init(_) ->
    SupFlags = #{
        strategy => one_for_one
    },
    {ok, {SupFlags, []}}.

