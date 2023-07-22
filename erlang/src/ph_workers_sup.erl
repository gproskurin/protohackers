-module(ph_workers_sup).

-behaviour(supervisor).

-include("include/ph.hrl").

-export([
    start_link/1,
    start_worker/3,

    init/1
]).


start_link(#ph_service_info{workers_sup = WorkersSup}) ->
    supervisor:start_link({local, WorkersSup}, ?MODULE, {}).


start_worker(WorkersSup, Socket, Hinfo) ->
    supervisor:start_child(WorkersSup, [Socket, Hinfo]).


init(_) ->
    ChildSpec = #{
        id => simple_id,
        type => worker,
        start => {ph_worker, start_link, []},
        restart => temporary
    },
    SupFlags = #{
        strategy => simple_one_for_one
    },
    {ok, {SupFlags, [ChildSpec]}}.

