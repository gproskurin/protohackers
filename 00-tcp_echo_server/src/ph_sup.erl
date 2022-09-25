-module(ph_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    WorkersSup = #{
        id => ph_workers_sup,
        type => supervisor,
        start => {ph_workers_sup, start_link, []}
    },
    Acceptor = #{
        id => ph_acceptor,
        start => {ph_acceptor, start_link, []}
    },
    ChildSpecs = [WorkersSup, Acceptor],
    {ok, {SupFlags, ChildSpecs}}.


%% internal functions
