-module(ph_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => rest_for_one
    },
    ChatServer = #{
        id => ph_chat_srv,
        start => {ph_3_chat_srv, start_link, []}
    },
    Acceptor = #{
        id => ph_acceptor,
        start => {ph_acceptor, start_link, []}
    },
    WorkersSup = #{
        id => ph_workers_sup,
        type => supervisor,
        start => {ph_workers_sup, start_link, []}
    },
    ChildSpecs = [ChatServer, Acceptor, WorkersSup],
    {ok, {SupFlags, ChildSpecs}}.


%% internal functions
