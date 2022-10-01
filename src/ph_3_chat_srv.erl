-module(ph_3_chat_srv).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ph_chat_srv).

-export([
    start_link/0,

    init/1,
    handle_info/2
]).


-record(state, {
}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, undefined, []).


init(_) ->
    {ok, #state{}}.


handle_info(Data, State) ->
    ?LOG_NOTICE("CHAT_SRV: info: data=~p", [Data]),
    {noreply, State}.

