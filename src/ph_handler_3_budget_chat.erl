-module(ph_handler_3_budget_chat).

-include_lib("kernel/include/logger.hrl").

-export([
    msg/2,
    disconnect/1,

    handle_connect/2,
    handle_data/3,
    handle_cast/3
]).

-record(hstate, {
    user = undefined
}).


msg(Pid, Msg) ->
    gen_server:cast(Pid, {msg, Msg}).


disconnect(Pid) ->
    gen_server:cast(Pid, disconnect).


handle_connect(S, Hs) ->
    ?LOG_NOTICE("CHAT_HANDLER: connect: socket=~p hstate=~p", [S, Hs]),
    ok = ph_3_chat_srv:connect(),
    Hs.


handle_data(_S, Line, undefined = _HState) ->
    ?LOG_NOTICE("CHAT_HANDLER: new user: user_name=~p pid=~p", [Line, self()]),
    ok = ph_3_chat_srv:new_user(Line),
    {<<>>, #hstate{user = Line}};

handle_data(_S, Line, HState) ->
    ok = ph_3_chat_srv:msg(Line),
    {<<>>, HState}.


handle_cast(S, {msg, Msg}, Hs) ->
    ok = gen_tcp:send(S, [Msg, $\n]),
    Hs;

handle_cast(_S, disconnect, _Hs) ->
    ?LOG_ERROR("CHAT_HANDLER: disconnecting: self=~p", [self()]),
    stop;

handle_cast(S, Msg, Hs) ->
    ?LOG_ERROR("CHAT_HANDLER: unknown cast: socket=~p msg=~p hstate=~p", [S, Msg, Hs]),
    Hs.

