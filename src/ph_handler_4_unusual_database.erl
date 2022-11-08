-module(ph_handler_4_unusual_database).

-export([handle_data/3]).

handle_data(S, Data, HState) ->
    ok = socket:send(S, Data),
    {<<>>, HState}.

