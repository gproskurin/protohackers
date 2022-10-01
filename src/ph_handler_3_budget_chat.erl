-module(ph_handler_3_budget_chat).

-export([
    handle_data/3
]).

-record(hstate, {
}).


handle_data(_S, _Line, HState) ->
    {<<>>, HState}.

