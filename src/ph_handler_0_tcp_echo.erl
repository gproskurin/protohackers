-module(ph_handler_0_tcp_echo).

-export([handle_data/3]).

handle_data(S, Data, HState) ->
    ok = gen_tcp:send(S, Data),
    {<<>>, HState}.

