-module(ph_handler_0_tcp_echo).

-export([handle_data/2]).

handle_data(S, Data) ->
    gen_tcp:send(S, Data).

