-module(ph_handler_2_means_to_an_end).

-include_lib("kernel/include/logger.hrl").

-export([handle_data/3]).

-record(hstate, {
    data = undefined
}).


new_hstate() ->
    #hstate{
        data = data_new()
    }.


handle_data(_S, Data, HState) when erlang:size(Data) < 9 ->
    {Data, HState};

handle_data(S, Data, undefined = _HState) ->
    handle_data(S, Data, new_hstate());

handle_data(S, <<"I",Ts:4/big-signed-integer-unit:8,Pr:4/big-signed-integer-unit:8,Rest/binary>>, HState) ->
    ?LOG_NOTICE("I ~p ~p", [Ts,Pr]),
    NewData = data_add(HState#hstate.data, Ts, Pr),
    handle_data(S, Rest, HState#hstate{data = NewData});

handle_data(S, <<"Q",Ts1:4/big-signed-integer-unit:8,Ts2:4/big-signed-integer-unit:8,Rest/binary>>, HState) ->
    ?LOG_NOTICE("Q ~p ~p", [Ts1,Ts2]),
    R = query(HState#hstate.data, Ts1, Ts2),
    ?LOG_NOTICE(" -> ~p", [R]),
    ok = socket:send(S, <<R:4/big-signed-integer-unit:8>>),
    handle_data(S, Rest, HState);

handle_data(_S, Data, _HState) ->
    ?LOG_ERROR("Unsupported data: data=~p", [Data]),
    throw(value_error).


query(Data, Ts1, Ts2) ->
    FoldFun = fun({_K,V}, {AccSum, AccCount}) -> {AccSum+V, AccCount+1} end,
    {S,C} = data_fold(Data, Ts1, Ts2, FoldFun, {0,0}),
    ?LOG_NOTICE(" === MEAN: sum=~p count=~p", [S,C]),
    case C of
        0 -> 0;
        _ -> erlang:round(S / C)
    end.


data_new() ->
    gb_trees:empty().

data_add(D, K, V) ->
    gb_trees:insert(K, V, D).

data_fold(D, Kfirst, Klast, FoldFun, FoldAcc) ->
    Iter = gb_trees:iterator_from(Kfirst, D),
    do_iter(Iter, Klast, FoldFun, FoldAcc).


do_iter(Iter, Klast, FoldFun, FoldAcc) ->
    case gb_trees:next(Iter) of
        none -> FoldAcc;
        {K, V, IterNext} ->
            case K =< Klast of
                false -> FoldAcc;
                true ->
                    NewAcc = FoldFun({K, V}, FoldAcc),
                    do_iter(IterNext, Klast, FoldFun, NewAcc)
            end
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


data_test() ->
    Kv = [{1, 10}, {5,50}, {3,30}, {8,80}, {4,40}],
    D = lists:foldl(
        fun({K,V}, AccD) -> data_add(AccD, K, V) end,
        data_new(),
        Kv
    ),
    ?assertEqual(
        lists:sort(Kv), % NOTE: order is not well defined for tuples?
        gb_trees:to_list(D)
    ),

    FoldFun = fun(Item, Acc) -> [Item | Acc] end,
    ?assertEqual(
        [{5,50},{4,40},{3,30}],
        data_fold(D, 2, 6, FoldFun, [])
    ),
    ?assertEqual(
        [{3,30},{1,10}],
        data_fold(D, 0, 3, FoldFun, [])
    ),
    ?assertEqual(
        [{8,80},{5,50}],
        data_fold(D, 5, 10, FoldFun, [])
    ),

    ?assertEqual(
        40,
        query(D, 2, 6)
    ),

    ?assertEqual(
        0,
        query(data_new(), 2, 6)
    ).


-endif.

