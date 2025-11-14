-module(ph_handler_4_unusual_database).

-export([handle_data/3]).

-define(KEY_VERSION, <<"version">>).
-define(REPLY_VERSION, <<?KEY_VERSION/binary,"=Unusual Database v1.0">>).


handle_data(S, Data, undefined) ->
    handle_data(S, Data, #{});

handle_data(S, Data, Map) ->
    case ph_utils:split_binary_eq(Data) of
        {?KEY_VERSION, _Value} ->
            % ignore attempts to set "version" key
            {<<>>, Map};
        {Key, Value} ->
            % set
            NewMap = db_update(Key, Value, Map),
            udp_reply(S, [Key, $= | Value]), % improper list
            {<<>>, NewMap};
        ?KEY_VERSION ->
            udp_reply(S, ?REPLY_VERSION),
            {<<>>, Map};
        Key ->
            % get
            case db_find(Key, Map) of
                {ok, Value} ->
                    udp_reply(S, [Key, $= | Value]),
                    {<<>>, Map};
                error ->
                    {<<>>, Map}
            end
    end.


db_update(Key, Value, Map) ->
    Map#{Key => Value}.


db_find(Key, Map) ->
    maps:find(Key, Map).


udp_reply({S, Peer}, Data) ->
    ok = socket:sendto(S, Data, Peer).

