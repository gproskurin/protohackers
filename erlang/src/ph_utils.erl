-module(ph_utils).

-export([
    split_newline/1,
    json_encode/1,
    json_decode/1
]).


split_newline(Str) ->
    split_by(Str, <<"\n">>).

split_by(Str, Pattern) ->
    case string:find(Str, Pattern) of
        nomatch ->
            nomatch;
        End ->
            LineLen = string:length(Str) - string:length(End),
            Line = string:slice(Str, 0, LineLen),
            Rest = string:slice(End, string:length(Pattern)),
            {Line, Rest}
    end.


json_encode(D) ->
    json:encode(D).


json_decode(J) ->
    json:decode(J).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_newline_test() ->
    ?assertEqual(nomatch, split_by(<<"qwerty">>, <<"Q">>)),
    ?assertEqual(
        {<<"qwer">>, <<"asdf">>},
        split_by(<<"qwerZXasdf">>, <<"ZX">>)
    ),
    ?assertEqual(
        {<<"qwer">>, <<"asdf\n">>},
        split_newline(<<"qwer\nasdf\n">>)
    ).


json_test() ->
    M = #{<<"qwe">> => 1, 2 => [<<"q">>, 2, #{<<"a">> => 1, b => 2}]},
    J = json_encode(M),
    M1 = json_decode(J),
    ?assertEqual(M, M1).


-endif.

