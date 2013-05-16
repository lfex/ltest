-module(erl_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


basic_test_() ->
    fun () -> ?assert(1 + 1 =:= 2) end.

simple_test() ->
    ?assert(1 + 1 =:= 2).

basic2_test_() ->
    ?_test(?assert(1 + 1 =:= 2)).

basic3_test_() ->
    ?_assert(1 + 1 =:= 2).

assertEqual_test () ->
    ?assertEqual("b" ++ "a", lists:reverse("ab")).

assertEqual2_test () ->
    ?assertEqual(0, 1).

doIt ()