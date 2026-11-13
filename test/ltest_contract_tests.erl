-module(ltest_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-export([real_skip_skip/0, wrong_skip/1, middle_skip_extra/0,
         'λ_skip'/0]).

real_skip_skip() -> ok.
wrong_skip(Value) -> Value.
middle_skip_extra() -> ok.
contains_skip_test_() -> {timeout, 1, fun() -> ok end}.
'λ_skip'() -> ok.

skip_export_contract_test() ->
    Exports = [{real_skip_skip, 0}, {wrong_skip, 1},
               {middle_skip_extra, 0}, {contains_skip_test_, 0},
               {'λ_skip', 0}],
    Expected = lists:sort([{real_skip_skip, 0}, {'λ_skip', 0}]),
    ?assertEqual(Expected, lists:sort(ltest:'get-skip-funcs'(?MODULE))),
    ?assertEqual(lists:sort(["real_skip", "λ"]),
                 lists:sort(ltest:'get-skipped-tests'(?MODULE))),
    ?assertEqual([{real_skip_skip, 0}, false, false, false, {'λ_skip', 0}],
                 ltest:'check-skip-funcs'(Exports)),
    ?assertEqual(["real_skip", false, false, false, "λ"],
                 ltest:'check-skipped-tests'(Exports)),
    ?assertEqual(Expected, 'ltest-util':'filter-skipped'(Exports)),
    ?assert('ltest-util':'skipped?'({'λ_skip', 0})),
    ?assertNot('ltest-util':'skipped?'({wrong_skip, 1})),
    ?assert('ltest-util':'skip-match?'({match, []})),
    ?assertNot('ltest-util':'skip-match?'(nomatch)).

elision_codepoint_boundaries_test() ->
    ZeroPaddingLength = 'ltest-const':'test-suite-width'()
                        - 'ltest-const':'func-indent'()
                        - length(" [fail]"),
    ?assertEqual(".", 'ltest-formatter':'get-elision'(
                         lists:duplicate(ZeroPaddingLength - 1, $a))),
    ?assertEqual("", 'ltest-formatter':'get-elision'(
                        lists:duplicate(ZeroPaddingLength, $a))),
    ?assertEqual("", 'ltest-formatter':'get-elision'(
                        lists:duplicate(ZeroPaddingLength + 1, $a))),
    ?assertEqual(".", 'ltest-formatter':'get-elision'(
                         unicode:characters_to_binary(
                           lists:duplicate(ZeroPaddingLength - 1, $λ)))).

unicode_formatter_output_test() ->
    LongText = lists:duplicate(96, $x),
    Eval = formatter_eval(LongText),
    Output = run_erl(Eval),
    ?assertNotEqual(nomatch, binary:match(Output, list_to_binary(LongText))),
    ?assertNotEqual(nomatch, binary:match(Output, <<"short">>)),
    UnicodeDescription = unicode:characters_to_binary("λ description Ж"),
    UnicodeName = unicode:characters_to_binary("λ"),
    ?assertNotEqual(nomatch, binary:match(Output, UnicodeDescription)),
    ?assertNotEqual(nomatch, binary:match(Output, UnicodeName)),
    ?assertNotEqual(nomatch, binary:match(Output,
                                         <<"literal ~ newline\n tab\t">>)).

formatter_eval(LongText) ->
    Long = io_lib:format("~p", [LongText]),
    Format = lists:flatten([
      "State = {state, undefined, unit, false, 0, 0, 0, 0, 0, 0}, ",
      "'ltest-formatter':'func-line'(lambda_test, ~s), ",
      "'ltest-formatter':'func-line'(lambda_test, ",
      "<<\"λ description Ж\"/utf8>>), ",
      "'ltest-formatter':'func-line'(short_test, undefined), ",
      "'ltest-formatter':'func-line'('λ_test', undefined), ",
      "'ltest-formatter':'skip-line'({'λ_skip', 0}, State), ",
      "io:put_chars(\"literal ~~ newline\\n tab\\t\\n\"), halt(0)."]),
    lists:flatten(io_lib:format(Format, [Long])).

run_erl(Eval) ->
    Erl = os:find_executable("erl"),
    AppEbin = filename:dirname(code:which('ltest-formatter')),
    PathArgs = ["-pa", AppEbin],
    Port = open_port({spawn_executable, Erl},
                     [binary, exit_status, use_stdio, stderr_to_stdout,
                      {args, ["-noshell", "-noinput"] ++ PathArgs
                             ++ ["-eval", Eval]}]),
    collect(Port, []).

collect(Port, Chunks) ->
    receive
        {Port, {data, Data}} -> collect(Port, [Data | Chunks]);
        {Port, {exit_status, 0}} -> iolist_to_binary(lists:reverse(Chunks));
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Chunks)),
            erlang:error({formatter_child_failed, Status, Output})
    after 30000 ->
        port_close(Port),
        erlang:error(formatter_child_timeout)
    end.
