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
    LongAscii = lists:duplicate(96, $x),
    LongUnicode = lists:duplicate(75, 955),
    OneDotText = lists:duplicate(62, $o),
    ZeroDotText = lists:duplicate(63, $z),
    BinaryText = [955 | " BIN~line\n\tend"],
    CharlistText = [1046 | " LIST~line\n\tend"],
    FunctionText = [955 | "_function"],
    SkipText = [1046 | "_skip_name"],
    Eval = formatter_eval(LongAscii),
    Output = run_erl(Eval),
    Expected = unicode:characters_to_binary(
                 lists:flatten(
                   lists:join("\n",
                     [expected_line(LongAscii, ""),
                      expected_line("short", ""),
                      expected_line(OneDotText, ""),
                      expected_line(ZeroDotText, ""),
                      expected_line(BinaryText, ""),
                      expected_line(CharlistText, ""),
                      expected_line(FunctionText, ""),
                      expected_line(LongUnicode, ""),
                      expected_line(SkipText, ". [skip]"),
                      ""]))),
    ?assertEqual(Expected, Output),
    ?assertEqual(nomatch, binary:match(Output, <<27>>)).

formatter_eval(LongText) ->
    Long = lists:flatten(io_lib:format("~p", [LongText])),
    lists:flatten([
      "State = {state, undefined, unit, false, 0, 0, 0, 0, 0, 0}, ",
      "BinaryText = unicode:characters_to_binary([955 | ",
      "\" BIN~line\\n\\tend\"]), ",
      "CharlistText = [1046 | \" LIST~line\\n\\tend\"], ",
      "LongUnicode = lists:duplicate(75, 955), ",
      "OneDotText = lists:duplicate(62, $o), ",
      "ZeroDotText = lists:duplicate(63, $z), ",
      "'ltest-formatter':'func-line'(lambda_test, ", Long, "), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(short_test, undefined), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(lambda_test, OneDotText), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(lambda_test, ZeroDotText), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(lambda_test, BinaryText), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(lambda_test, CharlistText), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(list_to_atom([955 | ",
      "\"_function_test\"]), undefined), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'func-line'(lambda_test, LongUnicode), ",
      "io:put_chars(\"\\n\"), ",
      "'ltest-formatter':'skip-line'({list_to_atom([1046 | ",
      "\"_skip_name_skip\"]), 0}, State), halt(0)."]).

expected_line(Text, Status) ->
    Width = 'ltest-const':'test-suite-width'(),
    Indent = 'ltest-const':'func-indent'(),
    Padding = max(0, Width - Indent - length(Text) - length(" [fail]")),
    lists:flatten([lists:duplicate(Indent, $ ), Text, " ",
                   lists:duplicate(Padding, $.), Status]).

run_erl(Eval) ->
    Erl = os:find_executable("erl"),
    AppEbin = filename:dirname(code:which('ltest-formatter')),
    PathArgs = ["-pa", AppEbin],
    EvalArg = binary_to_list(unicode:characters_to_binary(Eval)),
    Port = open_port({spawn_executable, Erl},
                     [binary, exit_status, use_stdio, stderr_to_stdout,
                      {args, ["-noshell", "-noinput"] ++ PathArgs
                             ++ ["-eval", EvalArg]}]),
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
