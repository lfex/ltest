-module(ltest_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

-export([selector_child_run/0, selector_empty_child_run/0,
         selector_unicode_child_run/0]).

runner_group_contract_test() ->
    Root = private_root(),
    try
        compile_fixture(Root, "runtime-unit-tests", "ltest-unit",
                        "unit_fail_test() -> record(\"unit\"), ?assert(false).\n"
                        "unit_equal_test() -> record(\"unit\"), ?assertEqual(1, 2).\n"
                        "unit_match_test() -> record(\"unit\"), ?assertMatch({ok, _}, error).\n"
                        "unit_exception_test() -> record(\"unit\"), ?assertException(error, expected, erlang:error(actual)).\n"
                        "unit_error_test() -> record(\"unit\"), erlang:error(deliberate).\n"
                        "unit_throw_test() -> record(\"unit\"), throw(deliberate_throw).\n"
                        "unit_exit_test() -> record(\"unit\"), exit(deliberate_exit).\n"
                        "unit_invalid_descriptor_test_() -> {skip, deliberate_wrapper_skip}.\n"
                        "unit_manual_skip_skip() -> record(\"manual-skip-body\"), erlang:error(manual_skip_body_executed).\n"
                        "setup_failure_test_() -> {setup, fun() -> erlang:error(deliberate_setup_failure) end, fun(_) -> ok end, [fun() -> ok end]}."),
        compile_fixture(Root, "runtime-direct-skip-tests", "ltest-unit",
                        "direct_manual_skip_skip() -> record(\"direct-skip-body\"), erlang:error(direct_skip_body_executed)."),
        compile_fixture(Root, "runtime-system-tests", "ltest-system",
                        "system_test() -> record(\"system\"), ok."),
        compile_fixture(Root, "runtime-integration-tests", "ltest-integration",
                        "integration_test() -> record(\"integration\"), ok."),
        ChildOutput = run_child(Root),
        ?assertNotEqual(nomatch, re:run(ChildOutput, "CHILD_CONTRACT_OK")),
        ?assertNotEqual(nomatch, re:run(ChildOutput, "DIRECT_MODULE_RESULT=ok")),
        case re:run(ChildOutput, "Tests: 1  Passed: 0  Skipped: 1  Failed: 0 Erred: 0") of
            nomatch -> erlang:error({direct_module_summary_missing, ChildOutput});
            _ -> ok
        end,
        ?assertEqual(nomatch, re:run(ChildOutput, "direct-skip-body")),
        NoColorOutput = run_no_color_child(Root),
        NoColorBytes = list_to_binary(NoColorOutput),
        ?assertEqual(nomatch, binary:match(NoColorBytes, <<27>>)),
        ?assertEqual(nomatch, binary:match(NoColorBytes, <<"There were no unit tests found">>)),
        case binary:match(NoColorBytes, <<"Tests: 11">>) of
            nomatch -> erlang:error({no_color_counts, NoColorOutput});
            _ -> ok
        end,
        ?assertNotEqual(nomatch, binary:match(NoColorBytes, <<"Skipped: 3">>)),
        ?assertNotEqual(nomatch, binary:match(NoColorBytes, <<"One or more tests were cancelled">>)),
        case binary:match(NoColorBytes, <<"Failed: 4 Erred: 3">>) of
            nomatch -> erlang:error({missing_error_count, NoColorOutput});
            _ -> ok
        end
    after
        remove_tree(Root)
    end.

selector_contract_test_() ->
    {timeout, 60, fun selector_contract/0}.

selector_unicode_test_() ->
    {timeout, 60, fun selector_unicode/0}.

selector_unicode() ->
    Root = private_root(),
    try
        Name = [$p, $0, $2, $-, 16#00E9, $-, $u, $n, $i, $t],
        compile_selector_fixture(
          Root, Name, "ltest-unit",
          "unicode_test() -> record(\"unicode-unit\"), ok."),
        Output = run_selector_child_eval(
                   Root, "ltest_runtime_tests:selector_unicode_child_run(), halt(0)."),
        ?assertNotEqual(nomatch, re:run(Output, "SELECTOR_UNICODE_OK"))
    after
        remove_tree(Root)
    end.

selector_unicode_child_run() ->
    Name = [$p, $0, $2, $-, 16#00E9, $-, $u, $n, $i, $t],
    NameAtom = list_to_atom(Name),
    NameBinary = unicode:characters_to_binary(Name),
    Base = #{color => false, 'suite-headers' => false},
    selector_case(Base#{modules => NameAtom}, fun ltest:unit/1,
                  ok, <<"unicode-unit\n">>, <<>>),
    selector_case(Base#{modules => NameBinary}, fun ltest:unit/1,
                  ok, <<"unicode-unit\n">>, <<>>),
    selector_case(Base#{modules => Name}, fun ltest:unit/1,
                  ok, <<"unicode-unit\n">>, <<>>),
    selector_case(Base#{suite => NameBinary}, fun ltest:run/1,
                  ok, <<"unicode-unit\n">>, <<>>),
    LambdaName = [$p, $0, $2, $-, 16#03BB, $-, $u, $n, $i, $t],
    LambdaBinary = unicode:characters_to_binary(LambdaName),
    selector_case(Base#{modules => LambdaName}, fun ltest:unit/1,
                  {error, {unmatched_modules, [LambdaBinary]}}, <<>>, <<>>),
    io:format("SELECTOR_UNICODE_OK~n").

selector_contract() ->
    Root = private_root(),
    try
        compile_selector_fixture(
          Root, "p02-a-unit", "ltest-unit",
          "a_test_() -> "
          "file:write_file(\"phases\", \"generator\\n\", [append]), "
          "{setup, fun() -> file:write_file(\"phases\", \"setup\\n\", [append]), ok end, "
          "fun(_) -> ok end, [fun() -> record(\"a-unit\") end]}."),
        compile_selector_fixture(
          Root, "p02-z-unit", "ltest-unit",
          "z_test() -> record(\"z-unit\"), ?assert(false)."),
        compile_selector_fixture(
          Root, "p02-system", "ltest-system",
          "system_test() -> record(\"system\"), ok."),
        compile_selector_fixture(
          Root, "p02-integration", "ltest-integration",
          "integration_test() -> record(\"integration\"), ok."),
        compile_selector_fixture(
          Root, "p02-unmarked", "undefined",
          "unmarked_test_() -> "
          "file:write_file(\"phases\", \"unmarked-generator\\n\", [append]), "
          "{setup, fun() -> file:write_file(\"phases\", \"unmarked-setup\\n\", [append]), ok end, "
          "fun(_) -> record(\"unmarked\") end}."),
        Output = run_selector_child_eval(
                   Root, "ltest_runtime_tests:selector_child_run(), halt(0)."),
        ?assertNotEqual(nomatch, re:run(Output, "SELECTOR_SUITE_OK"))
    after
        remove_tree(Root)
    end.

selector_child_run() ->
    A = <<"p02-a-unit">>,
    Z = <<"p02-z-unit">>,
    System = <<"p02-system">>,
    Integration = <<"p02-integration">>,
    APhases = <<"generator\nsetup\n">>,
    AllVisited = <<"a-unit\nz-unit\nsystem\nintegration\n">>,
    AllPhases = APhases,
    Base = #{color => false, 'suite-headers' => false},
    selector_case(Base#{modules => 'p02-a-unit'}, fun ltest:unit/1,
                  ok, <<"a-unit\n">>, APhases),
    selector_case(Base#{module => A}, fun ltest:run/1,
                  ok, <<"a-unit\n">>, APhases),
    selector_case(Base#{suite => "p02-a-unit"}, fun ltest:unit/1,
                  ok, <<"a-unit\n">>, APhases),
    selector_case(Base#{modules => <<"p02-z-unit, p02-a-unit, p02-a-unit">>},
                  fun ltest:unit/1, error, <<"a-unit\nz-unit\n">>, APhases),
    selector_case(Base#{modules => [Z, "p02-a-unit"], module => "p02-z-unit,p02-a-unit",
                        suite => "p02-a-unit,p02-z-unit"},
                  fun ltest:run/1, {error, tests_failed},
                  <<"a-unit\nz-unit\n">>, APhases),
    selector_case(Base#{modules => A, module => Z, suite => System},
                  fun ltest:unit/1,
                  {error, {conflicting_selectors,
                           [{modules, [A]}, {module, [Z]}, {suite, [System]}]}},
                  <<>>, <<>>),
    selector_invalid_values(Base),
    selector_invalid_unicode(Base),
    selector_absent_and_unmatched(Base, A),
    selector_group_interaction(Base, A, System),
    selector_all_union(Base, A, Z, System, Integration,
                       AllVisited, AllPhases),
    selector_unsupported_test(Base, A),
    selector_run_beams(Base, A),
    io:format("SELECTOR_SUITE_OK~n").

selector_invalid_values(Base) ->
    Invalid = [[], <<>>, <<32, 9, 13, 10>>, <<",p02-a-unit">>,
               <<"p02-a-unit,">>, <<"p02-a-unit,,p02-z-unit">>,
               <<255>>, <<16#CE>>,
               42, {bad}, #{bad => value}, [<<"p02-a-unit">> | bad],
               [97, <<"p02-a-unit">>], [[ [<<"p02-a-unit">> ]]]],
    lists:foreach(
      fun(Key) ->
          lists:foreach(
            fun(Raw) ->
                selector_case(Base#{Key => Raw}, fun ltest:unit/1,
                              {error, {invalid_selector, Key, Raw}}, <<>>, <<>>)
            end, Invalid)
      end, [modules, module, suite]),
    selector_case(Base#{modules => undefined, module => undefined,
                        suite => undefined}, fun ltest:unit/1,
                  error, <<"a-unit\nz-unit\n">>, <<"generator\nsetup\n">>).

selector_invalid_unicode(Base) ->
    InvalidUnicode = [[16#D800], [-1], [16#110000]],
    lists:foreach(
      fun(Key) ->
          lists:foreach(
            fun(Raw) ->
                selector_case(Base#{Key => Raw}, fun ltest:unit/1,
                              {error, {invalid_selector, Key, Raw}}, <<>>, <<>>)
            end, InvalidUnicode)
      end, [modules, module, suite]),
    Nested = [<<"p02-a-unit">>, [16#D800]],
    selector_case(Base#{modules => Nested}, fun ltest:unit/1,
                  {error, {invalid_selector, modules, Nested}}, <<>>, <<>>),
    Surrogate = [16#D800],
    selector_case(Base#{module => Surrogate}, fun ltest:all/1,
                  {error, {invalid_selector, module, Surrogate}}, <<>>, <<>>),
    Negative = [-1],
    selector_case(Base#{suite => Negative, 'test-type' => all}, fun ltest:run/1,
                  {error, {invalid_selector, suite, Negative}}, <<>>, <<>>),
    AboveMax = [16#110000],
    selector_case(Base#{modules => AboveMax, 'test-type' => all}, fun ltest:run/1,
                  {error, {invalid_selector, modules, AboveMax}}, <<>>, <<>>).

selector_absent_and_unmatched(Base, A) ->
    selector_case(Base#{unrelated => retained}, fun ltest:unit/1,
                  error, <<"a-unit\nz-unit\n">>, <<"generator\nsetup\n">>),
    selector_case(Base#{test => undefined, modules => A}, fun ltest:run/1,
                  ok, <<"a-unit\n">>, <<"generator\nsetup\n">>),
    selector_case(Base#{modules => 'p02-unmarked'}, fun ltest:unit/1,
                  {error, {unmatched_modules, [<<"p02-unmarked">>]}}, <<>>, <<>>),
    Fresh = <<"p02-never-interned-selector">>,
    ?assertEqual(missing_atom, existing_atom_state(Fresh)),
    selector_case(Base#{modules => [A, Fresh]}, fun ltest:unit/1,
                  {error, {unmatched_modules, [Fresh]}}, <<>>, <<>>),
    ?assertEqual(missing_atom, existing_atom_state(Fresh)),
    selector_case(Base#{modules => [<<"p02-missing-z">>, A,
                                    <<"p02-missing-a">>]}, fun ltest:unit/1,
                  {error, {unmatched_modules,
                           [<<"p02-missing-a">>, <<"p02-missing-z">>]}}, <<>>, <<>>),
    selector_case(Base#{'test-type' => unsupported, test => [], modules => []},
                  fun ltest:run/1,
                  {error, io_lib:format("Unknown test type: ~p", [unsupported])},
                  <<>>, <<>>).

selector_group_interaction(Base, A, System) ->
    selector_case(Base#{modules => A, 'test-type' => system}, fun ltest:unit/1,
                  ok, <<"a-unit\n">>, <<"generator\nsetup\n">>),
    selector_case(Base#{modules => System}, fun ltest:unit/1,
                  {error, {unmatched_modules, [System]}}, <<>>, <<>>),
    selector_case(Base#{modules => System}, fun ltest:system/1,
                  ok, <<"system\n">>, <<>>),
    selector_case(Base#{modules => <<"p02-integration">>}, fun ltest:integration/1,
                  ok, <<"integration\n">>, <<>>).

selector_all_union(Base, A, Z, System, Integration, AllVisited, AllPhases) ->
    selector_case(Base#{modules => System}, fun ltest:all/1,
                  ok, <<"system\n">>, <<>>),
    selector_case(Base#{suite => Integration}, fun ltest:all/1,
                  ok, <<"integration\n">>, <<>>),
    selector_case(Base#{modules => [Integration, System, Z, A]}, fun ltest:all/1,
                  error, AllVisited, AllPhases),
    selector_case(Base#{modules => [Integration, System, Z, A], 'test-type' => all},
                  fun ltest:run/1, {error, tests_failed}, AllVisited, AllPhases),
    selector_case(Base#{modules => [A, <<"p02-missing-all">>]}, fun ltest:all/1,
                  {error, {unmatched_modules, [<<"p02-missing-all">>]}}, <<>>, <<>>),
    selector_case(Base#{modules => <<>>, 'test-type' => all}, fun ltest:run/1,
                  {error, {invalid_selector, modules, <<>>}}, <<>>, <<>>),
    selector_case(Base#{modules => [A, <<"p02-missing-run-all">>], 'test-type' => all},
                  fun ltest:run/1,
                  {error, {unmatched_modules, [<<"p02-missing-run-all">>]}}, <<>>, <<>>),
    selector_case(Base#{modules => A, module => Z, 'test-type' => all}, fun ltest:all/1,
                  {error, {conflicting_selectors,
                           [{modules, [A]}, {module, [Z]}]}}, <<>>, <<>>).

selector_unsupported_test(Base, A) ->
    lists:foreach(
      fun(Raw) ->
          selector_case(Base#{modules => A, test => Raw}, fun ltest:unit/1,
                        {error, {unsupported_test_selector, Raw}}, <<>>, <<>>),
          selector_case(Base#{modules => A, test => Raw, 'test-type' => all},
                        fun ltest:run/1,
                        {error, {unsupported_test_selector, Raw}}, <<>>, <<>>)
      end, [[], selected_test, <<"selected_test">>]).

selector_run_beams(Base, A) ->
    Root = os:getenv("LTEST_CHILD_ROOT"),
    Ebin = filename:join([Root, "_build", "test", "lib", "selector-fixtures", "ebin"]),
    BeamA = filename:join(Ebin, "p02-a-unit"),
    BeamZ = filename:join(Ebin, "p02-z-unit"),
    Discovery = fun(_) -> [BeamA, BeamZ] end,
    selector_case(Base#{modules => A},
                  fun(Opts) -> ltest:'run-beams'(unit, Discovery, Opts) end,
                  ok, <<"a-unit\n">>, <<"generator\nsetup\n">>),
    selector_case(Base#{modules => [A, <<"p02-run-beams-missing">>]},
                  fun(Opts) -> ltest:'run-beams'(unit, Discovery, Opts) end,
                  {error, {unmatched_modules, [<<"p02-run-beams-missing">>]}},
                  <<>>, <<>>).

selector_case(Options, Caller, ExpectedResult, ExpectedVisited, ExpectedPhases) ->
    ok = file:write_file("visited", <<>>),
    ok = file:write_file("phases", <<>>),
    Result = Caller(Options),
    ?assertEqual({ExpectedResult, {ok, ExpectedVisited}, {ok, ExpectedPhases}},
                 {Result, file:read_file("visited"), file:read_file("phases")}).

existing_atom_state(Name) ->
    try binary_to_existing_atom(Name, utf8) of
        _ -> existing_atom
    catch
        error:badarg -> missing_atom
    end.

selector_empty_discovery_test_() ->
    {timeout, 60, fun selector_empty_discovery/0}.

selector_empty_discovery() ->
    Root = private_root(),
    try
        Output = run_selector_child_eval(
                   Root, "ltest_runtime_tests:selector_empty_child_run(), halt(0)."),
        ?assertNotEqual(nomatch, re:run(Output, "SELECTOR_EMPTY_OK"))
    after
        remove_tree(Root)
    end.

selector_empty_child_run() ->
    ok = file:write_file("visited", <<>>),
    ok = file:write_file("phases", <<>>),
    Selected = ltest:unit(#{modules => <<"p02-a-unit">>, color => false,
                            'suite-headers' => false}),
    ?assertEqual({error, {unmatched_modules, [<<"p02-a-unit">>]}}, Selected),
    ?assertEqual({ok, <<>>}, file:read_file("visited")),
    Unfiltered = ltest:unit(#{color => false, 'suite-headers' => false}),
    ?assertEqual(ok, Unfiltered),
    io:format("SELECTOR_EMPTY_OK~n").

run_selector_child_eval(Root, Eval) ->
    Erl = os:find_executable("erl"),
    SelectorEbin = filename:join([Root, "_build", "test", "lib",
                                  "selector-fixtures", "ebin"]),
    Paths = lists:usort([filename:absname(Path)
                         || Path <- [SelectorEbin | code:get_path()]]),
    PathArgs = lists:append([["-pa", Path] || Path <- Paths]),
    Port = open_port({spawn_executable, Erl},
                     [binary, exit_status, use_stdio, stderr_to_stdout,
                      {args, ["-noshell", "-noinput"] ++ PathArgs ++ ["-eval", Eval]},
                      {cd, Root},
                      {env, [{"LTEST_CHILD_ROOT", Root}]}]),
    collect(Port, []).

compile_selector_fixture(Root, Name, Behaviour, TestBody) ->
    SourceDir = filename:join(Root, "src"),
    Ebin = filename:join([Root, "_build", "test", "lib", "selector-fixtures", "ebin"]),
    ok = filelib:ensure_dir(filename:join(SourceDir, "placeholder")),
    ok = filelib:ensure_dir(filename:join(Ebin, "placeholder")),
    Source = filename:join(SourceDir, Name ++ ".erl"),
    Module = list_to_atom(Name),
    Text = unicode:characters_to_binary(lists:flatten(io_lib:format(
        "%% coding: utf-8~n-module('~ts').~n"
        "~s"
        "-include_lib(\"eunit/include/eunit.hrl\").~n"
        "record(Value) -> file:write_file(\"visited\", [Value, \"\\n\"], [append]).~n"
        "~s~n", [Name, selector_behaviour(Behaviour), TestBody]))),
    ok = file:write_file(Source, Text),
    {ok, Module, Beam} = compile:file(Source, [binary, return_errors, export_all]),
    ok = file:write_file(filename:join(Ebin, Name ++ ".beam"), Beam).

selector_behaviour("undefined") -> [];
selector_behaviour(Behaviour) ->
    lists:flatten(io_lib:format("-behaviour('~s').~n", [Behaviour])).

descriptor_prefixes_consume_full_bitstrings_test() ->
    Beam = code:which('ltest-basic-tests'),
    FileDesc = iolist_to_binary(io_lib:format("file \"~s\"", [Beam])),
    ModuleDesc = <<"module 'ltest-basic-tests'">>,
    ?assert('ltest-listener':'is-file-desc?'(FileDesc)),
    ?assertEqual('ltest-basic-tests', 'ltest-listener':'module-for-desc'(FileDesc)),
    ?assertEqual('ltest-basic-tests', 'ltest-listener':'module-for-desc'(ModuleDesc)).

manual_skip_counts_once_for_real_file_and_module_groups_test() ->
    Beam = code:which('ltest-skipped-tests'),
    FileDesc = iolist_to_binary(io_lib:format("file \"~s\"", [Beam])),
    ModuleDesc = <<"module 'ltest-skipped-tests'">>,
    State0 = 'ltest-listener':init([{color, false}, {'test-type', unit}]),
    AfterFile = 'ltest-listener':'record-module-group'(FileDesc, 7, State0),
    AfterNestedModule = 'ltest-listener':'record-module-group'(ModuleDesc, 7, AfterFile),
    ?assertEqual(1, element(8, AfterNestedModule)),
    ?assertEqual(7, element(10, AfterNestedModule)),
    DirectModule = 'ltest-listener':'record-module-group'(ModuleDesc, 11, State0),
    ?assertEqual(1, element(8, DirectModule)),
    ?assertEqual(11, element(10, DirectModule)).

cancel_callback_updates_existing_state_test() ->
    State = 'ltest-listener':init([{color, false}, {'test-type', unit}]),
    Cancelled = 'ltest-listener':handle_cancel(test, [{reason, timeout}], State),
    ?assertEqual(1, element(9, Cancelled)).

run_result_adapter_test() ->
    ?assertEqual(ok, ltest:'adapt-run-result'(ok)),
    ?assertEqual({error, tests_failed}, ltest:'adapt-run-result'(error)),
    Tagged = {error, deliberate_reason},
    ?assertEqual(Tagged, ltest:'adapt-run-result'(Tagged)).

private_root() ->
    private_root(0).

private_root(Attempt) when Attempt < 32 ->
    Salt = erlang:phash2({node(), self(), erlang:system_time(nanosecond), make_ref()}),
    Name = "ltest-runtime-tests-" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
           ++ "-" ++ integer_to_list(Salt),
    Base = filename:join(os:getenv("TMPDIR", "/tmp"), Name),
    case file:make_dir(Base) of
        ok -> Base;
        {error, eexist} -> private_root(Attempt + 1);
        Error -> erlang:error({private_root_failed, Error})
    end;
private_root(Attempt) ->
    erlang:error({private_root_collisions, Attempt}).

compile_fixture(Root, Name, Behaviour, TestBody) ->
    SourceDir = filename:join(Root, "src"),
    Ebin = filename:join([Root, "_build", "test", "lib", "runtime-fixtures", "ebin"]),
    ok = filelib:ensure_dir(filename:join(SourceDir, "placeholder")),
    ok = filelib:ensure_dir(filename:join(Ebin, "placeholder")),
    Source = filename:join(SourceDir, Name ++ ".erl"),
    Module = list_to_atom(Name),
    Text = lists:flatten(io_lib:format(
        "-module('~s').~n-behaviour('~s').~n-include_lib(\"eunit/include/eunit.hrl\").~n"
        "record(Value) -> file:write_file(\"visited\", [Value, \"\\n\"], [append]).~n"
        "~s~n", [Name, Behaviour, TestBody])),
    ok = file:write_file(Source, Text),
    {ok, Module, Beam} = compile:file(Source, [binary, return_errors, export_all]),
    ok = file:write_file(filename:join(Ebin, Name ++ ".beam"), Beam).

run_child(Root) ->
    run_child_eval(Root, child_eval()).

run_no_color_child(Root) ->
    Eval = "R = ltest:unit(#{color => false, 'suite-headers' => false}), "
           "io:format(\"NO_COLOR_RESULT=~p~n\", [R]), "
           "case R of error -> halt(0); Other -> io:format(\"UNEXPECTED=~p~n\", [Other]), halt(42) end.",
    run_child_eval(Root, Eval).

run_child_eval(Root, Eval) ->
    Erl = os:find_executable("erl"),
    Paths = lists:usort([filename:absname(Path)
                         || Path <- [filename:join([Root, "_build", "test", "lib",
                                                    "runtime-fixtures", "ebin"])
                                     | code:get_path()]]),
    PathArgs = lists:append([["-pa", Path] || Path <- Paths]),
    Port = open_port({spawn_executable, Erl},
                     [binary, exit_status, use_stdio, stderr_to_stdout,
                      {args, ["-noshell", "-noinput"] ++ PathArgs ++ ["-eval", Eval]},
                      {cd, Root},
                      {env, [{"LTEST_CHILD_ROOT", Root}]}]),
    collect(Port, []).

child_eval() ->
    "Root = os:getenv(\"LTEST_CHILD_ROOT\"), "
    "Opts = #{color => false, 'suite-headers' => false, 'test-type' => system, additive => retained}, "
    "file:write_file(\"visited\", <<>>), "
    "D = eunit:test(['runtime-direct-skip-tests'], [no_tty, {report, {'ltest-listener', [{color, false}, {'test-type', unit}]}}]), "
    "io:format(\"DIRECT_MODULE_RESULT=~p~n\", [D]), "
    "file:write_file(\"visited\", <<>>), "
    "U = ltest:unit(Opts), UV = file:read_file(\"visited\"), "
    "file:write_file(\"visited\", <<>>), "
    "A = ltest:all(Opts), AV = file:read_file(\"visited\"), "
    "file:write_file(\"visited\", <<>>), "
    "R = ltest:run(maps:put('test-type', all, Opts)), RV = file:read_file(\"visited\"), "
    "file:write_file(\"visited\", <<>>), "
    "L = 'ltest-runner':all(), LV = file:read_file(\"visited\"), "
    "Messages = element(2, process_info(self(), messages)), "
    "NoDuplicate = not lists:any(fun({result, _, _}) -> true; (_) -> false end, Messages), "
    "case {U, UV, A, AV, R, RV, L, LV, NoDuplicate, filelib:is_dir(Root)} of "
    "{error, {ok, <<\"unit\\nunit\\nunit\\nunit\\nunit\\nunit\\nunit\\n\">>}, error, "
    "{ok, <<\"unit\\nunit\\nunit\\nunit\\nunit\\nunit\\nunit\\nsystem\\nintegration\\n\">>}, "
    "{error, tests_failed}, {ok, <<\"unit\\nunit\\nunit\\nunit\\nunit\\nunit\\nunit\\nsystem\\nintegration\\n\">>}, "
    "error, {ok, <<\"unit\\nunit\\nunit\\nunit\\nunit\\nunit\\nunit\\nintegration\\nsystem\\n\">>}, true, true} -> "
    "io:format(\"CHILD_CONTRACT_OK~n\"), halt(0); "
    "Other -> io:format(\"CHILD_CONTRACT_FAILED ~p~n\", [Other]), halt(41) end.".

collect(Port, Chunks) ->
    receive
        {Port, {data, Data}} -> collect(Port, [Data | Chunks]);
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Chunks)),
            case Status of
                0 -> binary_to_list(Output);
                _ -> erlang:error({child_failed, Status, binary_to_list(Output)})
            end
    after 30000 ->
        port_close(Port),
        erlang:error(child_timeout)
    end.

remove_tree(Path) ->
    case file:del_dir_r(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        Error -> erlang:error({cleanup_failed, Path, Error})
    end.
