-module(ltest_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

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
