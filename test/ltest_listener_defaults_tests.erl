-module(ltest_listener_defaults_tests).

-include_lib("eunit/include/eunit.hrl").

-export([pass_case/0, fail_case/0]).

listener_init_defaults_and_overrides_test() ->
    Empty = 'ltest-listener':init([]),
    ?assertEqual(unit, element(3, Empty)),
    ?assertEqual(true, element(4, Empty)),
    ColorOnly = 'ltest-listener':init([{color, false}]),
    ?assertEqual(unit, element(3, ColorOnly)),
    ?assertEqual(false, element(4, ColorOnly)),
    TypeOnly = 'ltest-listener':init([{'test-type', system}]),
    ?assertEqual(system, element(3, TypeOnly)),
    ?assertEqual(true, element(4, TypeOnly)),
    Explicit = 'ltest-listener':init([{'test-type', integration}, {color, false}]),
    ?assertEqual(integration, element(3, Explicit)),
    ?assertEqual(false, element(4, Explicit)).

listener_empty_and_partial_options_report_test() ->
    Root = private_root(),
    try
        EmptyOutput = run_child(Root, child_eval("EMPTY", "[]", "pass_case")),
        assert_completed_report(EmptyOutput, "EMPTY", "ok", "Passed: 1"),
        ?assertNotEqual(nomatch, binary:match(list_to_binary(EmptyOutput), <<27>>)),

        TypeOutput = run_child(Root,
                               child_eval("TYPE_ONLY", "[{'test-type', system}]",
                                          "pass_case")),
        assert_completed_report(TypeOutput, "TYPE_ONLY", "ok", "Passed: 1"),
        ?assertNotEqual(nomatch, binary:match(list_to_binary(TypeOutput), <<27>>)),

        ColorOutput = run_child(Root,
                                child_eval("COLOR_ONLY", "[{color, false}]",
                                           "fail_case")),
        assert_completed_report(ColorOutput, "COLOR_ONLY", "error", "Failed: 1"),
        ?assertEqual(nomatch, binary:match(list_to_binary(ColorOutput), <<27>>))
    after
        remove_tree(Root)
    end.

assert_completed_report(Output, Label, Result, CountText) ->
    Plain = re:replace(Output, [27] ++ "\\[[0-9;]*m", "",
                       [global, {return, list}]),
    ?assertNotEqual(nomatch, re:run(Plain, Label ++ "_RESULT=" ++ Result)),
    ?assertNotEqual(nomatch, re:run(Plain, "Tests: 1")),
    ?assertNotEqual(nomatch, re:run(Plain, CountText)),
    ?assertEqual(nomatch, re:run(Output, "Internal EUnit listener error")),
    ?assertEqual(nomatch, re:run(Output, "function_clause|badkey|CHILD_FAILED")).

pass_case() -> ok.

fail_case() ->
    Expected = make_ref(),
    Actual = make_ref(),
    ?assertEqual(Expected, Actual).

private_root() ->
    private_root(0).

private_root(Attempt) when Attempt < 32 ->
    Salt = erlang:phash2({node(), self(), erlang:system_time(nanosecond), make_ref()}),
    Name = "ltest-listener-defaults-" ++
           integer_to_list(erlang:unique_integer([positive, monotonic])) ++
           "-" ++ integer_to_list(Salt),
    Root = filename:join(os:getenv("TMPDIR", "/tmp"), Name),
    case file:make_dir(Root) of
        ok -> Root;
        {error, eexist} -> private_root(Attempt + 1);
        Error -> erlang:error({private_root_failed, Error})
    end;
private_root(Attempt) ->
    erlang:error({private_root_collisions, Attempt}).

run_child(Root, Eval) ->
    Erl = os:find_executable("erl"),
    AppEbin = filename:dirname(code:which('ltest-listener')),
    TestEbin = filename:dirname(code:which(?MODULE)),
    Paths = lists:usort([filename:absname(Path)
                         || Path <- [AppEbin, TestEbin | code:get_path()]]),
    PathArgs = lists:append([["-pa", Path] || Path <- Paths]),
    Args = ["-noshell", "-noinput"] ++ PathArgs ++ ["-eval", Eval],
    Port = open_port({spawn_executable, Erl},
                     [binary, exit_status, use_stdio, stderr_to_stdout,
                      {args, Args}, {cd, Root},
                      {env, [{"ERL_CRASH_DUMP", filename:join(Root, "crash.dump")}]}]),
    collect(Port, []).

child_eval(Label, Options, Case) ->
    lists:flatten(io_lib:format(
      "R = eunit:test([fun ltest_listener_defaults_tests:~s/0], "
      "[no_tty, {report, {'ltest-listener', ~s}}]), "
      "io:format(\"~s_RESULT=~~p~~n\", [R]), halt(0).",
      [Case, Options, Label])).

collect(Port, Chunks) ->
    receive
        {Port, {data, Data}} -> collect(Port, [Data | Chunks]);
        {Port, {exit_status, 0}} -> binary_to_list(iolist_to_binary(lists:reverse(Chunks)));
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Chunks)),
            erlang:error({child_failed, Status, Output})
    after 30000 ->
        port_close(Port),
        erlang:error(child_timeout)
    end.

remove_tree(Root) ->
    case file:del_dir_r(Root) of
        ok -> ok;
        {error, enoent} -> ok;
        Error -> erlang:error({cleanup_failed, Root, Error})
    end.
