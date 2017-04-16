%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
-module(mock_io_test).

-include_lib("eunit/include/eunit.hrl").

%------------------------------------------------------------------------------
% Usage examples.
%------------------------------------------------------------------------------

setup() ->
    GL = erlang:group_leader(),
    Pid = mock_io:start_link(),
    true = erlang:group_leader(Pid, self()),
    {Pid, GL}.

teardown({Pid, GL}) ->
    true = erlang:group_leader(GL, self()),
    ok = mock_io:stop(Pid).

example_capturing_stdout_test() ->
    {Pid, GL} = setup(),
    uut_that_writes_to_stdout(),
    ?assertEqual("1 a ciao\n", mock_io:extract(Pid)),
    teardown({Pid, GL}).

uut_that_writes_to_stdout() ->
    io:fwrite("~p ~p ~s~n", [1, a, "ciao"]).

%%example_injecting_to_stdin(_) ->
%%    String = "pizza pazza puzza\n",
%%    mock_io:inject(String), [
%%        ?_assertEqual(String, uut_that_reads_from_stdin()),
%%        ?_assertEqual("", mock_io:remaining_injected())
%%    ].

%------------------------------------------------------------------------------


%%uut_that_reads_from_stdin() ->
%%    io:get_line("").

%------------------------------------------------------------------------------
% The real tests.
%------------------------------------------------------------------------------

mock_io_can_start_and_stop_test() ->
    Pid = mock_io:start_link(),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(ok, mock_io:stop(Pid)),
    ?assertNot(is_process_alive(Pid)).

%------------------------------------------------------------------------------

%
% This fixture contains the tests where the UUT writes to stdout. We can
% change the group leader in the setup/0 function and the mock can receive what
% the UUT has written.
%
mock_io_group_leader_in_setup_works_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
         fun extract_without_uut_write_returns_empty_string/1,
         fun can_access_what_has_been_injected/1,
         fun mock_can_extract_what_uut_has_written_simple/1,
         fun mock_can_extract_what_uut_has_written_multi_args/1
     ]}.

extract_without_uut_write_returns_empty_string({Pid, _}) ->
    ?_assertEqual("", mock_io:extract(Pid)).

can_access_what_has_been_injected({Pid, _}) ->
    ok = mock_io:inject(Pid, "hello"),
    ?_assertEqual("hello", mock_io:unread(Pid)).

mock_can_extract_what_uut_has_written_simple({Pid, _}) ->
    io:fwrite("pizza"), %  <- this is the UUT
    ?_assertEqual("pizza", mock_io:extract(Pid)).

mock_can_extract_what_uut_has_written_multi_args({Pid, _}) ->
    io:fwrite("~p ~p ~s~n", [1, a, "ciao"]),  %  <- this is the UUT
    ?_assertEqual("1 a ciao\n", mock_io:extract(Pid)).


%------------------------------------------------------------------------------

%
% We cannot use a fixture when the tests where the UUT reads from stdin: if we
% change the group leader in the setup/0 function, the mock doesn't receive any
% message.
%
% This is related to the fact that EUnit changes the group leader in order to
% capture stdout. What is strange is that stdout works, it is stdin that requires
% to change the group leader inside each single test. I know that with the
% foreach fixture, the process calling setup and teardown is not the same as the
% one running the tests, and that the instantiated tests are run later compared
% to the time the test instantiator runs.
%

uut_get_line_reads_what_mock_has_injected_test() ->
    {Pid, GL} = setup(),

    ok = mock_io:inject(Pid, "margherita\n"),
    ?assertEqual("margherita\n", io:get_line("prompt")), % <- This is the UUT

    teardown({Pid, GL}).

uut_fread_reads_what_mock_has_injected_test() ->
    {Pid, GL} = setup(),

    ok = mock_io:inject(Pid, "margherita"),
    ?assertEqual({ok, ["margherita"]}, io:fread("prompt", "~s")), % <- This is the UUT

    teardown({Pid, GL}).
