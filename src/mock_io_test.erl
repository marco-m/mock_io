%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
-module(mock_io_test).

-include_lib("eunit/include/eunit.hrl").

%------------------------------------------------------------------------------
% Usage examples.
%------------------------------------------------------------------------------

example_capturing_stdout_test() ->
    {IO, GL} = mock_io:setup(),
    uut_that_writes_to_stdout(),
    ?assertEqual("1 a ciao\n", mock_io:extract(IO)),
    mock_io:teardown({IO, GL}).

uut_that_writes_to_stdout() ->
    io:fwrite("~p ~p ~s~n", [1, a, "ciao"]).

example_injecting_to_stdin_test() ->
    {IO, GL} = mock_io:setup(),
    String = "pizza pazza puzza\n",
    mock_io:inject(IO, String),
    ?assertEqual(String, uut_that_reads_from_stdin()),
    ?assertEqual("", mock_io:unread(IO)),
    mock_io:teardown({IO, GL}).

uut_that_reads_from_stdin() ->
    io:get_line("prompt").

%------------------------------------------------------------------------------
% The real tests.
%------------------------------------------------------------------------------

mock_io_can_start_and_stop_test() ->
    IO = mock_io:start_link(),
    ?assert(is_process_alive(IO)),
    ?assertEqual(ok, mock_io:stop(IO)),
    ?assertNot(is_process_alive(IO)).

%------------------------------------------------------------------------------

%
% This fixture contains the tests where the UUT writes to stdout. We can
% change the group leader in the setup/0 function and the mock can receive what
% the UUT has written.
%
mock_io_group_leader_in_setup_works_test_() ->
    {foreach,
     fun mock_io:setup/0,
     fun mock_io:teardown/1,
     [
         fun extract_without_uut_write_returns_empty_string/1,
         fun can_access_what_has_been_injected/1,
         fun mock_can_extract_what_uut_has_written_simple/1,
         fun mock_can_extract_what_uut_has_written_multi_args/1
     ]}.

extract_without_uut_write_returns_empty_string({IO, _}) ->
    ?_assertEqual("", mock_io:extract(IO)).

can_access_what_has_been_injected({IO, _}) ->
    ok = mock_io:inject(IO, "hello"),
    ?_assertEqual("hello", mock_io:unread(IO)).

mock_can_extract_what_uut_has_written_simple({IO, _}) ->
    io:fwrite("pizza"), %  <- this is the UUT
    ?_assertEqual("pizza", mock_io:extract(IO)).

mock_can_extract_what_uut_has_written_multi_args({IO, _}) ->
    io:fwrite("~p ~p ~s~n", [1, a, "ciao"]),  %  <- this is the UUT
    ?_assertEqual("1 a ciao\n", mock_io:extract(IO)).


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
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "margherita\n"),
    ?assertEqual("margherita\n", io:get_line("prompt")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

uut_fread_reads_what_mock_has_injected_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "margherita"),
    ?assertEqual({ok, ["margherita"]}, io:fread("prompt", "~s")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

uut_get_line_reads_with_spaces_test() ->
    {IO, GL} = mock_io:setup(),
    String = "pizza pazza puzza\n",
    ok = mock_io:inject(IO, String),
    ?assertEqual(String, io:get_line("prompt")),
    ?assertEqual("", mock_io:unread(IO)),
    mock_io:teardown({IO, GL}).

uut_fread_reads_with_spaces_test() ->
    {IO, GL} = mock_io:setup(),
    String = "pizza pazza puzza\n",
    ok = mock_io:inject(IO, String),
    ?assertEqual({ok, ["pizza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    ?assertEqual({ok, ["pazza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    ?assertEqual({ok, ["puzza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

uut_get_line_reads_with_newlines_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "pizza pazza\npuzza pezza\n"),
    ?assertEqual("pizza pazza\n", io:get_line("prompt")),
    ?assertEqual("puzza pezza\n", io:get_line("prompt")),
    ?assertEqual("", mock_io:unread(IO)),
    mock_io:teardown({IO, GL}).

uut_freads_reads_with_newlines_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "pizza pazza\npuzza\n"),
    ?assertEqual({ok, ["pizza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    ?assertEqual({ok, ["pazza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    ?assertEqual({ok, ["puzza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

uut_get_line_gets_eof_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "pizza pazza\n"),
    ?assertEqual("pizza pazza\n", io:get_line("prompt")),
    ?assertEqual(eof, io:get_line("prompt")),
    mock_io:teardown({IO, GL}).

uut_fread_gets_eof_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "pizza\n"),
    ?assertEqual({ok, ["pizza"]}, io:fread("prompt", "~s")), % <- This is the UUT
    ?assertEqual(eof, io:fread("prompt", "~s")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

% Verified with:
% printf "1 2\n" | escript _build/default/lib/mock_io/ebin/command_line.beam
% io:fread("", "~d ~d ~d")
uut_fread_gets_error_when_too_many_args_and_newline_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "1 2\n"),
    ?assertEqual({error, {fread, input}}, io:fread("", "~d ~d ~d")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

% Verified with:
% printf "\n" | escript _build/default/lib/mock_io/ebin/command_line.beam
% io:fread("", "~d ~d ~d")
uut_fread_gets_error_when_only_newline_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "\n"),
    ?assertEqual({error, {fread, input}}, io:fread("", "~d ~d ~d")), % <- This is the UUT
    mock_io:teardown({IO, GL}).

% Verified with
% printf "2 2 8\n" | escript
% io:fread("", "~d ~d ~d"),      % <- 1st, no need to read \n explicitly
% X = io:fread("", "~d ~d ~d"),  % <- 2nd, no need to read \n explicitly
uut_fread_consumes_newline_implicitly_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "2 2 8\n"),
    ?assertEqual({ok, [2, 2, 8]}, io:fread("", "~d ~d ~d")),
    ?assertEqual(eof, io:fread("", "~d ~d ~d")),
    mock_io:teardown({IO, GL}).

mock_supports_setopt_binary_test() ->
    {IO, GL} = mock_io:setup(),
    ok = mock_io:inject(IO, "ciao"),
    ?assertEqual(ok, io:setopts(standard_io, [binary])),
    {ok, Data} = file:read(standard_io, 10000),
    ?assertEqual(eof, file:read(standard_io, 10000)),
    ?assertEqual(<<"ciao">>, Data),
    mock_io:teardown({IO, GL}).

prompt_of_fread_gets_copied_to_mock_output_channel_test() ->
    {IO, GL} = mock_io:setup(),
    % feed something to the UUT simply to make it return.
    mock_io:inject(IO, "\n"),
    {ok, _} = io:fread("I am the prompt", "~s"),
    ?assertEqual("I am the prompt", mock_io:extract(IO)),
    mock_io:teardown({IO, GL}).

prompt_of_get_line_gets_copied_to_mock_output_channel_test() ->
    {IO, GL} = mock_io:setup(),
    % feed something to the UUT simply to make it return.
    mock_io:inject(IO, "\n"),
    "\n" = io:get_line("I am the prompt"),
    ?assertEqual("I am the prompt", mock_io:extract(IO)),
    mock_io:teardown({IO, GL}).
