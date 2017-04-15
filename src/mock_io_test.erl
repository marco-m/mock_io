%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
-module(mock_io_test).

-include_lib("eunit/include/eunit.hrl").

%------------------------------------------------------------------------------
% Usage examples.
%------------------------------------------------------------------------------

%%mock_io_usage_examples_test_() ->
%%    {foreach,
%%     fun setup/0,
%%     fun teardown/1,
%%     [
%%         fun example_capturing_stdout/1,
%%         fun example_injecting_to_stdin/1
%%     ]}.

setup() ->
    mock_io:start_link().

teardown(Pid) ->
    ok = mock_io:stop(Pid).

%%example_capturing_stdout(_) ->
%%    uut_that_writes_to_stdout(),
%%    ?_assertEqual("1 a ciao\n", mock_io:extract()).
%%
%%example_injecting_to_stdin(_) ->
%%    String = "pizza pazza puzza\n",
%%    mock_io:inject(String), [
%%        ?_assertEqual(String, uut_that_reads_from_stdin()),
%%        ?_assertEqual("", mock_io:remaining_injected())
%%    ].
%%
%%uut_that_writes_to_stdout() ->
%%    io:fwrite("~p ~p ~s~n", [1, a, "ciao"]).
%%
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

mock_io_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
         fun extract_without_uut_write_returns_empty_string/1,
         fun can_access_what_has_been_injected/1
     ]}.

extract_without_uut_write_returns_empty_string(Pid) ->
    ?_assertEqual("", mock_io:extract(Pid)).

can_access_what_has_been_injected(Pid) ->
    ok = mock_io:inject(Pid, "hello"),
    ?_assertEqual("hello", mock_io:unread(Pid)).
