% Use the command-line to verify the standard behavior
%
% Using `escript` is a LOT faster than `erl` because the boot sequence is optimized.
% rebar3 compile && printf "1 2\n" | escript _build/default/lib/mock_io/ebin/command_line.beam

-module(command_line).

-export([main/1]).

% For escript.
main(_) -> main().

main() ->
    X = io:fread("", "~d ~d ~d"),
    io:fwrite("~p~n", [X]),
    true.
