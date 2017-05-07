# Mock IO for Erlang

Version 0.1

A simple mock of the Erlang I/O protocol, to allow unit testing code that performs I/O operations (`io:fwrite`, `io:fread`, `file:read`, `file:write`, ...).

By default, it mocks the `standard_io` device by manipulating the group leader, but you can mock any device/file.

Focuses on simplicity and allowing painless mocking of I/O, not on performance.

## Status

Uses semantic versioning.

Usable, but still beta code. Missing functionalities and API might change without attempting to maintain backward compatibility as long as the version is 0.x.

PRs are welcome. Please focus on simplicity and provide full test coverage.

## Usage examples

### Extracting output from UUT

UUT:
```erlang
-module(uut).
write_to_stdout() ->
    io:fwrite("~p ~p ~s~n", [1, a, "ciao"]).
```

EUnit test:

```erlang
capture_stdout_test() ->
    {IO, GL} = mock_io:setup(),
    uut:write_to_stdout(),
    ?assertEqual(<<"1 a ciao\n">>, mock_io:extract(IO)),
    mock_io:teardown({IO, GL}).
```

### Injecting input to UUT

UUT:

```erlang
-module(uut).
read_from_stdin() ->
    io:get_line("prompt").
```

EUnit test:

```erlang
inject_to_stdin_test() ->
    {IO, GL} = mock_io:setup(),
    mock_io:inject(IO, <<"pizza pazza puzza\n">>),
    ?assertEqual("pizza pazza puzza\n", uut:read_from_stdin()),
    ?assertEqual(<<>>, mock_io:remaining_input(IO)),
    mock_io:teardown({IO, GL}).
```

See also the first tests in `mock_io_test.erl` that show how to use `mock_io` from the point of view of a client.

## Caveats

Why the examples perform the setup/teardown themselves instead of using a EUnit fixture?

Because EUnit performs tricks on the group leader to be able to capture the output of the UUT, and this somehow breaks mock_io if we put setup/teardown in a fixture :-( Solving this will probably require to change EUnit code, patches are more than welcome!

## Roadmap

- I would love to see `mock_io` being part of EUnit, and also be usable transparently from Common Test.

## API v0.1

WRITEME
