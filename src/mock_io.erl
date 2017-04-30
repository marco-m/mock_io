%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
% http://erlang.org/doc/apps/stdlib/io_protocol.html
%
-module(mock_io).

% The mock_io API.
-export([inject/2, extract/1, remaining_input/1]).

% Process-lifetime API.
-export([start_link/0, stop/1]).

% Helper functions for unit tests.
-export([setup/0, teardown/1]).

%------------------------------------------------------------------------------
% Mock IO API
%------------------------------------------------------------------------------

-spec extract(pid()) -> string().
extract(Pid) ->
    {extracted, String} = mock_call(Pid, extract),
    String.

-spec inject(pid(), string()) -> ok.
inject(Pid, String) ->
    injected = mock_call(Pid, {inject, String}),
    ok.

-spec remaining_input(pid()) -> string().
remaining_input(Pid) ->
    {remaining_input, String} = mock_call(Pid, remaining_input),
    String.

%------------------------------------------------------------------------------
% Process-lifetime API
%------------------------------------------------------------------------------

-spec start_link() -> pid().
start_link() ->
    spawn_link(fun loop/0).

-spec stop(pid()) -> ok.
stop(Pid) ->
    stopped = mock_call(Pid, stop),
    ok.

%------------------------------------------------------------------------------
% Helper functions API
%------------------------------------------------------------------------------

-spec setup() -> {pid(), pid()}.
setup() ->
    GL = erlang:group_leader(),
    Pid = start_link(),
    true = erlang:group_leader(Pid, self()),
    {Pid, GL}.

-spec teardown({pid(), pid()}) -> ok.
teardown({Pid, GL}) ->
    true = erlang:group_leader(GL, self()),
    ok = stop(Pid).

%------------------------------------------------------------------------------

loop() -> loop([], [], list).

loop(Input, Output, Mode) ->
    receive
        {mock, From, Args} ->
            case handle_mock_protocol(From, Args, {Input, Output, Mode}) of
                {Input2, Output2, Mode2} -> loop(Input2, Output2, Mode2);
                stop -> stop
            end;
        {io_request, From, Opaque, Args} ->
            {Input2, Output2, Mode2} =
                handle_io_protocol(From, Opaque, Args, {Input, Output, Mode}),
            loop(Input2, Output2, Mode2)
    end.

handle_mock_protocol(From, Args, {Input, Output, Mode}) ->
    case Args of
        {inject, String} ->
            mock_reply(From, injected),
            {Input ++ String, Output, Mode};
        remaining_input ->
            mock_reply(From, {remaining_input, Input}),
            {Input, Output, Mode};
        extract ->
            mock_reply(From, {extracted, Output}),
            {Input, Output, Mode};
        stop ->
            mock_reply(From, stopped),
            stop
    end.

handle_io_protocol(From, Opaque, Args, {Input, Output, Mode}) ->
    %%        {io_request, From, Opaque,
    %%         {put_chars,latin1, Bin}} when is_binary(Bin) ->
    %%            reply(io_reply, From, Opaque, ok),
    %%            loop(Input, Output ++ lists:flatten(io_lib:format(Format, Data)), Mode);

    case Args of

        {put_chars, unicode, io_lib, format, [Format, Data]} ->
            io_reply(From, Opaque, ok),
            {Input, Output ++ lists:flatten(io_lib:format(Format, Data)), Mode};

        {get_line, unicode, Prompt} ->
            % We are emulating io:get_line(), which reads until newline and returns
            % that newline. We cannot use io_lib:fread(), because it has no notion
            % of newline.
            {Reply, RestInput} =
                case Input of
                    "" ->
                        {eof, ""};
                    Input ->
                        Options = [{return, list}, {parts, 2}],
                        [Data, Leftover] = re:split(Input, "\n", Options),
                        {Data ++ "\n", Leftover}
                end,
            io_reply(From, Opaque, Reply),
            {RestInput, Output ++ Prompt, Mode};

        {get_until, unicode, Prompt, io_lib, fread, [Format]} ->
            {Reply, RestInput} =
                case Input of
                    "" -> {eof, ""};
                    Input ->
                        case io_lib:fread(Format, Input) of
                            {more, _RestFormat, _Nchars, InputStack} ->
                                {{error, {fread, input}}, InputStack};
                            {error, _Reason} ->
                                {{error, {fread, input}}, Input};
                            {ok, Data, Rest} ->
                                case Rest of
                                    "\n" -> {{ok, Data}, ""};
                                    Rest -> {{ok, Data}, Rest}
                                end
                        end
                end,
            io_reply(From, Opaque, Reply),
            {RestInput, Output ++ Prompt, Mode};

        % Handle file:read/2, which still uses the old get_chars format
        {get_chars, _Prompt, N} ->
            {Reply, RestInput} =
                case Input of
                    "" -> {eof, ""};
                    Input ->
                        {Data, Rest} =
                            % TODO instead of length + split, write our own split that
                            % doesn't error if N > length!
                            case length(Input) > N of
                                true -> lists:split(N, Input);
                                false ->
                                    {Input, ""}
                            end,
                        case Mode of
                            binary -> {{ok, list_to_binary(Data)}, Rest};
                            list -> {{ok, Data}, Rest}
                        end
                end,
            io_reply(From, Opaque, Reply),
            {RestInput, Output, Mode};

        % By default, all I/O devices in OTP are set in `list` mode.
        % If set in binary mode (`binary` or `{binary, true}`), the I/O server sends
        % binary data (encoded in UTF-8) as answers to the `get_line`, `get_chars` and
        % `get_until` requests
        {setopts, [binary]} ->
            io_reply(From, Opaque, ok),
            {Input, Output, binary};

        Any ->
            erlang:error({unexpected, Any})
    end.

io_reply(To, Opaque, Reply) ->
    To ! {io_reply, Opaque, Reply},
    ok.

mock_reply(To, Reply) ->
    To ! {mock, self(), Reply},
    ok.

mock_call(Pid, MsgOut) ->
    Pid ! {mock, self(), MsgOut},
    receive
        {mock, Pid, MsgIn} -> MsgIn
    after 1000 -> erlang:error(timeout)
    end.
