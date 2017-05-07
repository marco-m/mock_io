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

% Low-level test the mock itself API
-export([getopts/1]).


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
% Low-level test the mock itself API
%------------------------------------------------------------------------------

-spec getopts(pid()) -> list().
getopts(Pid) ->
    {getopts, Opts} = mock_call(Pid, getopts),
    Opts.

%------------------------------------------------------------------------------

loop() -> loop([], [], false).

loop(Input, Output, IsBinary) ->
    receive
        {mock, From, Args} ->
            case handle_mock_protocol(From, Args, {Input, Output, IsBinary}) of
                {Input2, Output2, IsBinary2} -> loop(Input2, Output2, IsBinary2);
                stop -> stop
            end;
        {io_request, From, Opaque, Args} ->
            {Input2, Output2, IsBinary2} =
                handle_io_protocol(From, Opaque, Args, {Input, Output, IsBinary}),
            loop(Input2, Output2, IsBinary2)
    end.

handle_mock_protocol(From, Args, {Input, Output, IsBinary}) ->
    case Args of
        {inject, String} ->
            mock_reply(From, injected),
            {Input ++ String, Output, IsBinary};
        remaining_input ->
            mock_reply(From, {remaining_input, Input}),
            {Input, Output, IsBinary};
        extract ->
            mock_reply(From, {extracted, Output}),
            {Input, Output, IsBinary};
        stop ->
            mock_reply(From, stopped),
            stop;

        getopts ->
            mock_reply(From, {getopts, [{binary, IsBinary}]}),
            {Input, Output, IsBinary}

    end.

handle_io_protocol(From, Opaque, Req, {Input, Output, IsBinary}) ->
    %%        {io_request, From, Opaque,
    %%         {put_chars,latin1, Bin}} when is_binary(Bin) ->
    %%            reply(io_reply, From, Opaque, ok),
    %%            loop(Input, Output ++ lists:flatten(io_lib:format(Format, Data)), IsBinary);

    case Req of

        {put_chars, Encoding, Mod, Fun, Args} ->
            {Reply, Output2} =
                io_handle_put_chars(Encoding, Mod, Fun, Args, {Output, IsBinary}),
            io_reply(From, Opaque, Reply),
            {Input, Output2, IsBinary};

        {get_line, Encoding, Prompt} ->
            {Reply, Input2, Output2} =
                io_handle_get_line(Encoding, Prompt, {Input, Output, IsBinary}),
            io_reply(From, Opaque, Reply),
            {Input2, Output2, IsBinary};

        {get_until, Encoding, Prompt, Mod, Fun, Args} ->
            {Reply, Input2, Output2} =
                io_handle_get_until(Encoding, Prompt, Mod, Fun, Args,
                                    {Input, Output, IsBinary}),
            io_reply(From, Opaque, Reply),
            {Input2, Output2, IsBinary};

        {get_chars, Prompt, NChars} ->
            {Reply, Input2} = io_handle_get_chars(Prompt, NChars, {Input, IsBinary}),
            io_reply(From, Opaque, Reply),
            {Input2, Output, IsBinary};

        {setopts, Args} ->
            {Reply, IsBinary2} = io_handle_setopts(Args, IsBinary),
            io_reply(From, Opaque, Reply),
            {Input, Output, IsBinary2};

        getopts ->
            io_reply(From, Opaque, [{binary, IsBinary}]),
            {Input, Output, IsBinary};

        Any ->
            erlang:error({unexpected, Any})
    end.

io_handle_put_chars(Encoding, Mod, Fun, Args, {Output, _IsBinary}) ->
    case {Encoding, Mod, Fun, Args} of
        {unicode, io_lib, format, [Format, Data]} ->
            {ok, Output ++ lists:flatten(io_lib:format(Format, Data))}
    end.

io_handle_get_line(_Encoding, Prompt, {Input, Output, _IsBinary}) ->
    % We are emulating io:get_line(), which reads until newline and returns
    % that newline. We cannot use io_lib:fread(), because it has no notion
    % of newline.
    {Reply, RestInput} =
        case Input of
            "" -> {eof, ""};
            Input ->
                Options = [{return, list}, {parts, 2}],
                [Data, Leftover] = re:split(Input, "\n", Options),
                {Data ++ "\n", Leftover}
        end,
    {Reply, RestInput, Output ++ Prompt}.

io_handle_get_until(_Encoding, Prompt, _Mod = io_lib, _Fun = fread, _Args = [Format],
                    {Input, Output, _IsBinary}) ->
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
    {Reply, RestInput, Output ++ Prompt}.

% Handle file:read/2, which still uses the old get_chars format
io_handle_get_chars(_Prompt, NChars, {Input, IsBinary}) ->
    {Reply, RestInput} =
        case Input of
            "" -> {eof, ""};
            Input ->
                {Data, Rest} =
                    % TODO instead of length + split, write our own split that
                    % doesn't error if N > length!
                    case length(Input) > NChars of
                        true -> lists:split(NChars, Input);
                        false -> {Input, ""}
                    end,
                case IsBinary of
                    true -> {{ok, list_to_binary(Data)}, Rest};
                    false -> {{ok, Data}, Rest}
                end
        end,
    {Reply, RestInput}.

% By default, all I/O devices in OTP are set in `list` mode.
% If set in binary mode (`binary` or `{binary, true}`), the I/O server sends
% binary data (encoded in UTF-8) as answers to the `get_line`, `get_chars` and
% `get_until` requests
io_handle_setopts(Args, IsBinary) when is_list(Args) ->
    case Args of
        [binary] -> {ok, true};
        [{binary, true}] -> {ok, true};
        [{binary, false}] -> {ok, false};
        [list] -> {ok, false};
        Args -> {{error, enotsup}, IsBinary}
    end;
io_handle_setopts(_, IsBinary) -> {{error, request}, IsBinary}.



%------------------------------------------------------------------------------

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
