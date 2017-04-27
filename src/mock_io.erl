%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
% http://erlang.org/doc/apps/stdlib/io_protocol.html
%
-module(mock_io).

-export([start_link/0, stop/1]).
-export([inject/2, extract/1, unread/1]).
-export([setup/0, teardown/1]).

-spec start_link() -> pid().
start_link() ->
    spawn_link(fun loop/0).

-spec stop(pid()) -> ok.
stop(Pid) ->
    stopped = call(Pid, stop),
    ok.

-spec extract(pid()) -> string().
extract(Pid) ->
    {extracted, String} = call(Pid, extract),
    String.

-spec inject(pid(), string()) -> ok.
inject(Pid, String) ->
    injected = call(Pid, {inject, String}),
    ok.

-spec unread(pid()) -> string().
unread(Pid) ->
    {unread, String} = call(Pid, unread),
    String.

setup() ->
    GL = erlang:group_leader(),
    Pid = start_link(),
    true = erlang:group_leader(Pid, self()),
    {Pid, GL}.

teardown({Pid, GL}) ->
    true = erlang:group_leader(GL, self()),
    ok = stop(Pid).

%------------------------------------------------------------------------------

loop() -> loop([], [], list).

loop(Input, Output, Mode) ->
    receive

    % Mock protocol

        {From, {inject, String}} ->
            From ! {self(), injected},
            loop(Input ++ String, Output, Mode);
        {From, unread} ->
            From ! {self(), {unread, Input}},
            loop(Input, Output, Mode);
        {From, extract} ->
            From ! {self(), {extracted, Output}},
            loop(Input, Output, Mode);
        {From, stop} ->
            From ! {self(), stopped};

    % I/O protocol

        {io_request, From, Opaque,
         {put_chars, unicode, io_lib, format, [Format, Data]}} ->
            reply(io_reply, From, Opaque, ok),
            loop(Input, Output ++ lists:flatten(io_lib:format(Format, Data)), Mode);

        {io_request, From, Opaque, {get_line, unicode, Prompt}} ->
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
            reply(io_reply, From, Opaque, Reply),
            loop(RestInput, Output ++ Prompt, Mode);

        {io_request, From, Opaque,
         {get_until, unicode, Prompt, io_lib, fread, [Format]}} ->
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
            reply(io_reply, From, Opaque, Reply),
            loop(RestInput, Output ++ Prompt, Mode);

    % Handle file:read/2, which still uses the old get_chars format
        {io_request, From, Opaque, {get_chars, _Prompt, N}} ->
            {Reply, RestInput} =
                case Input of
                    "" -> {eof, ""};
                    Input ->
                        {Data, Rest} =
                        % TODO instead of length + split, write our own split that
                        % doesn't error if N > length!
                            case length(Input) > N of
                                true -> lists:split(N, Input);
                                false -> {Input, ""}
                            end,
                        case Mode of
                            binary -> {{ok, list_to_binary(Data)}, Rest};
                            list -> {{ok, Data}, Rest}
                        end
                end,
            reply(io_reply, From, Opaque, Reply),
            loop(RestInput, Output, Mode);

        {io_request, From, Opaque, {setopts, [binary]}} ->
            reply(io_reply, From, Opaque, ok),
            loop(Input, Output, binary);

        Any ->
            erlang:error({unexpected, Any})
    end.

reply(Type, To, Opaque, Reply) ->
    To ! {Type, Opaque, Reply},
    ok.

call(Pid, MsgOut) ->
    Pid ! {self(), MsgOut},
    receive
        {Pid, MsgIn} -> MsgIn
    after 1000 -> erlang:error(timeout)
    end.
