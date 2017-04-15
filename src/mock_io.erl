%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
% http://erlang.org/doc/apps/stdlib/io_protocol.html
%
-module(mock_io).

-export([start_link/0, stop/1]).
-export([inject/2, extract/1, unread/1]).

-spec start_link() -> pid().
start_link() ->
    spawn_link(fun init/0).

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

%------------------------------------------------------------------------------

init() ->
    loop({[], []}).

loop({Input, Output}) ->
    receive

    % Mock protocol

        {From, {inject, String}} ->
            From ! {self(), injected},
            loop({[String | Input], Output});
        {From, unread} ->
            From ! {self(), {unread, lists:flatten(lists:reverse(Input))}},
            loop({Input, Output});
        {From, extract} ->
            From ! {self(), {extracted, lists:flatten(lists:reverse(Output))}},
            loop({Input, Output});
        {From, stop} ->
            From ! {self(), stopped};

    % I/O protocol

        {io_request, From, Opaque,
         {put_chars, unicode, io_lib, format, [Format, Data]}} ->
            reply(io_reply, From, Opaque, ok),
            loop({Input, [io_lib:format(Format, Data) | Output]});

        Any ->
            erlang:error({unexpected, Any})
    end.

reply(Type, To, Opaque, Reply) ->
    To ! {Type, Opaque, Reply}.

call(Pid, MsgOut) ->
    Pid ! {self(), MsgOut},
    receive
        {Pid, MsgIn} -> MsgIn
    after 1000 -> erlang:error(timeout)
    end.
