%
% Copyright (C) 2017 Marco Molteni.
% See LICENSE.txt for license information.
%
-module(mock_io).

-export([start_link/0, stop/1]).
-export([inject/2, extract/1, unread/1]).

-spec start_link() -> pid().
start_link() ->
    spawn_link(fun() -> loop({[], []}) end).

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

call(Pid, MsgOut) ->
    Pid ! {self(), MsgOut},
    receive
        {Pid, MsgIn} -> MsgIn
    after 1000 -> erlang:error(timeout)
    end.

loop({Input, Output}) ->
    receive
        {From, {inject, String}} ->
            From ! {self(), injected},
            loop({[String | Input], Output});
        {From, unread} ->
            From ! {self(), {unread, lists:flatten(lists:reverse(Input))}},
            loop({Input, Output});
        {From, extract} ->
            From ! {self(), {extracted, ""}},
            loop({Input, Output});
        {From, stop} ->
            From ! {self(), stopped};
        Any ->
            erlang:error({unexpected, Any})
    end.

