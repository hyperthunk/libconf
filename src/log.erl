%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(log).

-include("libconf.hrl").

-export([reset/0]).
-export([verbose/1, verbose/2, out/1, out/2, to_file/1, to_file/2]).

logfile() ->
    env:cached_filename("config.log").

reset() ->
    TimeStamp = io_lib:format(lists:duplicate(82, "-") ++ "~n" ++
                              "Config Run: ~s\n" ++
                              lists:duplicate(82, "-") ++ "~n",
                              [iso_8601_fmt(erlang:localtime())]),
    file:write_file(logfile(), TimeStamp, [write]).

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

verbose(Msg) ->
    verbose(Msg, []).

verbose(Msg, Args) ->
    case get(verbose) of
        enabled ->
            out(Msg, Args);
        _ ->
            ok
    end.

to_file(Msg) ->
    to_file(Msg, []).

to_file(Msg, Args) ->
    Data = io_lib:format(Msg, cleanup(Args)),
    file:write_file(logfile(), Data, [append]),
    Data.

out(Msg) -> out(Msg, []).

out(Msg, Args) ->
    io:format(to_file(Msg, Args)).

cleanup(Args) when is_list(Args) ->
    [ cleanup(A) || A <- Args ];
cleanup(Check=#check{ type=rebar }) ->
    Check#check{ output=(<<"binary (truncated)...">>) };
cleanup(Other) ->
    Other.
