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
-module(sh).

-export([exec/1, exec/2]).

exec(Command) ->
    exec(Command, []).

exec(Command, Env) ->
    %Command = case os:type() of
    %   {win32,_} ->
    %        [Exe|Rest] = string:tokens(Command0, " "),
    %        BaseName = filename:basename(Exe, ".exe"),
    %        BatchFile = BaseName ++ ".bat",
    %        case filelib:is_regular(BatchFile) of
    %            true ->
    %                ok;
    %            false ->
    %                ok = file:write_file(BatchFile,
    %                            io_lib:format("@echo off\n~s %*", [Exe]), [write])
    %        end,
    %        string:join([BatchFile|Rest], " ");
    %    _ ->
    %        Command0
    %end,
    log:to_file("exec `~s'~n", [Command]),
    PortSettings = [exit_status, {line, 16384}, stderr_to_stdout, hide],
    sh_loop(open_port({spawn, Command}, PortSettings ++ Env), []).

sh_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, [Line ++ "\n" | Acc]);
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.
