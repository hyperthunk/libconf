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
-module(cc).

-export([find_compiler/1]).
-export([calculate_arch_flags/1]).

find_compiler(Options) ->
    case proplists:get_value("c-compiler", Options) of
        undefined ->
            case os:type() of
                {win32, _} ->
                    Path = filename:dirname(env:find_executable("cl")),
                    filename:join(Path, "Vcvarsall.bat");
                {unix,_} ->
                    env:find_executable("cc")
            end;
        UserSpecified ->
            log:verbose("Using user-specified compiler ~s~n", [UserSpecified]),
            UserSpecified
    end.

calculate_arch_flags('x86_64') ->
    case os:type() of
        {win32,_} ->
            %% consult http://msdn.microsoft.com/en-us/library/x4d2c09s.aspx
            %% and figure out how to handle amd64 on both and ia64 on unix
            "ia64";
        {unix,darwin} ->
            "-arch x86_64";
        {unix,_} ->
            "-m64"
    end;
calculate_arch_flags('x86') ->
    case os:type() of
        {win32,_} ->
            %% ref http://msdn.microsoft.com/en-us/library/x4d2c09s.aspx
            "x86";
        {unix,darwin} ->
            "-arch i386";
        {unix,_} ->
            "-m32"
    end.
