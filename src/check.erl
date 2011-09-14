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
-module(check).

-include("libconf.hrl").
-export([check/3]).

check(C=#check{ name=Name, type=library, capture=Capture,
    data=#require{ include=Include, path=LibPath, find=Find,
        incl_path=InclPath, code_path=CodePath }}, #os_conf{ os=OS }, Config) ->

    NameAsString = as_string(Name),
    log:out("check ~s... ", [NameAsString]),
    Result = case env:locate_library(opt:eval(LibPath, Config), 
                                     opt:eval(Find, Config)) of
        undefined ->
            C#check{ result='failed', output=as_string(Name) ++ " not found" };
        #library{ lib=_SoFile, arch=LibArch } ->
            LdPath = [ opt:eval(I, Config) || I <- CodePath ],
            IncludePath = [ opt:eval(I, Config) || I <- InclPath ],
            Src = check_lib(NameAsString, Include, Capture),
            Target = filename:join(filename:dirname(Src), 
                                   filename:basename(Src, ".c")),
            case cc:compile_and_link(Src, Target, IncludePath,
                                     LdPath, LibArch, OS, Config) of
                {error, ErrMsg} when is_list(ErrMsg) ->
                    C#check{ result='failed', output=ErrMsg };
                {error, {_Rc, StdIo}} ->
                    C#check{ result='failed', output=StdIo };
                {ok, StdOut} ->
                    case Capture of
                        undefined ->
                            C#check{ result='passed', output=StdOut };
                        _Defined ->
                            case sh:exec(Target) of
                                {error, {_Rc, ErrTxt}} ->
                                    C#check{ result='failed', output=ErrTxt };
                                {ok, Output} ->
                                    C#check{ result='passed', output=Output }
                            end
                    end
            end
    end,
    log:out("~p~n", [Result#check.result]),
    Result.

check_lib(Name, Include, Capture) ->
    Vars = case Capture of
        undefined ->
            [{incl, Include}];
        _ ->
            [{incl, Include},{stdout_check, Capture}]
    end,
    {ok, Bin} = check_lib_template:render(dict:from_list(Vars)),
    SrcFile = filename:join(outdir(), "check_" ++ Name ++ ".c"),
    %% io:format("writing ~s to ~s~n", [Bin, SrcFile]),
    ok = file:write_file(SrcFile, Bin, [write]),
    SrcFile.

outdir() ->
    env:relative_path(["build", "cache"]).

as_string(Thing) when is_atom(Thing) ->
    atom_to_list(Thing);
as_string(Thing) when is_list(Thing) ->
    Thing.
