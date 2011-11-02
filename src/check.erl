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

-define(DEFAULT_REBAR_URL, "git://github.com/basho/rebar.git").

check(C=#check{ name=Name }, Env, Config) ->
    NameAsString = libconf:as_string(Name),
    log:to_file("pre-check data: " ++ libconf:printable(C) ++ "~n"),
    Result = do_check(NameAsString, C, Env, Config),
    log:out("~p~n", [Result#check.result]),
    log:to_file("post-check data: " ++ libconf:printable(Result) ++ "~n"),
    Result.

do_check(NameAsString, C=#check{ type=rebar, capture=undefined }, _, _) ->
    log:out("checking ~s .... ", [NameAsString]),
    fail(C, "Invalid check config: check.capture must contain "
            "a valid path to the destination directory");
do_check(NameAsString, C=#check{ type=rebar, data=undefined },
        OsConf, Config) ->
    do_check(NameAsString, C#check{ data={?DEFAULT_REBAR_URL, "HEAD"} },
             OsConf, Config);
do_check(NameAsString, C=#check{ type=rebar, 
                                  data={GitUrl, Branch}, 
                                  capture=DestDir }, _OsConf, _Config) ->
    log:out("checking ~s .... ", [NameAsString]),
    Path = filename:join(DestDir, "rebar"),
    Exe = filename:join(Path, "rebar"),
    log:verbose("checking for rebar binary at ~s~n", [Exe]),
    case filelib:is_regular(Exe) of
        true ->
            read_file_to_output(C, Exe);
        false ->
            case sh:exec("git clone -b " ++ Branch ++ " " ++ GitUrl ++ " rebar",
                         [{cd, DestDir}]) of
                {ok, _Stdout} ->
                    case sh:exec("./bootstrap", [{cd, Path}]) of
                        {ok, _} ->
                            {ok, Bin} = 
                                file:read_file(Exe),
                            C#check{ result='passed', output=Bin };
                        {error, {RC, Reason}} ->
                            fail(C, integer_to_list(RC) ++ " " ++ Reason)
                    end;
                {error, {Ec, Err}} ->
                    fail(C, integer_to_list(Ec) ++ " " ++ Err)
            end
    end;
do_check(NameAsString, C=#check{ type=include, capture=Capture,
            data=#require{ include=Include, incl_path=InclPath} },
            #os_conf{ os=OS }, Config) ->
    log:out("checking ~s .... ", [Include]),
    IncludePath = [ opt:eval(I, Config) || I <- InclPath ],
    Src = generate_check_source(NameAsString, Include, Capture),
    case cc:compile(Src, IncludePath, OS, Config) of
        {error, ErrMsg} when is_list(ErrMsg) ->
            fail(C, ErrMsg);
        {error, {_Rc, StdIo}} ->
            fail(C, StdIo);
        {ok, _StdOut} ->
            C#check{ result='passed' }
    end;
do_check(NameAsString, C=#check{ name=Name, type=library, capture=Capture,
            data=#require{ include=Include, path=LibPath, find=Find,
            incl_path=InclPath, code_path=CodePath }=D},
            #os_conf{ os=OS }, Config) ->
    log:out("checking ~s .... ", [NameAsString]),
    LPath = opt:eval(LibPath, Config),
    LFind = opt:eval(Find, Config),
    log:to_file("~nsearching for ~s in path ~p ...~n", [LFind, LPath]),
    case env:locate_library(LPath, LFind) of
        undefined ->
            fail(C, libconf:as_string(Name) ++ " not found");
        #library{ path=LocPath, lib=SoFile, arch=LibArch } ->
            log:out("(found ~s) .... ", [SoFile]),
            LdPath = [ opt:eval(I, Config) || I <- CodePath ],
            IncludePath = [ opt:eval(I, Config) || I <- InclPath ],
            Src = generate_check_source(NameAsString, Include, Capture),
            Target = filename:join(filename:dirname(Src),
                                   filename:basename(Src, ".c")),
            Res = case cc:compile_and_link(Src, Target, IncludePath,
                                     LdPath, LibArch, OS, Config) of
                {error, ErrMsg} when is_list(ErrMsg) ->
                    fail(C, ErrMsg);
                {error, {_Rc, StdIo}} ->
                    fail(C, StdIo);
                {ok, StdOut} ->
                    case Capture of
                        undefined ->
                            C#check{ result='passed', output=StdOut };
                        _Defined ->
                            TargetOutputLog = Target ++ ".log",
                            case sh:exec(Target ++ " | tee " ++ TargetOutputLog) of
                                {error, {_Rc, ErrTxt}} ->
                                    C#check{ result='failed', output=ErrTxt };
                                {ok, []} ->
                                    case file:read_file(TargetOutputLog) of
                                        {ok, Bin} ->
                                            C#check{ result='passed', output=Bin };
                                        _Err ->
                                            C#check{ result='failed',
                                                     output="N/A" }
                                    end;
                                {ok, Output} ->
                                    C#check{ result='passed', output=Output }
                            end
                    end
            end,
            Res#check{
                data=D#require{
                    path=LocPath,
                    arch=LibArch,
                    incl_path=IncludePath,
                    code_path=LdPath
                }
            }
    end.

read_file_to_output(C, File) ->
    {ok, Bin} = 
        file:read_file(File),
    C#check{ result='passed', output=Bin }.

fail(Check, Output) ->
    Check#check{ result='failed', output=Output }.

generate_check_source(Name, Include, Capture) ->
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
