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
-module(env).

-include("libconf.hrl").

-export([locate_escript/1, default_escript_exe/0]).
-export([find_executable/1, find_executable/2]).
-export([executable_name/1]).
-export([check_arch/1, wordsize/0, inspect_os/0]).
-export([locate_library/2, detect_arch/1, load_path/1]).
-export([code_dir/0]).
-export([relative_path/1]).

-export([inspect/1]).

%% TODO: refactor the inspect_? code to pass around the OS type/family and match on it

inspect(Options) ->
    OptFile = env:relative_path(["build", "cache", "config.options"]),
    log:out("writing config.options to ~s~n", [OptFile]),
    file:write_file(OptFile, libconf:printable(Options)),
    run_inspect_env(OptFile, Options, false),
    ErlEnv = file:consult(env:relative_path(["build", "cache", "config.cache"])),
    %% OciEnv = inspect_oci(Options),
    #os_conf{
        os=env:inspect_os(),
        arch=env:wordsize(),
        erlang=ErlEnv
    }.

run_inspect_env(OptFile, Options, true) ->
    BinDir = filename:join(proplists:get_value("erlang", Options), "bin"),
    case env:locate_escript(BinDir) of
        {default, Exe} ->
            run_external(Exe, OptFile);
        Escript when is_list(Escript) ->
            run_external(Escript, OptFile)
    end;
run_inspect_env(OptFile, Options, false) ->
    BinDir = filename:join(proplists:get_value("erlang", Options), "bin"),
    case env:locate_escript(BinDir) of
        {default, _Exe} ->
            Path = env:relative_path(["build", "load_env.erl"]),
            case compile:file(Path,
                        [verbose,report_errors,report_warnings,export_all,
                         {outdir,env:relative_path(["build", "cache"])}]) of
                T when is_tuple(T) ->
                    [H|Rest] = erlang:tuple_to_list(T),
                    case H of
                        ok ->
                            ModName = erlang:hd(Rest),
                            log:out("Running ~p using the current emulator~n",
                                [ModName]),
                            code:add_patha(env:relative_path(["build", "cache"])),
                            ModName:main([OptFile]);
                        error ->
                            log:out("Unable to compile load_env.erl - "
                                      "trying as external escript command!"),
                            run_inspect_env(OptFile, Options, true)
                    end;
                error ->
                    run_inspect_env(OptFile, Options, true)
            end;
        Escript when is_list(Escript) ->
            run_external(Escript, OptFile)
    end.

run_external(Escript, OptFile) ->
    LoaderResults = sh:exec(Escript ++ " " ++
                       env:relative_path(["build", "load_env.erl"]) ++ " " ++
                       OptFile),
    case LoaderResults of
        {error, {_Rc, _Data}=Err} ->
            %% TODO: deal with output messages to the user
            libconf:abort("ERROR: ~p~n", [Err]);
        Ok ->
            Ok
    end.

locate_library(Path, Lib) ->
    case filelib:fold_files(Path, Lib, true,
                            fun(F, Acc) -> [F|Acc] end, []) of
        [] ->
            undefined;
        [LibFile|_] ->
            #library{ path=Path, lib=LibFile, arch=detect_arch(LibFile) }
    end.

detect_arch(LibPath) ->
    case re:run(os:cmd("file " ++ LibPath),
                "(64-bit|i686|x86_64)", [{capture,first,list}]) of
        {match, [M|_]} ->
            case M of
                "64-bit" -> 'i686';
                Arch -> list_to_atom(Arch)
            end;
        _ ->
            case re:run(os:cmd("file " ++ LibPath),
                        "(32-bit|i386|x86)", [{capture,first,list}]) of
                {match, [A|_]} ->
                    case A of
                        "32-bit" -> 'x86';
                        Other32BitArch -> list_to_atom(Other32BitArch)
                    end
            end
    end.

load_path(Path) ->
    {ENV, PathSep} = case os:type() of
        {win32,_} ->
            {"PATH", ";"};
        {unix,darwin} ->
            {"DYLD_LIBRARY_PATH", ":"};
        _ ->
            {"LD_LIBRARY_PATH", ":"}
    end,
    NewPath = case os:getenv(ENV) of
        false ->
            Path;
        Existing ->
            Parts = string:tokens(Existing, PathSep),
            string:join([Path | Parts], PathSep)
    end,
    {ENV, NewPath}.

locate_escript(undefined) ->
    default_escript_exe();
locate_escript(Path) ->
    log:out("checking for escript in path '~s'~n", [Path]),
    case find_executable("escript", Path) of
        false ->
            locate_escript(undefined);
        Exe ->
            Exe
    end.

default_escript_exe() ->
    {default, find_executable("escript",
                    filename:join(code:root_dir(), "bin"))}.

find_executable(Exe) ->
    find_executable(Exe, undefined).

find_executable(Exe, undefined) when is_list(Exe) ->
    os:find_executable(executable_name(Exe));
find_executable(Exe, Path) when is_list(Exe) andalso is_list(Path) ->
    os:find_executable(executable_name(Exe), Path).

executable_name(Exe) ->
    case os:type() of
        {win32,_} ->
            case lists:suffix(".exe", Exe) of
                true ->
                    Exe;
                false ->
                    string:join([Exe, "exe"], ".")
            end;
        _ ->
            Exe
    end.

check_arch(Arch) ->
    case wordsize() =:= Arch of
        true -> enabled;
        false -> disabled
    end.

wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.

inspect_os() ->
    case os:type() of
        {unix, undefined} ->
            posix;
        {unix, Other} ->
            Other;
        {win32, _} ->
            windows
    end.

code_dir() ->
    case os:getenv("ERL_LIBS") of
        false -> code:lib_dir();
        Path  -> Path
    end.

relative_path(SuffixList) ->
    filename:absname(filename:join(filename:dirname(escript:script_name()),
                     filename:join(SuffixList))).


