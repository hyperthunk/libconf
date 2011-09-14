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
-export([executable_name/1, library_path_env/1, path_sep/1]).
-export([locate_library/2, detect_arch/1, load_path/1]).
-export([code_dir/0, relative_path/1]).

-export([inspect/1]).

%% TODO: refactor the inspect_? code to pass around the OS type/family and match on it

inspect(Options) ->
    OptFile = relative_path(["build", "cache", "config.options"]),
    log:out("writing config.options to ~s~n", [OptFile]),
    file:write_file(OptFile, libconf:printable(Options)),
    run_inspect_env(OptFile, Options, false),
    ErlEnv = file:consult(relative_path(["build", "cache", "config.cache"])),
    OS = inspect_os(),
    Arch = detect_os_arch(OS),
    WordSize = detect_long_bit(OS),
    #os_conf{ os=OS, arch=Arch, wordsize=WordSize, erlang=ErlEnv }.

run_inspect_env(OptFile, Options, true) ->
    BinDir = filename:join(proplists:get_value("erlang", Options), "bin"),
    case locate_escript(BinDir) of
        {default, Exe} ->
            run_external(Exe, OptFile);
        Escript when is_list(Escript) ->
            run_external(Escript, OptFile)
    end;
run_inspect_env(OptFile, Options, false) ->
    BinDir = filename:join(proplists:get_value("erlang", Options), "bin"),
    case locate_escript(BinDir) of
        {default, _Exe} ->
            Path = ensure_load_env_module(),
            log:verbose("compiling load_env...~n"),
            case compile:file(Path,
                        [verbose,report_errors,report_warnings,export_all,
                         {outdir,relative_path(["build", "cache"])}]) of
                T when is_tuple(T) ->
                    [H|Rest] = erlang:tuple_to_list(T),
                    case H of
                        ok ->
                            ModName = erlang:hd(Rest),
                            log:out("Running ~p using the current emulator~n",
                                [ModName]),
                            code:add_patha(relative_path(["build", "cache"])),
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
    LoadEnvModule = ensure_load_env_module(),
    LoaderResults = sh:exec(Escript ++ " " ++ LoadEnvModule ++ " " ++ OptFile),
    case LoaderResults of
        {error, {_Rc, _Data}=Err} ->
            %% TODO: deal with output messages to the user
            libconf:abort("ERROR: ~p~n", [Err]);
        Ok ->
            Ok
    end.

ensure_load_env_module() ->
    File = relative_path(["build", "cache", "load_env.erl"]),
    case filelib:is_regular(File) of
        true ->
            File;
        false ->
            {ok, Bin} = load_env_template:render(),
            file:write_file(File, list_to_binary(Bin), [write]),
            File
    end.

inspect_os() ->
    case os:type() of
        {unix, _} ->
            OS = uname("-s"),
            Vsn = os_vsn(),
            {OS, Vsn};
        {win32, _} ->
            {windows, unknown}
    end.

locate_library(Path, Lib) ->
    case filelib:fold_files(Path, Lib, true,
                            fun(F, Acc) -> [F|Acc] end, []) of
        [] ->
            undefined;
        [LibFile|_] ->
            #library{ path=Path, lib=LibFile, arch=detect_arch(LibFile) }
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

%% TODO: make this work across the board...
library_path_env({windows, _})  -> "LIB";
library_path_env({darwin, _})   -> "DYLD_LIBRARY_PATH";
library_path_env(_)             -> "LD_LIBRARY_PATH".

path_sep({windows,_}) -> ";";
path_sep(_)           -> ":".

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
            case lists:suffix(".bat", Exe) of
                true -> Exe;
                false ->
                    case lists:suffix(".exe", Exe) of
                        true ->
                            Exe;
                        false ->
                            string:join([Exe, "exe"], ".")
                    end
            end;
        _ ->
            Exe
    end.

detect_arch(LibPath) ->
    grep_for_arch(trim_cmd(os:cmd("file " ++ LibPath))).

detect_os_arch({windows,_}) -> 
    %% TODO: handle on windows
    'x86';
detect_os_arch(_) ->
    grep_for_arch(uname("-a")).

detect_long_bit({windows,_}) -> 
    %% TODO: fix on windows
    32;
detect_long_bit(_) ->
    list_to_integer(trim_cmd(os:cmd("getconf LONG_BIT"))).

grep_for_arch(String) ->
    case re:run(String, "(64-bit|x86_64|ia64|amd64)", [{capture,first,list}]) of
        {match, [_M|_]} -> 'x86_64';
        _ ->
            case re:run(String, "(32-bit|i386|i486|i586|i686|x86)", 
                        [{capture,first,list}]) of
                {match, [_|_]} ->
                    %% TODO: deal with ia32 and amd32
                    'x86'
            end
    end.

os_vsn() ->
    [ list_to_integer(I) || I <- string:tokens(uname("-r"), ".") ].

uname(Flag) ->
    trim_cmd(os:cmd("uname " ++ Flag)).

trim_cmd(Output) ->
    re:replace(Output, "\\s", "", [{return, list}]).

code_dir() ->
    case os:getenv("ERL_LIBS") of
        false -> code:lib_dir();
        Path  -> Path
    end.

relative_path(SuffixList) ->
    filename:absname(filename:join(filename:dirname(escript:script_name()),
                     filename:join(SuffixList))).
