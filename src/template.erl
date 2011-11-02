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
-module(template).

-include("libconf.hrl").
-export([render/4]).

render(T0=#template{ name=Name, module=Mod },
        Checks, OsEnv, Config) ->
    %% the vars passed to the template will contain
    %% (a) config: defaults set by the template itself
    %% (b) options: parsed from the command line
    %% (c) environment: the #os_conf{} record for this environment
    %% (d) for each #check{} record, a proplist of its values mapped to its name
    %% (e) for each #check{} record, a proplist of its data mapped to <name>_data
    log:verbose("generating ~p~n", [Name]),

    %% templates must specify which check and/or check-data items they use
    CheckData = filter(checks, T0, Checks),
    log:to_file("found checks: ~p~n", [CheckData]),

    %% templates can add an optional module location for pre-processing
    code:ensure_loaded(T0#template.pre_render),
    T1 = case erlang:function_exported(T0#template.pre_render, pre_render, 4) of
        false ->
            log:to_file("~p has no pre_render hook.~n", [Name]), T0;
        true ->
            PreRenderMod = T0#template.pre_render,
            log:to_file("executing ~p pre_render hook in module ~p~n",
                        [Name, PreRenderMod]),
            PreRenderMod:pre_render(T0, CheckData, OsEnv, Config)
    end,

    log:to_file("post_hook template state: ~n" ++ libconf:printable(T1) ++ "~n"),

    %% template defaults are also pre-processed for interpolated values/expressions
    Data = filter(data, T1, Checks),
    VarList = [{options, Config}, unpack(OsEnv)] ++ CheckData ++ Data,
    Defaults2 = case T1#template.defaults of
        [] -> [];
        _ ->
            lists:foldl(fun({K, Val}, Acc) ->
                            lists:keyreplace(K, 1, Acc, {K, opt:eval(Val, Acc)})
                        end, T1#template.defaults, T1#template.defaults)
            % [ {K, opt:eval(Val, VarList)} || {K, Val} <- T1#template.defaults ]
    end,
    FullVars = [{config, Defaults2}] ++ VarList ++ libconf:copyright(),
    log:to_file("evaluating ~p with vars ~p~n", [Name, FullVars]),
    Vars = dict:from_list(FullVars),

    TempFileLocation =
        env:cached_filename(libconf:as_string(Name) ++ ".output.raw"),
    Template = case Mod of
        undefined -> Name;
        _ -> Mod
    end,

    case Template:render(Vars) of
        {ok, Bin} ->
            ok = file:write_file(TempFileLocation, Bin, [write]),
            DestFile = T1#template.output,
            case filelib:is_regular(DestFile) of
                true  ->
                    Mode = case T1#template.overwrite of
                        true -> write;
                        false -> append
                    end,
                    ok = file:write_file(DestFile, Bin, [Mode]);
                false ->
                    {ok, _} = file:copy(TempFileLocation, DestFile)
            end;
        Other ->
            libconf:abort("ERROR: failed to write template ~p: ~p~n",
                          [Name, Other])
    end.

filter(checks, #template{ checks=undefined }, _) ->
    [];
filter(checks, #template{ checks=Required }, Checks) ->
    log:to_file("filtering ~p against ~p~n", [Checks, Required]),
    [ unpack(C, all) || R <- Required, C=#check{ name=N } <- Checks, N =:= R ];
filter(data, #template{ data=undefined }, _) ->
    [];
filter(data, #template{ data=Required }, Checks) ->
    [ unpack(C#check{name=rename(N)}, F) ||
                {R, F} <- Required, C=#check{name=N} <- Checks, N =:= R ].

rename(N) when is_atom(N) ->
    rename(atom_to_list(N));
rename(N) when is_list(N) ->
    list_to_atom(N ++ "_data").

unpack(#os_conf{os=OS, arch=Arch, wordsize=WS, erlang=ERL}) ->
    {environment, [{os, OS}, {arch, Arch}, {wordsize, WS}, {erlang, ERL}]}.

unpack(#check{name=N, mandatory=M, type=T, data=D,
              capture=C, output=O, result=R}, all) ->
    {N, [{mandatory, M}, {type,T}, {data, D},
         {capture, C}, {output, O}, {result, R}]};
unpack(#check{name=Name, data=Data }, data) ->
    case Data of
        #require{ path=P, arch=A, find=F, include=I,
                  incl_path=IP, code_path=CP } ->
            {Name, [{path, P}, {arch, A}, {find, F},
                    {include, I}, {incl_path, IP}, {code_path, CP}]};
        Other ->
            {Name, Other}
    end.
