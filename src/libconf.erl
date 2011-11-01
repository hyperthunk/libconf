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
-module(libconf).

-include("libconf.hrl").
-export([abort/1, abort/2]).
-export([copyright/0]).
-export([configure/3, printable/1, as_string/1]).

copyright() ->
    Defaults = copyright_default(),
    {{Y,_,_},_} = calendar:universal_time(),
    case filelib:fold_files(env:root_dir(), "copyright.config", true,
                                   fun(F, Acc) -> [F|Acc] end, []) of
        [] ->
            Defaults;
        [P|_] ->
            case file:consult(P) of
                {ok, Data} ->
                    [{year, Y}|Data];
                _ ->
                    Defaults
            end
    end.

copyright_default() ->
    case scm:userinfo() of
        {User, Email} ->
            [{author, User}, {email, Email}];
        _ ->
            [{author, env:username()}, {email, ""}]
    end.

configure(Args, AvailableOpts, Rules) ->
    log:reset(),
    Options = opt:parse_args(Args, AvailableOpts),
    put(verbose, proplists:get_value("verbose", Options)),
    case kvc:path(help, Options) of
        enabled ->
            opt:help(AvailableOpts), halt(0);
        _ ->
            log:to_file("OPTIONS: ~s~n", [printable(Options)]),
            case proplists:get_value("nocache", Options, disabled) of
                enabled ->
                    env:clear_cache();
                _ ->
                    ok
            end,
            Env = env:inspect(Options),
            apply_rules(Env, Rules, Options)
    end.

apply_rules(Env, Rules, Options) ->
    Checks = proplists:get_value(checks, Rules, []),
    Results = [ check:check(Check, Env, Options) || Check <- Checks ],
    case [ C || C <- Results, C#check.result =/= 'passed' andalso
                              C#check.mandatory =:= true ] of
        [] ->
            ok;
        Failures ->
            [ write_failure_logs(C) || C <- Failures ],
            abort("Cannot proceed. Please fix these issues and try again.~n")
    end,
    Templates = apply_templates(Results, Env, Rules, Options),
    apply_warnings(Results, Templates, Env, Rules, Options).

apply_templates(Checks, Env, Rules, Config) ->
    Templates = proplists:get_value(templates, Rules, []),
    [ template:render(T, Checks, Env, Config) || T <- Templates ].

apply_warnings(_Results, _Templates, _Env, _Rules, _Options) ->
    ok.

write_failure_logs(#check{ name=Name, output=Err }) ->
    log:out("Mandatory Check ~p failed. "
              "See build/cache/config.log for details.~n", [Name]),
    log:to_file("ERROR: ~s: ~s~n", [Name, Err]).

%% Utilities

as_string(Thing) when is_atom(Thing) ->
    atom_to_list(Thing);
as_string(Thing) when is_list(Thing) ->
    Thing.

printable(Term) ->
    erl_prettypr:format(erl_parse:abstract(Term))  ++ ".".

abort(Msg) ->
    abort(Msg, []).

abort(Msg, Args) ->
    log:out(Msg, Args), halt(1).
