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
-export([configure/3]).

configure(Args, AvailableOpts, Rules) ->
    Options = opt:parse_args(Args, AvailableOpts),
    put(verbose, proplists:get_value(verbose, Options)),
    case lists:keymember(help, 1, Options) of
        true ->
            opt:help(AvailableOpts), halt(0);
        false ->
            log:out("~s~n", [printable(Options)]),
            Env = env:inspect(Options),
            log:out("~s~n", [printable(Env)]),
            apply_config(Env, Rules, Options)
    end.

apply_config(Env, Templates, Options) ->
    Setup = lists:map(fun(T) ->
                         configure_template(T, Env, Options) end, Templates),
    case has_errors(Setup) of
        true ->
            show_errors_and_exit(Setup);
        false ->
            apply_templates(Setup)
    end.

check("arch", _Env, _Options) ->
    ok.

configure_template(_Template, _Env, _Options) -> ok.

has_errors(_Setup) -> false.

show_errors_and_exit(_Setup) -> halt(1).

apply_templates(_Setup) -> ok.

%% Utilities

printable(Term) ->
    erl_prettypr:format(erl_parse:abstract(Term))  ++ ".".

abort(Msg) ->
    abort(Msg, []).

abort(Msg, Args) ->
    log:out(Msg, Args), halt(1).
