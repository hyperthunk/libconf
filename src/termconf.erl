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
-module(termconf).
-include("libconf.hrl").
-export([pre_render/4]).

pre_render(T=#template{ template_source=Source, defaults=Defaults },
            _Checks, _OsConf, Opts) ->
    case file:consult(Source) of
        {ok, Terms} ->
            Conf = Opts ++ Terms ++ Defaults,
            NewDefaults =
                lists:foldl(fun({K, Val}, Acc) ->
                                    lists:keyreplace(K, 1, Acc,
                                                {K, opt:eval(Val, Acc)});
                               (Term, Acc) ->
                                    log:verbose("Unexpected term in termconf "
                                                "pre-render hook: ~p~n", [Term]),
                                    Acc
                            end, Conf, Conf),
            T#template{ defaults=NewDefaults };
        {error, Reason} ->
            libconf:abort("termconf pre-render hook failed - "
                          "unable to read ~s: ~p~n", [Source, Reason])
    end.
