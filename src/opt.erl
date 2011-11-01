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
-module(opt).

-export([parse_args/2, help/1, eval/2]).

eval([H|_]=S, Opts) when is_integer(H) ->
    case re:split(S, "(\\$|\\%)\\{([^\\}]*)\\}", [{return, list}, trim]) of
        [Data] ->
            Data;
        Parts when is_list(Parts) ->
            lists:flatten(lists:reverse(lists:foldl(merge_opts(Opts), [], Parts)))
    end;
eval(S, _) ->
    S.

merge_opts(Opts) ->
    fun(E, [H|Acc]) when H == "$" ->
           [option(E, Opts)|Acc];
       (E, [T|Acc]) when T == "%" ->
           [scope_eval(E, Opts)|Acc];
       (E, Acc) ->
           [E|Acc]
    end.

scope_eval(S, ScopeEnv) ->
    S2 = re:replace(S, "(([\\w]+)\\.[\\w]+)", "kvc:path(\\1, Bindings)", 
                    [{return, list}]) ++ ".",
    log:to_file("scope_eval [~p] = ", [S2]),
    {ok, Scanned, _} = erl_scan:string(S2),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    case erl_eval:exprs(Parsed, [{'Bindings', ScopeEnv}]) of
        {value, Val, _} ->
            log:to_file(io_lib:format("~p~n", [Val])), Val;
        Other ->
            log:to_file(" FAILURE [exception: ~p]~n", [Other]), S
    end.

option(E, Opts) ->
    case kvc:path(E, Opts) of
        [] -> E;
        Other -> Other
    end.

%% Argument Parsing...

parse_args(Args, Options0) ->
    Options = case lists:keymember("--(?<option>.*)", 1, Options0) of
        false ->
            [{"--with-(?<lib>.*)=(?<erlydtl>.*)", fun erlang:list_to_tuple/1,
                    [lib, erlydtl],
                [{"erlydtl", "Location of erlydtl", "deps/erlydtl/ebin"}]},
             {"--(?<option>.*)",
                fun([X]) -> {X, enabled} end, [option],
                [{"verbose",
                    "Print lots of info out during configure process", disabled},
                 {"help", "Print out help and exit", undefined}]}|Options0];
        true ->
            libconf:abort("Internal Error: ~n"
                          "The --(option) group is reversed for internal use!")
    end,
    io:format("Options: ~p~n", [Options]),
    Opts = lists:flatten(lists:foldl(
        fun(Arg, Conf) -> [parse(Arg, Options)|Conf] end, [], Args)),
    apply_defaults(Opts, Options).

apply_defaults(Opts, Options) ->
    lists:foldl(
        fun({Thing,_,Default}, Acc) ->
            case lists:keymember(Thing, 1, Acc) of
                true ->
                    Acc;
                false ->
                    lists:keystore(Thing, 1, Acc, {Thing, Default})
            end
        end,
        lists:keysort(1, Opts),
        [ I || A <- Options, I <- element(4, A) ]).

parse(Arg, Options) when is_list(Arg) ->
    first_match(Arg, Options).

first_match(Arg, []) ->
    exit_badarg(Arg);
first_match(Arg, [Opt|Rest]) ->
    case match(Arg, Opt) of
        undefined ->
            first_match(Arg, Rest);
        Match ->
            Match
    end.

match(Arg, {Rx, Settings, Captures, Filter}) ->
    case re:run(Arg, Rx, [global, {capture, Captures,list}]) of
        {match, [Matches]} when is_list(Matches) ->
            Filtered = filter(Matches, Filter),
            case Filtered of
                [] ->
                    undefined;
                Found ->
                    case Settings of
                        S when is_list(S) ->
                            lists:zip(Found, Settings);
                        Merge when is_function(Merge) ->
                            Merge(Matches);
                        Other ->
                            libconf:abort("Invalid Settings: ~p~n", [Other])
                    end
            end;
        _NonMatch ->
            undefined
    end.

exit_badarg(Arg) ->
    libconf:abort("Unrecognised Option(s) ~s~n", [Arg]).

help(Options) ->
    io:format(
"`configure' configures this package to adapt to any supported system.

Usage: ./configure [OPTIONS]

Options:~n"),
    Indent = "    ",
    F = fun({Thing, Comments, Default}, {Rx, _, C, _}) ->
            Text = rejoin_option(Thing, Rx, C),
            Spacer = lists:concat(lists:duplicate(28 - length(Text), " ")),
            case Default of
                undefined ->
                    io:format("~s~s~s~s~n", [Indent, Text, Spacer, Comments]);
                Atom when is_atom(Atom) ->
                    io:format("~s~s~s~s [~p]~n",
                        [Indent, Text, Spacer, Comments, Default]);
                String when is_list(String) ->
                    io:format("~s~s~s~s [~s]~n",
                        [Indent, Text, Spacer, Comments, Default])
            end;
           (break, _) ->
            io:format("~n");
           (Thing2, _) ->
            io:format("~s~p~n", [Indent, Thing2])
        end,
    [ F(I, A) || A <- Options, I <- (element(4, A) ++ [break]) ].

rejoin_option(Thing, Rx, C) ->
    case re:run(Rx, "\\(\?\<([^\>]+)\>[^\\)]*\\)",
                [global, {capture, [1], list}]) of
        {match, Matches} ->
            %% NB: our ordering comes from the matches themselves
            Indexes = erlang:tl([ string:str(C,
                [list_to_atom(lists:nth(1, M))]) || M <- Matches ]),
            lists:foldl(fun(Idx, Acc) when Idx >= 0 ->
                            re:replace(Acc, "(\\([^\\)]*\\))",
                                string:to_upper(atom_to_list(lists:nth(Idx, C))),
                                    [ungreedy, {return,list}]);
                           (_, Acc) -> Acc
                        end,
                        re:replace(Rx, "(\\([^\\)]*\\))",
                            Thing, [ungreedy, {return,list}]),
                        Indexes);
        _ ->
            Rx
    end.

filter(M, []) ->
    M;
filter(Match=[H|_], Filter) when is_list(Match) andalso is_integer(H) ->
    case lists:any(fun(X) -> X == Match end, allowed(Filter)) of
        false ->
            [];
        true ->
            Match
    end;
filter(Matches, Filter) ->
    [ X || X <- lists:map(fun(X) -> filter(X, Filter) end, Matches),
           X =/= [] ].

allowed(Filter) ->
    lists:map(fun({E,_,_}) -> E;(N) -> N end, Filter).
