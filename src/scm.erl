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
-module(scm).

-export([detect/0, log/0, userinfo/0, userinfo/1]).

detect() ->
    case file:list_dir(env:root_dir()) of
        {ok, Files} ->
             [ list_to_atom(tl(F)) || 
                F <- Files, Vcs <- [".hg", ".git", "_darcs"], F == Vcs ];
        _ ->
            unknown
    end.

log() ->
    log(atom_to_list(detect())).

log(SCM) ->
    env:trim_cmd(os:cmd(SCM ++ " log")).

userinfo() ->
    userinfo(detect()).

userinfo([hg]) ->
    case re:run(env:trim_cmd("hg showconfig ui.username"), 
                "^(?<username>.*) <(?<email>.*)>$", 
                [{capture,[username, email], list}]) of
        {match, [User,Email|_]} ->
            {User, Email};
        _ ->
            env:username()
    end;
userinfo([git]) ->
    User = case sh:exec("git config --get user.name") of
        {ok, Usr} ->
            Usr;
        _ ->
            env:username()
    end,
    Email = case sh:exec("git config --get user.email") of
        {ok, Addr} ->
            Addr;
        _ ->
            env:username()
    end,
    {env:trim_cmd(User), env:trim_cmd(Email)};
userinfo([darcs]) ->
    case env:trim_cmd(sh:exec("darcs show authors")) of
        {ok, UserInfo} ->
            case re:run(UserInfo, "[\\d\\s]*(.*)", 
                        [{capture, all, list}, multiline]) of
                {match, [_, AUser]} ->
                    Tokens = string:tokens(AUser, " "),
                    Email = erlang:hd(lists:reverse(Tokens)),
                    User = string:join(lists:reverse(
                        erlang:tl(lists:reverse(Tokens))), " "),
                    {User, Email};
                _ -> 
                    env:username()
            end;
        _ ->
            env:username()
    end.

