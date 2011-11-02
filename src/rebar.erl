-module(rebar).
-export([render/1]).
-export([install/3]).

render(Vars) ->
    log:to_file("Evaluating rebar in config dictionary~n", []),
    {ok, kvc:path(<<"rebar.output">>, Vars)}.

install(Repo, Branch, DestDir) ->
    Path = filename:join(DestDir, "rebar"),
    Exe = filename:join(Path, "rebar"),
    log:verbose("checking for rebar binary at ~s~n", [Exe]),
    case filelib:is_regular(Exe) of
        true ->
            {exists, Exe};
        false ->
            case scm:get(git, {Repo, Branch}, DestDir) of
                ok ->
                    case sh:exec("./bootstrap", [{cd, Path}]) of
                        {ok, _} ->
                            {installed, Exe};
                        Err ->
                            Err
                    end;
                Err ->
                    Err
            end
    end.
