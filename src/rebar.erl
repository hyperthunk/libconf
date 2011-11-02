-module(rebar).
-export([render/1]).

render(Vars) ->
    log:to_file("Evaluating rebar in config dictionary~n", []),
    {ok, kvc:path(<<"rebar.output">>, Vars)}.
