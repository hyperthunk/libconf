-module(action).
-export([apply/4]).

-include("libconf.hrl").
-include_lib("kernel/include/file.hrl").


apply(Action=#action{ type=Type }, Checks, Env, Config) ->
    case failed_checks(Action, Checks) of
        {_, []} ->
            apply_it(Action, Checks, Env, Config);
        {_, Failed} ->
            log:out("Skipping ~p due to failed prerequisites ~p~n",
                    [Type, Failed]),
            ok
    end.

apply_it(#action{ type=chmod, target=Target,
                  config={mode, Mode}, dir=Dir }, _, _, _) ->
    Filename = filename:join(directory(Dir), Target),
    {ok, #file_info{mode = Prev}} = file:read_file_info(Filename),
    NewMode = Prev bor Mode,
    case file:change_mode(Filename, NewMode) of
        ok ->
            log:verbose("chmod ~p ~s succeeded~n", [NewMode, Filename]),
            ok;
        {error, Reason} ->
            Err = file:format_error(Reason),
            log:out("Unable to change mode on ~s: ~s~n", [Target, Err]),
            ok
    end.

directory(undefined) -> ?CWD;
directory(Dir) -> Dir.

failed_checks(#action{ checks=[] }, _) ->
    [];
failed_checks(#action{ checks=Required }, Checks) ->
    log:to_file("Check ~p against ~p~n", [Required, Checks]),
    lists:foldl(fun(#check{ name=Req, result='failed' }, {PreReqs, Acc}=AccIn) ->
                    case lists:member(Req, PreReqs) of
                        true ->
                            {PreReqs, [Req|Acc]};
                        false ->
                            AccIn
                    end;
                   (_, Acc) -> Acc
                end, {Required, []}, Checks).
