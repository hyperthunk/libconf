-module(config).

-export([read/1]).

-include("libconf.hrl").

read(File) ->
    Bs = erl_eval:new_bindings(),
    {ok, Cwd} = file:get_cwd(),
    case file:path_open([Cwd], File, [read]) of
        {ok, Fd, _} ->
            try eval_stream(Fd, return, Bs) of
                {ok, R} ->
                    process_it(R);
                E1 ->
                    E1
            after
                file:close(Fd)
            end;
        E2 ->
            E2
    end.

process_it({build_config, Config}) ->
    {build_config, [ process_it(Elem) || Elem <- Config ]};
process_it({Item, Config}) 
        when Item == rules orelse
             Item == checks orelse
             Item == templates orelse
             Item == actions ->
    {Item, [ process_it(Elem) || Elem <- Config ]};
process_it({check, Proplist}) ->
    {Fields, _, _} = get_fields(),
    set_fields(#check{}, Proplist, Fields);
process_it({template, Proplist}) ->
    {_, Fields, _} = get_fields(),
    set_fields(#template{}, Proplist, Fields);
process_it({action, Proplist}) ->
    {_, _, Fields} = get_fields(),
    set_fields(#action{}, Proplist, Fields);
process_it(Item) ->
    Item.

set_fields(Record, Proplist, Fields) ->
    {Names, _} = lists:unzip(Fields),
    lists:foldl(fun(Field, Rec) ->
                    case lists:keyfind(Field, 1, Proplist) of
                        {Field, Value} ->
                            setelement(index_of(Field, Fields), Rec, Value);
                        false ->
                            Rec
                    end
                end, Record, Names).

%% TODO: do this without using the process dict
get_fields() ->
    case get({?MODULE, field_mappings}) of
        undefined ->
            Mappings = 
                {index_map(record_info(fields, check)),
                 index_map(record_info(fields, template)),
                 index_map(record_info(fields, action))},
            put({?MODULE, field_mappings}, Mappings),
            Mappings;
        M ->
            M
    end.

index_map(List) ->
    lists:zip(List, lists:seq(1, length(List))).

index_of(Item, Map) ->
    {_, Index} = lists:keyfind(Item, 1, Map),
    Index.

config_fn_handler(Name, Arguments) ->
    apply(?MODULE, Name, Arguments).

ext_config_fn_handler(Func, Args) when is_function(Func) ->
    Func(Args);
ext_config_fn_handler({Mod, Func}, Args) ->
    apply(Mod, Func, Args).

eval_stream(Fd, Handling, Bs) ->
    eval_stream(Fd, Handling, 1, undefined, [], Bs).

eval_stream(Fd, H, Line, Last, E, Bs) ->
    eval_stream2(io:parse_erl_exprs(Fd, '', Line), Fd, H, Last, E, Bs).

eval_stream2({ok,Form,EndLine}, Fd, H, Last, E, Bs0) ->
    try erl_eval:exprs(Form, Bs0, {value, fun config_fn_handler/2},
                                  {value, fun ext_config_fn_handler/2}) of
        {value,V,Bs} ->
            eval_stream(Fd, H, EndLine, {V}, E, Bs)
    catch Class:Reason ->
        Error = {EndLine,?MODULE,{Class,Reason,erlang:get_stacktrace()}},
        eval_stream(Fd, H, EndLine, Last, [Error|E], Bs0)
    end;
eval_stream2({error,What,EndLine}, Fd, H, Last, E, Bs) ->
    eval_stream(Fd, H, EndLine, Last, [What | E], Bs);
eval_stream2({eof,EndLine}, _Fd, H, Last, E, _Bs) ->
    case {H, Last, E} of
        {return, {Val}, []} ->
            {ok, Val};
        {return, undefined, E} ->
            {error, hd(lists:reverse(E, [{EndLine,?MODULE,undefined_script}]))};
        {ignore, _, []} ->
            ok;
        {_, _, [_|_] = E} ->
            {error, hd(lists:reverse(E))}
    end.
