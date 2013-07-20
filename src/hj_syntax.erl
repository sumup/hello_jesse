-module(hj_syntax).

-export([generate_forms/2]).

%%%============================================================================

%% @doc
%% Generate module containing functions used for JSON Schema params validation.
%% Each function does the following:
%% ```
%%  method_foobar() ->
%%      Schema = {[...]},
%%      fun
%%          (WantType, {Params}) ->
%%              hj_utils:validate({Params}, Schema, WantType);
%%          (_, Params) when is_list(Params) ->
%%              {error, <<"unsuported_params_as_array">>}
%%      end.
%% '''
-spec generate_forms(binary(), module()) -> {ok, binary()} | {error, term()}.
generate_forms(JsonSchemas, ModName) ->
    try
        Schemas = decode_schemas(JsonSchemas),
        Forms = [module(ModName) | functions(Schemas)],
        {ok, erl_syntax:revert_forms(Forms)}
    catch
        throw:{error, Reason} -> {error, Reason}
    end.

%%%============================================================================
%%% Syntax tree generator
%%%============================================================================

module(Name) ->
    erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]).

functions(Schemas) ->
    [function(S) || S <- Schemas].

function(Schema) ->
    Name =      get_function_name(Schema),
    SchemaVal = erl_syntax:abstract(Schema),

    erl_syntax:function(
        erl_syntax:atom(Name),
        [erl_syntax:clause([], [], [validation_fun(SchemaVal)])]
    ).

validation_fun(SchemaVal) ->
    ParamsVar       = erl_syntax:variable('Params'),
    WantTypeVar     = erl_syntax:variable('WantType'),
    IsTuple         = erl_syntax:application(erl_syntax:atom(erlang),
                                             erl_syntax:atom(is_tuple),
                                             [ParamsVar]),
    IsTupleGuard    = erl_syntax:infix_expr(IsTuple,
                                            erl_syntax:operator("=:="),
                                            erl_syntax:atom('true')),
    CallValidate    = erl_syntax:application(erl_syntax:atom(hj_utils),
                                             erl_syntax:atom(validate),
                                             [ParamsVar, SchemaVal,
                                              WantTypeVar]),
    IsList          = erl_syntax:application(erl_syntax:atom(erlang),
                                             erl_syntax:atom(is_list),
                                             [ParamsVar]),
    IsListGuard     = erl_syntax:infix_expr(IsList,
                                            erl_syntax:operator("=:="),
                                            erl_syntax:atom('true')),
    NotSupportedErr = erl_syntax:abstract({error,
                                           <<"unsuported_params_as_array">>}),
    erl_syntax:fun_expr(
        [
            erl_syntax:clause([WantTypeVar, ParamsVar],
                              [IsTupleGuard],
                              [CallValidate]),
            erl_syntax:clause([WantTypeVar, ParamsVar],
                              [IsListGuard],
                              [NotSupportedErr])
        ]
    ).

%%%============================================================================
%%% Helpers
%%%============================================================================

get_function_name(Schema) ->
    case get_value(<<"title">>, Schema) of
        undefined ->
            throw({error, {title_not_found, Schema}});
        Title when is_binary(Title) ->
            list_to_atom(binary_to_list(Title))
    end.

decode_schemas(Json) ->
    try
        jiffy:decode(Json)
    catch
        throw:{error, _Reason} ->
            throw({error, invalid_json_schema_file})
    end.

get_value(Key, {Proplist}) ->
    proplists:get_value(Key, Proplist).
