-module(hj_utils).

-export([validate/3]).

%%%============================================================================

validate(Params, Schema, WantType) ->
    case jesse:validate_with_schema(Schema, Params) of
        {ok, NewParams} ->
            {ok, convert_to_want_type(WantType, NewParams)};
        {error, Reason} ->
            wrap_error(Reason)
    end.

%%%%%============================================================================

%% Wrap Jesse error and convert to Hello one.
wrap_error(_Error) ->
    io:format("Error: ~p~n", [_Error]),
    <<"todo">>.
%wrap_error({data_invalid, Value, not_correct_type, JsonSchema}) -> <<"todo1">>.

%% Convert params to WantType. The params in WantType will be passed to
%% Hello `handle_request' callbacks.
convert_to_want_type(object, Params) -> Params;
convert_to_want_type(list, {Params}) -> strip_keys(Params);
convert_to_want_type(proplist, {Params}) -> Params.

strip_keys(Params) -> lists:map(fun ({_, V}) -> V end, Params).
