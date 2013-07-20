-module(hj_utils).

-export([validate/3]).

%%%============================================================================

validate(Params, Schema, WantType) ->
    case jesse:validate_with_schema(Schema, Params) of
        {ok, NewParams} ->
            {ok, convert_to_want_type(WantType, NewParams)};
        {error, Reason} ->
            {error, wrap_error(Reason)}
    end.

%%%%%============================================================================

%% Wrap Jesse error and make them compatible with Hello.
wrap_error({data_invalid, Value, missing_required_property, Property}) ->
    ["'", Value, "' is missing required property '", Property, "'"];

wrap_error({data_invalid,
           {Value, _Extras}, no_extra_properties_allowed, _Schema}) ->
    ["No extra properties allowed in '", Value, "'"];

wrap_error({data_invalid, Value, no_extra_items_allowed, _Schema}) ->
    ["No extra items allowed in '", Value, "'"];

wrap_error({data_invalid, Value, not_enought_items, _Schema}) ->
    ["No enough of items in '", Value, "'"];

wrap_error({data_invalid, Value, missing_dependency, Dependency}) ->
    ["Missing dependency '", Dependency, "' in '", Value, "'"];

wrap_error({data_invalid, Value, not_in_range,
            {{_, A}, {_, B}}}) ->
    ["'", Value, "' not in range ", A, ";", B];

wrap_error({data_invalid, Value, not_correct_size, {min_items, MinItems}}) ->
    ["'", Value, "' is not of correct size (min ", MinItems, ")"];

wrap_error({data_invalid, Value, not_correct_size, {max_items, MaxItems}}) ->
    ["'", Value, "' is not of correct size (max ", MaxItems, ")"];

wrap_error({data_invalid, Value, {Item, not_unique}, _UniqueItems}) ->
    ["'", Item, "' is not unique in '", Value, "'"];

wrap_error({data_invalid, Value, no_match, Paterns}) ->
    ["'", Value, "' does not match ", Paterns];

wrap_error({data_invalid, Value, not_correct_length, {min_length, Len}}) ->
    ["'", Value, "' is not of correct len (min ", Len, " )"];

wrap_error({data_invalid, Value, not_correct_length, {max_length, Len}}) ->
    ["'", Value, "' is not of correct len (max ", Len, " )"];

wrap_error({data_invalid, Value, not_in_enum, Enum}) ->
    ["'", Value, "' is not in enum ", Enum];

wrap_error({data_invalid, Value, not_divisible_by, DivBy}) ->
    [Value, " is not divisible by ", DivBy];

wrap_error({data_invalid, Value, disallowed, _Schema}) ->
    ["'", Value, "' is disallowed"];

wrap_error({data_invalid, Value, Type, _Schema}) ->
    ["'", Value, "' is ", Type];

wrap_error({schema_invalid, _Items_, _Err}) ->
    ["Invalid schema"];

wrap_error(Error) ->
    ["Unknown error: ", Error].

%% Convert params to WantType. The params in WantType will be passed to
%% Hello `handle_request' callbacks.
convert_to_want_type(object, Params) -> Params;
convert_to_want_type(list, {Params}) -> strip_keys(Params);
convert_to_want_type(proplist, {Params}) -> Params.

strip_keys(Params) -> lists:map(fun ({_, V}) -> V end, Params).
