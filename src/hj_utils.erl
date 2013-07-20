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

-define(F(X), to_list(X)).
to_list(X) when is_integer(X)   -> integer_to_list(X);
to_list(X) when is_atom(X)      -> atom_to_list(X);
to_list(X) when is_float(X)     -> float_to_list(X);
to_list(X) when is_binary(X)    -> binary_to_list(X);
to_list(X) when is_list(X)      -> X;
to_list(X)                      -> io_lib:format("~p", [X]).

%% Wrap Jesse error and make them compatible with Hello.
wrap_error(Err) ->
    list_to_binary(wrap_error_1(Err)).

wrap_error_1({data_invalid, Value, missing_required_property, Property}) ->
    [?F(Value), " is missing required property ", ?F(Property)];

wrap_error_1({data_invalid,
           {Value, _Extras}, no_extra_properties_allowed, _Schema}) ->
    ["No extra properties allowed in ", ?F(Value)];

wrap_error_1({data_invalid, Value, no_extra_items_allowed, _Schema}) ->
    ["No extra items allowed in ", ?F(Value)];

wrap_error_1({data_invalid, Value, not_enought_items, _Schema}) ->
    ["No enough of items in ", ?F(Value)];

wrap_error_1({data_invalid, Value, missing_dependency, Dependency}) ->
    ["Missing dependency ", ?F(Dependency), " in ", ?F(Value)];

wrap_error_1({data_invalid, Value, not_in_range,
            {{_, A}, {_, B}}}) ->
    [?F(Value), " not in range ", ?F(A), "-", ?F(B)];

wrap_error_1({data_invalid, Value, not_correct_size, {min_items, MinItems}}) ->
    [?F(Value), " is not of correct size (min ", ?F(MinItems), ")"];

wrap_error_1({data_invalid, Value, not_correct_size, {max_items, MaxItems}}) ->
    [?F(Value), " is not of correct size (max ", ?F(MaxItems), ")"];

wrap_error_1({data_invalid, Value, {Item, not_unique}, _UniqueItems}) ->
    [?F(Item), " is not unique in ", ?F(Value)];

wrap_error_1({data_invalid, Value, no_match, Patern}) ->
    [?F(Value), " does not match ", ?F(Patern)];

wrap_error_1({data_invalid, Value, not_correct_length, {min_length, Len}}) ->
    [?F(Value), " is not of correct len (min ", ?F(Len), " )"];

wrap_error_1({data_invalid, Value, not_correct_length, {max_length, Len}}) ->
    [?F(Value), " is not of correct len (max ", ?F(Len), " )"];

wrap_error_1({data_invalid, Value, not_in_enum, Enum}) ->
    [?F(Value), " is not in enum ", ?F(Enum)];

wrap_error_1({data_invalid, Value, not_divisible_by, DivBy}) ->
    [?F(Value), " is not divisible by ", ?F(DivBy)];

wrap_error_1({data_invalid, Value, disallowed, _Schema}) ->
    [?F(Value), " is disallowed"];

wrap_error_1({data_invalid, Value, Type, _Schema}) ->
    [?F(Value), " is ", ?F(Type)];

wrap_error_1({schema_invalid, _Items_, _Err}) ->
    "Invalid schema";

wrap_error_1(Error) ->
    ["Unknown error: ", ?F(Error)].

%% Convert params to WantType. The params in WantType will be passed to
%% Hello `handle_request' callbacks.
convert_to_want_type(object, Params) -> Params;
convert_to_want_type(list, {Params}) -> strip_keys(Params);
convert_to_want_type(proplist, {Params}) -> Params.

strip_keys(Params) -> lists:map(fun ({_, V}) -> V end, Params).
