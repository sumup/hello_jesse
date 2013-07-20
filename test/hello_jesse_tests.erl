-module(hello_jesse_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("hello_jesse.hrl").

-define(COMPILE(Schema),
        hello_jesse:compile(filename:join(["../test/schema", Schema]), [])).
-define(FUN(Schema, Method), (validation_fun(Schema, Method))).

%%%============================================================================

'_test_'() ->
    [
        {"test1.schema: params ok", fun test_params_ok/0},
        {"test1.schema: invalid param", fun test_params_nok/0},
        {"test2.schema: misformatted schema json",
         fun test_misformatted_json/0},
        {"test3.schema: title missing",
         fun test_title_missing/0}
    ].

%%%============================================================================

test_params_ok() ->
    Schema = "test1.schema",
    ok = ?COMPILE(Schema),

    Json1 = {[{<<"subtrahend">>, 9}, {<<"minuend">>, 0}]},
    ?assertEqual({ok, [9, 0]},
                 ?FUN(Schema, "subtract")(list, Json1)),

    Json2 = {[{<<"addends">>, [1, 2, 3]}]},
    ?assertEqual({ok, [{<<"addends">>, [1, 2, 3]}]},
                 ?FUN(Schema, "add")(proplist, Json2)).

test_params_nok() ->
    Schema = "test1.schema",
    ok = ?COMPILE(Schema),

    Json = {[{<<"addends">>, [1, <<"abc">>, 3]}]},
    ?assertMatch({error, _}, ?FUN(Schema, "add")(object, Json)).


test_misformatted_json() ->
    ?assertEqual({error, invalid_json_schema_file},
                 ?COMPILE("test2.schema")).

test_title_missing() ->
    ?assertMatch({error, {title_not_found, _}},
                 ?COMPILE("test3.schema")).

%%%============================================================================

validation_fun(Schema, Method) ->
    Mod     = hj_compile:get_module_name(Schema),
    Method1 = list_to_atom(Method),

    Mod:Method1().
