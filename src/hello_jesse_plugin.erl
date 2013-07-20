-module(hello_jesse_plugin).

-export([compile/2]).

-define(DEFAULT_OUTDIR, "ebin").
-define(DEFAULT_SRC,  "src/schema").
-define(COMPILER,     hello_jesse).

%%%============================================================================

compile(Config, _App) ->
    maybe_compile(is_compiler_present(), Config).

%%%============================================================================

maybe_compile(true, Config) ->
    Opts    = rebar_config:get_local(Config, hello_jesse_plugin, []),
    Source  = get_abs_dir(proplists:get_value(src, Opts, ?DEFAULT_SRC)),
    Target  = get_abs_dir(proplists:get_value(outdir, Opts, ?DEFAULT_OUTDIR)),
    ok      = ensure_dir(Target),

    rebar_log:log(debug, "Compiling JSON Schemas from ~s to ~s~n",
                  [Source, Target]),
    ok = ?COMPILER:compile_dir(Source, [{outdir, Target}]);

maybe_compile(false, _Config) ->
    rebar_log:log(warning, "Hello JSON Schema compiler ~s is not present",
                  [?COMPILER]).

%%%============================================================================

get_abs_dir(Dir) ->
    filename:join([rebar_utils:get_cwd(), Dir]).

ensure_dir(Dir) ->
    filelib:ensure_dir(filename:join([Dir, "dummy"])).

is_compiler_present() ->
    code:which(?COMPILER) =/= non_existing.
