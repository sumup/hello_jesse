%% @doc Hello JSON Schema validators compiler.
-module(hello_jesse).
-author('Martynas Pumputis <martynasp@gmail.com>').

-export([compile/2, compile_dir/2]).

%%%============================================================================

-spec compile(file:filename(), hj_compile:opts()) -> ok | {error, term()}.
compile(File, Options) ->
    hj_compile:compile(File, Options).

-spec compile_dir(file:dirname(), hj_compile:opts()) -> ok | {error, term()}.
compile_dir(Dir, Options) ->
    hj_compile:compile_dir(Dir, Options).
