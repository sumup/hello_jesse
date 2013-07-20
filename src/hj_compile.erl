-module(hj_compile).

-export([compile/2, compile_dir/2]).

-include_lib("kernel/include/file.hrl").
-include("hello_jesse.hrl").

-define(log, io:format).

-type opts() :: [opt()].
-type opt()  :: {outdir, file:filename()}.

-export_type([opts/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%%============================================================================
%%% API
%%%============================================================================

-spec compile(file:filename(), opts()) -> ok | {error, term()}.
compile(File, Options) ->
    maybe_compile(File, Options).

-spec compile_dir(file:dirname(), opts()) -> ok | {error, term()}.
compile_dir(Dir, Options) ->
    compile_dir_1(find_schema_files(Dir), Options).

compile_dir_1([], _) -> ok;
compile_dir_1([File | T], Options) ->
    case maybe_compile(File, Options) of
        ok ->               compile_dir_1(T, Options);
        {error, _} = Err -> Err
    end.

%%%============================================================================

%% Find all schema files that maybe need to be compiled. Schema file identified
%% by extension (?EXTENSION).
find_schema_files(Dir) ->
    [filename:join(Dir, F) || F <- filelib:wildcard("*" ++ ?EXTENSION, Dir)].

get_output_file(File, Options) ->
    OutDir = proplists:get_value(outdir, Options, "."),
    ModName = get_module_name(File),
    OutputFile = filename:join(OutDir, atom_to_list(ModName) ++ ".beam"),
    {OutputFile, ModName}.

get_module_name(File) ->
    get_module_name_1(filename:basename(filename:rootname(File))).

get_module_name_1(File) when is_binary(File) ->
    get_module_name_1(binary_to_list(File));
get_module_name_1(File) when is_list(File) ->
    list_to_atom(?SCHEMA_MOD_PREFIX ++ File ++ ?SCHEMA_MOD_POSTFIX).

%%%============================================================================

maybe_compile(File, Options) ->
    {OutputFile, ModName} = get_output_file(File, Options),

    case needs_compile(File, OutputFile) of
        true ->     compile(File, OutputFile, ModName);
        false ->    ?log("Up to date ~s~n", [OutputFile])
    end.

compile(File, OutputFile, ModName) ->
    ?log("Compiling ~s~n", [File]),
    {ok, Schemas} = file:read_file(File),
    maybe_write_module(hj_syntax:generate_forms(Schemas, ModName), OutputFile).

maybe_write_module({ok, Forms}, OutputFile) ->
    case compile:forms(Forms, ?MOD_OPTIONS) of
        {ok, _Mod, Code, _Warnings} ->
            ?log("Writing ~s~n", [OutputFile]),

            case file:write_file(OutputFile, Code) of
                ok -> ok;
                {error, Error} ->
                    ?log("..Failed to write: ~p~n", [Error]),

                    {error, {write_file, OutputFile, Error}}
                end;
        {error, Errors, Warnings} ->
            ?log("..Failed to compile~n"
                 "....Errors:   ~p~n"
                 "....Warnings: ~p~n",
                 [Errors, Warnings]),

            {error, {compile_failed, Errors, Warnings}}
    end;
maybe_write_module({error, Reason}, _OutputFile) ->
    ?log("..Failed to generate: ~p~n", [Reason]),

    {error, Reason}.

%%%============================================================================

needs_compile(InputFile, OutputFile) ->
    mtime(InputFile) >= mtime(OutputFile).

mtime(File) ->
    case file:read_file_info(File) of
        {ok, Info} -> Info#file_info.mtime;
        {error, _} -> -1
    end.
