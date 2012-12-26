-module(ev8_require).

-export([
  start/0,
  stop/0,
  install/1,
  add_core_path/1,
  get_env/1,
  set_env/2
  ]).

start() ->
  application:start(erlang_v8),
  application:start(ev8_require).

stop() ->
  application:stop(ev8_require).

install(Context) ->
  Fun = fun() ->
      ev8:set(Context, global, <<"_require">>, fun require/3),
      ev8:set(Context, global, <<"_resolve">>, fun resolve/2),
      ev8:eval_file(Context, filename:join(code:priv_dir(ev8_require), "ev8_require.js")),

      ok
  end,
  ev8:transaction(Context, Fun).

add_core_path(Path) ->
  set_env(core_path, get_env(core_path) ++ [Path]).

get_env(Name) ->
  {ok, Result} = application:get_env(ev8_require, Name),
  Result.

set_env(Name, Value) ->
  application:set_env(ev8_require, Name, Value).

% Internal functions

require(Vm, File, Module) ->
  Fun = fun() ->
      try_require(Vm, resolve(File, Module))
  end,
  ev8cache:try_cache(Vm, {ev8_require, Module}, Fun).

try_require(_Vm, {error, not_found}) ->
  {error, not_found};
try_require(Vm, Module) ->
  cache_miss(Vm, binary_to_list(Module)).

cache_miss(Vm, ModuleFile) ->
  cache_miss(filename:extension(ModuleFile), Vm, ModuleFile).

cache_miss(".json", Vm, ModuleFile) ->
  cache_miss_json(Vm, ModuleFile);
cache_miss(_, Vm, ModuleFile) ->
  cache_miss_js(Vm, ModuleFile).

cache_miss_js(Vm, ModuleFile) ->
  C = ev8:new_context(Vm),
  ev8:install(C, [ev8_require]),
  Module = ev8:eval(C, <<"new Object">>),
  Exports = ev8:eval(C, <<"new Object">>),
  ev8:set(C, Module, <<"exports">>, Exports),
  ev8:set(C, global, [{<<"module">>, Module},
                      {<<"exports">>, Exports}]),
  ev8:eval_file(C, ModuleFile),

  Exports.

cache_miss_json(Vm, ModuleFile) ->
  C = ev8:new_context(Vm),
  {ok, Json} = file:read_file(ModuleFile),
  Js = list_to_binary("JSON.parse(JSON.stringify(" ++ binary_to_list(Json) ++ "))"),
  ev8:eval(C, {ModuleFile, 0}, Js).

resolve(File, Path) when is_binary(File) and is_binary(Path) ->
  resolve(binary_to_list(File), binary_to_list(Path));
resolve(File, Path) when is_list(File) and is_list(Path) ->
  resolve(do_resolve(File, Path)).

resolve({error, not_found}) ->
  {error, not_found};
resolve({ok, Result}) when is_list(Result) ->
  list_to_binary(Result);
resolve({ok, Result}) when is_binary(Result) ->
  Result.

do_resolve(File, Path) ->
  do_resolve(pathtype(Path), File, Path).

do_resolve(relative, File, Path) ->
  do_resolve(absolute, File, filename:join(filename:dirname(filename:absname(File)), Path));
do_resolve(absolute, _File, Path) ->
  resolve_absolute(Path);
do_resolve(library, _File, Path) ->
  resolve_library(Path).

resolve_library(Path) ->
  resolve_library(Path, get_env(core_path)).

resolve_library(_Path, []) ->
  {error, not_found};
resolve_library(Path, [Head | Tail]) ->
  resolve_library(resolve_absolute(filename:join(Head, Path)), Path, Tail).

resolve_library({ok, Path}, _Path, _Paths) ->
  {ok, Path};
resolve_library({error, not_found}, Path, Paths) ->
  resolve_library(Path, Paths).

resolve_absolute(Path) ->
  Path2 = filename:absname(Path),
  resolve_path(filelib:is_file(Path2), filelib:is_dir(Path2), Path2).

resolve_path(true, false, Path) ->
  {ok, Path};
resolve_path(true, true, Path) ->
  resolve_dir(Path);
resolve_path(false, false, Path) ->
  resolve_file(Path).

resolve_dir(Path) ->
  resolve_package(filelib:is_file(filename:join(Path, "package.json")), Path).

resolve_package(true, Path) ->
  {ok, Content} = file:read_file(filename:join(Path, "package.json")),
  {struct, Json} = mochijson2:decode(Content),

  case proplists:get_value(<<"main">>, Json) of
    undefined -> {error, invalid_package};
    File ->
      PackageFile = filename:join(Path, File),
      resolve_package_file(filelib:is_file(PackageFile), PackageFile)
  end;
resolve_package(false, Path) ->
  resolve_index(filelib:is_file(filename:join(Path, "index.js")), Path).

resolve_package_file(true, Path) ->
  {ok, Path};
resolve_package_file(false, _Path) ->
  {error, not_found}.

resolve_index(true, Path) ->
  {ok, filename:join(Path, "index.js")};
resolve_index(false, _Path) ->
  {error, not_found}.

resolve_file(Path) ->
  resolve_file(filename:extension(Path), Path).

resolve_file([], Path) ->
  resolve_js(filelib:is_file(Path ++ ".js"), Path);
resolve_file(_, _Path) ->
  {error, not_found}.

resolve_js(true, Path) ->
  {ok, Path ++ ".js"};
resolve_js(false, Path) ->
  resolve_json(filelib:is_file(Path ++ ".json"), Path).

resolve_json(true, Path) ->
  {ok, Path ++ ".json"};
resolve_json(false, _Path) ->
  {error, not_found}.

pathtype(Path) when is_binary(Path) ->
  pathtype(binary_to_list(Path));
pathtype(Path) ->
  [Head | _Parts] = filename:split(Path),
  case Head of
    "." -> relative;
    ".." -> relative;
    "/" -> absolute;
    _ -> library
  end.
