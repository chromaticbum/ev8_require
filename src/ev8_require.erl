-module(ev8_require).

-export([
  start/0,
  stop/0,
  install/1,
  add_path/1,
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

add_path(Path) ->
  set_env(path, get_env(path) ++ Path).

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
  io:format("HEYO: ~p~n", [filename:extension(ModuleFile)]),
  cache_miss(filename:extension(ModuleFile), Vm, ModuleFile).

cache_miss(".json", Vm, ModuleFile) ->
  cache_miss_json(filename:basename(ModuleFile), Vm, ModuleFile);
cache_miss(".js", Vm, ModuleFile) ->
  C = ev8:new_context(Vm),
  Module = ev8:eval(C, <<"new Object">>),
  Exports = ev8:eval(C, <<"new Object">>),
  ev8:set(C, Module, <<"exports">>, Exports),
  ev8:set(C, global, [{<<"module">>, Module},
                      {<<"exports">>, Exports}]),
  ev8:eval_file(C, ModuleFile),

  Exports.

cache_miss_json("package.json", Vm, ModuleFile) ->
  {ok, Contents} = file:read_file(ModuleFile),
  {struct, Json} = mochijson2:decode(Contents),
  case proplists:get_value(<<"main">>, Json) of
    undefined -> {error, invalid_package_json};
    File -> require(Vm, list_to_binary(ModuleFile), iolist_to_binary(filename:join(filename:dirname(ModuleFile), File)))
  end;
cache_miss_json(_, Vm, ModuleFile) ->
  C = ev8:new_context(Vm),
  {ok, Json} = file:read_file(ModuleFile),
  Js = list_to_binary("JSON.parse(JSON.stringify(" ++ binary_to_list(Json) ++ "))"),
  ev8:eval(C, {ModuleFile, 0}, Js).

resolve(File, Path) when is_binary(File) and is_binary(Path) ->
  io:format("Resolve from(~p): ~p~n", [File, Path]),
  resolve(resolve(pathtype(binary_to_list(Path)), binary_to_list(File), binary_to_list(Path))).

resolve({ok, Path}) -> list_to_binary(Path);
resolve({error, not_found}) -> {error, not_found}.

resolve(relative, File, Path) ->
  resolve(absolute, File, filename:join(filename:dirname(filename:absname(File)), Path));
resolve(absolute, _File, Path) ->
  resolve_path(Path);
resolve(library, _File, Path) ->
  resolve_library(Path, get_env(library_path)).

resolve_library(_Path, []) ->
  {error, not_found};
resolve_library(Path, [Head | Tail]) ->
  resolve_library(Path, Tail, resolve_path(filename:join(filename:absname(Head), Path))).

resolve_library(_Path, _Paths, {ok, FullPath}) ->
  {ok, FullPath};
resolve_library(Path, Paths, {error, not_found}) ->
  resolve_library(Path, Paths).

resolve_path(Path) ->
  resolve_path(filename:extension(Path), Path).

resolve_path([], Path) ->
  resolve_no_ext(js, Path);
resolve_path(".js", Path) ->
  resolve_ext(Path);
resolve_path(".json", Path) ->
  resolve_ext(Path);
resolve_path(_, _) ->
  {error, not_found}.

resolve_ext(Path) ->
  resolve_ext(filelib:is_file(Path), Path).

resolve_ext(true, Path) ->
  {ok, Path};
resolve_ext(false, _Path) ->
  {error, not_found}.

resolve_no_ext(js, Path) ->
  resolve_no_ext(js, filelib:is_file(Path ++ ".js"), Path).

resolve_no_ext(js, true, Path) ->
  {ok, Path ++ ".js"};
resolve_no_ext(js, false, Path) ->
  resolve_no_ext(json, filelib:is_file(Path ++ ".json"), Path);
resolve_no_ext(json, true, Path) ->
  {ok, Path ++ ".json"};
resolve_no_ext(json, false, Path) ->
  resolve_no_ext(dir, filelib:is_dir(Path), Path);
resolve_no_ext(dir, true, Path) ->
  resolve_dir(Path);
resolve_no_ext(dir, false, _Path) ->
  {error, not_found}.

resolve_dir(Path) ->
  resolve_dir(package, filelib:is_file(filename:join(Path, "package.json")), Path).

resolve_dir(package, true, Path) ->
  {ok, filename:join(Path, "package.json")};
resolve_dir(package, false, Path) ->
  resolve_dir(index, filelib:is_file(filename:join(Path, "index.js")), Path);
resolve_dir(index, true, Path) ->
  {ok, filename:join(Path, "index.js")};
resolve_dir(index, false, _Path) ->
  {error, not_found}.

pathtype(Path) ->
  [Head | _Parts] = filename:split(Path),
  case Head of
    "." -> relative;
    ".." -> relative;
    "/" -> absolute;
    _ -> library
  end.
