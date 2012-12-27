-module(ev8_require).

-export([
  start/0,
  stop/0,
  install/1,
  add_core_path/1,
  get_env/1,
  set_env/2,
  clean_absname/1
  ]).

start() ->
  application:start(erlang_v8),
  application:start(ev8_require).

stop() ->
  application:stop(ev8_require).

install(Context) ->
  Fun = fun() ->
      ev8:set(Context, global, <<"_require">>, fun require/4),
      ev8:set(Context, global, <<"_resolve">>, fun resolve/3),
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

require(This, Vm, File, Module) ->
  io:format("HEYO: ~p:~p:~p:~p~n", [This, Vm, File, Module]),
  require(resolve(This, File, Module), Vm).

require({error, not_found}, _Vm) ->
  {error, not_found};
require(Path, Vm) ->
  io:format("REQUIRING: ~p~n", [Path]),
  Fun = fun() ->
      try_require(Vm, Path)
  end,
  require(ev8cache:try_cache(Vm, {ev8_require, Path}, Fun)).

require(C) ->
  ev8:get(C, global, <<"exports">>).

try_require(Vm, Module) ->
  io:format("try_require ~p~n", [Module]),
  cache_miss(Vm, binary_to_list(Module)).

cache_miss(Vm, ModuleFile) ->
  io:format("cache_miss: ~p~n", [ModuleFile]),
  cache_miss(filename:extension(ModuleFile), Vm, ModuleFile).

cache_miss(".json", Vm, ModuleFile) ->
  cache_miss_json(Vm, ModuleFile);
cache_miss(".erl", Vm, ModuleFile) ->
  cache_miss_erl(Vm, ModuleFile);
cache_miss(_, Vm, ModuleFile) ->
  cache_miss_js(Vm, ModuleFile).

cache_miss_erl(Vm, ModuleFile) ->
  {ok, Module} = compile:file(filename:rootname(ModuleFile)),
  Info = Module:module_info(),
  ModuleExports = proplists:get_value(exports, Info),
  Props = lists:map(
      fun({Fun, _Arity}) ->
          Field = list_to_binary(atom_to_list(Fun)),
          {Field, {mf, {Module, Fun}}}
      end, ModuleExports
      ),

  C = ev8:new_context(Vm),
  ev8:set(C, global, <<"exports">>, {struct, Props}),
  
  C.

cache_miss_js(Vm, ModuleFile) ->
  C = ev8:new_context(Vm),
  ev8:install(C, [ev8_erlang, ev8_require]),
  Exports = ev8:eval(C, <<"new Object">>),
  ev8:set(C, global, [{<<"module">>, global},
                      {<<"exports">>, Exports}]),
  ev8cache:insert(Vm, {ev8_require, ModuleFile}, C),
  ev8:eval_file(C, ModuleFile),

  C.

cache_miss_json(Vm, ModuleFile) ->
  C = ev8:new_context(Vm),
  {ok, Json} = file:read_file(ModuleFile),
  Js = list_to_binary("JSON.parse(JSON.stringify(" ++ binary_to_list(Json) ++ "))"),
  Exports = ev8:eval(C, {ModuleFile, 0}, Js),
  ev8:set(C, global, <<"exports">>, Exports),

  C.

resolve(This, File, Path) when is_binary(File) and is_binary(Path) ->
  resolve(This, binary_to_list(File), binary_to_list(Path));
resolve(_This, File, Path) when is_list(File) and is_list(Path) ->
  resolve(do_resolve(File, Path)).

resolve({error, not_found}) ->
  {error, not_found};
resolve({ok, Result}) when is_list(Result) ->
  resolve({ok, list_to_binary(Result)});
resolve({ok, Result}) when is_binary(Result) ->
  clean_absname(Result).

do_resolve(File, Path) ->
  do_resolve(pathtype(Path), File, Path).

do_resolve(relative, File, Path) ->
  do_resolve(absolute, File, filename:join(filename:dirname(filename:absname(File)), Path));
do_resolve(absolute, _File, Path) ->
  io:format("do_resolve: ~p~n", [Path]),
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
resolve_path(_, _, Path) ->
  resolve_file(Path).

resolve_dir(true, Path) ->
  io:format("DIRRRR~n"),
  resolve_dir(Path);
resolve_dir(false, _Path) ->
  io:format("NOT DIRRRR~n"),
  {error, not_found}.

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
  resolve_js(filelib:is_file(Path ++ ".js"), Path).

resolve_js(true, Path) ->
  {ok, Path ++ ".js"};
resolve_js(false, Path) ->
  resolve_json(filelib:is_file(Path ++ ".json"), Path).

resolve_json(true, Path) ->
  {ok, Path ++ ".json"};
resolve_json(false, Path) ->
  resolve_erl(filelib:is_file(Path ++ ".erl"), Path).

resolve_erl(true, Path) ->
  {ok, Path ++ ".erl"};
resolve_erl(false, Path) ->
  resolve_dir(filelib:is_dir(Path), Path).

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

clean_absname(Path) when is_binary(Path) ->
  list_to_binary(clean_absname(binary_to_list(Path)));
clean_absname(Path) ->
  clean_absname(string:tokens(Path, "/"), []).

clean_absname([], Acc) ->
  "/" ++ string:join(lists:reverse(Acc), "/");
clean_absname([Head | Tail], Acc) ->
  case Head of
    "." -> clean_absname(Tail, Acc);
    ".." ->
      [_ | AccTail] = Acc,
      clean_absname(Tail, AccTail);
    Dir -> clean_absname(Tail, [Dir | Acc])
  end.
