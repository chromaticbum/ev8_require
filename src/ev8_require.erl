-module(ev8_require).

-export([
  start/0,
  install/1,
  add_path/1,
  get_env/1,
  set_env/2
  ]).

start() ->
  application:start(erlang_v8),
  application:start(ev8_require).

install(Context) ->
  Fun = fun() ->
      ev8:set(Context, global, <<"_require">>, fun require/2),
      Require = ev8:get(Context, global, <<"require">>),
      ev8:set(Context, Require, <<"resolve">>, fun resolve/1),
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

require(Vm, {ok, Path}) ->
  require_file(Vm, Path);
require(_Vm, {error, not_found}) ->
  {error, not_found};
require(Vm, Path) ->
  require(Vm, resolve(Path)).

require_file(Vm, Path) ->
  Context = ev8:new_context(Vm),
  Result = ev8cache:eval_file(Context, Path),
  io:format("Result: ~p~n", [evo8:get(Context, Result, <<"hello">>)]),
  Result.

resolve(Path) ->
  io:format("Resolve: ~p~n", [Path]),

  case {filelib:is_dir(Path), filelib:is_file(Path)} of
    {true, false} -> resolve_dir(Path);
    {false, true} -> {ok, Path};
    {false, false} -> resolve_not_found(Path)
  end.

resolve_not_found(Path) ->
  File = Path ++ ".js",
  case filelib:is_file(File) of
    true -> {ok, File};
    false -> {error, not_found}
  end.

resolve_dir(Path) ->
  File = filename:join(Path, "index.js"),
  case filelib:is_file(File) of
    true -> {ok, File};
    false -> {error, not_found}
  end.

