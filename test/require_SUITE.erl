-module(require_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  require_js/1,
  require_json/1,
  require_package_module/1,
  require_index_module/1
  ]).

all() -> [require_js,
         require_json,
         require_package_module,
         require_index_module].

init_per_suite(Config) ->
  ev8_require:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  ev8:install(Context, [ev8_require]),
  Path = filename:join(code:lib_dir(ev8_require), "test/module"),

  [{vm, Vm}, {context, Context},
   {module_dir, Path},
   {script_origin, {filename:join(Path, "test.js"), 0}} | Config].

end_per_suite(Config) ->
  ev8_require:stop(),
  Config.

require_js(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(
      C, ?config(script_origin, Config), <<"require('./module_js/module.js')">>),
  <<"awesome">> = evo8:get(C, Obj, <<"godzilla">>),

  ok.

require_json(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(
      C, ?config(script_origin, Config), <<"require('./module_js/module.json')">>),
  <<"awesome">> = evo8:get(C, Obj, <<"mothra">>),

  ok.

require_package_module(_Config) -> ok.

require_index_module(_Config) -> ok.
