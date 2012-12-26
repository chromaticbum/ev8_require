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
  require_index_module/1,
  require_core/1
  ]).

all() -> [require_js,
         require_json,
         require_package_module,
         require_index_module,
         require_core].

init_per_suite(Config) ->
  ev8_require:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  ev8:install(Context, [ev8_require]),
  Path = filename:join(code:lib_dir(ev8_require), "test/module"),
  CorePath = filename:join(Path, "core"),
  ev8_require:add_core_path(CorePath),

  [{vm, Vm}, {context, Context},
   {module_dir, Path},
   {script_origin, {filename:join(Path, "test.js"), 0}},
   {core_dir, CorePath} | Config].

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
      C, ?config(script_origin, Config), <<"require('./module_json/module.json')">>),
  <<"awesome">> = evo8:get(C, Obj, <<"mothra">>),

  ok.

require_package_module(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(
      C, ?config(script_origin, Config), <<"require('./module_with_package')">>),
  <<"bad">> = evo8:get(C, Obj, <<"monsterX">>),

  ok.

require_index_module(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(
      C, ?config(script_origin, Config), <<"require('./module_no_package')">>),
  <<"baddest">> = evo8:get(C, Obj, <<"kingGhidorah">>),

  ok.

require_core(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(
      C, ?config(script_origin, Config), <<"require('core1.js')">>),
  <<"core1.js">> = evo8:get(C, Obj, <<"coreMod">>),

  Obj2 = ev8:eval(
      C, ?config(script_origin, Config), <<"require('core2.json')">>),
  <<"core2.json">> = evo8:get(C, Obj2, <<"coreMod">>),

  Obj3 = ev8:eval(
      C, ?config(script_origin, Config), <<"require('core_mod')">>),
  <<"right here">> = evo8:get(C, Obj3, <<"coreMod">>),

  Obj4 = ev8:eval(
      C, ?config(script_origin, Config), <<"require('core_mod_index')">>),
  <<"index.js">> = evo8:get(C, Obj4, <<"coreMod">>),

  ok.
