-module(resolve_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  with_ext/1,
  no_ext/1,
  dir/1,
  core/1,
  not_found/1
  ]).

all() -> [with_ext,
          no_ext,
          dir,
          core,
          not_found].

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

with_ext(Config) ->
  C = ?config(context, Config),
  ExpectedJs = list_to_binary(filename:join(?config(module_dir, Config), "module_js/module.js")),
  ExpectedJs = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_js/module.js')">>),

  ExpectedJson = list_to_binary(filename:join(?config(module_dir, Config), "module_js/module.json")),
  ExpectedJson = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_js/module.json')">>),
  
  ok.

no_ext(Config) ->
  C = ?config(context, Config),
  ExpectedJs = list_to_binary(filename:join(?config(module_dir, Config), "module_js/module.js")),
  ExpectedJs = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_js/module')">>),

  ExpectedJson = list_to_binary(filename:join(?config(module_dir, Config), "module_json/module.json")),
  ExpectedJson = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_json/module')">>),

  ExpectedErl = list_to_binary(filename:join(?config(module_dir, Config), "module_erl/module.erl")),
  ExpectedErl = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_erl/module')">>),

  ok.

dir(Config) ->
  C = ?config(context, Config),
  ExpectedPackage = list_to_binary(filename:join(?config(module_dir, Config), "module_with_package/something.js")),
  ExpectedPackage = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_with_package')">>),

  ExpectedJson = list_to_binary(filename:join(?config(module_dir, Config), "module_no_package/index.js")),
  ExpectedJson = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_no_package')">>),

  ok.

core(Config) ->
  C = ?config(context, Config),
  Expected1 = list_to_binary(filename:join(?config(core_dir, Config), "core1.js")),
  Expected1 = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('core1.js')">>),

  Expected2 = list_to_binary(filename:join(?config(core_dir, Config), "core2.json")),
  Expected2 = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('core2.json')">>),

  Expected3 = list_to_binary(filename:join(?config(core_dir, Config), "core_mod/core_mod.js")),
  Expected3 = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('core_mod')">>),

  Expected4 = list_to_binary(filename:join(?config(core_dir, Config), "core_mod_index/index.js")),
  Expected4 = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('core_mod_index')">>),

  ok.

not_found(Config) ->
  C = ?config(context, Config),
  {error,
   {js_error, _, _}
  } = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_js/does_not_exist.js')">>),

  {error,
   {js_error, _, _}
  } = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_js/does_not_exist.json')">>),

  {error,
   {js_error, _, _}
  } = evo8:eval(C, ?config(script_origin, Config), <<"require.resolve('./module_nothing')">>),

  ok.
