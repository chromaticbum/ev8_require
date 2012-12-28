-module(ev8_module).

-export([
  mod_fun/2
  ]).

mod_fun(_, []) ->
  <<"mod_fun_result">>.
