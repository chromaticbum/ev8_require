-module(ev8_require_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ev8_require_sup:start_link().

stop(_State) ->
  ok.
