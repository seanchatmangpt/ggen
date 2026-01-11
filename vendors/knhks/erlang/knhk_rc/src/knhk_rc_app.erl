%% knhk_rc_app.erl — Application callback
-module(knhk_rc_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    knhk_rc_sup:start_link().

stop(_State) ->
    ok.

