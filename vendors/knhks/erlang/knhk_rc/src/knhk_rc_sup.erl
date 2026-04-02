%% knhk_rc_sup.erl — Supervisor tree
-module(knhk_rc_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    Children = [
        #{id => knhk_sigma, start => {knhk_sigma, start_link, []}},
        #{id => knhk_q,     start => {knhk_q, start_link, []}},
        #{id => knhk_ingest, start => {knhk_ingest, start_link, []}},
        #{id => knhk_unrdf, start => {knhk_unrdf, start_link, []}},
        #{id => knhk_shapes, start => {knhk_shapes, start_link, []}},
        #{id => knhk_lockchain, start => {knhk_lockchain, start_link, []}},
        #{id => knhk_bus,  start => {knhk_bus, start_link, []}},
        #{id => knhk_repl, start => {knhk_repl, start_link, []}},
        #{id => knhk_otel, start => {knhk_otel, start_link, []}},
        #{id => knhk_darkmatter, start => {knhk_darkmatter, start_link, []}},
        #{id => knhk_connect, start => {knhk_connect, start_link, []}},
        #{id => knhk_cover, start => {knhk_cover, start_link, []}},
        #{id => knhk_hooks, start => {knhk_hooks, start_link, []}},
        #{id => knhk_epoch, start => {knhk_epoch, start_link, []}},
        #{id => knhk_route, start => {knhk_route, start_link, []}}
    ],
    {ok, {SupFlags, Children}}.

