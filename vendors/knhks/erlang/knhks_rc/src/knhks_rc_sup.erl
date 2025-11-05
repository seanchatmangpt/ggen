%% knhks_rc_sup.erl — Supervisor tree
-module(knhks_rc_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    Children = [
        #{id => knhks_sigma, start => {knhks_sigma, start_link, []}},
        #{id => knhks_q,     start => {knhks_q, start_link, []}},
        #{id => knhks_ingest, start => {knhks_ingest, start_link, []}},
        #{id => knhks_unrdf, start => {knhks_unrdf, start_link, []}},
        #{id => knhks_shapes, start => {knhks_shapes, start_link, []}},
        #{id => knhks_lockchain, start => {knhks_lockchain, start_link, []}},
        #{id => knhks_bus,  start => {knhks_bus, start_link, []}},
        #{id => knhks_repl, start => {knhks_repl, start_link, []}},
        #{id => knhks_otel, start => {knhks_otel, start_link, []}},
        #{id => knhks_darkmatter, start => {knhks_darkmatter, start_link, []}},
        #{id => knhks_connect, start => {knhks_connect, start_link, []}},
        #{id => knhks_cover, start => {knhks_cover, start_link, []}},
        #{id => knhks_hooks, start => {knhks_hooks, start_link, []}},
        #{id => knhks_epoch, start => {knhks_epoch, start_link, []}},
        #{id => knhks_route, start => {knhks_route, start_link, []}}
    ],
    {ok, {SupFlags, Children}}.

