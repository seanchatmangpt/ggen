%%%-------------------------------------------------------------------
%% @doc tai_metrics: Prometheus metrics
%%
%% Counters:
%%   - signals_received
%%   - refusals_total
%%   - postpones_total
%%   - actions_attempted_total
%%   - actions_failed_total
%%
%% Histograms:
%%   - decision_latency_ms
%%   - action_latency_ms
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_metrics).

%% API
-export([init/0, signal_received/0, refusal/0, postpone/0, action_attempted/0, action_failed/0]).
-export([record_decision_latency/1, record_action_latency/1]).

%%%===================================================================
%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    prometheus_counter:declare([
        {name, signals_received},
        {help, "Total number of signals received"},
        {labels, [tenant_id]}
    ]),
    prometheus_counter:declare([
        {name, refusals_total},
        {help, "Total number of refusals"},
        {labels, [tenant_id, reason]}
    ]),
    prometheus_counter:declare([
        {name, postpones_total},
        {help, "Total number of postponed events"},
        {labels, [tenant_id]}
    ]),
    prometheus_counter:declare([
        {name, actions_attempted_total},
        {help, "Total number of actions attempted"},
        {labels, [tenant_id, action_type]}
    ]),
    prometheus_counter:declare([
        {name, actions_failed_total},
        {help, "Total number of actions failed"},
        {labels, [tenant_id, action_type]}
    ]),
    prometheus_histogram:declare([
        {name, decision_latency_ms},
        {help, "Decision latency in milliseconds"},
        {labels, [tenant_id]},
        {buckets, [10, 50, 100, 500, 1000, 5000]}
    ]),
    prometheus_histogram:declare([
        {name, action_latency_ms},
        {help, "Action latency in milliseconds"},
        {labels, [tenant_id, action_type]},
        {buckets, [100, 500, 1000, 5000, 10000, 30000]}
    ]),
    ok.

-spec signal_received() -> ok.
signal_received() ->
    prometheus_counter:inc(signals_received, [<<>>]).

-spec refusal() -> ok.
refusal() ->
    prometheus_counter:inc(refusals_total, [<<>>, <<"unknown">>]).

-spec postpone() -> ok.
postpone() ->
    prometheus_counter:inc(postpones_total, [<<>>]).

-spec action_attempted() -> ok.
action_attempted() ->
    prometheus_counter:inc(actions_attempted_total, [<<>>, <<"unknown">>]).

-spec action_failed() -> ok.
action_failed() ->
    prometheus_counter:inc(actions_failed_total, [<<>>, <<"unknown">>]).

-spec record_decision_latency(Milliseconds) -> ok
  when Milliseconds :: non_neg_integer().
record_decision_latency(Milliseconds) ->
    prometheus_histogram:observe(decision_latency_ms, [<<>>], Milliseconds),
    ok.

-spec record_action_latency(Milliseconds) -> ok
  when Milliseconds :: non_neg_integer().
record_action_latency(Milliseconds) ->
    prometheus_histogram:observe(action_latency_ms, [<<>>, <<"unknown">>], Milliseconds),
    ok.
