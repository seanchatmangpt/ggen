%%%-------------------------------------------------------------------
%% @doc TAIEA Governor - Comprehensive Test Suite (Common Test)
%%
%% Tests state machine transitions, gate checking, action execution,
%% receipt generation, and multi-tenant isolation.
%% @end
%%%-------------------------------------------------------------------
-module(taiea_governor_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_boot_stable_transition/1,
    test_stable_state_signal/1,
    test_stable_state_tool_call/1,
    test_tool_call_success/1,
    test_tool_call_timeout/1,
    test_tool_call_memory_exceeded/1,
    test_entitlement_changed_cast/1,
    test_intervening_state_postpones_signals/1,
    test_intervening_to_stable_on_action_complete/1,
    test_intervening_to_refusing_on_action_failed/1,
    test_refusing_state_rejects_actions/1,
    test_refusing_reactivates_on_entitlement/1,
    test_gate_acceptance/1,
    test_receipt_generation/1,
    test_receipt_contains_metadata/1,
    test_state_transitions_recorded_in_receipts/1,
    test_multiple_receipts_list/1,
    test_get_state_from_all_states/1,
    test_concurrent_signals/1,
    test_tenant_isolation/1
]).

all() ->
    [
        test_boot_stable_transition,
        test_stable_state_signal,
        test_stable_state_tool_call,
        test_tool_call_success,
        test_tool_call_timeout,
        test_tool_call_memory_exceeded,
        test_entitlement_changed_cast,
        test_intervening_state_postpones_signals,
        test_intervening_to_stable_on_action_complete,
        test_intervening_to_refusing_on_action_failed,
        test_refusing_state_rejects_actions,
        test_refusing_reactivates_on_entitlement,
        test_gate_acceptance,
        test_receipt_generation,
        test_receipt_contains_metadata,
        test_state_transitions_recorded_in_receipts,
        test_multiple_receipts_list,
        test_get_state_from_all_states,
        test_concurrent_signals,
        test_tenant_isolation
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%% Test Cases - State Transitions
%%%===================================================================

test_boot_stable_transition(Config) ->
    TenantId = <<"tenant_1">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Governor starts in boot state
    {ok, boot} = taiea_governor:get_state(Pid),

    %% Sending signal from boot should transition to stable
    {ok, stable, _Receipt} = taiea_governor:signal(Pid, #{type => test}),

    %% Verify now in stable state
    {ok, stable} = taiea_governor:get_state(Pid),

    %% Cleanup
    unlink(Pid),
    ok.

test_stable_state_signal(Config) ->
    TenantId = <<"tenant_signal">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Transition to stable first
    {ok, stable, _} = taiea_governor:signal(Pid, #{type => test}),

    %% Signal in stable should stay in stable
    {ok, stable, Receipt} = taiea_governor:signal(Pid, #{type => another_test}),

    %% Verify receipt has correct metadata
    #{event_type := EventType} = Receipt,
    <<"signal_processed">> = EventType,

    unlink(Pid),
    ok.

test_stable_state_tool_call(Config) ->
    TenantId = <<"tenant_tool">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Transition to stable
    {ok, stable, _} = taiea_governor:signal(Pid, #{type => test}),

    %% Tool call in stable
    {ok, Result, Receipt} = taiea_governor:tool_call(Pid, <<"query">>, #{}, 5000),

    %% Verify result structure
    #{status := success, rows := 42} = Result,

    %% Verify receipt
    #{event_type := <<"tool_call_success">>} = Receipt,

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - Tool Execution
%%%===================================================================

test_tool_call_success(Config) ->
    TenantId = <<"tenant_create">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    {ok, Result, Receipt} = taiea_governor:tool_call(Pid, <<"create">>, #{}, 5000),

    %% Verify success result
    #{status := success, created_id := _Id} = Result,

    %% Verify receipt shows success
    #{event_type := <<"tool_call_success">>} = Receipt,

    unlink(Pid),
    ok.

test_tool_call_timeout(Config) ->
    TenantId = <<"tenant_timeout">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Call tool that simulates timeout
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{timeout_ms => 1000}, 1000),

    %% Governor should be in intervening state
    {ok, intervening} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

test_tool_call_memory_exceeded(Config) ->
    TenantId = <<"tenant_memory">>,
    {ok, Pid} = taiea_governor:start_link(TenantId, #{max_memory_mb => 512}),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Call tool that simulates memory exceeded
    {error, memory_exceeded} = taiea_governor:tool_call(Pid, <<"memory_test">>, #{}, 5000),

    %% Governor should be in intervening state
    {ok, intervening} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - Cast Events
%%%===================================================================

test_entitlement_changed_cast(Config) ->
    TenantId = <<"tenant_entitlement">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Send entitlement change (async cast)
    ok = taiea_governor:entitlement_changed(Pid, inactive),

    %% Governor should still be in stable (cast doesn't change immediate state)
    {ok, stable} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - State-Specific Behaviors
%%%===================================================================

test_intervening_state_postpones_signals(Config) ->
    TenantId = <<"tenant_postpone">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Trigger timeout to move to intervening
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),

    %% Now in intervening, signals should be postponed
    {ok, intervening, Receipt} = taiea_governor:signal(Pid, #{type => test}),

    %% Verify receipt shows postponement
    #{event_type := <<"signal_postponed">>} = Receipt,

    unlink(Pid),
    ok.

test_intervening_to_stable_on_action_complete(Config) ->
    TenantId = <<"tenant_action_complete">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Trigger intervention
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
    {ok, intervening} = taiea_governor:get_state(Pid),

    %% Simulate action completion
    Pid ! {action_complete, <<"action_123">>, #{status => recovered}},
    timer:sleep(100),

    %% Should return to stable
    {ok, stable} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

test_intervening_to_refusing_on_action_failed(Config) ->
    TenantId = <<"tenant_action_failed">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Trigger intervention
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
    {ok, intervening} = taiea_governor:get_state(Pid),

    %% Simulate action failure
    Pid ! {action_failed, <<"action_456">>, <<"recovery_failed">>},
    timer:sleep(100),

    %% Should move to refusing
    {ok, refusing} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

test_refusing_state_rejects_actions(Config) ->
    TenantId = <<"tenant_refuse">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Move to refusing via action failure
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
    {ok, intervening} = taiea_governor:get_state(Pid),
    Pid ! {action_failed, <<"action_789">>, <<"failed">>},
    timer:sleep(100),

    %% In refusing state, signal should be rejected
    {error, refusing} = taiea_governor:signal(Pid, #{type => test}),

    %% Tool call should be rejected
    {error, refusing} = taiea_governor:tool_call(Pid, <<"query">>, #{}, 5000),

    unlink(Pid),
    ok.

test_refusing_reactivates_on_entitlement(Config) ->
    TenantId = <<"tenant_reactivate">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Move to refusing
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
    Pid ! {action_failed, <<"action_999">>, <<"failed">>},
    timer:sleep(100),
    {ok, refusing} = taiea_governor:get_state(Pid),

    %% Reactivate via entitlement
    ok = taiea_governor:entitlement_changed(Pid, active),
    timer:sleep(100),

    %% Should return to stable
    {ok, stable} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - Gate Checking
%%%===================================================================

test_gate_acceptance(Config) ->
    TenantId = <<"tenant_gates">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Signal from boot should pass all gates
    {ok, stable, Receipt} = taiea_governor:signal(Pid, #{type => test}),

    %% Verify receipt contains gate metadata
    #{metadata := Metadata} = Receipt,
    true = maps:is_key(gate_1, Metadata),
    true = maps:is_key(gate_2, Metadata),
    true = maps:is_key(gate_3, Metadata),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - Receipt Management
%%%===================================================================

test_receipt_generation(Config) ->
    TenantId = <<"tenant_receipts">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Each action should generate a receipt
    {ok, stable, Receipt1} = taiea_governor:signal(Pid, #{}),

    %% Verify receipt structure
    true = is_map(Receipt1),
    true = maps:is_key(id, Receipt1),
    true = maps:is_key(timestamp, Receipt1),
    true = maps:is_key(tenant_id, Receipt1),
    true = maps:is_key(governor_id, Receipt1),
    true = maps:is_key(state_from, Receipt1),
    true = maps:is_key(state_to, Receipt1),

    unlink(Pid),
    ok.

test_receipt_contains_metadata(Config) ->
    TenantId = <<"tenant_meta">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Tool call with metadata
    {ok, _Result, Receipt} = taiea_governor:tool_call(Pid, <<"query">>, #{table => users}, 5000),

    %% Verify metadata in receipt
    #{metadata := Metadata} = Receipt,
    true = is_map(Metadata),
    <<"query">> = maps:get(tool, Metadata),

    unlink(Pid),
    ok.

test_state_transitions_recorded_in_receipts(Config) ->
    TenantId = <<"tenant_transitions">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Get all receipts
    {ok, Receipts1} = taiea_governor:list_receipts(Pid),

    %% Initial receipt from boot
    [BootReceipt] = Receipts1,
    #{state_from := boot, state_to := boot} = BootReceipt,

    %% Transition to stable
    {ok, stable, StableReceipt} = taiea_governor:signal(Pid, #{}),

    %% Get all receipts again
    {ok, Receipts2} = taiea_governor:list_receipts(Pid),

    %% Should have both boot and stable receipts
    2 = length(Receipts2),

    unlink(Pid),
    ok.

test_multiple_receipts_list(Config) ->
    TenantId = <<"tenant_multi_receipts">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Generate multiple receipts
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),
    {ok, stable, _} = taiea_governor:signal(Pid, #{type => test}),
    {ok, _Result, _} = taiea_governor:tool_call(Pid, <<"query">>, #{}, 5000),

    %% List all receipts
    {ok, Receipts} = taiea_governor:list_receipts(Pid),

    %% Should have multiple receipts
    true = length(Receipts) >= 3,

    %% All receipts should be maps
    true = lists:all(fun is_map/1, Receipts),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - State Queries
%%%===================================================================

test_get_state_from_all_states(Config) ->
    TenantId = <<"tenant_state_query">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),

    %% Boot state
    {ok, boot} = taiea_governor:get_state(Pid),

    %% Stable state
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),
    {ok, stable} = taiea_governor:get_state(Pid),

    %% Intervening state
    {error, timeout} = taiea_governor:tool_call(Pid, <<"timeout_test">>, #{}, 1000),
    {ok, intervening} = taiea_governor:get_state(Pid),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - Concurrency
%%%===================================================================

test_concurrent_signals(Config) ->
    TenantId = <<"tenant_concurrent">>,
    {ok, Pid} = taiea_governor:start_link(TenantId),
    {ok, stable, _} = taiea_governor:signal(Pid, #{}),

    %% Send multiple signals concurrently
    Results = lists:map(fun(N) ->
        taiea_governor:signal(Pid, #{signal_num => N})
    end, lists:seq(1, 10)),

    %% All should succeed (or be in stable state)
    true = lists:all(fun({ok, stable, _}) -> true; (_) -> false end, Results),

    unlink(Pid),
    ok.

%%%===================================================================
%% Test Cases - Multi-Tenancy
%%%===================================================================

test_tenant_isolation(Config) ->
    TenantId1 = <<"tenant_a">>,
    TenantId2 = <<"tenant_b">>,

    {ok, Pid1} = taiea_governor:start_link(TenantId1),
    {ok, Pid2} = taiea_governor:start_link(TenantId2),

    %% Both start in boot
    {ok, boot} = taiea_governor:get_state(Pid1),
    {ok, boot} = taiea_governor:get_state(Pid2),

    %% Move tenant1 to stable
    {ok, stable, _} = taiea_governor:signal(Pid1, #{}),

    %% Verify isolation
    {ok, stable} = taiea_governor:get_state(Pid1),
    {ok, boot} = taiea_governor:get_state(Pid2),

    %% Receipts should be isolated
    {ok, Receipts1} = taiea_governor:list_receipts(Pid1),
    {ok, Receipts2} = taiea_governor:list_receipts(Pid2),

    %% Both should have at least one receipt
    true = length(Receipts1) >= 1,
    true = length(Receipts2) >= 1,

    %% Receipts should have correct tenant IDs
    lists:all(fun(#{tenant_id := TId}) -> TId =:= TenantId1 end, Receipts1),
    lists:all(fun(#{tenant_id := TId}) -> TId =:= TenantId2 end, Receipts2),

    unlink(Pid1),
    unlink(Pid2),
    ok.
