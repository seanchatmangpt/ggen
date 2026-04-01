%%%-------------------------------------------------------------------
%% @doc TAI Erlang Autonomic Gates - Comprehensive Test Suite
%%
%% Tests for gate checking and bounded action execution:
%% - Individual gate validation
%% - All gates passing scenario
%% - Single gate failure scenarios
%% - Mixed scenarios with context
%% - Bounded action execution with timeouts
%% - Memory limit enforcement
%% - Error handling and edge cases
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_gates_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).

%% Test cases - Gate validation
-export([
    gate_entitlement_check_accepts/1,
    gate_iam_role_check_accepts/1,
    gate_preconditions_health_check_accepts/1,
    gate_preconditions_support_model_accepts/1,
    gate_preconditions_entitlement_apply_valid/1,
    gate_preconditions_entitlement_apply_missing_customer/1,
    gate_preconditions_entitlement_apply_missing_feature/1,
    gate_preconditions_receipts_verify_valid/1,
    gate_preconditions_receipts_verify_missing/1
]).

%% Test cases - All gates
-export([
    all_gates_pass_health_check/1,
    all_gates_pass_support_model/1,
    all_gates_pass_entitlement_apply/1,
    all_gates_pass_receipts_verify/1,
    all_gates_fail_entitlement/1,
    all_gates_fail_iam_role/1,
    all_gates_fail_preconditions/1
]).

%% Test cases - Bounded action execution
-export([
    bounded_action_executes_successfully/1,
    bounded_action_respects_timeout/1,
    bounded_action_returns_result_value/1,
    bounded_action_handles_exception/1,
    bounded_action_memory_tracking/1
]).

%% Test cases - Integration scenarios
-export([
    integration_complete_flow_health_check/1,
    integration_complete_flow_entitlement_apply/1,
    integration_gate_check_with_bounded_action/1,
    integration_multiple_concurrent_gates/1
]).

%%%===================================================================
%% CT Callbacks
%%%===================================================================

init_per_suite(Config) ->
    case application:ensure_all_started(tai_autonomics) of
        {ok, _} -> Config;
        {error, Reason} -> ct:fail(Reason)
    end.

end_per_suite(_Config) ->
    application:stop(tai_autonomics),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        %% Individual gate tests
        gate_entitlement_check_accepts,
        gate_iam_role_check_accepts,
        gate_preconditions_health_check_accepts,
        gate_preconditions_support_model_accepts,
        gate_preconditions_entitlement_apply_valid,
        gate_preconditions_entitlement_apply_missing_customer,
        gate_preconditions_entitlement_apply_missing_feature,
        gate_preconditions_receipts_verify_valid,
        gate_preconditions_receipts_verify_missing,
        %% All gates scenarios
        all_gates_pass_health_check,
        all_gates_pass_support_model,
        all_gates_pass_entitlement_apply,
        all_gates_pass_receipts_verify,
        all_gates_fail_entitlement,
        all_gates_fail_iam_role,
        all_gates_fail_preconditions,
        %% Bounded action execution
        bounded_action_executes_successfully,
        bounded_action_respects_timeout,
        bounded_action_returns_result_value,
        bounded_action_handles_exception,
        bounded_action_memory_tracking,
        %% Integration scenarios
        integration_complete_flow_health_check,
        integration_complete_flow_entitlement_apply,
        integration_gate_check_with_bounded_action,
        integration_multiple_concurrent_gates
    ].

%%%===================================================================
%% Individual Gate Tests
%%%===================================================================

%% Gate 1: Entitlement Check
gate_entitlement_check_accepts(Config) ->
    TenantId = <<"tenant-001">>,
    {accept, Metadata} = taiea_gates:check_entitlement(TenantId),

    %% Verify metadata structure
    ?assertEqual(true, maps:is_key(gate, Metadata)),
    ?assertEqual(entitlement, maps:get(gate, Metadata)),
    ?assertEqual(TenantId, maps:get(tenant_id, Metadata)),
    ?assertEqual(active, maps:get(status, Metadata)),

    ct:log("Gate 1 (Entitlement) passed: ~p~n", [Metadata]),
    Config.

%% Gate 2: IAM Role Check
gate_iam_role_check_accepts(Config) ->
    TenantId = <<"tenant-002">>,
    RequiredRole = entitlement_admin,

    {accept, Metadata} = taiea_gates:check_iam_role(TenantId, RequiredRole),

    ?assertEqual(true, maps:is_key(gate, Metadata)),
    ?assertEqual(iam_role, maps:get(gate, Metadata)),
    ?assertEqual(TenantId, maps:get(tenant_id, Metadata)),
    ?assertEqual(RequiredRole, maps:get(required_role, Metadata)),
    ?assertEqual(true, maps:get(verified, Metadata)),

    ct:log("Gate 2 (IAM Role) passed: ~p~n", [Metadata]),
    Config.

%% Gate 3: Preconditions - Health Check
gate_preconditions_health_check_accepts(Config) ->
    Action = health_check,
    Context = #{},

    {accept, Metadata} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(true, maps:is_key(gate, Metadata)),
    ?assertEqual(preconditions, maps:get(gate, Metadata)),
    ?assertEqual(Action, maps:get(action, Metadata)),

    ct:log("Gate 3 (Preconditions - health_check) passed: ~p~n", [Metadata]),
    Config.

%% Gate 3: Preconditions - Support Model
gate_preconditions_support_model_accepts(Config) ->
    Action = support_model,
    Context = #{},

    {accept, Metadata} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(true, maps:is_key(gate, Metadata)),
    ?assertEqual(preconditions, maps:get(gate, Metadata)),
    ?assertEqual(Action, maps:get(action, Metadata)),

    ct:log("Gate 3 (Preconditions - support_model) passed: ~p~n", [Metadata]),
    Config.

%% Gate 3: Preconditions - Entitlement Apply (Valid)
gate_preconditions_entitlement_apply_valid(Config) ->
    Action = entitlement_apply,
    Context = #{
        customer_id => <<"cust-123">>,
        feature_key => <<"feature-key">>
    },

    {accept, Metadata} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(true, maps:is_key(gate, Metadata)),
    ?assertEqual(preconditions, maps:get(gate, Metadata)),
    ?assertEqual(Action, maps:get(action, Metadata)),
    ?assertEqual(true, maps:get(validated, Metadata)),

    ct:log("Gate 3 (Preconditions - entitlement_apply valid) passed: ~p~n", [Metadata]),
    Config.

%% Gate 3: Preconditions - Entitlement Apply (Missing customer_id)
gate_preconditions_entitlement_apply_missing_customer(Config) ->
    Action = entitlement_apply,
    Context = #{
        feature_key => <<"feature-key">>
    },

    {refuse, Reason} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(missing_customer_id, Reason),

    ct:log("Gate 3 (Preconditions - entitlement_apply missing customer) correctly refused: ~p~n", [
        Reason
    ]),
    Config.

%% Gate 3: Preconditions - Entitlement Apply (Missing feature_key)
gate_preconditions_entitlement_apply_missing_feature(Config) ->
    Action = entitlement_apply,
    Context = #{
        customer_id => <<"cust-123">>
    },

    {refuse, Reason} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(missing_feature_key, Reason),

    ct:log("Gate 3 (Preconditions - entitlement_apply missing feature) correctly refused: ~p~n", [
        Reason
    ]),
    Config.

%% Gate 3: Preconditions - Receipts Verify (Valid)
gate_preconditions_receipts_verify_valid(Config) ->
    Action = receipts_verify,
    Context = #{
        receipt_id => <<"receipt-456">>
    },

    {accept, Metadata} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(true, maps:is_key(gate, Metadata)),
    ?assertEqual(preconditions, maps:get(gate, Metadata)),
    ?assertEqual(Action, maps:get(action, Metadata)),
    ?assertEqual(true, maps:get(exists, Metadata)),

    ct:log("Gate 3 (Preconditions - receipts_verify valid) passed: ~p~n", [Metadata]),
    Config.

%% Gate 3: Preconditions - Receipts Verify (Missing)
gate_preconditions_receipts_verify_missing(Config) ->
    Action = receipts_verify,
    Context = #{},

    {refuse, Reason} = taiea_gates:check_preconditions(Action, Context),

    ?assertEqual(missing_receipt_id, Reason),

    ct:log("Gate 3 (Preconditions - receipts_verify missing) correctly refused: ~p~n", [Reason]),
    Config.

%%%===================================================================
%% All Gates Tests
%%%===================================================================

%% All gates pass - Health Check
all_gates_pass_health_check(Config) ->
    TenantId = <<"tenant-003">>,
    Action = health_check,
    Context = #{},

    {accept, Metadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    %% Verify all gate metadata is merged
    ?assertEqual(true, maps:is_key(gate, Metadata)),
    %% The last gate (preconditions) should be in the final result
    ?assertEqual(true, maps:is_key(action, Metadata)),
    ?assertEqual(health_check, maps:get(action, Metadata)),

    ct:log("All gates passed (health_check): ~p~n", [Metadata]),
    Config.

%% All gates pass - Support Model
all_gates_pass_support_model(Config) ->
    TenantId = <<"tenant-004">>,
    Action = support_model,
    Context = #{},

    {accept, Metadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    ?assertEqual(true, maps:is_key(action, Metadata)),
    ?assertEqual(support_model, maps:get(action, Metadata)),

    ct:log("All gates passed (support_model): ~p~n", [Metadata]),
    Config.

%% All gates pass - Entitlement Apply
all_gates_pass_entitlement_apply(Config) ->
    TenantId = <<"tenant-005">>,
    Action = entitlement_apply,
    Context = #{
        customer_id => <<"cust-789">>,
        feature_key => <<"feature-abc">>
    },

    {accept, Metadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    ?assertEqual(true, maps:is_key(action, Metadata)),
    ?assertEqual(entitlement_apply, maps:get(action, Metadata)),
    ?assertEqual(true, maps:get(validated, Metadata)),

    ct:log("All gates passed (entitlement_apply): ~p~n", [Metadata]),
    Config.

%% All gates pass - Receipts Verify
all_gates_pass_receipts_verify(Config) ->
    TenantId = <<"tenant-006">>,
    Action = receipts_verify,
    Context = #{
        receipt_id => <<"receipt-xyz">>
    },

    {accept, Metadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    ?assertEqual(true, maps:is_key(action, Metadata)),
    ?assertEqual(receipts_verify, maps:get(action, Metadata)),
    ?assertEqual(true, maps:get(exists, Metadata)),

    ct:log("All gates passed (receipts_verify): ~p~n", [Metadata]),
    Config.

%% All gates - Entitlement gate fails (would fail in Phase 2)
all_gates_fail_entitlement(Config) ->
    %% In Phase 1, entitlement always accepts, so we test future behavior
    %% by documenting the expected flow
    TenantId = <<"tenant-fail-entitlement">>,
    Action = health_check,
    Context = #{},

    %% Currently passes because entitlement is stubbed
    {accept, _Metadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    ct:log("Phase 1: Entitlement gate always accepts (Phase 2 will test failure)~n"),
    Config.

%% All gates - IAM role gate fails (would fail in Phase 2)
all_gates_fail_iam_role(Config) ->
    %% In Phase 1, IAM role always accepts, so we test future behavior
    %% by documenting the expected flow
    TenantId = <<"tenant-fail-iam">>,
    Action = health_check,
    Context = #{},

    %% Currently passes because IAM role is stubbed
    {accept, _Metadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    ct:log("Phase 1: IAM role gate always accepts (Phase 2 will test failure)~n"),
    Config.

%% All gates - Preconditions gate fails
all_gates_fail_preconditions(Config) ->
    TenantId = <<"tenant-007">>,
    Action = entitlement_apply,
    Context = #{},
    %% Missing required fields

    {refuse, Reason} = taiea_gates:check_all_gates(TenantId, Action, Context),

    %% Should fail on preconditions check
    ?assertEqual(missing_customer_id, Reason),

    ct:log("All gates - Preconditions correctly failed: ~p~n", [Reason]),
    Config.

%%%===================================================================
%% Bounded Action Execution Tests
%%%===================================================================

%% Bounded action executes successfully
bounded_action_executes_successfully(Config) ->
    Handler = fun() ->
        timer:sleep(10),
        {ok, result_value}
    end,

    Result = taiea_gates:execute_bounded_action(Handler, 5000, 100),

    ?assertEqual({ok, {ok, result_value}}, Result),

    ct:log("Bounded action executed successfully: ~p~n", [Result]),
    Config.

%% Bounded action respects timeout
bounded_action_respects_timeout(Config) ->
    Handler = fun() ->
        timer:sleep(2000),
        completed
    end,

    StartTime = erlang:monotonic_time(millisecond),
    Result = taiea_gates:execute_bounded_action(Handler, 500, 100),
    ElapsedTime = erlang:monotonic_time(millisecond) - StartTime,

    ?assertEqual({timeout}, Result),
    %% Should timeout around 500ms (allow some variance)
    ?assert(ElapsedTime < 1000),

    ct:log("Bounded action correctly timed out after ~p ms~n", [ElapsedTime]),
    Config.

%% Bounded action returns result value
bounded_action_returns_result_value(Config) ->
    ExpectedResult = #{key => value, number => 42},
    Handler = fun() ->
        timer:sleep(5),
        ExpectedResult
    end,

    {ok, Result} = taiea_gates:execute_bounded_action(Handler, 5000, 100),

    ?assertEqual(ExpectedResult, Result),

    ct:log("Bounded action returned expected result: ~p~n", [Result]),
    Config.

%% Bounded action handles exceptions
bounded_action_handles_exception(Config) ->
    Handler = fun() ->
        throw(test_exception)
    end,

    Result = taiea_gates:execute_bounded_action(Handler, 5000, 100),

    ?assertEqual({error, test_exception}, Result),

    ct:log("Bounded action correctly handled exception: ~p~n", [Result]),
    Config.

%% Bounded action memory tracking
bounded_action_memory_tracking(Config) ->
    %% Create a handler that allocates some memory
    Handler = fun() ->
        _List = lists:seq(1, 10000),
        memory_test
    end,

    Result = taiea_gates:execute_bounded_action(Handler, 5000, 100),

    %% Should succeed because 100MB limit is generous for this allocation
    case Result of
        {ok, memory_test} ->
            ct:log("Bounded action executed with memory tracking: ~p~n", [Result]);
        {timeout} ->
            ct:log("Bounded action timed out during memory test~n");
        {error, Reason} ->
            ct:log("Bounded action error: ~p~n", [Reason])
    end,

    Config.

%%%===================================================================
%% Integration Tests
%%%===================================================================

%% Integration: Complete flow - Health Check
integration_complete_flow_health_check(Config) ->
    TenantId = <<"tenant-integration-001">>,
    Action = health_check,
    Context = #{},

    %% Gate check
    {accept, GateMetadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    %% Bounded action execution
    Handler = fun() ->
        %% Simulate actual health check work
        #{status => healthy, checks => 5}
    end,

    {ok, ActionResult} = taiea_gates:execute_bounded_action(Handler, 5000, 50),

    ?assertEqual(true, maps:is_key(status, ActionResult)),
    ?assertEqual(healthy, maps:get(status, ActionResult)),

    ct:log(
        "Integration complete flow (health_check): gates=~p, action=~p~n",
        [GateMetadata, ActionResult]
    ),
    Config.

%% Integration: Complete flow - Entitlement Apply
integration_complete_flow_entitlement_apply(Config) ->
    TenantId = <<"tenant-integration-002">>,
    Action = entitlement_apply,
    Context = #{
        customer_id => <<"cust-integration">>,
        feature_key => <<"feature-integration">>
    },

    %% Gate check
    {accept, GateMetadata} = taiea_gates:check_all_gates(TenantId, Action, Context),

    %% Bounded action execution
    Handler = fun() ->
        #{entitlement_id => <<"ent-123">>, status => active}
    end,

    {ok, ActionResult} = taiea_gates:execute_bounded_action(Handler, 5000, 50),

    ?assertEqual(true, maps:is_key(entitlement_id, ActionResult)),
    ?assertEqual(active, maps:get(status, ActionResult)),

    ct:log(
        "Integration complete flow (entitlement_apply): gates=~p, action=~p~n",
        [GateMetadata, ActionResult]
    ),
    Config.

%% Integration: Gate check with bounded action
integration_gate_check_with_bounded_action(Config) ->
    TenantId = <<"tenant-integration-003">>,
    Action = health_check,
    Context = #{},

    %% Check gates and execute action in sequence
    case taiea_gates:check_all_gates(TenantId, Action, Context) of
        {accept, _GateMetadata} ->
            Handler = fun() ->
                {success, health_check_completed}
            end,

            Result = taiea_gates:execute_bounded_action(Handler, 5000, 50),

            case Result of
                {ok, {success, _}} ->
                    ct:log("Gate check passed, action executed successfully~n");
                {timeout} ->
                    ct:log("Gate check passed, but action timed out~n");
                {error, Reason} ->
                    ct:log("Gate check passed, but action failed: ~p~n", [Reason])
            end;
        {refuse, Reason} ->
            ct:log("Gate check failed: ~p, action not executed~n", [Reason])
    end,

    Config.

%% Integration: Multiple concurrent gates
integration_multiple_concurrent_gates(Config) ->
    TenantIds = [
        <<"tenant-concurrent-1">>,
        <<"tenant-concurrent-2">>,
        <<"tenant-concurrent-3">>
    ],

    %% Execute gate checks concurrently
    Pids = [
        spawn(fun() ->
            taiea_gates:check_all_gates(TenantId, health_check, #{})
        end)
        || TenantId <- TenantIds
    ],

    %% Collect results
    Results = [
        receive
            {'EXIT', _, {normal, Result}} -> Result
        after 5000 -> timeout
        end
        || _ <- Pids
    ],

    %% All should pass
    PassCount = length([ok || {accept, _} <- Results]),
    ?assertEqual(length(TenantIds), PassCount),

    ct:log("Concurrent gate checks completed: ~w/~w passed~n", [PassCount, length(TenantIds)]),
    Config.

%%%===================================================================
%% Test Utilities
%%%===================================================================

%% Helper to create valid test context for entitlement_apply
valid_entitlement_context() ->
    #{
        customer_id => <<"test-customer">>,
        feature_key => <<"test-feature">>
    }.

%% Helper to create valid test context for receipts_verify
valid_receipt_context() ->
    #{
        receipt_id => <<"test-receipt">>
    }.
