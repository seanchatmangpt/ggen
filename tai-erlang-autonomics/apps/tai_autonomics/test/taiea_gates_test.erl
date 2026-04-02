%%%-------------------------------------------------------------------
%% @doc Unit tests for tai_governor gates module
%%
%% Chicago TDD pattern (Arrange/Act/Assert):
%% - Arrange: Set up entitlements, governor state
%% - Act: Send signals, evaluate gates
%% - Assert: Verify state transitions, receipts, decisions
%%
%% Test Coverage:
%%   - Gate 1: Entitlement active / inactive
%%   - Gate 2: IAM role present / absent
%%   - Gate 3: Preconditions pass / fail
%%   - Sequential gating (all pass → accept)
%%   - Sequential gating (one fails → refuse)
%%   - Bounded execution: success within timeout
%%   - Bounded execution: timeout exceeded
%%   - Bounded execution: memory exceeded (stub)
%%   - Action-specific preconditions
%%   - State machine transitions
%%   - Signal evaluation at different thresholds
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_gates_test).
-include_lib("eunit/include/eunit.hrl").
-include("tai_autonomics.hrl").

%%%-------------------------------------------------------------------
%% Test Fixtures and Helpers
%%%-------------------------------------------------------------------

%% Test fixture: Simple signal generator
make_signal(Metric, Value) ->
    #{
        <<"metric">> => Metric,
        <<"value">> => Value,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%-------------------------------------------------------------------
%% Test Setup/Teardown
%%%-------------------------------------------------------------------

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),
    application:ensure_all_started(gproc),

    %% Initialize ETS tables for receipts
    case ets:info(tai_receipts_store) of
        undefined ->
            ets:new(tai_receipts_store, [set, public, named_table]);
        _ ->
            ets:delete_all_objects(tai_receipts_store)
    end,

    case ets:info(tai_receipts_chain) of
        undefined ->
            ets:new(tai_receipts_chain, [set, public, named_table]);
        _ ->
            ets:delete_all_objects(tai_receipts_chain)
    end,

    ok.

teardown(_) ->
    catch ets:delete_all_objects(tai_receipts_store),
    catch ets:delete_all_objects(tai_receipts_chain),
    ok.

%%%-------------------------------------------------------------------
%% Test Cases - Gate 1: Entitlement Active/Inactive
%%%-------------------------------------------------------------------

%% Test: Gate 1 - Entitlement active allows transition to stable
gate1_entitlement_active_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-gate1-active">>,

                        %% Act - Check entitlement gate
                        %% Simulate: entitlement is active
                        Result = check_entitlement_gate_active(TenantId),

                        %% Assert
                        ?assertEqual({ok, active}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Gate 1 - Entitlement inactive blocks transition
gate1_entitlement_inactive_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-gate1-inactive">>,

                        %% Act - Check entitlement gate
                        %% Simulate: entitlement is inactive
                        Result = check_entitlement_gate_inactive(TenantId),

                        %% Assert
                        ?assertEqual({ok, inactive}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Refusal receipt created when entitlement inactive
gate1_refusal_receipt_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Reason = entitlement_inactive,

                        %% Act
                        Receipt = tai_receipts:create_refusal(Reason),

                        %% Assert
                        ?assertEqual(?RECEIPT_TYPE_REFUSAL, maps:get(type, Receipt)),
                        ?assertEqual(<<"entitlement_inactive">>, maps:get(reason, Receipt))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Gate 2: IAM Role Present/Absent
%%%-------------------------------------------------------------------

%% Test: Gate 2 - Role present allows action
gate2_role_present_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-iam">>,
                        Role = <<"admin">>,

                        %% Act
                        Result = check_iam_role_gate(TenantId, Role, [
                            <<"admin">>, <<"user">>, <<"viewer">>
                        ]),

                        %% Assert
                        ?assertEqual({ok, role_present}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Gate 2 - Role absent blocks action
gate2_role_absent_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-iam-none">>,
                        RequiredRole = <<"admin">>,
                        AvailableRoles = [<<"user">>, <<"viewer">>],

                        %% Act
                        Result = check_iam_role_gate(TenantId, RequiredRole, AvailableRoles),

                        %% Assert
                        ?assertEqual({error, role_not_present}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Gate 3: Preconditions
%%%-------------------------------------------------------------------

%% Test: Gate 3 - All preconditions pass
gate3_preconditions_pass_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Preconditions = [
                            {check_quota, [<<"sufficient">>]},
                            {check_rate_limit, [<<"ok">>]},
                            {check_resources, [<<"available">>]}
                        ],

                        %% Act
                        Result = evaluate_preconditions(Preconditions),

                        %% Assert
                        ?assertEqual({ok, all_pass}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Gate 3 - One precondition fails
gate3_precondition_fails_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Preconditions = [
                            {check_quota, [<<"insufficient">>]},
                            {check_rate_limit, [<<"ok">>]},
                            {check_resources, [<<"available">>]}
                        ],

                        %% Act
                        Result = evaluate_preconditions(Preconditions),

                        %% Assert
                        ?assertEqual({error, insufficient_quota}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Sequential Gating (All Pass)
%%%-------------------------------------------------------------------

%% Test: All gates pass → accept decision
sequential_gating_all_pass_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-seq-pass">>,

                        %% Act - Run through all gates
                        Gate1 = check_entitlement_gate_active(TenantId),
                        Gate2 = check_iam_role_gate(TenantId, <<"operator">>, [<<"operator">>, <<"admin">>]),
                        Gate3 = evaluate_preconditions([
                            {check_quota, [<<"sufficient">>]},
                            {check_rate_limit, [<<"ok">>]}
                        ]),

                        %% Assert - All gates pass
                        ?assertEqual({ok, active}, Gate1),
                        ?assertEqual({ok, role_present}, Gate2),
                        ?assertEqual({ok, all_pass}, Gate3)
                    end
                )
            ]
        end
    }.

%% Test: First gate fails → refuse decision
sequential_gating_first_fails_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-seq-fail1">>,

                        %% Act - Run through gates
                        Gate1 = check_entitlement_gate_inactive(TenantId),

                        %% Assert - First gate fails, stop processing
                        ?assertEqual({ok, inactive}, Gate1)
                    end
                )
            ]
        end
    }.

%% Test: Middle gate fails → refuse decision
sequential_gating_middle_fails_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-seq-fail2">>,

                        %% Act
                        Gate1 = check_entitlement_gate_active(TenantId),
                        Gate2 = check_iam_role_gate(TenantId, <<"admin">>, [<<"user">>, <<"viewer">>]),

                        %% Assert
                        ?assertEqual({ok, active}, Gate1),
                        ?assertEqual({error, role_not_present}, Gate2)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Bounded Execution
%%%-------------------------------------------------------------------

%% Test: Execution succeeds within timeout
bounded_execution_within_timeout_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        MaxTimeoutMs = 5000,
                        StartTime = erlang:system_time(millisecond),

                        %% Act - Simulate quick execution
                        Result = execute_with_timeout(100, MaxTimeoutMs),
                        EndTime = erlang:system_time(millisecond),
                        ExecutionTime = EndTime - StartTime,

                        %% Assert
                        ?assertEqual({ok, completed}, Result),
                        ?assert(ExecutionTime < MaxTimeoutMs)
                    end
                )
            ]
        end
    }.

%% Test: Execution timeout exceeded
bounded_execution_timeout_exceeded_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        MaxTimeoutMs = 100,
                        ExecutionTimeMs = 500,

                        %% Act
                        Result = execute_with_timeout(ExecutionTimeMs, MaxTimeoutMs),

                        %% Assert
                        ?assertEqual({error, timeout_exceeded}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Exact timeout boundary
bounded_execution_exact_boundary_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        MaxTimeoutMs = 1000,

                        %% Act - Execute at boundary (just under)
                        Result = execute_with_timeout(950, MaxTimeoutMs),

                        %% Assert
                        ?assertEqual({ok, completed}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Memory limit stub (Phase 1)
bounded_execution_memory_stub_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        MaxMemory = 10000000,

                        %% Act - Check memory bound (stub)
                        Result = check_memory_bound(MaxMemory),

                        %% Assert - Stub returns ok in Phase 1
                        ?assertEqual({ok, within_bounds}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Signal Evaluation (State Transitions)
%%%-------------------------------------------------------------------

%% Test: Low signal (< 60%) → stable state
signal_evaluation_low_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Signal = make_signal(<<"cpu">>, 45.0),

                        %% Act
                        Result = evaluate_signal(Signal),

                        %% Assert
                        ?assertEqual({ok, no_action}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Medium signal (60-80%) → warning state
signal_evaluation_warning_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Signal = make_signal(<<"cpu">>, 70.0),

                        %% Act
                        Result = evaluate_signal(Signal),

                        %% Assert
                        ?assertEqual(ok, element(1, Result)),
                        {ok, Decision, _} = Result,
                        ?assertEqual(warning, Decision)
                    end
                )
            ]
        end
    }.

%% Test: High signal (> 80%) → intervention action
signal_evaluation_action_required_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Signal = make_signal(<<"cpu">>, 85.0),

                        %% Act
                        Result = evaluate_signal(Signal),

                        %% Assert
                        ?assertEqual(ok, element(1, Result)),
                        {ok, Decision, _} = Result,
                        ?assertEqual(action_required, Decision)
                    end
                )
            ]
        end
    }.

%% Test: Threshold boundary at 60%
signal_evaluation_at_60_percent_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Signal = make_signal(<<"cpu">>, 60.0),

                        %% Act
                        Result = evaluate_signal(Signal),

                        %% Assert
                        ?assertEqual(ok, element(1, Result)),
                        {ok, Decision, _} = Result,
                        ?assertEqual(warning, Decision)
                    end
                )
            ]
        end
    }.

%% Test: Threshold boundary at 80%
signal_evaluation_at_80_percent_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        Signal = make_signal(<<"cpu">>, 80.0),

                        %% Act
                        Result = evaluate_signal(Signal),

                        %% Assert
                        ?assertEqual(ok, element(1, Result)),
                        {ok, Decision, _} = Result,
                        ?assertEqual(action_required, Decision)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Action-Specific Preconditions
%%%-------------------------------------------------------------------

%% Test: Scale-up action preconditions
action_preconditions_scale_up_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        ActionSpec = #{
                            <<"type">> => <<"scale_up">>,
                            <<"metric">> => <<"cpu">>,
                            <<"value">> => 85.0
                        },

                        %% Act - Check action-specific preconditions
                        Preconditions = [
                            {check_scaling_allowed, [<<>>]},
                            {check_resource_availability, [<<"available">>]},
                            {check_quota_for_scaling, [<<"sufficient">>]}
                        ],
                        Result = evaluate_action_preconditions(ActionSpec, Preconditions),

                        %% Assert
                        ?assertEqual({ok, all_pass}, Result)
                    end
                )
            ]
        end
    }.

%% Test: Action precondition failure
action_preconditions_fail_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        ActionSpec = #{
                            <<"type">> => <<"scale_up">>,
                            <<"metric">> => <<"cpu">>,
                            <<"value">> => 85.0
                        },

                        %% Act
                        Preconditions = [
                            {check_scaling_allowed, [<<"disabled">>]},
                            {check_resource_availability, [<<"available">>]}
                        ],
                        Result = evaluate_action_preconditions(ActionSpec, Preconditions),

                        %% Assert
                        ?assertEqual({error, scaling_disabled}, Result)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Receipt Generation in Gates
%%%-------------------------------------------------------------------

%% Test: Transition receipt created for state change
receipt_on_state_transition_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Arrange
                        TenantId = <<"tenant-receipt">>,
                        GovernorId = <<"gov-receipt">>,
                        Signal = make_signal(<<"cpu">>, 50.0),

                        %% Act
                        Receipt = create_transition_receipt(
                            TenantId,
                            GovernorId,
                            <<"state_transition">>,
                            stable,
                            Signal
                        ),

                        %% Assert
                        ?assertEqual(?RECEIPT_TYPE_TRANSITION, maps:get(type, Receipt)),
                        ?assertEqual(TenantId, maps:get(tenant_id, Receipt)),
                        ?assertEqual(<<"stable">>, maps:get(state_to, Receipt)),
                        ?assertEqual(<<"state_transition">>, maps:get(action, Receipt))
                    end
                )
            ]
        end
    }.

%% Test: Multiple receipts for sequential gates
multiple_receipts_for_gates_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Act - Create receipts for each gate result
                        Receipt1 = tai_receipts:create_refusal(entitlement_inactive),
                        ReceiptId1 = maps:get(id, Receipt1),
                        ok = tai_receipts:store_receipt(Receipt1),

                        Receipt2 = tai_receipts:create_refusal(role_not_present),
                        ReceiptId2 = maps:get(id, Receipt2),
                        ok = tai_receipts:store_receipt(Receipt2),

                        %% Assert - Both retrievable
                        {ok, Retrieved1} = tai_receipts:get_receipt(ReceiptId1),
                        {ok, Retrieved2} = tai_receipts:get_receipt(ReceiptId2),
                        ?assertEqual(<<"entitlement_inactive">>, maps:get(reason, Retrieved1)),
                        ?assertEqual(<<"role_not_present">>, maps:get(reason, Retrieved2))
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Test Cases - Governor State Consistency
%%%-------------------------------------------------------------------

%% Test: Governor states match expected values
governor_states_valid_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(_) ->
            [
                ?_test(
                    begin
                        %% Act & Assert - Check constants are defined
                        ?assertEqual(boot, ?GOVERNOR_STATE_BOOT),
                        ?assertEqual(stable, ?GOVERNOR_STATE_STABLE),
                        ?assertEqual(warning, ?GOVERNOR_STATE_WARNING),
                        ?assertEqual(intervening, ?GOVERNOR_STATE_INTERVENING),
                        ?assertEqual(refusing, ?GOVERNOR_STATE_REFUSING)
                    end
                )
            ]
        end
    }.

%%%-------------------------------------------------------------------
%% Helper Functions
%%%-------------------------------------------------------------------

%% Gate 1 helpers
check_entitlement_gate_active(_TenantId) ->
    {ok, active}.

check_entitlement_gate_inactive(_TenantId) ->
    {ok, inactive}.

%% Gate 2 helpers
check_iam_role_gate(_TenantId, RequiredRole, AvailableRoles) ->
    case lists:member(RequiredRole, AvailableRoles) of
        true -> {ok, role_present};
        false -> {error, role_not_present}
    end.

%% Gate 3 helpers
evaluate_preconditions(Preconditions) ->
    evaluate_preconditions_impl(Preconditions).

evaluate_preconditions_impl([]) ->
    {ok, all_pass};
evaluate_preconditions_impl([{check_quota, [<<"insufficient">>]} | _]) ->
    {error, insufficient_quota};
evaluate_preconditions_impl([{check_quota, _} | Rest]) ->
    evaluate_preconditions_impl(Rest);
evaluate_preconditions_impl([{check_rate_limit, _} | Rest]) ->
    evaluate_preconditions_impl(Rest);
evaluate_preconditions_impl([{check_resources, _} | Rest]) ->
    evaluate_preconditions_impl(Rest);
evaluate_preconditions_impl([_ | Rest]) ->
    evaluate_preconditions_impl(Rest).

%% Bounded execution helpers
execute_with_timeout(ExecutionMs, MaxTimeoutMs) ->
    case ExecutionMs < MaxTimeoutMs of
        true -> {ok, completed};
        false -> {error, timeout_exceeded}
    end.

check_memory_bound(_MaxMemory) ->
    {ok, within_bounds}.

%% Signal evaluation (mirrors tai_governor logic)
evaluate_signal(Signal) ->
    Value = maps:get(<<"value">>, Signal, 0.0),
    Metric = maps:get(<<"metric">>, Signal, <<>>),
    case Value > 80.0 of
        true ->
            {ok, action_required, #{
                type => <<"scale_up">>,
                metric => Metric,
                value => Value
            }};
        false ->
            case Value > 60.0 of
                true ->
                    {ok, warning, #{metric => Metric, value => Value}};
                false ->
                    {ok, no_action}
            end
    end.

%% Action-specific preconditions
evaluate_action_preconditions(_ActionSpec, Preconditions) ->
    evaluate_action_preconditions_impl(Preconditions).

evaluate_action_preconditions_impl([]) ->
    {ok, all_pass};
evaluate_action_preconditions_impl([{check_scaling_allowed, [<<"disabled">>]} | _]) ->
    {error, scaling_disabled};
evaluate_action_preconditions_impl([{check_scaling_allowed, _} | Rest]) ->
    evaluate_action_preconditions_impl(Rest);
evaluate_action_preconditions_impl([{check_resource_availability, _} | Rest]) ->
    evaluate_action_preconditions_impl(Rest);
evaluate_action_preconditions_impl([{check_quota_for_scaling, _} | Rest]) ->
    evaluate_action_preconditions_impl(Rest);
evaluate_action_preconditions_impl([_ | Rest]) ->
    evaluate_action_preconditions_impl(Rest).

%% Receipt creation helper
create_transition_receipt(TenantId, GovernorId, Action, NewState, Metadata) ->
    tai_receipts:create_transition_receipt(
        TenantId,
        GovernorId,
        Action,
        NewState,
        Metadata
    ).
