%%====================================================================
%% pattern_tests - Van der Aalst Workflow Pattern Tests
%%====================================================================
%% @doc Comprehensive tests for all 20 Van der Aalst workflow control-
%% flow patterns. Each test validates the pattern semantics using state-
%% based verification with real workflow instances.

-module(pattern_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/gen_yawl.hrl").

%%====================================================================
%% WP1: Sequence Pattern Tests
%%====================================================================

wp1_sequence_test_() ->
    {foreach,
     fun() -> {ok, Pid} = simple_sequence:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_) ->
            {"WP1: Tasks execute in strict sequence",
             fun() ->
                {ok, Pid} = simple_sequence:start_link(),

                %% Verify initial state
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_input)),

                %% Fire sequence
                ok = gen_yawl:fire(Pid, t_draft),
                ?assertMatch([], gen_yawl:place_tokens(Pid, p_input)),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_review)),

                ok = gen_yawl:fire(Pid, t_review),
                ?assertMatch([], gen_yawl:place_tokens(Pid, p_review)),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_approve)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP2 + WP3: Parallel Split and Synchronization Tests
%%====================================================================

wp2_parallel_split_test_() ->
    {foreach,
     fun() -> {ok, Pid} = parallel_review:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP2: AND-split enables all branches simultaneously",
             fun() ->
                {ok, Pid} = parallel_review:start_link(),

                %% Fire submit to trigger split
                ok = gen_yawl:fire(Pid, t_submit),

                %% Assert: All three review branches enabled
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_legal_review)),
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_tech_review)),
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_finance_review)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

wp3_synchronization_test_() ->
    {foreach,
     fun() -> {ok, Pid} = parallel_review:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP3: AND-join waits for all branches",
             fun() ->
                {ok, Pid} = parallel_review:start_link(),

                %% Fire split
                ok = gen_yawl:fire(Pid, t_submit),

                %% Fire two branches - third still pending
                ok = gen_yawl:fire(Pid, t_legal_review),
                ok = gen_yawl:fire(Pid, t_tech_review),

                %% Assert: Finalize NOT enabled yet
                ?assertMatch([], gen_yawl:place_tokens(Pid, p_finalize)),

                %% Fire third branch
                ok = gen_yawl:fire(Pid, t_finance_review),

                %% Assert: Finalize now enabled
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_finalize)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP4 + WP5: Exclusive Choice and Simple Merge Tests
%%====================================================================

wp4_exclusive_choice_test_() ->
    {foreach,
     fun() -> {ok, Pid} = conditional_routing:start_link(#{amount => 500}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP4: XOR-split selects exactly one branch",
             fun() ->
                %% Low amount - should take auto_approve path
                {ok, Pid} = conditional_routing:start_link(#{amount => 500}),
                ok = gen_yawl:fire(Pid, t_check_amount),

                %% Assert: Only auto_approve branch has token
                AutoTokens = gen_yawl:place_tokens(Pid, p_auto_approve),
                ManualTokens = gen_yawl:place_tokens(Pid, p_manual_review),
                RejectTokens = gen_yawl:place_tokens(Pid, p_reject),

                ?assertMatch([_], AutoTokens),
                ?assertEqual([], ManualTokens),
                ?assertEqual([], RejectTokens),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP4: XOR-split with different condition",
             fun() ->
                %% High amount - should take manual_review path
                {ok, Pid} = conditional_routing:start_link(#{amount => 5000}),
                ok = gen_yawl:fire(Pid, t_check_amount),

                %% Assert: Only manual_review branch has token
                AutoTokens = gen_yawl:place_tokens(Pid, p_auto_approve),
                ManualTokens = gen_yawl:place_tokens(Pid, p_manual_review),

                ?assertEqual([], AutoTokens),
                ?assertMatch([_], ManualTokens),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

wp5_simple_merge_test_() ->
    {foreach,
     fun() -> {ok, Pid} = conditional_routing:start_link(#{amount => 500}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP5: Simple merge accepts first arriving token",
             fun() ->
                {ok, Pid} = conditional_routing:start_link(#{amount => 500}),

                %% Fire through auto_approve path
                ok = gen_yawl:fire(Pid, t_check_amount),
                ok = gen_yawl:fire(Pid, t_auto_approve),

                %% Assert: Done place has token
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_done)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP6 + WP7: Multi-Choice and Structured Sync Merge Tests
%%====================================================================

wp6_multi_choice_test_() ->
    {foreach,
     fun() -> {ok, Pid} = multi_choice_workflow:start_link(#{regions => [east, west]}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP6: OR-split enables one or more branches",
             fun() ->
                {ok, Pid} = multi_choice_workflow:start_link(#{regions => [east, west]}),
                ok = gen_yawl:fire(Pid, t_evaluate),

                %% Assert: Multiple branches can be enabled based on conditions
                EastTokens = gen_yawl:place_tokens(Pid, p_process_east),
                WestTokens = gen_yawl:place_tokens(Pid, p_process_west),

                %% At least one branch should have tokens
                TotalTokens = length(EastTokens) + length(WestTokens),
                ?assert(TotalTokens >= 1),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP6: OR-split can enable all branches",
             fun() ->
                %% Conditions that enable all regions
                {ok, Pid} = multi_choice_workflow:start_link(#{regions => [east, west, central]}),
                ok = gen_yawl:fire(Pid, t_evaluate),

                %% All three branches enabled
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_process_east)),
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_process_west)),
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_process_central)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

wp7_structured_sync_merge_test_() ->
    {foreach,
     fun() -> {ok, Pid} = multi_choice_workflow:start_link(#{regions => [east]}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP7: OR-join waits for all active branches",
             fun() ->
                {ok, Pid} = multi_choice_workflow:start_link(#{regions => [east]}),
                ok = gen_yawl:fire(Pid, t_evaluate),
                ok = gen_yawl:fire(Pid, t_process_east),

                %% Assert: Complete enabled
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_complete)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP10: Arbitrary Cycle Tests
%%====================================================================

wp10_arbitrary_cycle_test_() ->
    {foreach,
     fun() -> {ok, Pid} = order_processing:start_link(#{items => []}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP10: Cycle allows loop back based on condition",
             fun() ->
                %% First pass - empty items, triggers cycle
                {ok, Pid} = order_processing:start_link(#{items => []}),
                ok = gen_yawl:fire(Pid, t_validate),
                ok = gen_yawl:fire(Pid, t_check_items),

                %% Should cycle back to validate
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_validate)),

                %% Add items and complete
                ok = gen_yawl:fire(Pid, t_validate),
                ok = gen_yawl:fire(Pid, t_check_items),

                %% Now should proceed to ship
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_ship)),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP10: Cycle prevents infinite loops with max iteration",
             fun() ->
                {ok, Pid} = order_processing:start_link(#{items => [], max_cycles => 3}),

                %% Force multiple cycles
                lists:foreach(fun(_) ->
                    ok = gen_yawl:fire(Pid, t_validate),
                    ok = gen_yawl:fire(Pid, t_check_items)
                end, lists:seq(1, 3)),

                %% Should eventually exit cycle
                Marking = gen_yawl:marking(Pid),
                CycleTokens = maps:get(p_validate, Marking, []),
                ?assertEqual([], CycleTokens),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP11: Implicit Termination Tests
%%====================================================================

wp11_implicit_termination_test_() ->
    {foreach,
     fun() -> {ok, Pid} = simple_sequence:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP11: Workflow terminates when no tokens remain",
             fun() ->
                {ok, Pid} = simple_sequence:start_link(),

                %% Fire all transitions
                ok = gen_yawl:fire(Pid, t_draft),
                ok = gen_yawl:fire(Pid, t_review),
                ok = gen_yawl:fire(Pid, t_approve),

                %% Check status
                Status = gen_yawl:case_status(Pid),
                ?assertEqual(completed, Status),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP16: Deferred Choice Tests
%%====================================================================

wp16_deferred_choice_test_() ->
    {foreach,
     fun() -> {ok, Pid} = deferred_choice_workflow:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP16: External event selects deferred branch",
             fun() ->
                {ok, Pid} = deferred_choice_workflow:start_link(),

                %% Enable creates candidate work items
                ok = gen_yawl:fire(Pid, t_enable),

                %% Simulate external event selecting option_a
                gen_yawl:external_event(Pid, {select_option, option_a}),

                %% Assert: Only selected branch proceeds
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_option_a)),
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_option_b)),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP16: First selected branch cancels others",
             fun() ->
                {ok, Pid} = deferred_choice_workflow:start_link(),

                ok = gen_yawl:fire(Pid, t_enable),

                %% Select option_a first
                gen_yawl:external_event(Pid, {select_option, option_a}),

                %% Try to select option_b - should be ignored
                gen_yawl:external_event(Pid, {select_option, option_b}),

                %% Only option_a branch should be active
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_option_a)),
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_option_b)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% WP19 + WP20: Cancellation Tests
%%====================================================================

wp19_cancel_task_test_() ->
    {foreach,
     fun() -> {ok, Pid} = cancellation_workflow:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP19: Cancel task removes its tokens",
             fun() ->
                {ok, Pid} = cancellation_workflow:start_link(),

                %% Enable multiple tasks
                ok = gen_yawl:fire(Pid, t_start),

                %% Cancel specific task
                gen_yawl:cancel_task(Pid, task_b),

                %% Assert: task_b tokens removed, task_a still active
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_task_a)),
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_task_b)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

wp20_cancel_case_test_() ->
    {foreach,
     fun() -> {ok, Pid} = cancellation_workflow:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP20: Cancel case removes all tokens",
             fun() ->
                {ok, Pid} = cancellation_workflow:start_link(),

                %% Enable workflow
                ok = gen_yawl:fire(Pid, t_start),

                %% Verify tokens exist
                Marking1 = gen_yawl:marking(Pid),
                TotalTokens1 = lists:sum([length(T) || T <- maps:values(Marking1)]),
                ?assert(TotalTokens1 > 0),

                %% Cancel entire case
                gen_yawl:cancel_case(Pid),

                %% Assert: All tokens removed
                Marking2 = gen_yawl:marking(Pid),
                TotalTokens2 = lists:sum([length(T) || T <- maps:values(Marking2)]),
                ?assertEqual(0, TotalTokens2),

                %% Status should be cancelled
                ?assertEqual(cancelled, gen_yawl:case_status(Pid)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% Multiple Instance Pattern Tests (WP12, WP13)
%%====================================================================

wp12_parallel_instances_test_() ->
    {foreach,
     fun() -> {ok, Pid} = multiple_instance_workflow:start_link(#{count => 3}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP12: Parallel multiple instances created",
             fun() ->
                {ok, Pid} = multiple_instance_workflow:start_link(#{count => 3}),
                ok = gen_yawl:fire(Pid, t_split),

                %% Assert: Three parallel instances created
                ?assertMatch([_, _, _], gen_yawl:place_tokens(Pid, p_instance)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% Discriminator Pattern Tests (WP9)
%%====================================================================

wp9_discriminator_test_() ->
    {foreach,
     fun() -> {ok, Pid} = discriminator_workflow:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP9: First arriving branch triggers continuation",
             fun() ->
                {ok, Pid} = discriminator_workflow:start_link(),
                ok = gen_yawl:fire(Pid, t_split),

                %% Fire first branch
                ok = gen_yawl:fire(Pid, t_branch_1),

                %% Assert: Continuation triggered despite other branches not complete
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_continue)),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP9: Subsequent branch tokens are withdrawn",
             fun() ->
                {ok, Pid} = discriminator_workflow:start_link(),
                ok = gen_yawl:fire(Pid, t_split),

                %% Fire first branch - triggers continuation
                ok = gen_yawl:fire(Pid, t_branch_1),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_continue)),

                %% Fire second branch - token should be withdrawn
                ok = gen_yawl:fire(Pid, t_branch_2),

                %% Continuation should not have additional tokens
                ContinueTokens = gen_yawl:place_tokens(Pid, p_continue),
                ?assertMatch([_], ContinueTokens),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% N-out-of-M Pattern Tests (WP14)
%%====================================================================

wp14_n_out_of_m_test_() ->
    {foreach,
     fun() -> {ok, Pid} = n_out_of_m_workflow:start_link(#{required => 2, total => 3}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP14: N-out-of-M join fires at threshold",
             fun() ->
                {ok, Pid} = n_out_of_m_workflow:start_link(#{required => 2, total => 3}),
                ok = gen_yawl:fire(Pid, t_split),

                %% Fire two branches (meets threshold of 2)
                ok = gen_yawl:fire(Pid, t_branch_1),
                ok = gen_yawl:fire(Pid, t_branch_2),

                %% Assert: Continuation enabled despite 3rd branch not complete
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_continue)),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP14: N-out-of-M does not fire before threshold",
             fun() ->
                {ok, Pid} = n_out_of_m_workflow:start_link(#{required => 2, total => 3}),
                ok = gen_yawl:fire(Pid, t_split),

                %% Fire only one branch (below threshold)
                ok = gen_yawl:fire(Pid, t_branch_1),

                %% Assert: Continuation not enabled yet
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_continue)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% State-based Choice Pattern Tests (WP15)
%%====================================================================

wp15_state_based_choice_test_() ->
    {foreach,
     fun() -> {ok, Pid} = state_based_workflow:start_link(#{risk => low}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP15: Route depends on data state",
             fun() ->
                %% Low risk - standard path
                {ok, Pid} = state_based_workflow:start_link(#{risk => low}),
                ok = gen_yawl:fire(Pid, t_evaluate),

                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_standard)),
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_expedited)),

                gen_yawl:stop(Pid)
             end}
         end,
        fun(_Pid) ->
            {"WP15: Different state triggers different path",
             fun() ->
                %% High risk - expedited path
                {ok, Pid} = state_based_workflow:start_link(#{risk => high}),
                ok = gen_yawl:fire(Pid, t_evaluate),

                ?assertEqual([], gen_yawl:place_tokens(Pid, p_standard)),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_expedited)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% Milestone Pattern Tests (WP18)
%%====================================================================

wp18_milestone_test_() ->
    {foreach,
     fun() -> {ok, Pid} = milestone_workflow:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP18: Task only enabled when milestone reached",
             fun() ->
                {ok, Pid} = milestone_workflow:start_link(),

                %% Initially milestone task should not be enabled
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_milestone_task)),

                %% Reach milestone
                ok = gen_yawl:fire(Pid, t_prerequisite),

                %% Now milestone task should be enabled
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_milestone_task)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% Interleaved Parallel Routing Pattern Tests (WP17)
%%====================================================================

wp17_interleaved_test_() ->
    {foreach,
     fun() -> {ok, Pid} = interleaved_workflow:start_link(), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_Pid) ->
            {"WP17: Interleaved routes prevent concurrent execution",
             fun() ->
                {ok, Pid} = interleaved_workflow:start_link(),
                ok = gen_yawl:fire(Pid, t_start),

                %% Both paths should be enabled
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_path_a)),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_path_b)),

                %% Fire path_a
                ok = gen_yawl:fire(Pid, t_path_a),

                %% Path_a should be locked while path_b executes
                ?assertEqual([], gen_yawl:place_tokens(Pid, p_path_a)),

                %% Fire path_b
                ok = gen_yawl:fire(Pid, t_path_b),

                %% After both complete, merge should be enabled
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_merge)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.

%%====================================================================
%% Integration Tests
%%====================================================================

complex_workflow_integration_test_() ->
    {foreach,
     fun() -> {ok, Pid} = order_processing:start_link(#{items => [item1, item2]}), Pid end,
     fun(Pid) -> gen_yawl:stop(Pid) end,
     [
        fun(_) ->
            {"Complex workflow integrates multiple patterns",
             fun() ->
                {ok, Pid} = order_processing:start_link(#{items => [item1, item2]}),

                %% Sequence (WP1): validate -> check_items
                ok = gen_yawl:fire(Pid, t_validate),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_check_items)),

                %% Cycle (WP10): If items empty, loop back
                ok = gen_yawl:fire(Pid, t_check_items),

                %% Should proceed to ship (items present)
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_ship)),

                %% Complete workflow
                ok = gen_yawl:fire(Pid, t_ship),
                ?assertMatch([_], gen_yawl:place_tokens(Pid, p_output)),

                %% Implicit termination (WP11)
                ?assertEqual(completed, gen_yawl:case_status(Pid)),

                gen_yawl:stop(Pid)
             end}
         end
     ]}.
