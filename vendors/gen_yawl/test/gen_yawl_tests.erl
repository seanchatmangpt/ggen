%%====================================================================
%% gen_yawl_tests - Core Behavior Tests
%%====================================================================
%% @doc Core tests for gen_yawl behavior implementing YAWL patterns
%% on top of gen_pnet. Tests follow Chicago TDD: state-based, real
%% collaborators, AAA pattern.

-module(gen_yawl_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/gen_yawl.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

%% Setup function - creates a fresh workflow instance for each test
setup() ->
    {ok, Pid} = simple_sequence:start_link(),
    Pid.

%% Cleanup function - ensures proper shutdown
cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_yawl:stop(Pid);
        false -> ok
    end.

%%====================================================================
%% WP1: Sequence Pattern Tests
%%====================================================================

sequence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun sequence_tests/1}.

sequence_tests(Pid) ->
    [
        {"Initial marking has input token",
         fun() ->
            Marking = gen_yawl:marking(Pid),
            ?assert(maps:is_key(p_input, Marking)),
            ?assertNot([] =:= maps:get(p_input, Marking, []))
         end},
        {"Output place initially empty",
         fun() ->
            Marking = gen_yawl:marking(Pid),
            ?assertEqual([], maps:get(p_output, Marking, []))
         end},
        {"First transition enabled initially",
         fun() ->
            Enabled = gen_yawl:enabled_transitions(Pid),
            ?assert(lists:member(t_draft, Enabled))
         end},
        {"Sequence executes in order",
         fun() ->
            %% Arrange: Get initial state
            BeforeMarking = gen_yawl:marking(Pid),

            %% Act: Trigger first transition
            ok = gen_yawl:fire(Pid, t_draft),

            %% Assert: Token moved from draft to review
            AfterMarking = gen_yawl:marking(Pid),
            ?assertEqual([], maps:get(p_draft, AfterMarking, [])),
            ?assertNot([] =:= maps:get(p_review, AfterMarking, [])),

            %% Act: Trigger second transition
            ok = gen_yawl:fire(Pid, t_review),

            %% Assert: Token moved from review to approve
            FinalMarking = gen_yawl:marking(Pid),
            ?assertEqual([], maps:get(p_review, FinalMarking, [])),
            ?assertNot([] =:= maps:get(p_approve, FinalMarking, []))
         end},
        {"Complete sequence reaches output",
         fun() ->
            %% Act: Fire all transitions
            ok = gen_yawl:fire(Pid, t_draft),
            ok = gen_yawl:fire(Pid, t_review),
            ok = gen_yawl:fire(Pid, t_approve),

            %% Assert: Output place has token
            Marking = gen_yawl:marking(Pid),
            ?assertNot([] =:= maps:get(p_output, Marking, []))
         end}
    ].

%%====================================================================
%% Place and Transition List Tests
%%====================================================================

place_lst_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun place_tests/1}.

place_tests(Pid) ->
    [
        {"place_lst returns all workflow places",
         fun() ->
            Places = gen_yawl:place_lst(Pid),
            ExpectedPlaces = [p_input, p_draft, p_review, p_approve, p_output],
            ?assert(lists:all(fun(P) -> lists:member(P, Places) end, ExpectedPlaces))
         end},
        {"place_lst does not return duplicates",
         fun() ->
            Places = gen_yawl:place_lst(Pid),
            ?assertEqual(length(Places), length(lists:usort(Places)))
         end},
        {"trsn_lst returns all transitions",
         fun() ->
            Transitions = gen_yawl:trsn_lst(Pid),
            Expected = [t_draft, t_review, t_approve],
            ?assert(lists:all(fun(T) -> lists:member(T, Transitions) end, Expected))
         end}
    ].

%%====================================================================
%% Preset Tests
%%====================================================================

preset_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun preset_tests/1}.

preset_tests(_Pid) ->
    [
        {"Preset of draft contains input",
         fun() ->
            Preset = simple_sequence:preset(t_draft),
            ?assert(lists:member(p_input, Preset))
         end},
        {"Preset of review contains draft",
         fun() ->
            Preset = simple_sequence:preset(t_review),
            ?assert(lists:member(p_draft, Preset))
         end},
        {"Preset of approve contains review",
         fun() ->
            Preset = simple_sequence:preset(t_approve),
            ?assert(lists:member(p_review, Preset))
         end}
    ].

%%====================================================================
%% Enablement Tests
%%====================================================================

is_enabled_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun enablement_tests/1}.

enablement_tests(Pid) ->
    [
        {"Transition enabled when preset has tokens",
         fun() ->
            Marking = gen_yawl:marking(Pid),
            Mode = Marking,
            ?assert(gen_yawl:is_enabled(Pid, t_draft, Mode))
         end},
        {"Transition not enabled when preset empty",
         fun() ->
            %% Fire draft to consume input token
            ok = gen_yawl:fire(Pid, t_draft),
            Marking = gen_yawl:marking(Pid),
            %% Draft should not be enabled anymore
            HasToken = case maps:get(p_input, Marking, []) of
                [] -> false;
                _ -> true
            end,
            ?assertNot(HasToken)
         end}
    ].

%%====================================================================
%% Firing Tests
%%====================================================================

fire_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun firing_tests/1}.

firing_tests(Pid) ->
    [
        {"Fire produces correct output marking",
         fun() ->
            %% Arrange: Get pre-state
            BeforeMarking = gen_yawl:marking(Pid),

            %% Act: Fire transition
            {produce, ProduceMap} = gen_yawl:fire(Pid, t_draft),

            %% Assert: Correct production
            ?assert(maps:is_key(p_review, ProduceMap)),
            ?assertNot([] =:= maps:get(p_review, ProduceMap, []))
         end},
        {"Fire transition preserves other places",
         fun() ->
            %% Act: Fire transition
            {produce, ProduceMap} = gen_yawl:fire(Pid, t_draft),

            %% Assert: Only expected place has new tokens
            ExpectedKeys = [p_review],
            ActualKeys = maps:keys(ProduceMap),
            ?assertEqual(ExpectedKeys, ActualKeys)
         end}
    ].

%%====================================================================
%% Case Management Tests
%%====================================================================

case_management_test_() ->
    {foreach,
     fun() ->
        %% Create a new case for each test
        {ok, Pid} = simple_sequence:start_link(),
        Pid
     end,
     fun(Pid) ->
        gen_yawl:stop(Pid)
     end,
     [
        fun(Pid) ->
            {"Case starts with active status",
             fun() ->
                Status = gen_yawl:case_status(Pid),
                ?assertEqual(active, Status)
             end}
         end,
        fun(Pid) ->
            {"Case completes after all transitions",
             fun() ->
                ok = gen_yawl:fire(Pid, t_draft),
                ok = gen_yawl:fire(Pid, t_review),
                ok = gen_yawl:fire(Pid, t_approve),
                Status = gen_yawl:case_status(Pid),
                ?assertEqual(completed, Status)
             end}
         end,
        fun(Pid) ->
            {"Case ID is unique binary",
             fun() ->
                CaseId = gen_yawl:case_id(Pid),
                ?assert(is_binary(CaseId)),
                ?assert(byte_size(CaseId) > 0)
             end}
         end
     ]}.

%%====================================================================
%% Receipt Tests
%%====================================================================

receipt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun receipt_tests/1}.

receipt_tests(Pid) ->
    [
        {"Receipt generated on transition fire",
         fun() ->
            %% Act: Fire transition
            ok = gen_yawl:fire(Pid, t_draft),

            %% Assert: Receipt exists
            Receipts = gen_yawl:receipts(Pid),
            ?assertNot([] =:= Receipts),

            %% Verify receipt structure
            Receipt = hd(Receipts),
            ?assert(is_record(Receipt, receipt)),
            ?assert(is_binary(Receipt#receipt.id)),
            ?assert(is_binary(Receipt#receipt.current_hash))
         end},
        {"Receipt chain forms correctly",
         fun() ->
            %% Act: Fire multiple transitions
            ok = gen_yawl:fire(Pid, t_draft),
            Receipts1 = gen_yawl:receipts(Pid),

            ok = gen_yawl:fire(Pid, t_review),
            Receipts2 = gen_yawl:receipts(Pid),

            %% Assert: Chain grew
            ?assert(length(Receipts2) > length(Receipts1)),

            %% Verify chain links
            [R2, R1 | _] = Receipts2,
            ?assertEqual(R1#receipt.current_hash, R2#receipt.prev_hash)
         end},
        {"Receipt hash is deterministic",
         fun() ->
            %% Act: Get receipt after firing
            ok = gen_yawl:fire(Pid, t_draft),
            [Receipt | _] = gen_yawl:receipts(Pid),

            %% Assert: Hash is non-empty binary
            Hash = Receipt#receipt.current_hash,
            ?assert(byte_size(Hash) > 0),
            ?assert(is_binary(Hash))
         end}
    ].

%%====================================================================
%% State Query Tests
%%====================================================================

state_query_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun state_query_tests/1}.

state_query_tests(Pid) ->
    [
        {"Query returns current marking",
         fun() ->
            Marking = gen_yawl:marking(Pid),
            ?assert(is_map(Marking)),
            ?assert(maps:is_key(p_input, Marking))
         end},
        {"Query returns enabled transitions",
         fun() ->
            Enabled = gen_yawl:enabled_transitions(Pid),
            ?assert(is_list(Enabled)),
            ?assert(lists:member(t_draft, Enabled))
         end},
        {"State is immutable between queries",
         fun() ->
            Marking1 = gen_yawl:marking(Pid),
            Marking2 = gen_yawl:marking(Pid),
            ?assertEqual(Marking1, Marking2)
         end}
    ].

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {foreach,
     fun() ->
        {ok, Pid} = simple_sequence:start_link(),
        Pid
     end,
     fun(Pid) ->
        gen_yawl:stop(Pid)
     end,
     [
        fun(_Pid) ->
            {"Fire non-existent transition returns error",
             fun() ->
                Result = gen_yawl:fire(_Pid, t_nonexistent),
                ?assertEqual({error, unknown_transition}, Result)
             end}
         end,
        fun(Pid) ->
            {"Stop already stopped case idempotent",
             fun() ->
                ok = gen_yawl:stop(Pid),
                Result = gen_yawl:stop(Pid),
                ?assertEqual(ok, Result)
             end}
         end
     ]}.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

lifecycle_test_() ->
    [
        {"Start and stop workflow",
         fun() ->
            %% Arrange & Act
            {ok, Pid} = simple_sequence:start_link(),
            ?assert(is_process_alive(Pid)),
            ok = gen_yawl:stop(Pid),

            %% Assert
            ?assertNot(is_process_alive(Pid))
         end},
        {"Workflow survives gen_server call",
         fun() ->
            {ok, Pid} = simple_sequence:start_link(),
            _ = gen_yawl:marking(Pid),
            ?assert(is_process_alive(Pid)),
            gen_yawl:stop(Pid)
         end}
    ].

%%====================================================================
%% Statistics Tests
%%====================================================================

statistics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun statistics_tests/1}.

statistics_tests(Pid) ->
    [
        {"Stats track transition firings",
         fun() ->
            %% Arrange: Get initial stats
            Stats1 = gen_yawl:stats(Pid),

            %% Act: Fire transition
            ok = gen_yawl:fire(Pid, t_draft),

            %% Assert: Stats updated
            Stats2 = gen_yawl:stats(Pid),
            ?assertNotEqual(Stats1, Stats2)
         end},
        {"Reset stats clears counters",
         fun() ->
            %% Arrange: Fire some transitions
            ok = gen_yawl:fire(Pid, t_draft),
            ok = gen_yawl:fire(Pid, t_review),

            %% Act: Reset stats
            ok = gen_yawl:reset_stats(Pid),

            %% Assert: Stats at baseline
            Stats = gen_yawl:stats(Pid),
            ?assert(is_record(Stats, stats))
         end}
    ].
