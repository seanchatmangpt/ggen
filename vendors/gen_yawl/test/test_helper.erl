%%====================================================================
%% test_helper - Test Utilities for gen_yawl
%%====================================================================
%% @doc Helper functions and common setup for gen_yawl tests.

-module(test_helper).
-include_lib("eunit/include/eunit.hrl").
-include("include/gen_yawl.hrl").

-export([
    start_workflow/2,
    stop_workflow/1,
    fire_all/2,
    wait_for_completion/1,
    wait_for_place/3,
    get_all_tokens/1,
    assert_place_has/3,
    assert_tokens_count/3
]).

%%====================================================================
%% Workflow Lifecycle Helpers
%%====================================================================

%% @doc Start a workflow with given module and initial data
start_workflow(Module, InitialData) ->
    case Module:start_link(InitialData) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Error -> Error
    end.

%% @doc Stop a workflow case
stop_workflow(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_pnet:stop(Pid);
        false -> ok
    end;
stop_workflow(_Name) ->
    ok.

%%====================================================================
%% Workflow Execution Helpers
%%====================================================================

%% @doc Fire all transitions in sequence
fire_all(Pid, Transitions) when is_list(Transitions) ->
    lists:foreach(fun(T) ->
        case gen_pnet:call(Pid, {fire, T}) of
            ok -> ok;
            {error, not_enabled} -> ok  %% Skip if not enabled
        end
    end, Transitions).

%% @doc Wait for workflow to complete (status = completed)
wait_for_completion(Pid) ->
    wait_for_condition(
        fun() -> gen_pnet:call(Pid, case_status) end,
        completed,
        5000
    ).

%% @doc Wait for a place to have a specific number of tokens
wait_for_place(Pid, Place, Count) ->
    wait_for_condition(
        fun() ->
            case gen_pnet:ls(Pid, Place) of
                {ok, Tokens} -> length(Tokens);
                _ -> 0
            end
        end,
        Count,
        5000
    ).

%% @private Wait for a condition to be met
wait_for_condition(Fun, Expected, Timeout) ->
    wait_for_condition(Fun, Expected, Timeout, 100).

wait_for_condition(_Fun, _Expected, Timeout, _Interval) when Timeout =< 0 ->
    {error, timeout};
wait_for_condition(Fun, Expected, Timeout, Interval) ->
    case Fun() of
        Expected -> ok;
        _ ->
            timer:sleep(Interval),
            wait_for_condition(Fun, Expected, Timeout - Interval, Interval)
    end.

%%====================================================================
%% Token Inspection Helpers
%%====================================================================

%% @doc Get all tokens from a workflow
get_all_tokens(Pid) ->
    Marking = gen_pnet:marking(Pid),
    maps:fold(fun(Place, Tokens, Acc) ->
        Acc#{Place => length(Tokens)}
    end, #{}, Marking).

%% @doc Assert that a place has tokens matching a predicate
assert_place_has(Pid, Place, Predicate) when is_function(Predicate, 1) ->
    case gen_pnet:ls(Pid, Place) of
        {ok, Tokens} ->
            case lists:any(Predicate, Tokens) of
                true -> ok;
                false -> erlang:error({assert_failed, {place_has_no_match, Place, Tokens}})
            end;
        _ ->
            erlang:error({assert_failed, {place_not_found, Place}})
    end.

%% @doc Assert that a place has exactly N tokens
assert_tokens_count(Pid, Place, ExpectedCount) ->
    case gen_pnet:ls(Pid, Place) of
        {ok, Tokens} ->
            ActualCount = length(Tokens),
            case ActualCount of
                ExpectedCount -> ok;
                _ -> erlang:error({assert_failed, {token_count_mismatch, Place, ExpectedCount, ActualCount}})
            end;
        _ ->
            erlang:error({assert_failed, {place_not_found, Place}})
    end.

%%====================================================================
%% Receipt Verification Helpers
%%====================================================================

%% @doc Verify receipt chain is valid
verify_receipt_chain([]) ->
    true;
verify_receipt_chain([_]) ->
    true;
verify_receipt_chain([R1, R2 | Rest]) ->
    case R1#receipt.current_hash =:= R2#receipt.prev_hash of
        true -> verify_receipt_chain([R2 | Rest]);
        false -> false
    end.

%% @doc Get receipts for a specific event type
get_receipts_by_type(Pid, EventType) ->
    Receipts = gen_pnet:call(Pid, receipts),
    lists:filter(fun(R) -> R#receipt.event_type =:= EventType end, Receipts).

%%====================================================================
%% Pattern Testing Helpers
%%====================================================================

%% @doc Test WP1 Sequence pattern completion
assert_sequence_completed(Pid) ->
    ?assertEqual(completed, gen_pnet:call(Pid, case_status)),
    ?assertEqual([], element(2, gen_pnet:ls(Pid, p_input))),
    ?assertNotEqual([], element(2, gen_pnet:ls(Pid, p_output))).

%% @doc Test WP2+WP3 Parallel split/join completed
assert_parallel_join_completed(Pid, BranchCount) ->
    %% After join, all branches should be empty
    ?assertNotEqual([], element(2, gen_pnet:ls(Pid, p_finalize))).

%% @doc Test WP4 XOR split selected correct branch
assert_xor_branch_selected(Pid, ExpectedPlace) ->
    ExpectedTokens = element(2, gen_pnet:ls(Pid, ExpectedPlace)),
    ?assertNotEqual([], ExpectedTokens),
    %% Other branches should be empty
    OtherPlaces = lists:delete(ExpectedPlace, [p_auto_approve, p_manual_review, p_reject]),
    lists:foreach(fun(P) ->
        ?assertEqual([], element(2, gen_pnet:ls(Pid, P)))
    end, OtherPlaces).

%%====================================================================
%% Mock Data Generators
%%====================================================================

%% @doc Generate mock case data
mock_case_data() ->
    #{
        amount => rand:uniform(10000),
        items => [item1, item2, item3],
        user_id => <<"user-123">>,
        timestamp => os:system_time(millisecond)
    }.

%% @doc Generate mock work item
mock_work_item(TaskId) ->
    #work_item{
        id => generate_id(),
        case_id => generate_case_id(),
        task_id = TaskId,
        status = enabled,
        data = mock_case_data(),
        enabled_at = os:system_time(nanosecond)
    }.

%% @doc Generate mock receipt
mock_receipt(EventType, CaseId) ->
    #receipt{
        id = generate_id(),
        prev_hash = <<>>,
        current_hash = crypto:hash(blake3, <<EventType>>),
        timestamp = os:system_time(nanosecond),
        event_type = EventType,
        case_id = CaseId,
        justification = #{}
    }.

%%====================================================================
%% ID Generation
%%====================================================================

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
