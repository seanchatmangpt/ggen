%%%-------------------------------------------------------------------
%% @doc gcp_failure_wrapper: Wrapper for GCP services with failure injection
%%
%% Provides failure injection hooks for GCP service calls.
%% Integrates with gcp_failure_injector to simulate production failures.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_failure_wrapper).

%% API
-export([check_firestore_failure/1, check_pubsub_failure/1, check_metadata_failure/1, check_action_failure/1]).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Check if Firestore operation should fail
-spec check_firestore_failure(Operation) -> {should_fail, Type, Reason} | should_succeed
    when Operation :: atom(),
         Type :: atom(),
         Reason :: term().
check_firestore_failure(Operation) ->
    case whereis(gcp_failure_injector) of
        undefined ->
            should_succeed;
        _Pid ->
            gcp_failure_injector:check_failure(firestore, Operation)
    end.

%% @doc Check if Pub/Sub operation should fail
-spec check_pubsub_failure(Operation) -> {should_fail, Type, Reason} | should_succeed
    when Operation :: atom(),
         Type :: atom(),
         Reason :: term().
check_pubsub_failure(Operation) ->
    case whereis(gcp_failure_injector) of
        undefined ->
            should_succeed;
        _Pid ->
            gcp_failure_injector:check_failure(pubsub, Operation)
    end.

%% @doc Check if metadata operation should fail
-spec check_metadata_failure(Operation) -> {should_fail, Type, Reason} | should_succeed
    when Operation :: atom(),
         Type :: atom(),
         Reason :: term().
check_metadata_failure(Operation) ->
    case whereis(gcp_failure_injector) of
        undefined ->
            should_succeed;
        _Pid ->
            gcp_failure_injector:check_failure(metadata, Operation)
    end.

%% @doc Check if action execution should fail
-spec check_action_failure(Operation) -> {should_fail, Type, Reason} | should_succeed
    when Operation :: atom(),
         Type :: atom(),
         Reason :: term().
check_action_failure(Operation) ->
    case whereis(gcp_failure_injector) of
        undefined ->
            should_succeed;
        _Pid ->
            gcp_failure_injector:check_failure(actions, Operation)
    end.
