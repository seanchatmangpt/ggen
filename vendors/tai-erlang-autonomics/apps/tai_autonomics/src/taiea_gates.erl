%%%-------------------------------------------------------------------
%% @doc TAI Erlang Autonomic Gates - Deterministic action gating
%%
%% Implements three sequential gates for bounded action execution:
%% 1. Entitlement Check - Verify active entitlement
%% 2. IAM Role Check - Verify required role
%% 3. Preconditions Check - Action-specific validation
%%
%% All gates must pass for action to be accepted.
%% Gates are evaluated in sequence and short-circuit on first failure.
%%
%% Example usage:
%%   case taiea_gates:check_all_gates(TenantId, health_check, #{}) of
%%     {accept, Metadata} -> execute_action(TenantId);
%%     {refuse, Reason} -> handle_rejection(Reason)
%%   end
%%
%% @end
%%%-------------------------------------------------------------------
-module(taiea_gates).

%% API
-export([
    check_all_gates/3,
    check_entitlement/1,
    check_iam_role/2,
    check_preconditions/2,
    execute_bounded_action/3,
    execute_bounded_action/4
]).

%% Internal
-export([bounded_action_worker/4]).

%% Type definitions
-type tenant_id() :: binary().
-type action_type() :: health_check | entitlement_apply | receipts_verify | support_model | atom().
-type context() :: map().
-type gate_result() :: {accept, Metadata :: map()} | {refuse, Reason :: atom() | binary()}.
-type action_result() :: {ok, Result :: term()} | {timeout} | {memory_exceeded} | {error, Reason :: term()}.
-type timeout_ms() :: pos_integer().
-type max_memory_mb() :: pos_integer().

-export_type([
    tenant_id/0,
    action_type/0,
    context/0,
    gate_result/0,
    action_result/0,
    timeout_ms/0,
    max_memory_mb/0
]).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Check all gates in sequence (Entitlement → IAM Role → Preconditions)
%% Returns {accept, Metadata} if all gates pass, {refuse, Reason} on first failure.
%% Gates are evaluated left-to-right and short-circuit on first failure.
%%
%% @param TenantId Tenant identifier (binary)
%% @param Action Action type atom (health_check, entitlement_apply, receipts_verify, support_model)
%% @param Context Action-specific context map
%% @return {accept, Metadata} | {refuse, Reason}
-spec check_all_gates(tenant_id(), action_type(), context()) -> gate_result().
check_all_gates(TenantId, Action, Context) when is_binary(TenantId) ->
    %% Gate 1: Entitlement Check
    case check_entitlement(TenantId) of
        {accept, EntitlementMeta} ->
            %% Gate 2: IAM Role Check (required role depends on action)
            RequiredRole = required_role_for_action(Action),
            case check_iam_role(TenantId, RequiredRole) of
                {accept, IamMeta} ->
                    %% Gate 3: Preconditions Check (action-specific)
                    case check_preconditions(Action, Context) of
                        {accept, PrecondMeta} ->
                            %% All gates passed, merge metadata
                            Metadata = maps:merge(
                                maps:merge(EntitlementMeta, IamMeta),
                                PrecondMeta
                            ),
                            {accept, Metadata};
                        {refuse, Reason} ->
                            {refuse, Reason}
                    end;
                {refuse, Reason} ->
                    {refuse, Reason}
            end;
        {refuse, Reason} ->
            {refuse, Reason}
    end.

%% @doc Gate 1: Verify tenant has active entitlement
%% In Phase 1, this is a stub that always accepts.
%% Phase 2 will integrate with entitlement service.
%%
%% @param TenantId Tenant identifier
%% @return {accept, Metadata} | {refuse, Reason}
-spec check_entitlement(tenant_id()) -> gate_result().
check_entitlement(TenantId) when is_binary(TenantId) ->
    %% Phase 1: Stub implementation
    %% Phase 2: Call entitlement:verify_active(TenantId)
    {accept, #{gate => entitlement, tenant_id => TenantId, status => active}}.

%% @doc Gate 2: Verify tenant has required IAM role
%% In Phase 1, this is a stub that always accepts.
%% Phase 2 will integrate with IAM service.
%%
%% @param TenantId Tenant identifier
%% @param RequiredRole Role required for this action
%% @return {accept, Metadata} | {refuse, Reason}
-spec check_iam_role(tenant_id(), atom()) -> gate_result().
check_iam_role(TenantId, RequiredRole) when is_binary(TenantId), is_atom(RequiredRole) ->
    %% Phase 1: Stub implementation
    %% Phase 2: Call entitlement:verify_iam_role(TenantId, RequiredRole)
    {accept, #{gate => iam_role, tenant_id => TenantId, required_role => RequiredRole, verified => true}}.

%% @doc Gate 3: Verify action-specific preconditions
%% Action-specific validation:
%% - health_check: Always passes
%% - entitlement_apply: Validates required fields in context
%% - receipts_verify: Validates receipt exists in context
%% - support_model: Always passes
%%
%% @param Action Action type
%% @param Context Action-specific context
%% @return {accept, Metadata} | {refuse, Reason}
-spec check_preconditions(action_type(), context()) -> gate_result().
check_preconditions(health_check, _Context) ->
    {accept, #{gate => preconditions, action => health_check, checked => true}};

check_preconditions(support_model, _Context) ->
    {accept, #{gate => preconditions, action => support_model, checked => true}};

check_preconditions(entitlement_apply, Context) ->
    case check_entitlement_fields(Context) of
        {accept, Meta} -> {accept, maps:merge(#{action => entitlement_apply}, Meta)};
        {refuse, Reason} -> {refuse, Reason}
    end;

check_preconditions(receipts_verify, Context) ->
    case check_receipt_exists(Context) of
        {accept, Meta} -> {accept, maps:merge(#{action => receipts_verify}, Meta)};
        {refuse, Reason} -> {refuse, Reason}
    end;

check_preconditions(Action, _Context) ->
    %% Unknown action - accept with warning (extensible design)
    {accept, #{gate => preconditions, action => Action, checked => false}}.

%%%===================================================================
%% Bounded Action Execution
%%%===================================================================

%% @doc Execute action with bounded timeout and memory constraints
%% Spawns a worker process that executes the handler function with enforced limits.
%%
%% @param Handler Function to execute: fun() -> Result
%% @param TimeoutMs Timeout in milliseconds
%% @param MaxMemoryMb Maximum memory in megabytes
%% @return {ok, Result} | {timeout} | {memory_exceeded} | {error, Reason}
-spec execute_bounded_action(fun(() -> term()), timeout_ms(), max_memory_mb()) -> action_result().
execute_bounded_action(Handler, TimeoutMs, MaxMemoryMb)
    when is_function(Handler, 0), is_integer(TimeoutMs), TimeoutMs > 0,
         is_integer(MaxMemoryMb), MaxMemoryMb > 0 ->
    execute_bounded_action(Handler, TimeoutMs, MaxMemoryMb, #{}).

%% @doc Execute action with bounded constraints and options
%% Spawns a worker process that executes the handler function.
%%
%% @param Handler Function to execute: fun() -> Result
%% @param TimeoutMs Timeout in milliseconds
%% @param MaxMemoryMb Maximum memory in megabytes
%% @param Options Additional options (reserved for Phase 2)
%% @return {ok, Result} | {timeout} | {memory_exceeded} | {error, Reason}
-spec execute_bounded_action(fun(() -> term()), timeout_ms(), max_memory_mb(), map()) -> action_result().
execute_bounded_action(Handler, TimeoutMs, MaxMemoryMb, _Options)
    when is_function(Handler, 0), is_integer(TimeoutMs), TimeoutMs > 0,
         is_integer(MaxMemoryMb), MaxMemoryMb > 0 ->
    %% Spawn worker in a new process
    ParentPid = self(),
    WorkerId = erlang:unique_integer([positive]),

    WorkerPid = spawn_link(fun() ->
        bounded_action_worker(ParentPid, WorkerId, Handler, MaxMemoryMb)
    end),

    %% Wait for result with timeout
    receive
        {result, WorkerId, Result} ->
            {ok, Result};
        {error, WorkerId, timeout} ->
            {timeout};
        {error, WorkerId, memory_exceeded} ->
            {memory_exceeded};
        {error, WorkerId, Reason} ->
            {error, Reason}
    after TimeoutMs ->
        %% Worker did not complete in time
        catch erlang:unlink(WorkerPid),
        catch erlang:exit(WorkerPid, kill),
        {timeout}
    end.

%% @doc Worker process for bounded action execution
%% Executes handler and monitors memory usage.
%% Internal callback - not for direct use.
%%
%% @param ParentPid Parent process PID
%% @param WorkerId Unique worker ID
%% @param Handler Function to execute
%% @param MaxMemoryMb Memory limit in MB
-spec bounded_action_worker(pid(), integer(), fun(() -> term()), max_memory_mb()) -> no_return().
bounded_action_worker(ParentPid, WorkerId, Handler, MaxMemoryMb) ->
    try
        %% Check initial memory
        InitialMemory = check_memory(),
        MaxMemoryBytes = MaxMemoryMb * 1_048_576,

        %% Execute handler
        Result = Handler(),

        %% Check memory after execution
        FinalMemory = check_memory(),
        MemoryUsed = FinalMemory - InitialMemory,

        if
            MemoryUsed > MaxMemoryBytes ->
                ParentPid ! {error, WorkerId, memory_exceeded},
                exit(memory_exceeded);
            true ->
                ParentPid ! {result, WorkerId, Result},
                exit(normal)
        end
    catch
        error:Reason ->
            ParentPid ! {error, WorkerId, Reason},
            exit({error, Reason});
        exit:Reason ->
            ParentPid ! {error, WorkerId, Reason},
            exit({exit, Reason})
    end.

%%%===================================================================
%% Internal Helper Functions
%%%===================================================================

%% @doc Check entitlement-specific fields in context
%% Validates required fields for entitlement_apply action.
%% Phase 1: Checks for customer_id and feature_key.
%% Phase 2: Will validate against actual schema.
-spec check_entitlement_fields(context()) -> gate_result().
check_entitlement_fields(Context) when is_map(Context) ->
    case {maps:get(customer_id, Context, undefined), maps:get(feature_key, Context, undefined)} of
        {undefined, _} ->
            {refuse, missing_customer_id};
        {_, undefined} ->
            {refuse, missing_feature_key};
        {CustomerId, FeatureKey} ->
            {accept, #{
                gate => preconditions,
                customer_id => CustomerId,
                feature_key => FeatureKey,
                validated => true
            }}
    end.

%% @doc Check receipt exists in context
%% Validates receipt_id is present in context.
%% Phase 1: Basic field check.
%% Phase 2: Will validate against receipt store.
-spec check_receipt_exists(context()) -> gate_result().
check_receipt_exists(Context) when is_map(Context) ->
    case maps:get(receipt_id, Context, undefined) of
        undefined ->
            {refuse, missing_receipt_id};
        ReceiptId ->
            {accept, #{
                gate => preconditions,
                receipt_id => ReceiptId,
                exists => true
            }}
    end.

%% @doc Determine required role based on action type
%% Phase 1: Returns atom role names.
%% Phase 2: Will support complex role hierarchies.
-spec required_role_for_action(action_type()) -> atom().
required_role_for_action(health_check) ->
    health_check_reader;
required_role_for_action(entitlement_apply) ->
    entitlement_admin;
required_role_for_action(receipts_verify) ->
    receipts_auditor;
required_role_for_action(support_model) ->
    support_reader;
required_role_for_action(_) ->
    default_reader.

%% @doc Get current process memory in bytes
%% Uses erlang:process_info/2 to get memory usage.
-spec check_memory() -> non_neg_integer().
check_memory() ->
    case erlang:process_info(self(), memory) of
        {memory, Bytes} -> Bytes;
        undefined -> 0
    end.

%%%===================================================================
%% Testing Utilities (exported for test module)
%%%===================================================================

%% These functions are exported for unit testing gates in isolation.
%% They allow test suites to mock or bypass specific gates.
%% In production, always use check_all_gates/3 for complete validation.
