%%%-------------------------------------------------------------------
%% @doc tai_actions: Bounded action executor with poolboy
%%
%% Manages worker pool for outbound actions with bounded concurrency,
%% timeouts, and backoff. Emits attempt and result receipts.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_actions).

%% API
-export([execute/3, execute_async/3]).

-include("tai_autonomics.hrl").

%%%===================================================================
%% API
%%%===================================================================

%% @doc Execute action synchronously
-spec execute(TenantId, ActionId, ActionSpec) -> {ok, Result} | {error, Reason}
  when TenantId :: binary(),
       ActionId :: binary(),
       ActionSpec :: map(),
       Result :: map(),
       Reason :: atom() | timeout.
execute(TenantId, ActionId, ActionSpec) ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_action_failure(execute) of
        {should_fail, Type, Reason} ->
            logger:warning("Action execution failure injected: ~p - ~p", [Type, Reason]),
            ErrorReceipt = create_action_receipt(result, TenantId, ActionId, #{error => Reason}),
            {error, Reason};
        should_succeed ->
            execute_impl(TenantId, ActionId, ActionSpec)
    end.

-spec execute_impl(TenantId, ActionId, ActionSpec) -> {ok, Result} | {error, Reason}
    when TenantId :: binary(),
         ActionId :: binary(),
         ActionSpec :: map(),
         Result :: map(),
         Reason :: atom() | timeout.
execute_impl(TenantId, ActionId, ActionSpec) ->
    AttemptReceipt = create_action_receipt(attempt, TenantId, ActionId, ActionSpec),
    case poolboy:transaction(action_pool, fun(Worker) ->
        gen_server:call(Worker, {execute, ActionSpec}, 30000)
    end) of
        {ok, Result} ->
            ResultReceipt = create_action_receipt(result, TenantId, ActionId, Result),
            {ok, Result};
        {error, timeout} ->
            ErrorReceipt = create_action_receipt(result, TenantId, ActionId, #{error => timeout}),
            {error, timeout};
        {error, Reason} ->
            ErrorReceipt = create_action_receipt(result, TenantId, ActionId, #{error => Reason}),
            {error, Reason}
    end.

%% @doc Execute action asynchronously
-spec execute_async(TenantId, ActionId, ActionSpec) -> {ok, Pid}
  when TenantId :: binary(),
       ActionId :: binary(),
       ActionSpec :: map(),
       Pid :: pid().
execute_async(TenantId, ActionId, ActionSpec) ->
    spawn(fun() ->
        execute(TenantId, ActionId, ActionSpec)
    end).

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec create_action_receipt(Type, TenantId, ActionId, Data) -> map()
  when Type :: attempt | result,
       TenantId :: binary(),
       ActionId :: binary(),
       Data :: map().
create_action_receipt(Type, TenantId, ActionId, Data) ->
    tai_receipts:create_action_receipt(Type, TenantId, ActionId, Data).
