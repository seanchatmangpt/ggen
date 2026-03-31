%%%-------------------------------------------------------------------
%% @doc TAIEA Governor - gen_statem for entitlement + bounded action control
%%
%% States: boot, stable, intervening, refusing
%%
%% State Transitions:
%%   boot -> stable (after entitlement check)
%%   stable -> intervening (when action required and gates pass)
%%   stable -> refusing (when gates fail)
%%   intervening -> stable (after action completes successfully)
%%   intervening -> refusing (if action fails and recovery impossible)
%%   refusing -> stable (manual override or recovery)
%%
%% Gate Checks (Phase 1 stubs, Agent 8/9 implement full logic):
%%   Gate 1: Is entitlement active? (stub)
%%   Gate 2: Is IAM role enabled? (stub)
%%   Gate 3: Action preconditions? (stub)
%%
%% Action Execution with bounded resources:
%%   - Timeout enforcement
%%   - Memory usage tracking
%%   - Result receipt generation
%%
%% Uses gen_statem for deterministic state management with immutable receipts.
%% ETS provides per-governor state storage.
%% @end
%%%-------------------------------------------------------------------
-module(taiea_governor).
-behaviour(gen_statem).

%% API
-export([start_link/1, start_link/2]).
-export([signal/2, tool_call/4, get_state/1, list_receipts/1]).
-export([entitlement_changed/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Internal utilities
-export([init_receipt_table/1, generate_receipt_id/0, timestamp/0]).

-include("tai_autonomics.hrl").

%% Type definitions
-type tenant_id() :: binary().
-type governor_id() :: binary().
-type tool_name() :: binary().
-type governor_state() :: boot | stable | intervening | refusing.
-type gate_result() :: {accept, Metadata :: map()} | {refuse, Reason :: binary()}.
-type timestamp() :: non_neg_integer().

-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    governor_id => governor_id(),
    state_from => governor_state(),
    state_to => governor_state(),
    event_type => binary(),
    reason => binary(),
    metadata => map()
}.

-record(taiea_data, {
    tenant_id :: tenant_id(),
    governor_id :: governor_id(),
    receipt_table :: atom(),
    entitlement_state :: active | inactive | undefined,
    in_flight_action :: undefined | {ActionId :: binary(), Pid :: pid(), StartTime :: timestamp()},
    action_timeout_ms :: non_neg_integer(),
    max_memory_mb :: non_neg_integer(),
    created_at :: timestamp(),
    last_transition :: timestamp(),
    signal_count :: non_neg_integer(),
    receipt_count :: non_neg_integer()
}).

-type data() :: #taiea_data{}.

%% Type specs
-spec start_link(TenantId :: tenant_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec start_link(TenantId :: tenant_id(), Opts :: map()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec signal(Pid :: pid(), Signal :: map()) ->
    {ok, governor_state(), receipt()} | {error, term()}.
-spec tool_call(Pid :: pid(), ToolName :: tool_name(), Arguments :: map(), TimeoutMs :: non_neg_integer()) ->
    {ok, Result :: map(), receipt()} | {error, term()}.
-spec get_state(Pid :: pid()) ->
    {ok, governor_state()} | {error, term()}.
-spec list_receipts(Pid :: pid()) ->
    {ok, [receipt()]} | {error, term()}.
-spec entitlement_changed(Pid :: pid(), NewEntitlementState :: active | inactive) ->
    ok.

%%%===================================================================
%% API
%%%===================================================================

start_link(TenantId) ->
    start_link(TenantId, #{}).

start_link(TenantId, Opts) ->
    GovernorId = maps:get(governor_id, Opts, TenantId),
    gen_statem:start_link(
        {via, gproc, {n, l, {taiea_governor, TenantId}}},
        ?MODULE,
        {TenantId, GovernorId, Opts},
        []
    ).

%% @doc Signal to governor (async cast)
signal(Pid, Signal) ->
    gen_statem:call(Pid, {signal, Signal}).

%% @doc Tool call with timeout and memory bounds
tool_call(Pid, ToolName, Arguments, TimeoutMs) ->
    gen_statem:call(Pid, {tool_call, ToolName, Arguments, TimeoutMs}).

%% @doc Get current governor state
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc List all receipts for this governor
list_receipts(Pid) ->
    gen_statem:call(Pid, list_receipts).

%% @doc Update entitlement state (called by entitlement subsystem)
entitlement_changed(Pid, NewState) ->
    gen_statem:cast(Pid, {entitlement_changed, NewState}).

%%%===================================================================
%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    handle_event_function.

init({TenantId, GovernorId, Opts}) ->
    ReceiptTable = receipt_table_name(TenantId, GovernorId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    ActionTimeoutMs = maps:get(action_timeout_ms, Opts, 30000),
    MaxMemoryMb = maps:get(max_memory_mb, Opts, 512),

    Data = #taiea_data{
        tenant_id = TenantId,
        governor_id = GovernorId,
        receipt_table = ReceiptTable,
        entitlement_state = undefined,
        in_flight_action = undefined,
        action_timeout_ms = ActionTimeoutMs,
        max_memory_mb = MaxMemoryMb,
        created_at = timestamp(),
        last_transition = timestamp(),
        signal_count = 0,
        receipt_count = 0
    },

    emit_receipt(Data, boot, boot, <<"initialization">>, #{}),
    {ok, boot, Data}.

%%%===================================================================
%% Boot State
%%%===================================================================

handle_event(enter, _OldState, boot, Data) ->
    {keep_state, Data};

handle_event({call, From}, {signal, Signal}, boot, Data) ->
    %% Boot: check entitlement before moving to stable
    case check_gates(Data#taiea_data.tenant_id, Signal) of
        {accept, Metadata} ->
            NewData = Data#taiea_data{entitlement_state = active},
            Receipt = emit_receipt(NewData, boot, stable, <<"entitlement_verified">>, Metadata),
            {next_state, stable, NewData, [{reply, From, {ok, stable, Receipt}}]};
        {refuse, Reason} ->
            _Receipt = emit_receipt(Data, boot, refusing, <<"entitlement_check_failed">>, #{reason => Reason}),
            {next_state, refusing, Data, [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, get_state, boot, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, boot}}]};

handle_event({call, From}, list_receipts, boot, Data) ->
    Receipts = get_all_receipts(Data#taiea_data.receipt_table),
    {keep_state_and_data, [{reply, From, {ok, Receipts}}]};

handle_event(_EventType, _EventContent, boot, _Data) ->
    {keep_state_and_data};

%%%===================================================================
%% Stable State
%%%===================================================================

handle_event(enter, _OldState, stable, Data) ->
    {keep_state, Data};

handle_event({call, From}, {signal, Signal}, stable, Data) ->
    %% Stable: accept signal, stay in stable
    case check_gates(Data#taiea_data.tenant_id, Signal) of
        {accept, Metadata} ->
            NewData = Data#taiea_data{signal_count = Data#taiea_data.signal_count + 1},
            Receipt = emit_receipt(NewData, stable, stable, <<"signal_processed">>, Metadata),
            {keep_state, NewData, [{reply, From, {ok, stable, Receipt}}]};
        {refuse, Reason} ->
            _Receipt = emit_receipt(Data, stable, stable, <<"signal_refused">>, #{reason => Reason}),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, {tool_call, ToolName, Arguments, TimeoutMs}, stable, Data) ->
    %% Tool call: run through gates, execute with bounds, emit receipt
    case check_gates(Data#taiea_data.tenant_id, #{tool => ToolName, args => Arguments}) of
        {accept, _GateMetadata} ->
            case execute_bounded_action(ToolName, Arguments, TimeoutMs, Data#taiea_data.max_memory_mb) of
                {ok, Result} ->
                    Metadata = #{
                        tool => ToolName,
                        result => Result,
                        timeout_ms => TimeoutMs
                    },
                    Receipt = emit_receipt(Data, stable, stable, <<"tool_call_success">>, Metadata),
                    {keep_state, Data, [{reply, From, {ok, Result, Receipt}}]};

                {error, Reason} ->
                    Metadata = #{
                        tool => ToolName,
                        error => Reason,
                        timeout_ms => TimeoutMs
                    },
                    _Receipt = emit_receipt(Data, stable, stable, <<"tool_call_failed">>, Metadata),
                    {keep_state, Data, [{reply, From, {error, Reason}}]};

                {timeout, _} ->
                    Metadata = #{
                        tool => ToolName,
                        timeout_ms => TimeoutMs
                    },
                    _Receipt = emit_receipt(Data, stable, intervening, <<"action_timeout">>, Metadata),
                    ActionId = generate_action_id(),
                    NewData = Data#taiea_data{
                        in_flight_action = {ActionId, self(), timestamp()}
                    },
                    {next_state, intervening, NewData, [{reply, From, {error, timeout}}]};

                {memory_exceeded, Used} ->
                    Metadata = #{
                        tool => ToolName,
                        memory_used_mb => Used,
                        max_memory_mb => Data#taiea_data.max_memory_mb
                    },
                    _Receipt = emit_receipt(Data, stable, intervening, <<"memory_exceeded">>, Metadata),
                    NewData = Data#taiea_data{
                        in_flight_action = {generate_action_id(), self(), timestamp()}
                    },
                    {next_state, intervening, NewData, [{reply, From, {error, memory_exceeded}}]}
            end;

        {refuse, Reason} ->
            Metadata = #{
                tool => ToolName,
                reason => Reason
            },
            _Receipt = emit_receipt(Data, stable, stable, <<"tool_call_refused">>, Metadata),
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, get_state, stable, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, stable}}]};

handle_event({call, From}, list_receipts, stable, Data) ->
    Receipts = get_all_receipts(Data#taiea_data.receipt_table),
    {keep_state_and_data, [{reply, From, {ok, Receipts}}]};

handle_event(cast, {entitlement_changed, NewState}, stable, Data) ->
    NewData = Data#taiea_data{entitlement_state = NewState},
    Metadata = #{entitlement_state => NewState},
    emit_receipt(NewData, stable, stable, <<"entitlement_changed">>, Metadata),
    {keep_state, NewData};

handle_event(_EventType, _EventContent, stable, _Data) ->
    {keep_state_and_data};

%%%===================================================================
%% Intervening State
%%%===================================================================

handle_event(enter, _OldState, intervening, Data) ->
    {keep_state, Data};

handle_event({call, From}, {signal, _Signal}, intervening, Data) ->
    %% While intervening, postpone new signals
    Receipt = emit_receipt(Data, intervening, intervening, <<"signal_postponed">>, #{}),
    {keep_state, Data, [{reply, From, {ok, intervening, Receipt}}]};

handle_event({call, From}, {tool_call, _ToolName, _Arguments, _TimeoutMs}, intervening, Data) ->
    %% While intervening, refuse new tool calls
    _Receipt = emit_receipt(Data, intervening, intervening, <<"tool_call_refused_intervening">>, #{}),
    {keep_state, Data, [{reply, From, {error, intervening}}]};

handle_event({call, From}, get_state, intervening, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, intervening}}]};

handle_event({call, From}, list_receipts, intervening, Data) ->
    Receipts = get_all_receipts(Data#taiea_data.receipt_table),
    {keep_state_and_data, [{reply, From, {ok, Receipts}}]};

handle_event(info, {action_complete, ActionId, Result}, intervening, Data) ->
    case Data#taiea_data.in_flight_action of
        {ActionId, _Pid, _StartTime} ->
            Metadata = #{
                action_id => ActionId,
                result => Result
            },
            NewData = Data#taiea_data{in_flight_action = undefined},
            emit_receipt(NewData, intervening, stable, <<"action_complete">>, Metadata),
            {next_state, stable, NewData};

        _ ->
            {keep_state, Data}
    end;

handle_event(info, {action_failed, ActionId, Reason}, intervening, Data) ->
    case Data#taiea_data.in_flight_action of
        {ActionId, _Pid, _StartTime} ->
            Metadata = #{
                action_id => ActionId,
                reason => Reason
            },
            NewData = Data#taiea_data{in_flight_action = undefined},
            emit_receipt(NewData, intervening, refusing, <<"action_failed">>, Metadata),
            {next_state, refusing, NewData};

        _ ->
            {keep_state, Data}
    end;

handle_event(cast, {entitlement_changed, NewState}, intervening, Data) ->
    NewData = Data#taiea_data{entitlement_state = NewState},
    Metadata = #{entitlement_state => NewState},
    emit_receipt(NewData, intervening, intervening, <<"entitlement_changed_intervening">>, Metadata),
    {keep_state, NewData};

handle_event(_EventType, _EventContent, intervening, _Data) ->
    {keep_state_and_data};

%%%===================================================================
%% Refusing State
%%%===================================================================

handle_event(enter, _OldState, refusing, Data) ->
    {keep_state, Data};

handle_event({call, From}, {signal, _Signal}, refusing, Data) ->
    _Receipt = emit_receipt(Data, refusing, refusing, <<"signal_refused">>, #{}),
    {keep_state, Data, [{reply, From, {error, refusing}}]};

handle_event({call, From}, {tool_call, _ToolName, _Arguments, _TimeoutMs}, refusing, Data) ->
    _Receipt = emit_receipt(Data, refusing, refusing, <<"tool_call_refused">>, #{}),
    {keep_state, Data, [{reply, From, {error, refusing}}]};

handle_event({call, From}, get_state, refusing, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, refusing}}]};

handle_event({call, From}, list_receipts, refusing, Data) ->
    Receipts = get_all_receipts(Data#taiea_data.receipt_table),
    {keep_state_and_data, [{reply, From, {ok, Receipts}}]};

handle_event(cast, {entitlement_changed, active}, refusing, Data) ->
    %% Entitlement reactivated, return to stable
    NewData = Data#taiea_data{entitlement_state = active},
    emit_receipt(NewData, refusing, stable, <<"entitlement_reactivated">>, #{}),
    {next_state, stable, NewData};

handle_event(cast, {entitlement_changed, NewState}, refusing, Data) ->
    NewData = Data#taiea_data{entitlement_state = NewState},
    Metadata = #{entitlement_state => NewState},
    emit_receipt(NewData, refusing, refusing, <<"entitlement_changed_refusing">>, Metadata),
    {keep_state, NewData};

handle_event(_EventType, _EventContent, refusing, _Data) ->
    {keep_state_and_data};

%%%===================================================================
%% Catch-all
%%%===================================================================

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_event, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#taiea_data.receipt_table,
    ets:delete(ReceiptTable),
    ok.

%%%===================================================================
%% Internal Functions - Gate Checking
%%%===================================================================

%% @doc Check all gates for action execution
%% Returns {accept, Metadata} or {refuse, Reason}
-spec check_gates(TenantId :: tenant_id(), ActionContext :: map()) -> gate_result().
check_gates(TenantId, ActionContext) ->
    %% Phase 1: All gates are stubs
    %% Gate 1: Is entitlement active?
    case gate_1_entitlement_active(TenantId) of
        {accept, Meta1} ->
            %% Gate 2: Is IAM role enabled?
            case gate_2_iam_enabled(TenantId, ActionContext) of
                {accept, Meta2} ->
                    %% Gate 3: Action preconditions?
                    case gate_3_action_preconditions(TenantId, ActionContext) of
                        {accept, Meta3} ->
                            %% All gates passed
                            {accept, maps:merge(maps:merge(Meta1, Meta2), Meta3)};
                        {refuse, Reason} ->
                            {refuse, Reason}
                    end;
                {refuse, Reason} ->
                    {refuse, Reason}
            end;
        {refuse, Reason} ->
            {refuse, Reason}
    end.

%% @doc Gate 1: Is entitlement active? (Phase 1 stub)
gate_1_entitlement_active(_TenantId) ->
    %% Phase 1 stub: always accept (Agent 9 implements full entitlement logic)
    {accept, #{gate_1 => <<"entitlement_active">>}}.

%% @doc Gate 2: Is IAM role enabled? (Phase 1 stub)
gate_2_iam_enabled(_TenantId, _ActionContext) ->
    %% Phase 1 stub: always accept (Agent 8 implements full IAM policy evaluation)
    {accept, #{gate_2 => <<"iam_enabled">>}}.

%% @doc Gate 3: Action preconditions? (Phase 1 stub)
gate_3_action_preconditions(_TenantId, _ActionContext) ->
    %% Phase 1 stub: always accept (specific validators implement action-level preconditions)
    {accept, #{gate_3 => <<"preconditions_met">>}}.

%%%===================================================================
%% Internal Functions - Action Execution
%%%===================================================================

%% @doc Execute bounded action with timeout and memory limits
-spec execute_bounded_action(
    ToolName :: tool_name(),
    Arguments :: map(),
    TimeoutMs :: non_neg_integer(),
    MaxMemoryMb :: non_neg_integer()
) -> {ok, Result :: map()} | {error, term()} | {timeout, term()} | {memory_exceeded, non_neg_integer()}.
execute_bounded_action(ToolName, Arguments, TimeoutMs, _MaxMemoryMb) ->
    %% Phase 1 stub: Simulate action execution
    %% In production, this would spawn a monitored worker with resource limits
    try
        %% Simulate action based on tool name
        case simulate_tool_execution(ToolName, Arguments) of
            {ok, Result} ->
                {ok, Result};
            {error, Reason} ->
                {error, Reason};
            {timeout, _} ->
                {timeout, TimeoutMs};
            {memory_exceeded, Used} ->
                {memory_exceeded, Used}
        end
    catch
        C:E ->
            {error, {C, E}}
    end.

%% @doc Simulate tool execution for Phase 1
-spec simulate_tool_execution(ToolName :: tool_name(), Arguments :: map()) ->
    {ok, map()} | {error, term()} | {timeout, term()} | {memory_exceeded, non_neg_integer()}.
simulate_tool_execution(ToolName, Arguments) ->
    %% Phase 1: Simple simulation based on tool name
    case ToolName of
        <<"query">> ->
            %% Simulate successful query
            {ok, #{
                status => success,
                rows => 42,
                duration_ms => 125
            }};

        <<"create">> ->
            %% Simulate successful creation
            {ok, #{
                status => success,
                created_id => generate_action_id(),
                duration_ms => 87
            }};

        <<"scale">> ->
            %% Simulate scaling action
            {ok, #{
                status => success,
                scaled_instances => 3,
                duration_ms => 2150
            }};

        <<"timeout_test">> ->
            %% Simulate timeout for testing
            TimeoutMs = case is_map(Arguments) of
                true -> maps:get(timeout_ms, Arguments, 1000);
                false -> 30000
            end,
            {timeout, TimeoutMs};

        <<"memory_test">> ->
            %% Simulate memory exceeded for testing
            {memory_exceeded, 1024};

        _ ->
            %% Unknown tool: return generic success
            {ok, #{
                status => success,
                tool => ToolName,
                args => Arguments,
                duration_ms => 50
            }}
    end.

%%%===================================================================
%% Internal Functions - Receipt Management
%%%===================================================================

%% @doc Emit a receipt and return it
-spec emit_receipt(Data :: data(), FromState :: governor_state(), ToState :: governor_state(),
                   EventType :: binary(), Metadata :: map()) -> receipt().
emit_receipt(Data, FromState, ToState, EventType, Metadata) ->
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#taiea_data.tenant_id,
        governor_id => Data#taiea_data.governor_id,
        state_from => FromState,
        state_to => ToState,
        event_type => EventType,
        reason => <<>>,
        metadata => Metadata
    },
    ReceiptTable = Data#taiea_data.receipt_table,
    ets:insert(ReceiptTable, {receipt, Receipt}),
    Receipt.

%% @doc Get all receipts for this governor
-spec get_all_receipts(Table :: atom()) -> [receipt()].
get_all_receipts(Table) ->
    Receipts = ets:match_object(Table, {receipt, '_'}),
    [R || {receipt, R} <- Receipts].

%%%===================================================================
%% Internal Functions - Utilities
%%%===================================================================

%% @doc Generate unique receipt ID
-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).

%% @doc Generate unique action ID
-spec generate_action_id() -> binary().
generate_action_id() ->
    Id = crypto:strong_rand_bytes(16),
    << IdInt:128 >> = Id,
    iolist_to_binary(io_lib:format("~32.16.0b", [IdInt])).

%% @doc Get current timestamp in milliseconds
-spec timestamp() -> non_neg_integer().
timestamp() ->
    erlang:system_time(millisecond).

%% @doc Receipt table name
-spec receipt_table_name(TenantId :: tenant_id(), GovernorId :: governor_id()) -> atom().
receipt_table_name(TenantId, GovernorId) ->
    binary_to_atom(
        << TenantId/binary, "_", GovernorId/binary, "_receipts" >>,
        utf8
    ).

%% @doc Initialize receipt table (for manual setup if needed)
-spec init_receipt_table(TableName :: atom()) -> atom().
init_receipt_table(TableName) ->
    ets:new(TableName, [public, named_table, {write_concurrency, true}]).
