%%%-------------------------------------------------------------------
%%% @doc Compliance Audit Governor - 5-state compliance FSM
%%%
%%% States: pending_audit -> under_review -> compliant | non_compliant
%%%         non_compliant -> remediation_required
%%%
%%% Manages compliance audits with automatic escalation and remediation workflows.
%%% @end
%%%-------------------------------------------------------------------
-module(compliance_audit_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, start_link/3]).
-export([start_audit/3, begin_review/3, mark_compliant/3]).
-export([mark_non_compliant/3, request_remediation/3]).
-export([get_state/2, list_receipts/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% Type definitions
-type tenant_id() :: binary().
-type audit_id() :: binary().
-type compliance_domain() :: binary().
-type compliance_state() :: pending_audit | under_review | compliant
                          | non_compliant | remediation_required.
-type severity() :: low | medium | high | critical.
-type timestamp() :: non_neg_integer().
-type receipt() :: #{
    id => binary(),
    timestamp => timestamp(),
    tenant_id => tenant_id(),
    audit_id => audit_id(),
    state_from => compliance_state(),
    state_to => compliance_state(),
    severity => severity(),
    reason => binary(),
    metadata => map()
}.

-record(compliance_data, {
    tenant_id :: tenant_id(),
    audit_id :: audit_id(),
    domain :: compliance_domain() | undefined,
    severity :: severity(),
    receipt_table :: atom(),
    created_at :: timestamp(),
    review_started_at :: timestamp() | undefined,
    review_completed_at :: timestamp() | undefined,
    violations :: list()
}).

-type data() :: #compliance_data{}.

%% Type specs
-spec start_link(tenant_id(), audit_id()) ->
    {ok, pid()} | {error, {already_started, pid()} | term()}.
-spec start_audit(pid(), compliance_domain(), map()) ->
    {ok, compliance_state()} | {error, atom()}.
-spec begin_review(pid(), binary(), map()) ->
    {ok, compliance_state()} | {error, atom()}.
-spec mark_compliant(pid(), binary(), map()) ->
    {ok, compliance_state()} | {error, atom()}.
-spec mark_non_compliant(pid(), binary(), map()) ->
    {ok, compliance_state()} | {error, atom()}.
-spec request_remediation(pid(), binary(), map()) ->
    {ok, compliance_state()} | {error, atom()}.
-spec get_state(pid(), audit_id()) ->
    {ok, compliance_state()} | {error, atom()}.
-spec list_receipts(pid(), audit_id()) ->
    {ok, [receipt()]} | {error, atom()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(TenantId, AuditId) ->
    start_link(TenantId, AuditId, AuditId).

start_link(TenantId, AuditId, _Opts) ->
    gen_statem:start_link(
        {local, audit_name(TenantId, AuditId)},
        ?MODULE,
        {TenantId, AuditId},
        []
    ).

start_audit(Pid, Domain, Metadata) ->
    gen_statem:call(Pid, {start_audit, Domain, Metadata}).

begin_review(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {begin_review, Reason, Metadata}).

mark_compliant(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {mark_compliant, Reason, Metadata}).

mark_non_compliant(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {mark_non_compliant, Reason, Metadata}).

request_remediation(Pid, Reason, Metadata) ->
    gen_statem:call(Pid, {request_remediation, Reason, Metadata}).

get_state(Pid, AuditId) ->
    gen_statem:call(Pid, {get_state, AuditId}).

list_receipts(Pid, AuditId) ->
    gen_statem:call(Pid, {list_receipts, AuditId}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init({TenantId, AuditId}) ->
    ReceiptTable = audit_receipt_table(TenantId, AuditId),
    ets:new(ReceiptTable, [public, named_table, {write_concurrency, true}]),

    Data = #compliance_data{
        tenant_id = TenantId,
        audit_id = AuditId,
        domain = undefined,
        severity = low,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        review_started_at = undefined,
        review_completed_at = undefined,
        violations = []
    },

    emit_receipt(Data, pending_audit, pending_audit, <<"initialization">>, low, #{}),
    {ok, pending_audit, Data}.

callback_mode() ->
    handle_event_function.

%% Pending Audit -> Under Review
handle_event({call, From}, {start_audit, Domain, Metadata}, pending_audit, Data) ->
    NewData = Data#compliance_data{domain = Domain},
    emit_receipt(NewData, pending_audit, under_review, <<"audit_started">>, low, Metadata),
    {next_state, under_review, NewData, [{reply, From, {ok, under_review}}]};

%% Under Review -> Compliant
handle_event({call, From}, {mark_compliant, Reason, Metadata}, under_review, Data) ->
    NewData = Data#compliance_data{review_completed_at = timestamp()},
    emit_receipt(NewData, under_review, compliant, Reason, low, Metadata),
    {next_state, compliant, NewData, [{reply, From, {ok, compliant}}]};

%% Under Review -> Non-Compliant
handle_event({call, From}, {mark_non_compliant, Reason, Metadata}, under_review, Data) ->
    Severity = extract_severity(Metadata),
    NewData = Data#compliance_data{severity = Severity, review_completed_at = timestamp()},
    emit_receipt(NewData, under_review, non_compliant, Reason, Severity, Metadata),
    {next_state, non_compliant, NewData, [{reply, From, {ok, non_compliant}}]};

%% Non-Compliant -> Remediation Required
handle_event({call, From}, {request_remediation, Reason, Metadata}, non_compliant, Data) ->
    emit_receipt(Data, non_compliant, remediation_required, Reason, Data#compliance_data.severity, Metadata),
    {next_state, remediation_required, Data, [{reply, From, {ok, remediation_required}}]};

%% Remediation Required -> Compliant (after remediation)
handle_event({call, From}, {mark_compliant, Reason, Metadata}, remediation_required, Data) ->
    NewData = Data#compliance_data{severity = low},
    emit_receipt(NewData, remediation_required, compliant, Reason, low, Metadata),
    {next_state, compliant, NewData, [{reply, From, {ok, compliant}}]};

handle_event({call, From}, {begin_review, _Reason, _Metadata}, State, _Data)
  when State =:= under_review ->
    {keep_state_and_data, [{reply, From, {error, {already_under_review, State}}}]};

handle_event({call, From}, {get_state, _AuditId}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {list_receipts, _AuditId}, _State, Data) ->
    ReceiptTable = Data#compliance_data.receipt_table,
    Receipts = ets:match_object(ReceiptTable, {receipt, '_'}),
    FormattedReceipts = [format_receipt(R) || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _Event, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_transition, State}}}]};

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data}.

terminate(_Reason, _State, Data) ->
    ReceiptTable = Data#compliance_data.receipt_table,
    ets:delete(ReceiptTable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

emit_receipt(Data, FromState, ToState, Reason, Severity, Metadata) ->
    ReceiptTable = Data#compliance_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tenant_id => Data#compliance_data.tenant_id,
        audit_id => Data#compliance_data.audit_id,
        state_from => FromState,
        state_to => ToState,
        severity => Severity,
        reason => Reason,
        metadata => Metadata
    },
    ets:insert(ReceiptTable, {receipt, Receipt}).

format_receipt(Receipt) -> Receipt.

generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

timestamp() ->
    erlang:system_time(millisecond).

extract_severity(Metadata) ->
    maps:get(severity, Metadata, medium).

audit_name(TenantId, AuditId) ->
    binary_to_atom(
        <<TenantId/binary, "_", AuditId/binary, "_audit">>,
        utf8
    ).

audit_receipt_table(TenantId, AuditId) ->
    binary_to_atom(
        <<TenantId/binary, "_", AuditId/binary, "_audit_receipts">>,
        utf8
    ).
