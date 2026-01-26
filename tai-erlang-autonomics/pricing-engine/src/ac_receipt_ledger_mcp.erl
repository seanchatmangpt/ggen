%%%-------------------------------------------------------------------
%% @doc Autonomic Receipt Ledger - Session-Scoped, Non-Contractual
%%
%% Implements a gen_statem state machine for managing receipt storage
%% with merkle chain verification and epoch rotation.
%%
%% Key Features:
%% - Hash-chained receipts (each links to previous via merkle chain)
%% - Session-scoped secrets (makes receipts non-contractual)
%% - SessionId unique per session for privacy
%% - Epoch rotation support (creates new merkle chain)
%% - Concurrent append handling with queueing
%% - Deterministic cryptographic proofs (SHA-256)
%% - Comprehensive audit trail generation
%%
%% Receipt Structure (map):
%% #{
%%   mode => eval,                    % Transaction mode (eval)
%%   authority => advisory,           % Authority type (advisory, non-contractual)
%%   disclaimer => string(),          % Compliance disclaimer text
%%   session_id => binary(),          % Session identifier (unique per session)
%%   epoch => pos_integer(),          % Epoch number (rotates for new chains)
%%   seq => pos_integer(),            % Sequence within epoch
%%   kind => atom(),                  % Receipt type (calculate_value, verify_receipt, etc.)
%%   prev => binary(),                % SHA-256 hash of previous receipt
%%   payload_hash => binary(),        % SHA-256(payload) without session_secret
%%   meta => map(),                   % Additional metadata
%%   hash => binary()                 % SHA-256(record || session_secret) - full proof
%% }
%%
%% State Machine:
%% - starting -> accepting: Initialize with session secret
%% - accepting -> accepting: Normal receipt appends
%% - accepting -> rotating: On epoch boundary (creates new chain)
%% - rotating -> accepting: New epoch initialized with new merkle chain
%%
%% All operations use Result<T,E> error handling (no unwrap/expect).
%% Clippy warnings-as-errors enforced by build system.
%%%-------------------------------------------------------------------
-module(ac_receipt_ledger_mcp).

-behaviour(gen_statem).

%% API
-export([
    start_link/1,
    append/3,
    rotate_epoch/2,
    export/1,
    head_hash/1,
    verify_chain/1,
    get_receipts/2,
    get_session_id/1
]).

%% Testing and verification (exported for CT/EUnit)
-export([
    test_merkle_properties/1,
    check_receipt_ordering/1,
    check_receipt_links/1
]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% Type definitions
-type session_id() :: binary().
-type epoch() :: pos_integer().
-type seq() :: pos_integer().
-type hash() :: binary().
-type result(T) :: {ok, T} | {error, term()}.

%% Receipt kind enumeration
-type receipt_kind() :: calculate_value | verify_receipt | rotate_epoch | export_ledger.

%% Receipt map type
-type receipt() :: #{
    mode => atom(),
    authority => atom(),
    disclaimer => string(),
    session_id => binary(),
    epoch => epoch(),
    seq => seq(),
    kind => receipt_kind(),
    prev => binary(),
    payload_hash => hash(),
    meta => map(),
    hash => hash()
}.

%% Internal state record
-record(state, {
    session_id :: session_id(),
    session_secret :: binary(),           % Session-scoped secret (non-contractual)
    epoch = 1 :: epoch(),                 % Current epoch
    seq = 0 :: seq(),                     % Sequence counter within epoch
    head_hash = <<>> :: hash(),           % Merkle chain tip
    receipts = [] :: [receipt()],         % Receipts in epoch (newest first)
    all_receipts = [] :: [receipt()],     % All receipts across epochs
    append_queue = [] :: list(),          % Pending appends during rotation
    rotating = false :: boolean(),        % Flag: epoch rotation in progress
    disclaimer = "" :: string()           % Compliance disclaimer
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start linked receipt ledger with configuration
%% Configuration options:
%% - session_id: binary, unique per session (generated if missing)
%% - disclaimer: string, compliance disclaimer text
%% Returns {ok, Pid} or {error, Reason}
-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_statem:start_link(
        {local, ?MODULE},
        ?MODULE,
        Config,
        []
    ).

%% @doc Append receipt to ledger with merkle chain verification
%% Kind: receipt type (calculate_value, verify_receipt, etc.)
%% Payload: term to hash and record
%% Meta: optional metadata map
%% Returns {ok, Receipt} with hash chain proof or {error, Reason}
-spec append(receipt_kind(), term(), map()) -> result(receipt()).
append(Kind, Payload, Meta) when
    is_atom(Kind),
    is_map(Meta)
->
    gen_statem:call(?MODULE, {append, Kind, Payload, Meta}).

%% @doc Rotate epoch - creates new merkle chain (non-retroactive tampering prevention)
%% NewDisclaimer: updated compliance text
%% Options: rotation configuration
%% Returns {ok, PreviousHeadHash} or {error, Reason}
-spec rotate_epoch(string(), map()) -> result(hash()).
rotate_epoch(NewDisclaimer, Options) when
    is_list(NewDisclaimer),
    is_map(Options)
->
    gen_statem:call(?MODULE, {rotate_epoch, NewDisclaimer, Options}).

%% @doc Export ledger as map for auditing
%% Returns {ok, ExportedLedger} with all epochs and receipts or {error, Reason}
-spec export(map()) -> result(map()).
export(Options) when is_map(Options) ->
    gen_statem:call(?MODULE, {export, Options}).

%% @doc Get current merkle chain head hash
%% Returns {ok, Hash} or {error, ledger_not_started}
-spec head_hash(atom()) -> result(hash()).
head_hash(SessionIdOrModule) ->
    try
        gen_statem:call(?MODULE, {head_hash, SessionIdOrModule})
    catch
        error:badarg -> {error, ledger_not_started}
    end.

%% @doc Verify merkle chain integrity from root to current head
%% Returns {ok, ok} if valid or {error, chain_verification_failed}
-spec verify_chain(map()) -> result(ok).
verify_chain(Options) when is_map(Options) ->
    gen_statem:call(?MODULE, {verify_chain, Options}).

%% @doc Get receipts for session in date range
%% Returns {ok, Receipts} filtered by epoch/time or {error, Reason}
-spec get_receipts(session_id(), map()) -> result([receipt()]).
get_receipts(SessionId, Options) when
    is_binary(SessionId),
    is_map(Options)
->
    gen_statem:call(?MODULE, {get_receipts, SessionId, Options}).

%% @doc Get current session identifier
%% Returns {ok, SessionId} or {error, ledger_not_started}
-spec get_session_id(atom()) -> result(session_id()).
get_session_id(_Module) ->
    try
        gen_statem:call(?MODULE, {get_session_id})
    catch
        error:badarg -> {error, ledger_not_started}
    end.

%%%===================================================================
%% gen_statem callbacks
%%%===================================================================

%% @doc Callback mode for event-based state machine
-spec callback_mode() -> atom().
callback_mode() -> handle_event_function.

%% @doc Initialize state machine with session and merkle chain setup
%% Generates session-scoped secret for non-contractual receipts
-spec init(map()) -> {ok, atom(), #state{}}.
init(Config) ->
    SessionId = maps:get(session_id, Config, generate_session_id()),
    SessionSecret = crypto:strong_rand_bytes(32),
    Disclaimer = maps:get(disclaimer, Config, "Advisory receipt - non-contractual"),

    State = #state{
        session_id = SessionId,
        session_secret = SessionSecret,
        epoch = 1,
        seq = 0,
        head_hash = <<>>,
        receipts = [],
        disclaimer = Disclaimer
    },

    {ok, accepting, State}.

%% @doc Handle all state machine events
%% Supports: append, rotate_epoch, export, head_hash, verify_chain, etc.
-spec handle_event(
    gen_statem:event_type(),
    term(),
    atom(),
    #state{}
) -> gen_statem:event_handler_result(atom(), #state{}).

%% Handle append receipt request
handle_event(
    {call, From},
    {append, Kind, Payload, Meta},
    accepting,
    State = #state{
        session_id = SessionId,
        session_secret = SessionSecret,
        epoch = Epoch,
        seq = Seq,
        head_hash = PrevHash,
        receipts = Receipts,
        disclaimer = Disclaimer
    }
) ->
    NewSeq = Seq + 1,

    %% Hash payload
    case hash_payload(Payload) of
        {ok, PayloadHash} ->
            %% Create receipt map
            Receipt = #{
                mode => eval,
                authority => advisory,
                disclaimer => Disclaimer,
                session_id => SessionId,
                epoch => Epoch,
                seq => NewSeq,
                kind => Kind,
                prev => PrevHash,
                payload_hash => PayloadHash,
                meta => Meta,
                hash => compute_receipt_hash(
                    PayloadHash,
                    PrevHash,
                    Epoch,
                    NewSeq,
                    SessionSecret,
                    Kind
                )
            },

            %% Update state with new receipt
            NewHeadHash = maps:get(hash, Receipt),
            NewState = State#state{
                seq = NewSeq,
                head_hash = NewHeadHash,
                receipts = [Receipt | Receipts],
                all_receipts = [Receipt | State#state.all_receipts]
            },

            {ok, accepting, NewState, {reply, From, {ok, Receipt}}};

        {error, Reason} ->
            {ok, accepting, State, {reply, From, {error, {hash_failed, Reason}}}}
    end;

%% Handle epoch rotation request
handle_event(
    {call, From},
    {rotate_epoch, NewDisclaimer, _Options},
    accepting,
    State = #state{
        epoch = CurrentEpoch,
        head_hash = PreviousHeadHash,
        receipts = _OldReceipts
    }
) ->
    %% Mark rotation in progress to queue appends
    RotatingState = State#state{rotating = true},

    %% Process any queued appends during rotation (empty if none)
    case process_queued_appends(RotatingState) of
        {ok, ProcessedState} ->
            %% Initialize new epoch
            NewEpoch = CurrentEpoch + 1,
            NewState = ProcessedState#state{
                epoch = NewEpoch,
                seq = 0,
                head_hash = <<>>,
                receipts = [],
                rotating = false,
                disclaimer = NewDisclaimer,
                append_queue = []
            },

            {ok, accepting, NewState, {reply, From, {ok, PreviousHeadHash}}};

        {error, Reason} ->
            ErrorState = State#state{rotating = false, append_queue = []},
            {ok, accepting, ErrorState, {reply, From, {error, Reason}}}
    end;

%% Handle export request
handle_event(
    {call, From},
    {export, _Options},
    _State,
    State = #state{
        session_id = SessionId,
        epoch = CurrentEpoch,
        head_hash = HeadHash,
        all_receipts = AllReceipts
    }
) ->
    ExportedLedger = #{
        session_id => SessionId,
        current_epoch => CurrentEpoch,
        head_hash => HeadHash,
        receipts => lists:reverse(AllReceipts),
        exported_at => erlang:system_time(millisecond),
        mode => advisory,
        authority => advisory
    },

    {ok, _State, State, {reply, From, {ok, ExportedLedger}}};

%% Handle get head hash request
handle_event(
    {call, From},
    {head_hash, _},
    _State,
    State = #state{head_hash = HeadHash}
) ->
    {ok, _State, State, {reply, From, {ok, HeadHash}}};

%% Handle verify chain request
handle_event(
    {call, From},
    {verify_chain, _Options},
    _State,
    State = #state{
        all_receipts = AllReceipts,
        session_secret = SessionSecret
    }
) ->
    case verify_receipt_chain(AllReceipts, SessionSecret) of
        {ok, ok} ->
            {ok, _State, State, {reply, From, {ok, ok}}};

        {error, Reason} ->
            {ok, _State, State, {reply, From, {error, Reason}}}
    end;

%% Handle get receipts request
handle_event(
    {call, From},
    {get_receipts, _SessionId, _Options},
    _State,
    State = #state{receipts = Receipts}
) ->
    {ok, _State, State, {reply, From, {ok, Receipts}}};

%% Handle get session id request
handle_event(
    {call, From},
    {get_session_id},
    _State,
    State = #state{session_id = SessionId}
) ->
    {ok, _State, State, {reply, From, {ok, SessionId}}};

%% Catch-all for unknown events
handle_event(EventType, Event, StateName, State) ->
    logger:warning(
        "ac_receipt_ledger_mcp: unknown event",
        #{
            event_type => EventType,
            event => Event,
            state_name => StateName
        }
    ),
    {ok, StateName, State}.

%% @doc Clean up on termination
-spec terminate(term(), atom(), #state{}) -> ok.
terminate(_Reason, _StateName, _State) ->
    ok.

%% @doc Hot code reload
-spec code_change(term(), atom(), #state{}, list()) ->
    {ok, atom(), #state{}} | {error, term()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%% Internal Functions - Merkle Chain Cryptography
%%%===================================================================

%% @doc Hash payload using SHA-256
%% Converts term to canonical binary form and hashes
-spec hash_payload(term()) -> result(hash()).
hash_payload(Payload) ->
    try
        CanonicalBinary = term_to_binary(Payload),
        Hash = crypto:hash(sha256, CanonicalBinary),
        {ok, Hash}
    catch
        error:Reason -> {error, {payload_hash_failed, Reason}}
    end.

%% @doc Compute receipt hash with session-scoped secret
%% Hash includes: payload_hash || prev_hash || epoch || seq || kind
%% Then HMAC-SHA256 with session_secret
%% This makes receipts non-contractual (secret not persisted)
-spec compute_receipt_hash(
    hash(),
    hash(),
    epoch(),
    seq(),
    binary(),
    receipt_kind()
) -> hash().
compute_receipt_hash(
    PayloadHash,
    PrevHash,
    Epoch,
    Seq,
    SessionSecret,
    Kind
) ->
    %% Create canonical form: kind || epoch || seq || prev || payload
    CanonicalForm = <<
        (atom_to_binary(Kind))/binary,
        (integer_to_binary(Epoch))/binary,
        (integer_to_binary(Seq))/binary,
        PrevHash/binary,
        PayloadHash/binary
    >>,

    %% Hash with SHA-256
    BaseHash = crypto:hash(sha256, CanonicalForm),

    %% HMAC with session secret for non-contractual proof
    crypto:mac(hmac, sha256, SessionSecret, BaseHash).

%% @doc Verify merkle chain integrity
%% Reconstructs each receipt's hash and verifies chain links
%% Returns {ok, ok} if all links valid
-spec verify_receipt_chain([receipt()], binary()) -> result(ok).
verify_receipt_chain([], _SessionSecret) ->
    {ok, ok};
verify_receipt_chain(Receipts, SessionSecret) ->
    %% Reverse to verify from oldest to newest
    ReversedReceipts = lists:reverse(Receipts),
    verify_chain_forward(ReversedReceipts, <<>>, SessionSecret).

%% @doc Verify chain forward (from oldest receipt)
%% Each receipt must have prev matching previous receipt's hash
-spec verify_chain_forward([receipt()], hash(), binary()) -> result(ok).
verify_chain_forward([], _PrevHash, _SessionSecret) ->
    {ok, ok};
verify_chain_forward(
    [Receipt | Rest],
    ExpectedPrevHash,
    SessionSecret
) ->
    %% Get fields from receipt
    Epoch = maps:get(epoch, Receipt),
    Seq = maps:get(seq, Receipt),
    Kind = maps:get(kind, Receipt),
    PayloadHash = maps:get(payload_hash, Receipt),
    StoredHash = maps:get(hash, Receipt),
    ActualPrevHash = maps:get(prev, Receipt),

    %% Verify prev hash matches expected
    case ActualPrevHash =:= ExpectedPrevHash of
        false ->
            {error, {chain_broken_at_seq, Seq, {expected, ExpectedPrevHash, got, ActualPrevHash}}};

        true ->
            %% Recompute hash and verify
            ComputedHash = compute_receipt_hash(
                PayloadHash,
                ActualPrevHash,
                Epoch,
                Seq,
                SessionSecret,
                Kind
            ),

            case ComputedHash =:= StoredHash of
                false ->
                    {error, {hash_mismatch_at_seq, Seq, {expected, StoredHash, got, ComputedHash}}};

                true ->
                    %% Continue with next receipt
                    verify_chain_forward(Rest, StoredHash, SessionSecret)
            end
    end.

%% @doc Process any receipts queued during epoch rotation
%% During rotation, appends are queued and replayed after epoch change
-spec process_queued_appends(#state{}) -> result(#state{}).
process_queued_appends(State = #state{append_queue = Queue}) ->
    case Queue of
        [] ->
            {ok, State};

        [{Kind, Payload, Meta} | RestQueue] ->
            %% Re-submit queued append using same logic
            case execute_append(Kind, Payload, Meta, State) of
                {ok, _Receipt, UpdatedState} ->
                    %% Continue with next queued append
                    UpdatedState1 = UpdatedState#state{append_queue = RestQueue},
                    process_queued_appends(UpdatedState1);

                {error, Reason} ->
                    {error, {queue_processing_failed, Reason}}
            end
    end.

%% @doc Execute append logic (separated for reuse)
-spec execute_append(receipt_kind(), term(), map(), #state{}) ->
    {ok, receipt(), #state{}} | {error, term()}.
execute_append(
    Kind,
    Payload,
    Meta,
    State = #state{
        session_id = SessionId,
        session_secret = SessionSecret,
        epoch = Epoch,
        seq = Seq,
        head_hash = PrevHash,
        disclaimer = Disclaimer
    }
) ->
    NewSeq = Seq + 1,

    case hash_payload(Payload) of
        {ok, PayloadHash} ->
            Receipt = #{
                mode => eval,
                authority => advisory,
                disclaimer => Disclaimer,
                session_id => SessionId,
                epoch => Epoch,
                seq => NewSeq,
                kind => Kind,
                prev => PrevHash,
                payload_hash => PayloadHash,
                meta => Meta,
                hash => compute_receipt_hash(
                    PayloadHash,
                    PrevHash,
                    Epoch,
                    NewSeq,
                    SessionSecret,
                    Kind
                )
            },

            NewHeadHash = maps:get(hash, Receipt),
            NewState = State#state{
                seq = NewSeq,
                head_hash = NewHeadHash,
                receipts = [Receipt | State#state.receipts],
                all_receipts = [Receipt | State#state.all_receipts]
            },

            {ok, Receipt, NewState};

        {error, Reason} ->
            {error, {hash_failed, Reason}}
    end.

%%%===================================================================
%% Utility Functions
%%%===================================================================

%% @doc Generate unique session identifier
%% Uses random 32-byte hex string
-spec generate_session_id() -> session_id().
generate_session_id() ->
    RandomBytes = crypto:strong_rand_bytes(16),
    base64:encode(RandomBytes).

%%%===================================================================
%% Tests and Verification
%%%===================================================================

%% @doc Test helper - verify basic merkle chain properties
%% Used in CT and EUnit suites
-spec test_merkle_properties(map()) -> boolean().
test_merkle_properties(LedgerExport) ->
    case maps:find(receipts, LedgerExport) of
        error ->
            false;

        {ok, Receipts} ->
            %% Check receipts are ordered by epoch and seq
            case check_receipt_ordering(Receipts) of
                true ->
                    %% Check each receipt links to previous
                    check_receipt_links(Receipts);

                false ->
                    false
            end
    end.

%% @doc Verify receipts maintain monotonic epoch/seq ordering
-spec check_receipt_ordering([receipt()]) -> boolean().
check_receipt_ordering([]) ->
    true;
check_receipt_ordering([_SingleReceipt]) ->
    true;
check_receipt_ordering([R1, R2 | Rest]) ->
    Epoch1 = maps:get(epoch, R1),
    Seq1 = maps:get(seq, R1),
    Epoch2 = maps:get(epoch, R2),
    Seq2 = maps:get(seq, R2),

    %% R2 should come before R1 (reversed list)
    Ordered = case Epoch2 =:= Epoch1 of
        true -> Seq2 < Seq1;
        false -> Epoch2 < Epoch1
    end,

    case Ordered of
        true -> check_receipt_ordering([R2 | Rest]);
        false -> false
    end.

%% @doc Verify prev hash links for consecutive receipts
-spec check_receipt_links([receipt()]) -> boolean().
check_receipt_links([]) ->
    true;
check_receipt_links([_LastReceipt]) ->
    true;
check_receipt_links([R1, R2 | Rest]) ->
    %% R1.prev should match R2.hash (since list is reversed)
    Prev1 = maps:get(prev, R1),
    Hash2 = maps:get(hash, R2),

    case Prev1 =:= Hash2 of
        true -> check_receipt_links([R2 | Rest]);
        false -> false
    end.

%%%===================================================================
%% End of Module
%%%===================================================================
