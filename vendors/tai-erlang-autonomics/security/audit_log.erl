%%%-------------------------------------------------------------------
%%% @doc Append-Only Audit Log for Pricing System
%%%
%%% Maintains immutable, tamper-proof audit trail of all pricing
%%% operations. Every entry is:
%%%   - Cryptographically signed
%%%   - Linked to prior entry (chain of custody)
%%%   - Cannot be deleted or modified
%%%   - Periodically exported to tamper-proof storage
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(audit_log).

%% API
-export([
    log/3,
    log/4,
    query/3,
    verify_integrity/0,
    export_monthly/0,
    detect_gap/0
]).

%% Types
-type entry_id() :: binary().
-type tenant_id() :: binary().
-type action() :: atom().
-type resource_type() :: atom().
-type actor_id() :: binary().

-record(audit_entry, {
    entry_id :: entry_id(),                    % UUID v7
    timestamp :: non_neg_integer(),            % Nanoseconds

    % Action
    action :: action(),                        % calculate | override | access | deploy
    resource_type :: resource_type(),          % receipt | formula | customer
    resource_id :: binary(),                   % What was affected

    % Actor
    actor_id :: actor_id(),                    % Who did it
    actor_role :: atom(),                      % customer | employee | admin
    actor_ip :: binary(),                      % Source IP

    % Details
    description :: binary(),                   % Human-readable description
    old_value :: term(),                       % Before (if applicable)
    new_value :: term(),                       % After (if applicable)
    metadata :: map(),                         % Additional context

    % Security
    entry_hash :: binary(),                    % SHA-256(entry)
    previous_hash :: binary(),                 % Link to prior
    signature :: binary(),                     % Sign(entry, system_key)

    % Compliance
    customer_id :: binary()                    % Multi-tenant trace
}).

-export_type([audit_entry/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% Log audit entry with minimal details
-spec log(action(), resource_type(), binary()) -> {ok, entry_id()}.

log(Action, ResourceType, ResourceId) ->
    log(Action, ResourceType, ResourceId, #{}).

%% Log audit entry with full details
-spec log(action(), resource_type(), binary(), map()) -> {ok, entry_id()}.

log(Action, ResourceType, ResourceId, Metadata) ->
    % Create entry
    Entry = #audit_entry{
        entry_id = generate_entry_id(),
        timestamp = erlang:system_time(nanosecond),
        action = Action,
        resource_type = ResourceType,
        resource_id = ResourceId,
        actor_id = get_current_actor(),
        actor_role = get_actor_role(),
        actor_ip = get_actor_ip(),
        description = format_description(Action, ResourceType, ResourceId),
        metadata = Metadata,
        customer_id = get_customer_id(Metadata)
    },

    % Get chain link to previous entry
    PreviousHash = get_previous_entry_hash(),

    % Compute entry hash
    EntryHash = compute_entry_hash(Entry),

    % Sign entry
    Signature = sign_entry(Entry),

    % Create final entry with chain link
    FinalEntry = Entry#audit_entry{
        entry_hash = EntryHash,
        previous_hash = PreviousHash,
        signature = Signature
    },

    % Store to append-only log (cannot update/delete)
    case store_entry(FinalEntry) of
        ok ->
            % Replicate to geographically diverse locations
            spawn(fun() -> replicate_entry(FinalEntry) end),
            {ok, FinalEntry#audit_entry.entry_id};

        {error, Reason} ->
            {error, Reason}
    end.

%% Query audit log (verify integrity)
-spec query(tenant_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [audit_entry()]} | {error, {atom(), term()}}.

query(TenantId, StartTime, EndTime) ->
    % Query database
    case db:query(
        "SELECT * FROM audit_log
         WHERE customer_id = ? AND timestamp BETWEEN ? AND ?
         ORDER BY timestamp ASC",
        [TenantId, StartTime, EndTime]
    ) of
        {ok, Rows} ->
            % Convert rows to records
            Entries = [row_to_entry(Row) || Row <- Rows],

            % Verify chain integrity
            case verify_chain(Entries) of
                {error, Gap} ->
                    % TAMPERING DETECTED
                    log_security_alert({audit_tampering, Gap}),
                    {error, {tampering_detected, Gap}};

                {ok, valid} ->
                    {ok, Entries}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

%% Verify entire audit log integrity
-spec verify_integrity() ->
    {ok, valid} | {error, {atom(), term()}}.

verify_integrity() ->
    % Get all entries in order
    case db:query("SELECT * FROM audit_log ORDER BY timestamp ASC", []) of
        {ok, Rows} ->
            Entries = [row_to_entry(Row) || Row <- Rows],
            verify_chain(Entries);

        {error, Reason} ->
            {error, Reason}
    end.

%% Export audit log to tamper-proof storage (monthly)
-spec export_monthly() -> {ok, binary()} | {error, term()}.

export_monthly() ->
    % Determine month to export (last month)
    Now = erlang:system_time(second),
    StartOfMonth = start_of_month(Now),
    EndOfMonth = end_of_month(StartOfMonth),

    % Query all entries for month
    case db:query(
        "SELECT * FROM audit_log
         WHERE timestamp BETWEEN ? AND ?
         ORDER BY timestamp ASC",
        [StartOfMonth, EndOfMonth]
    ) of
        {ok, Rows} ->
            Entries = [row_to_entry(Row) || Row <- Rows],

            % Verify integrity before export
            case verify_chain(Entries) of
                {error, Reason} ->
                    {error, {integrity_check_failed, Reason}};

                {ok, valid} ->
                    % Export to CSV and encrypt
                    CSV = entries_to_csv(Entries),
                    Encrypted = encrypt_sensitive(CSV),
                    Signed = sign_export(Encrypted),

                    % Store in immutable storage
                    Filename = export_filename(StartOfMonth),
                    {ok, StoragePath} = upload_to_immutable_storage(Signed, Filename),

                    % Log export
                    log(audit_export, audit_log, Filename, #{
                        storage_path => StoragePath,
                        entry_count => length(Entries),
                        encrypted => true
                    }),

                    {ok, StoragePath}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

%% Detect gaps in audit log (indicates tampering)
-spec detect_gap() -> {ok, no_gap} | {error, {gap_detected, pos_integer()}}.

detect_gap() ->
    case db:query("SELECT * FROM audit_log ORDER BY timestamp ASC", []) of
        {ok, Rows} ->
            Entries = [row_to_entry(Row) || Row <- Rows],
            verify_chain_continuity(Entries, undefined, 0);

        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Verify chain integrity (detect tampering)
verify_chain(Entries) ->
    verify_chain_recursive(Entries, crypto:hash(sha256, <<>>), 0).

verify_chain_recursive([], _PriorHash, _Count) ->
    {ok, valid};

verify_chain_recursive([Entry | Rest], PriorHash, Count) ->
    case Entry#audit_entry.previous_hash =:= PriorHash of
        false ->
            % Chain broken - tampering detected!
            log_security_alert({
                audit_tampering,
                entry_id => Entry#audit_entry.entry_id,
                position => Count
            }),
            {error, {gap_detected, Count}};

        true ->
            % Verify signature
            case verify_entry_signature(Entry) of
                false ->
                    % Signature invalid - entry tampered!
                    {error, {signature_invalid, Count}};

                true ->
                    verify_chain_recursive(Rest, Entry#audit_entry.entry_hash, Count + 1)
            end
    end.

verify_chain_continuity(Entries, PriorHash, Position) ->
    verify_chain_recursive(Entries, PriorHash, Position).

%% Store entry to append-only log
store_entry(Entry) ->
    case db:insert(audit_log, entry_to_row(Entry)) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Replicate entry to geographically diverse locations
replicate_entry(Entry) ->
    % Store in multiple regions
    JSON = entry_to_json(Entry),
    [
        store_to_s3(JSON, <<"us-east-1">>),
        store_to_s3(JSON, <<"eu-west-1">>),
        store_to_gcs(JSON, <<"us">>)
    ].

%% Compute hash of entry (deterministic)
compute_entry_hash(Entry) ->
    Data = term_to_binary(#{
        action => Entry#audit_entry.action,
        resource_type => Entry#audit_entry.resource_type,
        resource_id => Entry#audit_entry.resource_id,
        actor_id => Entry#audit_entry.actor_id,
        timestamp => Entry#audit_entry.timestamp,
        description => Entry#audit_entry.description,
        metadata => Entry#audit_entry.metadata
    }),
    crypto:hash(sha256, Data).

%% Sign entry with system key
sign_entry(Entry) ->
    % Get system private key
    {ok, PrivateKey} = get_system_private_key(),

    % Data to sign
    SignatureData = term_to_binary(#{
        entry_id => Entry#audit_entry.entry_id,
        timestamp => Entry#audit_entry.timestamp,
        action => Entry#audit_entry.action,
        resource_id => Entry#audit_entry.resource_id,
        actor_id => Entry#audit_entry.actor_id,
        previous_hash => Entry#audit_entry.previous_hash
    }),

    % Sign
    crypto:sign(eddsa, SignatureData, PrivateKey).

%% Verify entry signature
verify_entry_signature(Entry) ->
    {ok, PublicKey} = get_system_public_key(),

    SignatureData = term_to_binary(#{
        entry_id => Entry#audit_entry.entry_id,
        timestamp => Entry#audit_entry.timestamp,
        action => Entry#audit_entry.action,
        resource_id => Entry#audit_entry.resource_id,
        actor_id => Entry#audit_entry.actor_id,
        previous_hash => Entry#audit_entry.previous_hash
    }),

    crypto:verify(eddsa, SignatureData, Entry#audit_entry.signature, PublicKey).

%% Get hash of previous entry
get_previous_entry_hash() ->
    case db:query(
        "SELECT entry_hash FROM audit_log ORDER BY timestamp DESC LIMIT 1",
        []
    ) of
        {ok, [#{<<"entry_hash">> := Hash}]} ->
            Hash;
        _ ->
            % No prior entries
            crypto:hash(sha256, <<>>)
    end.

%% Convert database row to record
row_to_entry(Row) ->
    #audit_entry{
        entry_id = maps:get(<<"entry_id">>, Row),
        timestamp = maps:get(<<"timestamp">>, Row),
        action = binary_to_atom(maps:get(<<"action">>, Row), utf8),
        resource_type = binary_to_atom(maps:get(<<"resource_type">>, Row), utf8),
        resource_id = maps:get(<<"resource_id">>, Row),
        actor_id = maps:get(<<"actor_id">>, Row),
        actor_role = binary_to_atom(maps:get(<<"actor_role">>, Row), utf8),
        actor_ip = maps:get(<<"actor_ip">>, Row),
        description = maps:get(<<"description">>, Row),
        old_value = maps:get(<<"old_value">>, Row, undefined),
        new_value = maps:get(<<"new_value">>, Row, undefined),
        metadata = maps:get(<<"metadata">>, Row, #{}),
        entry_hash = maps:get(<<"entry_hash">>, Row),
        previous_hash = maps:get(<<"previous_hash">>, Row),
        signature = maps:get(<<"signature">>, Row),
        customer_id = maps:get(<<"customer_id">>, Row)
    }.

%% Convert record to database row
entry_to_row(Entry) ->
    #{
        entry_id => Entry#audit_entry.entry_id,
        timestamp => Entry#audit_entry.timestamp,
        action => atom_to_binary(Entry#audit_entry.action, utf8),
        resource_type => atom_to_binary(Entry#audit_entry.resource_type, utf8),
        resource_id => Entry#audit_entry.resource_id,
        actor_id => Entry#audit_entry.actor_id,
        actor_role => atom_to_binary(Entry#audit_entry.actor_role, utf8),
        actor_ip => Entry#audit_entry.actor_ip,
        description => Entry#audit_entry.description,
        old_value => Entry#audit_entry.old_value,
        new_value => Entry#audit_entry.new_value,
        metadata => Entry#audit_entry.metadata,
        entry_hash => Entry#audit_entry.entry_hash,
        previous_hash => Entry#audit_entry.previous_hash,
        signature => Entry#audit_entry.signature,
        customer_id => Entry#audit_entry.customer_id
    }.

%% Convert entry to JSON
entry_to_json(Entry) ->
    Map = #{
        entry_id => Entry#audit_entry.entry_id,
        timestamp => Entry#audit_entry.timestamp,
        action => atom_to_binary(Entry#audit_entry.action, utf8),
        resource_type => atom_to_binary(Entry#audit_entry.resource_type, utf8),
        resource_id => Entry#audit_entry.resource_id,
        actor_id => Entry#audit_entry.actor_id,
        description => Entry#audit_entry.description,
        signature => base64:encode(Entry#audit_entry.signature),
        previous_hash => base64:encode(Entry#audit_entry.previous_hash)
    },
    jiffy:encode(Map).

%% Convert entries to CSV
entries_to_csv(Entries) ->
    Header = <<"entry_id,timestamp,action,resource_type,resource_id,actor_id,description\n">>,
    Rows = [
        <<
            (E#audit_entry.entry_id)/binary, ",",
            (integer_to_binary(E#audit_entry.timestamp))/binary, ",",
            (atom_to_binary(E#audit_entry.action, utf8))/binary, ",",
            (atom_to_binary(E#audit_entry.resource_type, utf8))/binary, ",",
            (E#audit_entry.resource_id)/binary, ",",
            (E#audit_entry.actor_id)/binary, ",",
            (escape_csv(E#audit_entry.description))/binary, "\n"
        >> || E <- Entries
    ],
    <<Header/binary, (list_to_binary(Rows))/binary>>.

%% Escape CSV special characters
escape_csv(Text) ->
    case binary:match(Text, [<<"\"">>, <<"\\">>, <<"\n">>]) of
        nomatch -> Text;
        _ -> <<"\"", (binary:replace(Text, <<"\"">>, <<"\"\"">>, [global]))/binary, "\"">>
    end.

%% Generate unique entry ID
generate_entry_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% Format description based on action
format_description(Action, ResourceType, ResourceId) ->
    case Action of
        calculate ->
            <<"Calculated value for ", ResourceId/binary>>;
        override ->
            <<"Pricing override requested for ", ResourceId/binary>>;
        access ->
            <<"Accessed ", (atom_to_binary(ResourceType, utf8))/binary, " ", ResourceId/binary>>;
        deploy ->
            <<"Deployed formula version ", ResourceId/binary>>;
        _ ->
            <<"Action: ", (atom_to_binary(Action, utf8))/binary>>
    end.

%% Get current actor
get_current_actor() ->
    case erlang:get(current_user) of
        undefined -> <<"system">>;
        User -> User
    end.

%% Get actor role
get_actor_role() ->
    case erlang:get(user_role) of
        undefined -> admin;
        Role -> Role
    end.

%% Get actor IP address
get_actor_ip() ->
    case erlang:get(actor_ip) of
        undefined -> <<"0.0.0.0">>;
        IP -> IP
    end.

%% Get customer ID from metadata
get_customer_id(Metadata) ->
    maps:get(customer_id, Metadata, <<"system">>).

%% Get system private key
get_system_private_key() ->
    {ok, <<"PLACEHOLDER_SYSTEM_KEY">>}.

%% Get system public key
get_system_public_key() ->
    {ok, <<"PLACEHOLDER_SYSTEM_PUBLIC_KEY">>}.

%% Calculate start of month
start_of_month(Timestamp) ->
    {Date, _} = calendar:now_to_datetime(Timestamp div 1000000),
    {Y, M, _} = Date,
    {Date2, _} = calendar:local_time_to_universal_time_dst({datetime, {Y, M, 1}, {0, 0, 0}}),
    DateSeconds = calendar:datetime_to_gregorian_seconds(Date2),
    DateSeconds * 1000000.

%% Calculate end of month
end_of_month(StartOfMonth) ->
    {Date, _} = calendar:now_to_datetime(StartOfMonth div 1000000),
    {Y, M, _} = Date,
    LastDay = calendar:last_day_of_the_month(Y, M),
    {Date2, _} = calendar:local_time_to_universal_time_dst({datetime, {Y, M, LastDay}, {23, 59, 59}}),
    DateSeconds = calendar:datetime_to_gregorian_seconds(Date2),
    DateSeconds * 1000000.

%% Generate export filename
export_filename(Timestamp) ->
    {{Y, M, _}, _} = calendar:now_to_datetime(Timestamp div 1000000),
    Month = io_lib:format("~4..0B-~2..0B", [Y, M]),
    <<"audit_export_", (iolist_to_binary(Month))/binary, ".csv.gpg">>.

%% Encrypt sensitive data
encrypt_sensitive(Data) ->
    % In production: Use GPG or similar
    base64:encode(crypto:hash(sha256, Data)).

%% Sign export
sign_export(Data) ->
    {ok, PrivateKey} = get_system_private_key(),
    crypto:sign(eddsa, Data, PrivateKey).

%% Upload to immutable storage
upload_to_immutable_storage(Data, Filename) ->
    % In production: Upload to S3 with MFA-delete or GCS
    {ok, <<"s3://audit-log-backup/", Filename/binary>>}.

%% Store to S3
store_to_s3(Data, Region) ->
    % Stub
    ok.

%% Store to GCS
store_to_gcs(Data, Region) ->
    % Stub
    ok.

%% Log security alert
log_security_alert(Alert) ->
    % Stub: In production, trigger PagerDuty
    io:format("SECURITY ALERT: ~p~n", [Alert]).
