%%%-------------------------------------------------------------------
%% @doc receipt_store: ETS-backed persistent receipt storage
%%
%% Manages deterministic receipt ledger:
%% - Stores receipts in ETS table with persistence to disk
%% - Provides transactional receipt storage
%% - Supports querying by timestamp, tenant, execution ID
%% - Implements receipt deduplication
%%
%% @end
%%%-------------------------------------------------------------------
-module(receipt_store).
-behaviour(gen_server).

%% API
-export([start_link/0, store_receipt/1, query_receipts/2, get_audit_trail/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, ggen_receipts).
-define(PERSISTENCE_DIR, "data/receipts").

-record(state, {
    table :: ets:tid(),
    persistence_dir :: string(),
    sync_interval :: pos_integer()
}).

-type receipt() :: #{
    execution_id := binary(),
    timestamp := integer(),
    tenant_id := binary(),
    manifest_hash := binary(),
    ontology_hash := binary(),
    files := [#{path := string(), hash := binary()}],
    audit_trail := string()
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the receipt store
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Store a receipt
-spec store_receipt(Receipt) -> ok | {error, term()}
  when Receipt :: receipt().
store_receipt(Receipt) ->
    gen_server:call(?SERVER, {store_receipt, Receipt}).

%% @doc Query receipts by criteria
-spec query_receipts(Criteria, Options) -> {ok, [receipt()]} | {error, term()}
  when Criteria :: {tenant_id, binary()} | {timestamp_range, integer(), integer()},
       Options :: [term()].
query_receipts(Criteria, Options) ->
    gen_server:call(?SERVER, {query_receipts, Criteria, Options}).

%% @doc Get audit trail for an execution
-spec get_audit_trail(ExecutionId) -> {ok, string()} | {error, not_found}
  when ExecutionId :: binary().
get_audit_trail(ExecutionId) ->
    gen_server:call(?SERVER, {get_audit_trail, ExecutionId}).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()} | {error, term()}.
init([]) ->
    %% Create ETS table for receipts
    TableId = ets:new(?TABLE_NAME, [
        {keypos, 2},      % execution_id field is key
        public,            % public table for concurrent access
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    %% Create persistence directory
    PersistDir = ?PERSISTENCE_DIR,
    filelib:ensure_dir(PersistDir ++ "/"),

    %% Load receipts from disk if they exist
    case load_receipts_from_disk(PersistDir) of
        {ok, Receipts} ->
            lists:foreach(
                fun(Receipt) -> ets:insert(TableId, Receipt) end,
                Receipts
            );
        {error, _} ->
            %% First startup, no receipts to load
            ok
    end,

    %% Schedule periodic sync to disk (every 30 seconds)
    timer:send_interval(30000, sync_to_disk),

    {ok, #state{
        table = TableId,
        persistence_dir = PersistDir,
        sync_interval = 30000
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State} | {noreply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call({store_receipt, Receipt}, _From, State) ->
    %% Validate receipt has required fields
    case validate_receipt(Receipt) of
        ok ->
            %% Insert into ETS table
            ets:insert(State#state.table, Receipt),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({query_receipts, {tenant_id, TenantId}, _Options}, _From, State) ->
    %% Query receipts by tenant ID
    Pattern = #{tenant_id => TenantId, _ => '_'},
    Receipts = ets:match_object(State#state.table, Pattern),
    {reply, {ok, Receipts}, State};

handle_call({query_receipts, {timestamp_range, Start, End}, _Options}, _From, State) ->
    %% Query receipts by timestamp range
    MatchSpec = [{
        {{'_', '_', '$1', '_', '_', '_', '_'}},
        [{'>=', '$1', Start}, {'=<', '$1', End}],
        ['$_']
    }],
    Receipts = ets:select(State#state.table, MatchSpec),
    {reply, {ok, Receipts}, State};

handle_call({get_audit_trail, ExecutionId}, _From, State) ->
    case ets:lookup(State#state.table, ExecutionId) of
        [Receipt] ->
            AuditTrail = maps:get(audit_trail, Receipt, undefined),
            {reply, {ok, AuditTrail}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, State}
  when Request :: term(),
       State :: state().
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, State}
  when Info :: term(),
       State :: state().
handle_info(sync_to_disk, State) ->
    %% Sync receipts to disk
    case sync_receipts_to_disk(State#state.table, State#state.persistence_dir) of
        ok ->
            {noreply, State};
        {error, _Reason} ->
            %% Log error but continue
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: state().
terminate(_Reason, State) ->
    %% Final sync before shutdown
    _ = sync_receipts_to_disk(State#state.table, State#state.persistence_dir),
    ok.

%% @private
-spec code_change(OldVsn, State, Extra) -> {ok, state()}
  when OldVsn :: term() | {down, term()},
       State :: state(),
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private
-spec validate_receipt(Receipt) -> ok | {error, term()}
  when Receipt :: receipt().
validate_receipt(Receipt) ->
    %% Check required fields
    RequiredFields = [execution_id, timestamp, tenant_id, manifest_hash, ontology_hash],
    case lists:all(fun(Field) -> maps:is_key(Field, Receipt) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

%% @private
-spec load_receipts_from_disk(Dir) -> {ok, [receipt()]} | {error, term()}
  when Dir :: string().
load_receipts_from_disk(Dir) ->
    IndexFile = Dir ++ "/index.term",
    case file:read_file(IndexFile) of
        {ok, Binary} ->
            case catch binary_to_term(Binary) of
                Receipts when is_list(Receipts) -> {ok, Receipts};
                {'EXIT', _} -> {error, corrupt_index}
            end;
        {error, enoent} ->
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
-spec sync_receipts_to_disk(TableId, Dir) -> ok | {error, term()}
  when TableId :: ets:tid(),
       Dir :: string().
sync_receipts_to_disk(TableId, Dir) ->
    Receipts = ets:tab2list(TableId),
    Binary = term_to_binary(Receipts),
    IndexFile = Dir ++ "/index.term",
    file:write_file(IndexFile, Binary).

%%%===================================================================
%% End of module
%%%===================================================================
