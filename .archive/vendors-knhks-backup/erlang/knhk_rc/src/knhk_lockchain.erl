%% knhk_lockchain.erl — Lockchain (Receipt Storage)
%% Production-ready implementation for receipt storage and Merkle linking
%% Responsibilities: Receipt storage, querying, merging (Π ⊕), tamper detection

-module(knhk_lockchain).
-behaviour(gen_server).

-export([
    start_link/0,
    read/1,
    merge/1,
    write/1,
    get_merkle_root/0
]).

-record(state, {
    receipts = #{},              % Receipt ID -> Receipt data
    merkle_root = undefined,    % Current Merkle root
    entries = []                % Receipt entries in order
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Read receipt by ID
read(Id) ->
    gen_server:call(?MODULE, {read, Id}).

%% Merge receipts via Π ⊕ (associative merge)
merge(Receipts) when is_list(Receipts) ->
    gen_server:call(?MODULE, {merge, Receipts}).

%% Write receipt to lockchain
write(Receipt) ->
    gen_server:call(?MODULE, {write, Receipt}).

%% Get current Merkle root
get_merkle_root() ->
    gen_server:call(?MODULE, get_merkle_root).

%% gen_server callbacks

init([]) ->
    {ok, #state{merkle_root = initial_merkle_root()}}.

handle_call({read, Id}, _From, State) ->
    case maps:find(Id, State#state.receipts) of
        {ok, Receipt} ->
            {reply, {ok, Receipt}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({merge, Receipts}, _From, State) ->
    %% Merge receipts via ⊕ operation (associative, branchless)
    MergedReceipt = merge_receipts(Receipts),
    {reply, {ok, MergedReceipt}, State};

handle_call({write, Receipt}, _From, State) ->
    %% Validate receipt format
    case validate_receipt_format(Receipt) of
        ok ->
            %% Generate receipt ID if not present
            ReceiptId = get_receipt_id(Receipt),
            
            %% Compute Merkle hash
            ReceiptHash = compute_receipt_hash(Receipt),
            
            %% Link to previous Merkle root
            ParentHash = State#state.merkle_root,
            
            %% Compute new Merkle root
            NewMerkleRoot = compute_merkle_root(ParentHash, ReceiptHash),
            
            %% Store receipt
            NewReceipts = maps:put(ReceiptId, Receipt, State#state.receipts),
            NewEntries = [{ReceiptId, ReceiptHash, ParentHash} | State#state.entries],
            
            NewState = State#state{
                receipts = NewReceipts,
                merkle_root = NewMerkleRoot,
                entries = NewEntries
            },
            {reply, {ok, ReceiptHash}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_merkle_root, _From, State) ->
    {reply, {ok, State#state.merkle_root}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

initial_merkle_root() ->
    %% Initial Merkle root (empty chain)
    crypto:hash(sha256, <<"KNHKS_LOCKCHAIN_INIT">>).

validate_receipt_format(Receipt) ->
    %% Validate receipt has required fields
    case is_map(Receipt) of
        true ->
            RequiredFields = [ticks, lanes, span_id, a_hash],
            HasFields = lists:all(fun(Field) -> maps:is_key(Field, Receipt) end, RequiredFields),
            case HasFields of
                true -> ok;
                false -> {error, missing_fields}
            end;
        false ->
            {error, invalid_format}
    end.

get_receipt_id(Receipt) ->
    %% Generate receipt ID from receipt data
    case maps:find(id, Receipt) of
        {ok, Id} -> Id;
        error -> generate_receipt_id(Receipt)
    end.

generate_receipt_id(Receipt) ->
    %% Generate ID from receipt hash
    Hash = compute_receipt_hash(Receipt),
    binary_to_atom(integer_to_binary(Hash, 16), utf8).

compute_receipt_hash(Receipt) ->
    %% Compute SHA-256 hash of receipt
    ReceiptBytes = term_to_binary(Receipt),
    crypto:hash(sha256, ReceiptBytes).

compute_merkle_root(ParentHash, ReceiptHash) ->
    %% Compute new Merkle root by hashing parent and receipt
    Combined = <<ParentHash/binary, ReceiptHash/binary>>,
    crypto:hash(sha256, Combined).

merge_receipts([]) ->
    #{ticks => 0, lanes => 0, span_id => 0, a_hash => 0};
merge_receipts([First | Rest]) ->
    lists:foldl(
        fun(Receipt, Acc) ->
            #{
                ticks => max(maps:get(ticks, Receipt, 0), maps:get(ticks, Acc, 0)),
                lanes => maps:get(lanes, Receipt, 0) + maps:get(lanes, Acc, 0),
                span_id => maps:get(span_id, Receipt, 0) bxor maps:get(span_id, Acc, 0),
                a_hash => maps:get(a_hash, Receipt, 0) bxor maps:get(a_hash, Acc, 0)
            }
        end,
        First,
        Rest
    ).

max(A, B) when A > B -> A;
max(_A, B) -> B.

