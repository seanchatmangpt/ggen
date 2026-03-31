# AC Receipt Ledger MCP - Technical Specification

## Module: ac_receipt_ledger_mcp

**Type**: Erlang gen_statem state machine
**Behavior**: OTP compliant with handle_event_function callback mode
**Location**: `/Users/sac/ggen/tai-erlang-autonomics/pricing-engine/src/ac_receipt_ledger_mcp.erl`

## API Surface

### Type Definitions

```erlang
-type session_id() :: binary().
-type epoch() :: pos_integer().
-type seq() :: pos_integer().
-type hash() :: binary().
-type result(T) :: {ok, T} | {error, term()}.
-type receipt_kind() :: calculate_value | verify_receipt | rotate_epoch | export_ledger.
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
```

### Exported Functions (8 total)

#### 1. start_link/1
```erlang
-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
```
**Configuration**:
- `session_id` (optional): binary - Session identifier (auto-generated if missing)
- `disclaimer` (optional): string - Compliance disclaimer text

**Returns**:
- `{ok, Pid}` - Ledger process started
- `ignore` - Process initialization cancelled
- `{error, Reason}` - Startup failure

**Example**:
```erlang
{ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
    session_id => <<"session_123">>,
    disclaimer => "Advisory receipt - non-contractual"
}).
```

#### 2. append/3
```erlang
-spec append(receipt_kind(), term(), map()) -> result(receipt()).
```
**Parameters**:
- `Kind` (atom): Receipt type identifier
- `Payload` (term): Data to record (must be a valid term)
- `Meta` (map): Optional metadata

**Returns**:
- `{ok, Receipt}` - Receipt appended with hash chain proof
- `{error, {hash_failed, Reason}}` - Payload hashing failed

**Receipt Fields**:
```erlang
#{
    mode => eval,                          % Fixed
    authority => advisory,                 % Fixed
    disclaimer => string(),                % From config
    session_id => binary(),                % From session
    epoch => pos_integer(),                % Current epoch
    seq => pos_integer(),                  % Incremented
    kind => atom(),                        % From parameter
    prev => binary(),                      % Previous receipt hash
    payload_hash => binary(),              % SHA-256(payload)
    meta => map(),                         % From parameter
    hash => binary()                       % HMAC-SHA256(canonical || secret)
}
```

**Hash Computation**:
```
canonical_form = kind || epoch || seq || prev || payload_hash
base_hash = SHA256(canonical_form)
receipt_hash = HMAC-SHA256(base_hash, session_secret)
```

#### 3. rotate_epoch/2
```erlang
-spec rotate_epoch(string(), map()) -> result(hash()).
```
**Parameters**:
- `NewDisclaimer` (string): Updated compliance text
- `Options` (map): Rotation configuration (unused, for future extension)

**Returns**:
- `{ok, PreviousHeadHash}` - Hash of previous epoch's chain head
- `{error, Reason}` - Rotation failed

**Side Effects**:
- Increments epoch counter
- Resets seq to 0
- Clears receipts in current epoch (preserves in all_receipts)
- Updates disclaimer for new receipts
- Sets rotating flag to queue appends during transition

#### 4. export/1
```erlang
-spec export(map()) -> result(map()).
```
**Parameters**:
- `Options` (map): Export configuration (unused, for future extension)

**Returns**:
```erlang
{ok, #{
    session_id => binary(),
    current_epoch => pos_integer(),
    head_hash => binary(),
    receipts => [receipt()],              % Reversed (newest first)
    exported_at => integer(),             % erlang:system_time(millisecond)
    mode => advisory,
    authority => advisory
}}
```

**Non-Destructive**: Reading export does not modify ledger state

#### 5. head_hash/1
```erlang
-spec head_hash(atom()) -> result(hash()).
```
**Parameters**:
- `SessionIdOrModule` (atom): Module name (for call target)

**Returns**:
- `{ok, Hash}` - Current chain tip hash (empty binary if no receipts)
- `{error, ledger_not_started}` - Process not running

**Guarantees**: Returns hash of most recent receipt

#### 6. verify_chain/1
```erlang
-spec verify_chain(map()) -> result(ok).
```
**Parameters**:
- `Options` (map): Verification options (unused)

**Returns**:
- `{ok, ok}` - Chain is valid
- `{error, {chain_broken_at_seq, Seq, Details}}` - Prev hash mismatch
- `{error, {hash_mismatch_at_seq, Seq, Details}}` - Hash recomputation failed

**Non-Destructive**: Verification does not modify ledger

**Algorithm**:
```erlang
for receipt in all_receipts (oldest to newest):
  if receipt.prev != expected_prev:
    error(chain_broken_at_seq)
  if recompute_hash(receipt, session_secret) != receipt.hash:
    error(hash_mismatch_at_seq)
  expected_prev = receipt.hash
return ok
```

#### 7. get_receipts/2
```erlang
-spec get_receipts(session_id(), map()) -> result([receipt()]).
```
**Parameters**:
- `SessionId` (binary): Session identifier to filter
- `Options` (map): Filter options (future use)

**Returns**:
- `{ok, Receipts}` - List of receipts in current epoch
- `{error, Reason}` - Query failed

**Note**: Returns only receipts from current epoch; for all receipts use `export/1`

#### 8. get_session_id/1
```erlang
-spec get_session_id(atom()) -> result(session_id()).
```
**Parameters**:
- `Module` (atom): Module name (for call target)

**Returns**:
- `{ok, SessionId}` - Current session identifier
- `{error, ledger_not_started}` - Process not running

## State Machine Details

### Callback Mode
```erlang
callback_mode() -> handle_event_function.
```

### States
Only one state: `accepting`

### State Record
```erlang
-record(state, {
    session_id :: session_id(),            % Unique per ledger
    session_secret :: binary(),            % 32 bytes, ephemeral
    epoch = 1 :: epoch(),                  % Increments on rotation
    seq = 0 :: seq(),                      % Resets per epoch
    head_hash = <<>> :: hash(),            % Current chain tip
    receipts = [] :: [receipt()],          % Current epoch
    all_receipts = [] :: [receipt()],      % All epochs
    append_queue = [] :: list(),           % Queued appends during rotation
    rotating = false :: boolean(),         % Rotation in progress flag
    disclaimer = "" :: string()            % Compliance text
}).
```

### Event Handlers

#### {call, From} -> {append, Kind, Payload, Meta}
**Handler**: handle_event/4 (lines 114-155)

**Sequence**:
1. Validate payload is hashable
2. Hash payload with SHA-256
3. Compute receipt hash: HMAC-SHA256(canonical_form, session_secret)
4. Create receipt map
5. Update state (seq+1, new head_hash, add to receipts)
6. Reply to caller with receipt

**Time Complexity**: O(1) - single receipt creation
**Space Complexity**: O(1) - constant size receipt

#### {call, From} -> {rotate_epoch, Disclaimer, Options}
**Handler**: handle_event/4 (lines 157-193)

**Sequence**:
1. Set rotating flag
2. Process any queued appends
3. Increment epoch
4. Reset seq to 0
5. Clear receipts list
6. Update disclaimer
7. Reply with previous epoch's head hash

**Time Complexity**: O(Q) where Q = queued appends
**Space Complexity**: O(1) - no new receipts created

#### {call, From} -> {export, Options}
**Handler**: handle_event/4 (lines 195-220)

**Sequence**:
1. Collect all fields from state
2. Reverse receipts (newest first)
3. Create export map
4. Reply without modifying state

**Time Complexity**: O(N) where N = total receipts
**Space Complexity**: O(N) - export includes all receipts

#### {call, From} -> {head_hash, _}
**Handler**: handle_event/4 (lines 222-230)

**Returns**: Current head_hash from state

**Time Complexity**: O(1)

#### {call, From} -> {verify_chain, Options}
**Handler**: handle_event/4 (lines 232-250)

**Sequence**:
1. Call verify_receipt_chain/2
2. Walk all_receipts from oldest to newest
3. Check each receipt's prev link
4. Recompute hash and verify
5. Reply with result

**Time Complexity**: O(N) where N = total receipts
**Space Complexity**: O(1) - walk, don't copy

#### {call, From} -> {get_receipts, SessionId, Options}
**Handler**: handle_event/4 (lines 252-263)

**Returns**: Receipts from current epoch

**Time Complexity**: O(1) - return reference to state.receipts

#### {call, From} -> {get_session_id}
**Handler**: handle_event/4 (lines 265-271)

**Returns**: session_id from state

**Time Complexity**: O(1)

## Internal Functions

### hash_payload/1
```erlang
-spec hash_payload(term()) -> result(hash()).
```
**Implementation**:
```erlang
CanonicalBinary = term_to_binary(Payload),
Hash = crypto:hash(sha256, CanonicalBinary).
```

**Guarantees**: Deterministic (same term → same hash)

### compute_receipt_hash/6
```erlang
-spec compute_receipt_hash(hash(), hash(), epoch(), seq(), binary(), receipt_kind()) -> hash().
```

**Formula**:
```
canonical = kind || epoch || seq || prev || payload_hash
base = SHA256(canonical)
result = HMAC-SHA256(base, session_secret)
```

**Key Property**: Non-transferable without session_secret

### verify_receipt_chain/2
```erlang
-spec verify_receipt_chain([receipt()], binary()) -> result(ok).
```

**Algorithm**: See section 6 above

### generate_session_id/0
```erlang
-spec generate_session_id() -> session_id().
```

**Implementation**:
```erlang
RandomBytes = crypto:strong_rand_bytes(16),
base64:encode(RandomBytes).
```

## Cryptographic Properties

### Hash Chain
- Each receipt's `prev` field contains previous receipt's hash
- Forms unbroken chain from genesis (empty prev) to current
- Tamper detection: modifying any receipt breaks all subsequent links

### Session Secret
- 32 bytes of cryptographically-strong randomness
- Generated at ledger initialization
- NOT persisted (ephemeral)
- Used in HMAC for receipt hashing
- Makes receipts non-transferable across sessions

### Non-Contractual Property
- Receipt hash = HMAC-SHA256(base_hash, session_secret)
- Without session_secret, cannot independently verify hash
- Therefore, receipt is not independently verifiable
- Receipt explicitly marked with `authority => advisory`
- Suitable for advisory/non-binding evidence only

## Error Handling

### Pattern: Result<T, E>

All public functions return:
```erlang
{ok, Value} | {error, Reason}
```

**No unwrap/expect/panic equivalents** - All error paths explicit

### Error Types

| Error | Context | Recovery |
|-------|---------|----------|
| `ledger_not_started` | head_hash/1, get_session_id/1 | Start ledger with start_link/1 |
| `{hash_failed, Reason}` | append/3 | Validate payload is valid term |
| `{chain_broken_at_seq, Seq, Details}` | verify_chain/1 | Investigate corruption source |
| `{hash_mismatch_at_seq, Seq, Details}` | verify_chain/1 | Tampering suspected |
| `{queue_processing_failed, Reason}` | rotate_epoch/2 | Retry rotation after fix |

## Performance Characteristics

### Time Complexity

| Operation | Best | Average | Worst |
|-----------|------|---------|-------|
| append/3 | O(1) | O(1) | O(1) |
| rotate_epoch/2 | O(1) | O(Q) | O(Q) |
| export/1 | O(1) | O(N) | O(N) |
| head_hash/1 | O(1) | O(1) | O(1) |
| verify_chain/1 | O(N) | O(N) | O(N) |
| get_session_id/1 | O(1) | O(1) | O(1) |

Where: N = total receipts, Q = queued appends

### Space Complexity

| Operation | Storage |
|-----------|---------|
| Per receipt | ~500 bytes |
| Per session | ~1 KB |
| Typical ledger (1000 receipts) | ~500 KB |

### SLO Targets

| Operation | Target | Assumption |
|-----------|--------|-----------|
| Single append | <1 ms | Modern CPU, no GC pause |
| Chain verification | <N ms | Linear walk, N = 1000 |
| Epoch rotation | <10 ms | No queued appends |
| Export | <N/10 ms | Serialization |

## Thread Safety

**Model**: Erlang process with gen_statem

- All operations go through single process (serialized)
- No explicit locking needed
- Concurrent calls are queued and processed sequentially
- gen_statem ensures call/cast ordering

## Initialization Sequence

```erlang
start_link(Config)
  ↓
init(Config)
  - SessionId = generate or use from Config
  - SessionSecret = crypto:strong_rand_bytes(32)
  - Disclaimer = from Config or default
  - State = #state{...}
  ↓
{ok, accepting, State}
```

## Termination

```erlang
terminate(_Reason, _StateName, _State)
  - No special cleanup (no file handles, ports, etc.)
  - Returns ok
  - Process terminates
```

## Code Change / Hot Reload

```erlang
code_change(_OldVsn, StateName, State, _Extra)
  - Returns {ok, StateName, State}
  - No state transformation needed
```

## Testing Hooks

### Exported Test Utilities

```erlang
-spec test_merkle_properties(map()) -> boolean().
-spec check_receipt_ordering([receipt()]) -> boolean().
-spec check_receipt_links([receipt()]) -> boolean().
```

Used in EUnit and Common Test suites for property verification.

## Integration Points

### With pricing_engine.erl

```erlang
%% In calculate_value handler
{ok, Receipt} = ac_receipt_ledger_mcp:append(
    calculate_value,
    ValueRecord,
    #{timestamp => erlang:system_time(millisecond)}
).
```

### With pricing_security.erl

```erlang
%% In verify_receipt_authenticity
{ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),
ok = verify_hmac_signature(Receipt, HmacKey).
```

### With receipt_generator.erl

```erlang
%% In export_audit_trail
{ok, Ledger} = ac_receipt_ledger_mcp:export(#{}),
ExportedReceipts = maps:get(receipts, Ledger).
```

## Configuration Schema

```erlang
Config :: #{
    session_id => binary(),      % Optional
    disclaimer => string()       % Optional
}.
```

## Message Queue Behavior

### During Normal Operation
- Incoming append calls are queued in gen_statem
- Each is processed sequentially
- Head hash updated after each append

### During Epoch Rotation
- rotating flag set to true
- New appends are queued in append_queue
- After rotation completes, queued appends are replayed
- rotating flag cleared

## Known Limitations

1. **Single-Node Only**: No distributed consensus
2. **Session-Scoped**: Cannot verify receipts across sessions
3. **Non-Persistent**: Session secret is ephemeral (no recovery)
4. **Linear Chain**: No merkle tree proofs (always return full chain)
5. **No Timestamps**: No built-in timestamp verification (application responsibility)

## Future Enhancements

1. Merkle tree proofs (instead of full chain)
2. Persistent storage with recovery
3. Distributed ledger support (consensus)
4. Cross-session verification
5. Hardware security module integration

## References

- **Merkle Trees**: [Wikipedia](https://en.wikipedia.org/wiki/Merkle_tree)
- **HMAC**: [RFC 2104](https://tools.ietf.org/html/rfc2104)
- **Erlang gen_statem**: [Erlang Docs](https://erlang.org/doc/man/gen_statem.html)
- **Crypto Module**: [Erlang Docs](https://erlang.org/doc/man/crypto.html)

---

**Version**: 1.0.0
**Date**: 2026-01-26
**Status**: Production Ready
