# AC Receipt Ledger MCP - Session-Scoped Receipt Management

## Overview

The `ac_receipt_ledger_mcp` module implements a **session-scoped, non-contractual receipt ledger** using a `gen_statem` state machine. It provides cryptographically-secured receipt management with merkle chain verification, epoch rotation, and session isolation.

## Key Concepts

### 1. Non-Contractual Receipts

Receipts are **explicitly advisory** (non-contractual) because:
- Each session has a **unique, ephemeral session secret** (32 random bytes)
- This secret is **NOT persisted** and is **NOT shared**
- Receipt hashes are HMAC-SHA256 with the session secret
- Without the session secret, the receipt hash cannot be independently verified
- This prevents the receipt from being used as a contractual proof

### 2. Session-Scoped Security

Each ledger instance:
- Generates a unique `SessionId` (base64-encoded random bytes)
- Creates a session secret for HMAC operations
- Maintains complete isolation from other sessions
- All receipts include the session ID for traceability

### 3. Merkle Chain Verification

Receipts form a cryptographic chain:
- Each receipt's `prev` field contains the hash of the previous receipt
- This creates a **hash chain** preventing retroactive tampering
- Verification walks the chain from oldest to newest, checking each link
- If any receipt is modified, the `hash` field no longer matches

### 4. Epoch Rotation

Support for epoch boundaries:
- Each epoch starts with a new merkle chain (empty `prev`)
- Epoch rotation creates a **new chain** (non-retroactive tampering)
- Previous epochs' receipts are preserved in history
- Disclaimer can be updated on rotation

## Data Structure

### Receipt Map

```erlang
#{
  mode => eval,                           % Transaction mode
  authority => advisory,                  % Non-contractual authority
  disclaimer => string(),                 % Compliance disclaimer
  session_id => binary(),                 % Session identifier
  epoch => pos_integer(),                 % Epoch number (1, 2, 3, ...)
  seq => pos_integer(),                   % Sequence within epoch (1, 2, 3, ...)
  kind => atom(),                         % Receipt type (calculate_value, verify_receipt, rotate_epoch, export_ledger)
  prev => binary(),                       % SHA-256 hash of previous receipt
  payload_hash => binary(),               % SHA-256(payload) - deterministic proof
  meta => map(),                          % Optional metadata
  hash => binary()                        % HMAC-SHA256(canonical_form || session_secret)
}
```

### Receipt Fields Explained

| Field | Type | Purpose |
|-------|------|---------|
| `mode` | atom | Fixed to `eval` - indicates evaluation/advisory mode |
| `authority` | atom | Fixed to `advisory` - indicates non-contractual |
| `disclaimer` | string | Compliance text stating non-contractual nature |
| `session_id` | binary | Session identifier (unique per ledger instance) |
| `epoch` | integer | Epoch number (increments on rotation) |
| `seq` | integer | Sequence number within epoch (resets on rotation) |
| `kind` | atom | Type of receipt (e.g., `calculate_value`, `verify_receipt`) |
| `prev` | binary | Previous receipt's hash (empty for first receipt or epoch start) |
| `payload_hash` | binary | SHA-256 hash of the payload (deterministic) |
| `meta` | map | Application-specific metadata |
| `hash` | binary | HMAC-SHA256 with session secret (non-transferable proof) |

## State Machine

### States

```
Starting
   ↓
Accepting (normal operation)
   ↓
   ├─→ append(Kind, Payload, Meta) → Accepting
   │   - Increments seq
   │   - Computes hash with session secret
   │   - Links to previous receipt
   │
   └─→ rotate_epoch(NewDisclaimer, Options) → Rotating → Accepting
       - Returns previous epoch's head hash
       - Resets seq to 0
       - Initializes new merkle chain
```

### State Transitions

1. **Append**: Normal operation in `accepting` state
   - Adds receipt to ledger
   - Links via merkle chain
   - Increments sequence number

2. **Rotate Epoch**: Transition to new chain
   - Queues any concurrent appends
   - Returns previous epoch's head hash
   - Resets sequence to 0
   - Updates disclaimer

3. **Export**: Generate audit trail (non-destructive)
   - Captures all receipts across all epochs
   - Includes session ID and current head hash
   - Returns map suitable for auditing

4. **Verify Chain**: Integrity validation (non-destructive)
   - Walks merkle chain from oldest to newest
   - Recomputes each receipt's hash
   - Detects any tampering

## API Reference

### Starting the Ledger

```erlang
%% Start with default config
{ok, Pid} = ac_receipt_ledger_mcp:start_link(#{
    disclaimer => "Advisory receipt - non-contractual"
}).

%% Start with custom session ID
{ok, Pid} = ac_receipt_ledger_mcp:start_link(#{
    session_id => <<"my_session_123">>,
    disclaimer => "Custom disclaimer"
}).
```

### Appending Receipts

```erlang
%% Simple append
{ok, Receipt} = ac_receipt_ledger_mcp:append(
    calculate_value,
    #{customer_id => <<"cust_123">>, value => 42.5},
    #{source => pricing_engine}
).

%% Receipt is a map with hash, epoch, seq, etc.
```

### Rotating Epochs

```erlang
%% Rotate with new disclaimer
{ok, PreviousHeadHash} = ac_receipt_ledger_mcp:rotate_epoch(
    "Updated disclaimer after policy change",
    #{reason => policy_update}
).

%% PreviousHeadHash is the tip of the previous epoch's chain
```

### Exporting for Audit

```erlang
%% Export entire ledger
{ok, AuditTrail} = ac_receipt_ledger_mcp:export(#{}).

%% AuditTrail contains:
%% #{
%%   session_id => binary(),
%%   current_epoch => integer(),
%%   head_hash => binary(),
%%   receipts => [Receipt1, Receipt2, ...],
%%   exported_at => integer(),
%%   mode => advisory,
%%   authority => advisory
%% }
```

### Verifying Chain Integrity

```erlang
%% Verify chain is unbroken
{ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}).

%% Or check specific receipts
{ok, Receipts} = ac_receipt_ledger_mcp:get_receipts(SessionId, #{}).
```

### Getting Session Information

```erlang
%% Get current session ID
{ok, SessionId} = ac_receipt_ledger_mcp:get_session_id(ac_receipt_ledger_mcp).

%% Get current chain head
{ok, HeadHash} = ac_receipt_ledger_mcp:head_hash(ac_receipt_ledger_mcp).
```

## Usage Example: Complete Workflow

```erlang
%% 1. Start ledger
{ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
    disclaimer => "Advisory receipt - non-contractual pricing evidence"
}).

%% 2. Record calculations as receipts
{ok, R1} = ac_receipt_ledger_mcp:append(
    calculate_value,
    #{
        customer_id => <<"cust_001">>,
        metrics => [{<<"cpu">>, 45.2}, {<<"memory">>, 62.3}],
        calculated_value => 42.5,
        billed_price => 4250.0
    },
    #{
        calculation_method => weighted_sum,
        billing_period => monthly
    }
).

io:format("Receipt 1: ~p~n", [R1]).

%% 3. Add more calculations (forms merkle chain)
{ok, R2} = ac_receipt_ledger_mcp:append(
    verify_receipt,
    #{
        receipt_id => maps:get(hash, R1),
        status => verified,
        timestamp => erlang:system_time(millisecond)
    },
    #{verification_method => hmac}
).

%% 4. Verify chain integrity
{ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),
io:format("Chain is valid~n").

%% 5. Rotate epoch at billing period boundary
{ok, PrevHeadHash} = ac_receipt_ledger_mcp:rotate_epoch(
    "Updated disclaimer after policy review",
    #{reason => monthly_rollover}
),
io:format("Previous epoch head: ~p~n", [PrevHeadHash]).

%% 6. Continue recording in new epoch
{ok, R3} = ac_receipt_ledger_mcp:append(
    calculate_value,
    #{
        customer_id => <<"cust_001">>,
        metrics => [{<<"cpu">>, 48.1}, {<<"memory">>, 65.2}],
        calculated_value => 44.2,
        billed_price => 4420.0
    },
    #{calculation_method => weighted_sum}
).

%% 7. Export for audit trail
{ok, Ledger} = ac_receipt_ledger_mcp:export(#{}),
io:format("Total receipts: ~p~n", [length(maps:get(receipts, Ledger))]),
io:format("Current epoch: ~p~n", [maps:get(current_epoch, Ledger)]).

%% 8. Archive export for compliance (e.g., to file, database)
%% In production, this would be persisted to immutable storage
```

## Merkle Chain Verification Details

### Hash Computation

Each receipt's hash is computed as:

```
canonical_form = kind || epoch || seq || prev_hash || payload_hash
base_hash = SHA256(canonical_form)
receipt_hash = HMAC-SHA256(base_hash, session_secret)
```

### Verification Algorithm

```
for receipt in chain:
  expected_prev = previous_receipt.hash or <<>> for first
  actual_prev = receipt.prev

  if actual_prev != expected_prev:
    return error(chain_broken)

  recomputed = HMAC-SHA256(
    SHA256(receipt.kind || receipt.epoch || receipt.seq ||
           receipt.prev || receipt.payload_hash),
    session_secret
  )

  if recomputed != receipt.hash:
    return error(hash_mismatch)

  previous_receipt = receipt

return ok
```

## Security Properties

### What This Protects Against

1. **Retroactive Tampering**: Hash chain makes any modification detectable
2. **Cross-Session Mixing**: Session secrets prevent mixing receipts between sessions
3. **Signature Forgery**: HMAC prevents creating valid receipts without session secret
4. **Payload Fabrication**: Payload hash is deterministic (same input → same hash)

### What This Does NOT Protect Against

1. **Session Secret Exposure**: If session secret is compromised, receipts can be forged
2. **Contractual Enforcement**: Receipts are explicitly non-contractual (no persistence of proof)
3. **Timestamped History**: No built-in timestamp verification (application responsibility)
4. **Distributed Agreement**: Single-node ledger (no consensus mechanism)

## Error Handling

All operations return `Result<T, E>` pattern:

```erlang
%% Success case
{ok, Receipt} = ac_receipt_ledger_mcp:append(Kind, Payload, Meta).

%% Error cases
{error, {hash_failed, Reason}} = ac_receipt_ledger_mcp:append(...).
{error, {chain_broken_at_seq, Seq, Details}} = ac_receipt_ledger_mcp:verify_chain(...).
```

### Common Errors

| Error | Cause | Recovery |
|-------|-------|----------|
| `hash_failed` | Payload cannot be hashed (invalid term) | Fix payload structure |
| `chain_broken_at_seq` | Merkle chain link is invalid | Investigate source of corruption |
| `hash_mismatch_at_seq` | Receipt hash doesn't match recomputed | Potential tampering detected |
| `queue_processing_failed` | Queued append failed during rotation | Retry after rotation completes |

## Testing

### Unit Tests

Located in `/pricing-engine/test/ac_receipt_ledger_mcp_tests.erl`:

```bash
rebar3 eunit -m ac_receipt_ledger_mcp_tests
```

Tests include:
- Receipt creation and hashing
- Merkle chain linking
- Epoch rotation
- Session isolation
- Export and auditing

### Integration Tests

Located in `/pricing-engine/test/ac_receipt_ledger_mcp_integration_SUITE.erl`:

```bash
rebar3 ct --suite ac_receipt_ledger_mcp_integration_SUITE
```

Tests include:
- Multi-epoch workflows
- Concurrent appends
- Chain verification
- Disclaimer updates

## Performance Characteristics

### SLO Targets

- **Single append**: <1ms (SHA-256 + HMAC operations)
- **Chain verification** (N receipts): <N ms (linear walk)
- **Epoch rotation**: <10ms (metadata update)
- **Export** (N receipts): <N/10 ms (serialization)

### Memory Usage

- **Per receipt**: ~500 bytes (hash, metadata, pointers)
- **Session overhead**: ~1KB (session ID, secret, state)
- **Typical ledger** (1000 receipts): ~500KB

## Integration with Pricing Engine

### Append on Value Calculation

```erlang
%% In pricing_engine.erl
handle_event({call, From}, {calculate_value, ...}, State) ->
    %% Calculate value
    {ok, ValueRecord} = calculate_value_impl(...),

    %% Record in receipt ledger
    {ok, Receipt} = ac_receipt_ledger_mcp:append(
        calculate_value,
        ValueRecord,
        #{
            calculation_method => weighted_sum,
            timestamp => erlang:system_time(millisecond)
        }
    ),

    %% Continue with pricing
    ...
```

### Verify on Receipt Request

```erlang
%% In pricing_security.erl
verify_receipt_authenticity(Receipt, HmacKey, _Options) ->
    %% Verify receipt is in valid merkle chain
    case ac_receipt_ledger_mcp:verify_chain(#{}) of
        {ok, ok} ->
            %% Receipt is part of unbroken chain
            verify_hmac_signature(Receipt, HmacKey);
        {error, Reason} ->
            {error, {chain_verification_failed, Reason}}
    end.
```

## Compliance and Audit Trail

### Required Documentation

When using this ledger for billing/pricing:

1. **Display Disclaimer**: Every receipt must show the disclaimer ("Advisory, non-contractual")
2. **Export Trail**: Export ledger periodically for audit purposes
3. **Session Documentation**: Track session IDs for reconciliation
4. **Epoch Records**: Document when epochs rotated and why

### Sample Audit Export

```erlang
{ok, Export} = ac_receipt_ledger_mcp:export(#{}),

AuditReport = #{
    exported_at => maps:get(exported_at, Export),
    session_id => maps:get(session_id, Export),
    total_receipts => length(maps:get(receipts, Export)),
    total_epochs => maps:get(current_epoch, Export),
    chain_head => maps:get(head_hash, Export),
    first_receipt => hd(maps:get(receipts, Export)),
    last_receipt => lists:last(maps:get(receipts, Export))
}.

%% Persist to audit log
audit_log:record(pricing_audit, AuditReport).
```

## Troubleshooting

### Chain Verification Fails

1. Check receipt ordering (should be monotonic by epoch/seq)
2. Verify session secret hasn't been compromised
3. Inspect error details for specific seq that failed
4. Export ledger and verify via external tool

### Missing Receipts

1. Check if epoch rotation occurred (receipts may be in previous epoch)
2. Verify session ID matches (receipts are session-scoped)
3. Check time range if filtering by timestamp

### Hash Mismatches

1. Ensure payload is consistently formatted (use `term_to_binary`)
2. Verify session secret is stable during verification
3. Check for concurrent modification (should use gen_statem calls)

## Future Enhancements

1. **Timestamped Verification**: Add monotonic timestamps to detect time-travel attacks
2. **Merkle Tree Proofs**: Generate membership proofs without full chain
3. **Cross-Session Verification**: Support verifying receipts across session boundaries
4. **Persistent Storage**: Optional storage backend (RocksDB, Firestore)
5. **Distributed Ledger**: Support consensus-based verification across nodes
6. **Hardware Signing**: Integration with HSM for session secret management

## References

- **Merkle Trees**: https://en.wikipedia.org/wiki/Merkle_tree
- **HMAC Security**: https://tools.ietf.org/html/rfc2104
- **Erlang gen_statem**: https://erlang.org/doc/man/gen_statem.html
- **Cryptography in Erlang**: https://erlang.org/doc/man/crypto.html

## License and Disclaimer

This module is part of the TAI Autonomic System and is provided "AS-IS" for advisory purposes only. Receipts generated by this module are non-contractual and should not be used as sole evidence of transactions. Consult with legal counsel before using in regulated industries.

---

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Maintainer**: TAI Autonomic System Team
