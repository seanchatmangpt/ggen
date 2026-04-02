# Eval Mode - Quick Reference Guide

## One-Minute Overview

The `ac_eval_mode` module enforces that all pricing calculations are **advisory only**, never contractual. It does this by:

1. **Hardcoding** mode as `eval` (immutable, cannot change at runtime)
2. **Stamping** all outputs with `advisory` authority
3. **Injecting** disclaimers in every response
4. **Generating** unique session secrets that invalidate receipts
5. **Verifying** at startup that production mode is not configured

## Core API Cheat Sheet

### Startup (Called Once)

```erlang
%% Verify eval mode before starting service
case ac_eval_mode:ensure_eval() of
    ok -> start_service();
    {error, not_eval_mode} -> fail("Must run in eval mode")
end.
```

### Per-Request Flow

```erlang
%% 1. Start session (unique per request)
{ok, SessionId, _Secret} = ac_eval_mode:start_session(#{
    request_id => RequestId,
    client_id => ClientId
}),

%% 2. Your calculation logic here...
{ok, ValueRecord} = pricing_engine:calculate_value(...),

%% 3. Decorate before returning
{ok, Decorated} = ac_eval_mode:decorate_payload(ValueRecord),

%% 4. Always cleanup
ok = ac_eval_mode:end_session(SessionId).
```

### Decorate Outputs (3 Types)

```erlang
%% Decorate value calculations or any map
{ok, DecoratedValue} = ac_eval_mode:decorate_payload(#{
    <<"customer_id">> => <<"cust_123">>,
    <<"value">> => 100.0
}),
% Result: {..., eval_only: true, authority: advisory, disclaimer: "...", ...}

%% Decorate API response metadata
{ok, DecoratedMeta} = ac_eval_mode:decorate_meta(
    #{<<"status">> => <<"success">>},
    #{request_id => RequestId}
),
% Result: {..., mode: eval, authority: advisory, ...}

%% Decorate receipts (mark as non-contractual)
{ok, DecoratedReceipt} = ac_eval_mode:decorate_receipt(Receipt),
% Result: {..., eval_only: true, non_contractual: true, use_for_billing_prohibited: true, ...}
```

### Query Eval Mode

```erlang
%% Get current mode
eval = ac_eval_mode:mode(),

%% Get authority level
advisory = ac_eval_mode:authority(),

%% Get disclaimer banner
Banner = ac_eval_mode:banner(),
% <<"This pricing calculation is ADVISORY ONLY. Not a legal contract...">>
```

## Decorated Payload Example

**Before**:
```erlang
ValueRecord = #value_record{
    customer_id = <<"cust_123">>,
    calculated_value = 100.0,
    billed_price = 10000.0,
    timestamp = 1705000000000,
    metrics = [{<<"throughput">>, 100.0}],
    ...
}
```

**After `decorate_payload/1`**:
```erlang
#{
    <<"customer_id">> => <<"cust_123">>,
    <<"calculated_value">> => 100.0,
    <<"billed_price">> => 10000.0,
    <<"timestamp">> => 1705000000000,
    <<"metrics">> => [{<<"throughput">>, 100.0}],

    %% Added by decorator:
    <<"eval_only">> => true,
    <<"authority">> => advisory,
    <<"disclaimer">> => <<"This pricing calculation is ADVISORY ONLY...">>,
    <<"session_id">> => <<"8f47e8e5-7c9c-4e8a-b8d7-9e8f9c8f9c8f">>,
    <<"session_hash">> => <<...32 bytes HMAC-SHA256...>>,
    <<"session_timestamp">> => 1705000000100,
    ...
}
```

## Why Session Secrets Make Receipts Non-Contractual

```
Normal Receipt (could be forged):
  hash = SHA256(customer_id, value, date)
  → Anyone with the formula can reproduce hash
  → Receipt can be fabricated for billing

Eval-Mode Receipt (cannot be forged):
  hash = SHA256(customer_id, value, date)
  session_hash = HMAC-SHA256(session_id, SESSION_SECRET)
  → SESSION_SECRET is unique per session (32 random bytes)
  → SESSION_SECRET never persisted anywhere (only in RAM during session)
  → SESSION_SECRET erased on session end (erase/3)
  → Without SESSION_SECRET, session_hash cannot be reproduced
  → Receipt cannot be verified as authentic
  → Therefore, receipt is non-contractual
```

## Error Handling

All functions return `{ok, Result} | {error, Reason}`. Never panic:

```erlang
%% Good: Handle error
case ac_eval_mode:start_session(Options) of
    {ok, SessionId, Secret} -> handle_request(SessionId);
    {error, Reason} -> return_error(Reason)
end.

%% Bad: Will crash
{ok, SessionId, Secret} = ac_eval_mode:start_session(Options).  % Panics if error!
```

## Common Errors

| Error | Cause | Fix |
|-------|-------|-----|
| `{error, not_eval_mode}` | Production mode configured | Check `application:get_env(pricing_engine, mode)` |
| `{error, no_active_session}` | `start_session/0` not called | Call before decoration |
| `{error, invalid_session}` | Wrong SessionId in `end_session/1` | Use exact SessionId from `start_session/0` |
| `{error, invalid_hash}` | Session validation failed | Session was ended or corrupted |

## Testing

Run all tests:
```bash
cd /Users/sac/ggen/tai-erlang-autonomics/pricing-engine
rebar3 eunit --module=ac_eval_mode_tests
```

Individual test:
```erlang
% In Erlang shell
c(ac_eval_mode_tests).
ac_eval_mode_tests:test_mode_always_eval().
ac_eval_mode_tests:test_decorate_payload_map().
```

## Performance

- Session creation: ~100 μs
- Payload decoration: ~10-50 μs
- Hash verification: ~5 μs
- Session cleanup: <1 μs

**No blocking or contention** (process dictionary per Erlang process).

## Integration Locations

### 1. pricing_engine.erl (startup)
```erlang
init(Config) ->
    case ac_eval_mode:ensure_eval() of
        ok -> {ok, idle, #state{}};
        {error, not_eval_mode} -> {error, not_eval_mode}
    end.
```

### 2. HTTP handlers (per-request)
```erlang
handle_http_request(Req, State) ->
    {ok, SessionId, _Secret} = ac_eval_mode:start_session(#{
        request_id => get_header(Req, <<"x-request-id">>)
    }),
    try
        % Process request and decorate outputs
    after
        ac_eval_mode:end_session(SessionId)
    end.
```

### 3. API responses (all outputs)
```erlang
{ok, Result} = process_calculation(...),
{ok, Decorated} = ac_eval_mode:decorate_payload(Result),
{ok, Meta} = ac_eval_mode:decorate_meta(#{data => Decorated}, #{}),
return_json(Meta).
```

### 4. Receipt generation (non-contractual mark)
```erlang
{ok, Receipt} = generate_receipt(...),
{ok, Decorated} = ac_eval_mode:decorate_receipt(Receipt),
{ok, Decorated}.
```

## Type Specifications

```erlang
-type mode() :: eval | production.
-type authority() :: advisory | contractual.
-type session_id() :: binary().                    % UUID v4 (16 bytes)
-type session_secret() :: binary().                % Random 32 bytes
-type session_context() :: #{
    session_id => session_id(),
    session_secret => session_secret(),
    created_at => integer(),                       % milliseconds
    client_id => binary() | undefined,
    request_id => binary() | undefined,
    tags => [atom()]
}.
-type result(T) :: {ok, T} | {error, term()}.
```

## Hardcoded Invariants

These **cannot be changed** without code modification:

```erlang
-define(MODE, eval).                    % Always eval
-define(AUTHORITY, advisory).           % Always advisory
-define(BANNER, <<"...ADVISORY ONLY...">>).  % Always included
```

## Security Guarantees

1. **Non-Contractuality**: Session secrets make receipts non-reproducible
2. **Immutability**: Mode hardcoded, cannot change at runtime
3. **Audit Trail**: All operations stamped with session context
4. **Timing Safety**: Constant-time hash comparison prevents timing attacks

## Files

- **Module**: `/src/ac_eval_mode.erl` (core implementation)
- **Tests**: `/test/ac_eval_mode_tests.erl` (35+ test cases)
- **Docs**: `/docs/AC_EVAL_MODE.md` (complete reference)
- **Example**: `/examples/eval_mode_integration_example.erl` (integration template)

## TL;DR Usage Pattern

```erlang
%% Startup (once)
ac_eval_mode:ensure_eval(),

%% Per request
{ok, SessId, _} = ac_eval_mode:start_session(#{request_id => ReqId}),
try
    {ok, Value} = calculate_something(),
    {ok, Decorated} = ac_eval_mode:decorate_payload(Value),
    return_to_client(Decorated)
after
    ac_eval_mode:end_session(SessId)
end.
```

That's it! Everything else is optional documentation and edge cases.
