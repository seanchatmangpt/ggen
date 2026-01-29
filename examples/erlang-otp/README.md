# Telecom-Grade Erlang/OTP Examples

Comprehensive demonstration of Fortune 5 telecom capabilities using Erlang/OTP.

## Overview

This project showcases enterprise-grade patterns for high-availability, fault-tolerant systems:

- **Call Router**: High-throughput routing (>100K calls/sec) with circuit breaker
- **Billing Engine**: ACID-compliant transactions with audit trails
- **Fault Tolerance**: Carrier-grade supervision with layered recovery
- **Chaos Engineering**: Netflix-style resilience testing
- **Performance Benchmarking**: Basho Bench integration for load testing

## Architecture

```
telecom_sup (rest_for_one)
  ├── db_pool (connection pooling)
  ├── call_router_server (call routing with circuit breaker)
  └── billing_engine_server (ACID transactions with audit)
```

### Supervision Strategy

- **rest_for_one**: Dependency-aware restart ordering
- **Intensity limits**: Max 10 restarts in 60 seconds
- **Shutdown policies**:
  - db_pool: 5 seconds (flush connections)
  - call_router: 10 seconds (complete in-flight calls)
  - billing_engine: infinity (wait for all transactions)

## Quick Start

### Prerequisites

- Erlang/OTP 25+ (`brew install erlang` or `apt-get install erlang`)
- Rebar3 (`brew install rebar3` or download from https://rebar3.org)

### Build

```bash
cd examples/erlang-otp
rebar3 compile
```

### Run Interactive Shell

```bash
rebar3 shell
```

### Run Tests

```bash
# Unit tests
rebar3 eunit

# Coverage report
rebar3 cover

# Dialyzer (static analysis)
rebar3 dialyzer

# All quality checks
rebar3 do eunit, cover, dialyzer
```

## Usage Examples

### 1. Call Router

```erlang
%% Start the application
application:start(telecom).

%% Route a call
{ok, Target} = call_router_server:route_call(<<"CALL-001">>, <<"1-800-FORTUNE">>).
% Returns: {ok, <<"sip:fortune@telecom.example.com">>}

%% Add custom route
call_router_server:add_route(<<"1-800-CUSTOM">>, <<"sip:custom@example.com">>).

%% Get metrics
Metrics = call_router_server:get_metrics().
% Returns: #{routed => 1, failed => 0, dropped => 0, circuit_breaks => 0}

%% Enable circuit breaker
call_router_server:enable_circuit_breaker().
```

### 2. Billing Engine

```erlang
%% Charge an account
TxnId = <<"TXN-", (integer_to_binary(erlang:system_time()))/binary>>,
{ok, TxnId} = billing_engine_server:charge_account(TxnId, <<"ACC-001">>, 100.00).

%% Check balance
{ok, Balance, Currency} = billing_engine_server:get_account_balance(<<"ACC-001">>).
% Returns: {ok, 10100.00, <<"USD">>}

%% Refund transaction
{ok, RefundTxnId} = billing_engine_server:refund_transaction(TxnId, <<"Customer request">>).

%% Get audit log
{ok, AuditEntries} = billing_engine_server:get_audit_log(<<"ACC-001">>).

%% Verify transaction integrity
{ok, valid} = billing_engine_server:verify_transaction(TxnId).
```

### 3. Chaos Engineering

```erlang
%% Run chaos tests with medium intensity
chaos_monkey:simulate_failures(#{
    duration => 300000,      % 5 minutes
    intensity => medium,     % low | medium | high
    scenarios => [kill_random_worker, cpu_spike, memory_leak],
    verification_enabled => true
}).

%% Individual failure scenarios
{ok, Result} = chaos_monkey:kill_random_worker(#{}).
{ok, Result} = chaos_monkey:cpu_spike(#{}).
{ok, Result} = chaos_monkey:cascade_failure(#{}).

%% Verify system recovery
{ok, valid} = chaos_monkey:verify_recovery(#{}).
```

## Performance Benchmarking

### Using Basho Bench

```bash
# Install Basho Bench
rebar3 as bench compile

# Run benchmark (requires configuration file)
basho_bench bench/call_router.config

# View results
open tests/current/summary.png
```

### Example Benchmark Configuration

Create `bench/call_router.config`:

```erlang
{mode, max}.
{duration, 5}.  % 5 minutes
{concurrent, 100}.  % 100 concurrent workers

{driver, call_router_bench}.

{code_paths, ["_build/default/lib/*/ebin"]}.

{key_generator, {sequential_int, 1000000}}.
{value_generator, {uniform_bin, 20}}.

{operations, [
    {route_call, 80},   % 80% reads
    {add_route, 15},    % 15% writes
    {get_metrics, 5}    % 5% metrics queries
]}.
```

## SLA Targets

### Call Router
- **Throughput**: >100,000 calls/second
- **Latency**: P99 < 1ms, P50 < 100μs
- **Availability**: 99.999% (5 nines)
- **Memory**: <2GB for 10M active routes

### Billing Engine
- **Transaction latency**: P99 < 100ms
- **Throughput**: >10,000 transactions/second
- **Durability**: Zero data loss (ACID guarantees)
- **Audit query**: <1 second for 1M records

### System-Wide
- **Recovery time**: <500ms for worker failures
- **Zero downtime**: Hot code reload without dropping calls
- **Regulatory compliance**: SOX, GDPR, PCI-DSS, HIPAA

## Compliance Features

### SOX (Sarbanes-Oxley)
- Complete financial audit trail
- Transaction integrity verification
- Immutable audit logs with timestamps

### GDPR
- Account data protection
- Right to erasure support
- Audit log privacy controls

### PCI-DSS
- Secure payment processing
- Fraud detection and prevention
- Transaction idempotency

### HIPAA
- Healthcare billing compliance
- Audit trails for sensitive data
- Access control logging

## Key Patterns Demonstrated

### 1. Circuit Breaker
```erlang
%% Prevents cascade failures by opening circuit when error rate exceeds threshold
%% - Error threshold: 50% error rate
%% - Time window: 60 seconds
%% - Automatic recovery when errors subside
```

### 2. Load Shedding
```erlang
%% Gracefully degrades under extreme load
%% - Monitors message queue length
%% - Rejects requests when threshold exceeded
%% - Prevents system saturation
```

### 3. Idempotency
```erlang
%% Prevents duplicate transactions
%% - Transaction ID uniqueness enforced
%% - Duplicate detection before processing
%% - Critical for financial integrity
```

### 4. Write-Ahead Logging (WAL)
```erlang
%% Ensures durability of transactions
%% - Log written before state update
%% - Recovery from WAL on restart
%% - fsync for data persistence
```

### 5. Supervision Trees
```erlang
%% Layered fault tolerance
%% - rest_for_one for dependency chains
%% - Configurable restart intensity
%% - Graceful shutdown ordering
```

## Production Deployment

### Release Build

```bash
# Build production release
rebar3 as prod release

# Start release
_build/prod/rel/telecom/bin/telecom start

# Attach to console
_build/prod/rel/telecom/bin/telecom attach

# Stop release
_build/prod/rel/telecom/bin/telecom stop
```

### Hot Code Upgrade

```bash
# Create appup file for upgrade
# Edit src/telecom.appup

# Build upgrade
rebar3 as prod appup generate
rebar3 as prod relup

# Install upgrade
_build/prod/rel/telecom/bin/telecom upgrade "1.0.1"
```

### Monitoring

```bash
# Observer GUI
observer:start().

# System info
erlang:system_info(process_count).
erlang:memory().

# Supervisor status
supervisor:which_children(telecom_sup).
```

## Testing Strategy

### EUnit Tests
- Unit tests for individual modules
- Coverage target: >80%
- Located in `test/` directory

### Property-Based Tests (PropEr)
- Randomized testing for edge cases
- State machine verification
- Concurrent property testing

### Chaos Engineering
- Controlled failure injection
- System resilience verification
- Recovery time measurement

### Load Tests (Basho Bench)
- Throughput benchmarking
- Latency profiling
- SLA compliance verification

## File Structure

```
examples/erlang-otp/
├── src/                      # Source code
│   ├── call_router_server.erl
│   ├── billing_engine_server.erl
│   ├── db_pool.erl
│   ├── telecom_sup.erl
│   ├── telecom_app.erl
│   └── telecom.app.src
├── test/                     # EUnit tests
│   ├── call_router_tests.erl
│   ├── billing_engine_tests.erl
│   └── chaos_monkey.erl
├── bench/                    # Benchmarks
│   └── call_router_bench.erl
├── config/                   # Configuration
│   ├── sys.config
│   └── vm.args
├── priv/                     # Private resources
├── rebar.config             # Build configuration
└── README.md                # This file
```

## Further Reading

- [Erlang/OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Supervisor Behavior](https://www.erlang.org/doc/design_principles/sup_princ.html)
- [gen_server Behavior](https://www.erlang.org/doc/design_principles/gen_server_concepts.html)
- [Release Handling](https://www.erlang.org/doc/design_principles/release_handling.html)
- [Basho Bench Documentation](https://github.com/basho/basho_bench)

## License

Apache-2.0 (same as parent ggen project)

## Contributing

This is a demonstration project. For production use:
1. Replace mock database with real storage (PostgreSQL, Cassandra, etc.)
2. Add proper authentication and authorization
3. Implement comprehensive logging with lager
4. Add distributed Erlang for multi-node deployment
5. Implement proper health checks and monitoring (Prometheus, etc.)
6. Add rate limiting and DDoS protection
7. Implement proper secret management (Vault, etc.)
