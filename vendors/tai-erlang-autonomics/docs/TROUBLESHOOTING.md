# Troubleshooting Guide

## Common Issues & Solutions

### Startup Issues

#### Issue: "econnrefused" when accessing /health

**Symptoms**:
```
$ curl http://localhost:8080/health
curl: (7) Failed to connect to localhost port 8080: Connection refused
```

**Root Causes**:
1. Application not started
2. Port already in use
3. HTTP server failed to initialize

**Solutions**:

1. Check if application is running:
```bash
# If using rebar3
ps aux | grep erl

# If using release
ps aux | grep tai_autonomics
```

2. Check if port is in use:
```bash
lsof -i :8080
# or
netstat -an | grep 8080
```

3. View application logs:
```bash
# For release
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1

# For development
rebar3 shell
```

4. Try alternative port:
```bash
PORT=9090 rebar3 shell
curl http://localhost:9090/health
```

#### Issue: "eaddrinuse" - Port already in use

**Solutions**:
```bash
# Find process using port 8080
lsof -i :8080

# Kill process (if safe)
kill -9 <PID>

# Or use different port
PORT=9090 rebar3 shell
```

---

### Compilation Issues

#### Issue: Erlang/OTP version mismatch

**Symptoms**:
```
ERROR: Erlang/OTP 25 or later required, found 24
```

**Solutions**:
```bash
# Check Erlang version
erl -version

# Upgrade Erlang using asdf (if using asdf)
asdf install erlang 26.0
asdf local erlang 26.0

# Verify
erl -version
```

#### Issue: Missing rebar3

**Symptoms**:
```
rebar3: command not found
```

**Solutions**:
```bash
# Install rebar3
brew install rebar3

# Or download from GitHub
mkdir -p ~/.local/bin
curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o ~/.local/bin/rebar3
chmod +x ~/.local/bin/rebar3
export PATH="$PATH:$HOME/.local/bin"
```

#### Issue: Dependency resolution failure

**Symptoms**:
```
===> Errors compiling dependency 'xyz'
```

**Solutions**:
```bash
# Clean and rebuild
rebar3 clean
rm -rf _build rebar.lock
rebar3 compile

# Or check for network issues
rebar3 update
```

---

### HTTP Handler Issues

#### Issue: "Invalid JSON payload" responses

**Symptoms**:
```json
{
  "error": "Invalid JSON payload",
  "type": "refusal"
}
```

**Root Causes**:
1. Request body is not valid JSON
2. Content-Type header missing
3. Encoding issues

**Solutions**:

1. Verify JSON is valid:
```bash
echo '{"key": "value"}' | jq .
```

2. Set correct Content-Type:
```bash
curl -X POST http://localhost:8080/pubsub \
  -H "Content-Type: application/json" \
  -d '{"message": {"data": "test"}}'
```

3. Check encoding:
```bash
# Ensure UTF-8
file -i request.json
iconv -f ISO-8859-1 -t UTF-8 request.json > request_utf8.json
```

#### Issue: Signature verification failures

**Symptoms**:
```json
{
  "error": "Signature verification failed",
  "type": "refusal"
}
```

**Root Causes**:
1. Invalid JWT token
2. Wrong signing key
3. Clock skew
4. Verification disabled

**Solutions**:

1. Verify signature verification is enabled:
```erlang
application:get_env(tai_autonomics, verify_signatures)
```

2. For testing, disable verification:
```bash
VERIFY_SIGNATURES=false rebar3 shell
```

3. Check JWT token:
```bash
# Decode JWT (online: jwt.io)
echo "eyJhbGc..." | base64 -d | jq .
```

4. Check system time:
```bash
date
# Ensure server time is synchronized with NTP
```

---

### Governor & State Issues

#### Issue: Governor crashes repeatedly

**Symptoms**:
```
=SUPERVISOR REPORT==== XX-Jan-2026::12:34:56.789012 ===
Supervisor: tai_autonomics_sup
Context: child_terminated
Reason: killed
```

**Root Causes**:
1. Invalid state transition
2. Out of memory
3. Infinite loop

**Solutions**:

1. Check error logs:
```bash
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1 | grep -i error
```

2. Monitor memory:
```bash
rebar3 shell
> observer:start().
# Watch Memory Allocators
```

3. Check governor state:
```erlang
% In rebar3 shell
gproc:lookup_all({n, l, {tai_governor, <<"tenant_id">>}})
```

#### Issue: "quota exceeded" when quota should be available

**Symptoms**:
```json
{
  "status": "suspended",
  "type": "entitlement_quota_exceeded"
}
```

**Root Causes**:
1. Quota calculation error
2. Previous actions not released
3. Concurrent request race condition

**Solutions**:

1. Check governor state:
```erlang
% Get governor PID
{Pid} = gproc:lookup_element({n, l, {tai_governor, TenantId}}, p),
sys:get_status(Pid)
```

2. Reset quota (development only):
```erlang
% Send reset signal
Pid ! {reset_quota}
```

3. Check Firestore for pending actions:
```bash
# List actions for tenant
curl -X POST \
  https://firestore.googleapis.com/v1/projects/PROJECT_ID/databases/(default)/documents:query \
  -H "Authorization: Bearer $(gcloud auth print-access-token)" \
  -H "Content-Type: application/json" \
  -d '{
    "structuredQuery": {
      "from": [{"collectionId": "receipts"}],
      "where": {
        "fieldFilter": {
          "field": {"fieldPath": "tenant_id"},
          "op": "EQUAL",
          "value": {"stringValue": "TENANT_ID"}
        }
      }
    }
  }'
```

---

### Performance Issues

#### Issue: High latency (p99 > 500ms)

**Root Causes**:
1. Slow external service (Firestore, Pub/Sub)
2. High load/contention
3. Inefficient state machine transitions

**Solutions**:

1. Check latency breakdown:
```erlang
% In shell
statistics(runtime),
statistics(wall_clock),
erlang:statistics(reductions)
```

2. Monitor external service latency:
```bash
# Firestore latency
curl -v http://localhost:8080/marketplace \
  -H "Content-Type: application/json" \
  -d '...'
# Look for response time
```

3. Reduce concurrency:
```bash
# Set lower pool size
POOLBOY_SIZE=5 rebar3 shell
```

4. Enable profiling:
```erlang
% In shell
fprof:trace(start, {file, "trace.txt"}),
% Run test
fprof:trace(stop),
fprof:profile({file, "trace.txt"}),
fprof:analyse()
```

#### Issue: Memory growth over time

**Root Causes**:
1. Memory leak in receipt buffering
2. Unmatched message accumulation
3. Large state growth

**Solutions**:

1. Check memory usage:
```erlang
erlang:memory(total),
erlang:memory(processes),
erlang:memory(atom)
```

2. Force garbage collection:
```erlang
erlang:garbage_collect(),
erlang:memory(total)
```

3. Check for large process heaps:
```erlang
% Find largest processes
lists:sort(fun(A,B) ->
  element(2,A) > element(2,B)
end, [
  {P, erlang:process_info(P, memory)} || P <- processes()
])
```

---

### Docker Issues

#### Issue: "Docker daemon is not running"

**Solutions**:
```bash
# Start Docker daemon
docker daemon
# or
open /Applications/Docker.app
```

#### Issue: Container fails to start

**Symptoms**:
```
docker run: Error response from daemon: failed to create shim task
```

**Solutions**:

1. Check container logs:
```bash
docker run --rm tai-autonomics:dev
```

2. Build with verbose output:
```bash
docker build --progress=plain -f container/Containerfile -t tai-autonomics:dev .
```

3. Use Alpine Linux verification:
```bash
docker run -it --rm alpine:latest /bin/sh
# Test Erlang installation
apk add erlang
erl -version
```

#### Issue: Port binding in container

**Solutions**:
```bash
# Bind to different port
docker run -e PORT=9090 -p 9090:9090 tai-autonomics:dev

# Or expose all ports
docker run -P tai-autonomics:dev
```

---

### GCP Issues

#### Issue: "Unable to connect to Firestore"

**Symptoms**:
```
{error, {failed_connect, [{to_address, {"firestore.googleapis.com", 443}}, ...
```

**Solutions**:

1. Check GCP credentials:
```bash
gcloud auth list
gcloud auth application-default login
```

2. Use Firestore emulator:
```bash
# Start emulator
gcloud firestore emulators start

# In another terminal
export FIRESTORE_EMULATOR_HOST="localhost:8081"
rebar3 shell
```

3. Verify project ID:
```bash
gcloud config list
gcloud config set project PROJECT_ID
```

#### Issue: Pub/Sub subscription errors

**Solutions**:

1. Verify subscription exists:
```bash
gcloud pubsub subscriptions list
gcloud pubsub subscriptions describe erlang-autonomics-signals
```

2. Check IAM permissions:
```bash
gcloud projects get-iam-policy PROJECT_ID
```

3. Use Pub/Sub emulator:
```bash
# Start emulator
gcloud pubsub emulators start

# In another terminal
export PUBSUB_EMULATOR_HOST="localhost:8085"
rebar3 shell
```

---

### Testing Issues

#### Issue: "Failed to connect during tests"

**Symptoms**:
```
{badmatch, {error, {failed_connect, ...}}}
```

**Solutions**:

1. Ensure application starts in test:
```bash
rebar3 ct --verbose
```

2. Add wait loop in tests:
```erlang
wait_for_http_ready(10).

wait_for_http_ready(0) ->
    ct:fail("HTTP server not ready");
wait_for_http_ready(N) ->
    case httpc:request(get, {"http://localhost:8080/health", []}, [], []) of
        {ok, _} -> ok;
        _ ->
            timer:sleep(500),
            wait_for_http_ready(N-1)
    end.
```

#### Issue: "Test timeout"

**Solutions**:

1. Increase test timeout:
```bash
rebar3 ct --ct_opts='{timeout, 60000}'
```

2. Check for hanging processes:
```erlang
% In test
process_info(self(), messages)
```

---

## Diagnostic Tools

### Essential Commands

```bash
# Check process status
ps aux | grep erl

# Check port usage
lsof -i :8080

# View logs
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1

# Monitor system
top
htop

# Check disk space
df -h

# Network diagnostics
netstat -an | grep 8080
tcpdump -i lo port 8080
```

### Erlang Shell Diagnostics

```erlang
% List all processes
processes().

% Get process info
process_info(Pid).

% Check memory
erlang:memory().

% Get statistics
statistics(runtime).
statistics(wall_clock).

% List running applications
application:which_applications().

% Check process registry
gproc:lookup_all(names).

% View specific process state
sys:get_status(Pid).
```

### Tools

- **observer**: GUI monitoring tool (`observer:start()`)
- **rebar3_format**: Code formatting (`rebar3 format`)
- **dialyzer**: Static analysis (`rebar3 dialyzer`)
- **eunit**: Unit testing (`rebar3 eunit`)
- **common_test**: Integration testing (`rebar3 ct`)

---

## Support Resources

- [Architecture Guide](ARCHITECTURE.md)
- [Configuration Guide](CONFIG.md)
- [API Reference](ENDPOINTS.md)
- [Operations Guide](RUNBOOK.md)
- [Security Guide](SECURITY_REQUIREMENTS.md)

## Emergency Procedures

### Immediate Shutdown
```bash
# Release
_build/default/rel/tai_autonomics/bin/tai_autonomics stop

# Or (force)
pkill -9 erl
```

### Force Reset (Development Only)
```bash
# Clean state
rm -rf _build
rm rebar.lock

# Rebuild
rebar3 compile
```

### Restore from Backup

See [RUNBOOK.md](RUNBOOK.md) - Disaster Recovery section.
