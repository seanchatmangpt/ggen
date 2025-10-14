# Docker + Cleanroom Validation Scripts

## Overview

Two comprehensive validation scripts have been created to verify Docker and cleanroom integration:

1. **Comprehensive Validation** (`validate-docker-integration.sh`) - Full multi-strategy validation
2. **Quick Health Check** (`quick-docker-check.sh`) - Fast diagnostic tool with timeouts

## Scripts Location

- **Main Validation**: `/Users/sac/ggen/scripts/validate-docker-integration.sh`
- **Quick Check**: `/Users/sac/ggen/scripts/quick-docker-check.sh`

## Comprehensive Validation Script

### Purpose
Catches ALL false positives by verifying Docker and cleanroom are actually running together through 6 independent validation strategies.

### Validation Strategies

#### Strategy 1: Docker Daemon Health Check
**What it validates:**
- Docker command is available
- Docker version is accessible
- Docker daemon is running
- Docker socket exists

**How it catches false positives:**
- Verifies Docker is actually running, not just installed
- Checks daemon accessibility
- Confirms socket connectivity

#### Strategy 2: Container Lifecycle Tracking
**What it validates:**
- Counts containers before tests
- Runs cleanroom tests
- Verifies new containers were created
- Checks for testcontainers in container list

**How it catches false positives:**
- Ensures tests actually CREATE containers
- Verifies containers match testcontainers patterns
- Confirms container count increases during tests

#### Strategy 3: Port Accessibility Test
**What it validates:**
- Lists containers with exposed ports
- Tests TCP connectivity to each port
- Verifies services are reachable

**How it catches false positives:**
- Confirms ports are actually bound and accessible
- Tests real network connectivity
- Verifies services respond on expected ports

#### Strategy 4: Negative Testing
**What it validates:**
- Tests behavior when Docker is unavailable
- Verifies tests fail appropriately
- Confirms error messages are correct

**How it catches false positives:**
- Ensures tests don't pass when Docker is down
- Validates error handling

**Note:** Currently SKIPPED in automated runs (requires manual Docker stop)

#### Strategy 5: Container Inspection
**What it validates:**
- Captures container state before/after tests
- Checks for cleanroom-related container names
- Inspects container networks
- Verifies container logs exist

**How it catches false positives:**
- Confirms containers match expected patterns
- Verifies testcontainers networks are created
- Ensures containers have actual logs
- Tracks container lifecycle

#### Strategy 6: Real Service Validation
**What it validates:**
- Finds PostgreSQL containers
- Tests PostgreSQL connections (if psql available)
- Finds Redis containers
- Tests Redis connections (if redis-cli available)
- Verifies any service is reachable on exposed ports

**How it catches false positives:**
- Confirms services are ACTUALLY running
- Tests real database/service connectivity
- Verifies services respond to queries

### Exit Codes

```
0 = EXIT_SUCCESS        - All strategies passed
1 = EXIT_DOCKER_ISSUE   - Docker daemon issues detected
2 = EXIT_CLEANROOM_ISSUE - Cleanroom test issues detected
3 = EXIT_FALSE_POSITIVE - False positive detected (critical failure)
```

### Usage

```bash
# Run full validation
cd /Users/sac/ggen
./scripts/validate-docker-integration.sh

# Check exit code
echo $?

# View detailed output
./scripts/validate-docker-integration.sh 2>&1 | tee validation.log
```

### Output Format

The script provides:
- Color-coded output (green=PASS, red=FAIL, yellow=WARN, blue=INFO)
- Section headers for each strategy
- Detailed logging for debugging
- Summary report with pass/fail counts
- Overall pass rate percentage

### Pass Rate Thresholds

- **100%**: All strategies passed ✓
- **80-99%**: Mostly passed (review failures)
- **50-79%**: Partial failure (Docker may have issues)
- **<50%**: Critical failure (false positive detected)

## Quick Health Check Script

### Purpose
Fast diagnostic tool with strict timeouts to avoid hanging when Docker is unresponsive.

### What it checks

1. **Docker command availability** (instant)
2. **Docker version** (5s timeout)
3. **Docker daemon status** (5s timeout)
4. **Container count** (5s timeout)
5. **Testcontainers presence** (5s timeout)

### Usage

```bash
# Run quick check
cd /Users/sac/ggen
./scripts/quick-docker-check.sh

# Exit codes: 0=healthy, 1=issue detected
```

### When to use

- **Quick check**: Before running tests
- **Troubleshooting**: When Docker seems unresponsive
- **CI/CD**: Fast pre-test validation
- **Debugging**: When validation script hangs

## Integration with Tests

### Recommended Workflow

```bash
# 1. Quick check before tests (fast)
./scripts/quick-docker-check.sh || exit 1

# 2. Run cleanroom tests
cd cleanroom && cargo test

# 3. Comprehensive validation after tests
cd .. && ./scripts/validate-docker-integration.sh
```

### CI/CD Integration

```yaml
# Example GitHub Actions workflow
steps:
  - name: Quick Docker Check
    run: ./scripts/quick-docker-check.sh

  - name: Run Cleanroom Tests
    run: cd cleanroom && cargo test

  - name: Validate Docker Integration
    run: ./scripts/validate-docker-integration.sh
```

## Troubleshooting

### Script Hangs

**Symptom:** Validation script hangs indefinitely

**Cause:** Docker daemon is unresponsive

**Solution:**
```bash
# 1. Use quick check first
./scripts/quick-docker-check.sh

# 2. Restart Docker
# macOS: Restart Docker Desktop
# Linux: sudo systemctl restart docker

# 3. Verify Docker is responsive
docker ps
```

### No Containers Created

**Symptom:** Strategy 2 fails - no containers created

**Possible causes:**
- Tests are mocked (not using real Docker)
- Testcontainers is not properly configured
- Docker daemon is not accessible from tests

**Solution:**
- Check test code uses testcontainers-rs
- Verify DOCKER_HOST environment variable
- Run tests with `--nocapture` to see output

### Ports Not Accessible

**Symptom:** Strategy 3 fails - ports not responding

**Possible causes:**
- Containers starting slowly
- Port binding issues
- Firewall blocking connections

**Solution:**
- Increase wait times in script
- Check `docker ps` output for port mappings
- Verify firewall rules

### Services Not Responding

**Symptom:** Strategy 6 fails - services not reachable

**Possible causes:**
- Service clients (psql, redis-cli) not installed
- Services not fully started
- Wrong credentials

**Solution:**
- Install client tools: `brew install postgresql redis`
- Check container logs: `docker logs <container>`
- Verify service is ready (may need longer startup time)

## Implementation Details

### Technology Stack
- **Shell**: Bash (compatible with bash 3.2+)
- **Dependencies**: docker, timeout, wc, grep
- **Optional**: psql, redis-cli (for service validation)

### Data Structures
- Arrays for strategy tracking (bash 3.2+ compatible)
- Temporary files for state capture
- Exit codes for result reporting

### Parallel Execution
Currently strategies run sequentially for simplicity. Future enhancement could run independent strategies in parallel using background jobs.

### Error Handling
- `set -euo pipefail` for strict error checking
- Timeouts on all Docker operations
- Cleanup trap for test artifacts
- Graceful handling of missing tools

## Known Limitations

1. **Negative testing** (Strategy 4) requires manual Docker stop/start
2. **Service validation** (Strategy 6) requires client tools (psql, redis-cli)
3. **Sequential execution** - strategies don't run in parallel
4. **macOS-specific** socket paths may differ on Linux/Windows
5. **Timeout handling** - may need adjustment for slow systems

## Future Enhancements

### High Priority
- [ ] Add parallel strategy execution
- [ ] Implement automated negative testing (Docker pause/resume)
- [ ] Add container resource usage validation
- [ ] Create JSON output format for CI/CD parsing

### Medium Priority
- [ ] Add Windows/Linux compatibility
- [ ] Include network isolation testing
- [ ] Add volume mount verification
- [ ] Create detailed HTML report

### Low Priority
- [ ] Add performance benchmarking
- [ ] Include security scanning
- [ ] Add custom strategy plugins
- [ ] Create interactive mode

## Contributing

When adding new validation strategies:

1. Create a new `strategy_N_name()` function
2. Follow the existing pattern (status tracking, logging)
3. Call `record_result "strategy_name" "PASS|FAIL|SKIP"`
4. Add strategy to `run_all_strategies()`
5. Update this documentation
6. Add test cases

## References

- **Testcontainers-rs**: https://github.com/testcontainers/testcontainers-rs
- **Docker CLI**: https://docs.docker.com/engine/reference/commandline/cli/
- **Bash Best Practices**: https://google.github.io/styleguide/shellguide.html

## Changelog

### Version 1.0.0 (2025-10-13)
- Initial implementation
- 6 validation strategies
- Comprehensive reporting
- Quick health check script
- Color-coded output
- Exit code standards
