# Mock Detection - Action Items Checklist

## Immediate Actions (This Week)

### Code Quality
- [ ] Mark all mock tests with `#[ignore]` or move to `tests/unit/`
- [ ] Add `#[cfg(feature = "docker-integration")]` to real Docker tests
- [ ] Update test names to clearly indicate mock vs real (e.g., `test_postgres_mock_operations`)

### Documentation
- [ ] Add warning comments to all mock implementations
- [ ] Update README with test categories (unit vs integration)
- [ ] Document Docker requirements for integration tests

### CI/CD
- [ ] Split CI jobs: `unit-tests` (no Docker) and `integration-tests` (requires Docker)
- [ ] Add Docker availability check to integration tests
- [ ] Fail CI if integration tests are skipped on main branch

## Medium Term (Next 2 Weeks)

### PostgreSQL Container
- [ ] Implement `execute_sql()` using sqlx or tokio-postgres
  - [ ] Add connection pooling
  - [ ] Add error handling
  - [ ] Add timeout handling
- [ ] Implement `test_connection()` with retry logic
  - [ ] Use PostgreSQL PING or SELECT 1
  - [ ] Add configurable retry count/delay
- [ ] Add integration tests
  - [ ] Test table creation
  - [ ] Test data insertion/selection
  - [ ] Test transaction rollback
  - [ ] Test connection failure scenarios

### Redis Container
- [ ] Implement `execute_command()` using redis-rs
  - [ ] Add connection pooling
  - [ ] Add error handling
  - [ ] Add timeout handling
- [ ] Implement `test_connection()` with PING
  - [ ] Add configurable retry count/delay
- [ ] Add integration tests
  - [ ] Test SET/GET operations
  - [ ] Test DEL operations
  - [ ] Test connection failure scenarios

### Generic Container
- [ ] Implement `execute_command()` using container.exec()
  - [ ] Use testcontainers exec API
  - [ ] Parse stdout/stderr correctly
  - [ ] Handle exit codes
- [ ] Add integration tests
  - [ ] Test simple commands (echo, ls)
  - [ ] Test command failures
  - [ ] Test timeout scenarios

## Long Term (Next Month)

### Container Monitoring
- [ ] Implement real `status()` method
  - [ ] Query Docker API for container state
  - [ ] Handle all states: Starting, Running, Stopping, Stopped, Failed
  - [ ] Add health check support
- [ ] Implement real `metrics()` method
  - [ ] Use Docker stats API
  - [ ] Track CPU usage (real-time)
  - [ ] Track memory usage (current/max)
  - [ ] Track network I/O
  - [ ] Track disk I/O
- [ ] Add monitoring tests
  - [ ] Test metric accuracy
  - [ ] Test metric updates
  - [ ] Test status transitions

### Backend Improvements
- [ ] Implement volume mounting in TestcontainerBackend
  - [ ] Support host → container mounts
  - [ ] Support named volumes
  - [ ] Add permission handling
- [ ] Add timeout enforcement within container execution
- [ ] Improve error messages for Docker failures

### Test Architecture
- [ ] Create test harness module
  - [ ] Abstract mock vs real implementations
  - [ ] Provide feature flags for switching
  - [ ] Add test utilities for both modes
- [ ] Add property-based tests
  - [ ] Test container lifecycle properties
  - [ ] Test command execution properties
  - [ ] Test error handling properties

## Verification

### After Each Implementation
- [ ] Run tests with Docker running → should pass
- [ ] Run tests without Docker running → integration tests should fail appropriately
- [ ] Verify error messages are clear and actionable
- [ ] Update documentation

### Before Deployment
- [ ] All integration tests pass on CI with Docker
- [ ] Unit tests pass on CI without Docker
- [ ] Test coverage > 80% for real implementations
- [ ] No TODO comments in production code
- [ ] All mock implementations clearly marked

## Success Metrics

### Code Quality
- [ ] Zero TODO comments in src/containers.rs
- [ ] All mock implementations have explanatory comments
- [ ] All tests are correctly categorized

### Test Coverage
- [ ] Unit tests: 90%+ coverage
- [ ] Integration tests: 80%+ coverage
- [ ] E2E tests: Cover all critical paths

### CI/CD
- [ ] CI pipeline requires Docker for main branch
- [ ] Integration test failures block merges
- [ ] Test execution time < 5 minutes

## Risk Mitigation

### If Docker Unavailable in CI
- [ ] Provide mock implementations as fallback
- [ ] Mark tests as skipped (not passed)
- [ ] Add warning to CI output
- [ ] Require manual testing before merge

### If Tests Are Slow
- [ ] Use container reuse (singleton pattern)
- [ ] Parallelize independent tests
- [ ] Use faster images (alpine instead of full)
- [ ] Cache test containers

### If Tests Are Flaky
- [ ] Add retry logic to connection tests
- [ ] Increase timeout values
- [ ] Add wait conditions for container readiness
- [ ] Improve error messages for debugging

---

## Priority Matrix

| Task | Priority | Effort | Impact |
|------|----------|--------|--------|
| Mark mock tests | High | Low | High |
| Implement execute_sql() | High | Medium | High |
| Implement execute_command() | High | Medium | High |
| Implement status() | Medium | High | Medium |
| Implement metrics() | Medium | High | Medium |
| Volume mounting | Low | High | Low |
| Property-based tests | Low | High | Medium |

---

**Next Review**: Weekly team meeting
**Owner**: Mock Detection Specialist (Hive Mind)
**Status**: Ready for implementation
