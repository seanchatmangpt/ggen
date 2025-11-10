# Async - Close Gaps

Guide to identifying and fixing async/concurrency gaps, ensuring async code works correctly.

## Quick Reference

### Commands
```bash
# Run async tests
cargo test --test async_tests

# Run single-threaded async tests (deterministic)
cargo test --test async_tests -- --test-threads=1

# Test async code
cargo make test
```

## What to Verify

### Async Operations
- I/O operations use async (files, network, containers)
- Computation uses sync (pure functions)
- Async operations complete correctly
- Resources cleaned up properly

### Concurrency
- Parallel operations work correctly
- Sequential operations maintain order
- No race conditions
- No deadlocks

### Resource Management
- Async resources cleaned up
- No resource leaks
- Proper cleanup on errors
- Resources released promptly

## How to Identify Gaps

### Async Usage Gaps
Check:
- I/O operations not using async
- Blocking operations in async contexts
- Sync operations that should be async
- Async operations that should be sync

### Concurrency Gaps
Identify:
- Race conditions
- Deadlocks
- Incorrect parallelization
- Missing synchronization

### Resource Management Gaps
Find:
- Resources not cleaned up
- Resource leaks
- Cleanup not called on errors
- Resources held too long

## What to Fix

### Async Usage Issues
**Problem**: Wrong async/sync usage
**Fix**: Use async for I/O, sync for computation
**Verify**: Operations use appropriate async/sync

### Concurrency Issues
**Problem**: Race conditions or deadlocks
**Fix**: Add proper synchronization, fix race conditions
**Verify**: No race conditions, no deadlocks

### Resource Management Issues
**Problem**: Resources not cleaned up
**Fix**: Ensure cleanup happens, use proper resource management
**Verify**: Resources cleaned up, no leaks

### Blocking in Async Contexts
**Problem**: Blocking operations in async code
**Fix**: Use async alternatives, avoid blocking
**Verify**: No blocking in async contexts

## Gaps to Close Checklist

### Async Usage
- [ ] I/O operations use async
- [ ] Computation uses sync
- [ ] No blocking in async contexts
- [ ] Appropriate async/sync usage

### Concurrency
- [ ] No race conditions
- [ ] No deadlocks
- [ ] Proper synchronization
- [ ] Correct parallelization

### Resource Management
- [ ] Resources cleaned up
- [ ] No resource leaks
- [ ] Cleanup on errors
- [ ] Resources released promptly

### Testing
- [ ] Async tests use proper runtime
- [ ] Deterministic tests use single-threaded runtime
- [ ] Timeouts tested where appropriate
- [ ] Error paths tested in async code

## Common Gaps to Fix

### Gap: Blocking in Async Contexts
**Problem**: Blocking operations in async code
**Fix**: Use async alternatives, avoid blocking
**Verify**: No blocking in async contexts

### Gap: Race Conditions
**Problem**: Concurrent access causes issues
**Fix**: Add proper synchronization
**Verify**: No race conditions

### Gap: Resource Leaks
**Problem**: Resources not cleaned up
**Fix**: Ensure cleanup happens, use proper management
**Verify**: Resources cleaned up, no leaks

### Gap: Incorrect Async Usage
**Problem**: Wrong async/sync usage
**Fix**: Use async for I/O, sync for computation
**Verify**: Operations use appropriate async/sync

## Async Principles

### Use Async for I/O
- File operations should be async
- Network operations should be async
- Container operations should be async
- Database operations should be async

### Use Sync for Computation
- Pure computation should be sync
- Simple operations should be sync
- CPU-bound work should be sync
- No I/O operations should be sync

### Clean Up Resources
- Always clean up async resources
- Cleanup on errors
- Release resources promptly
- Use proper resource management

## Commands for Gap Analysis

```bash
# Run async tests
cargo test --test async_tests

# Run single-threaded to find race conditions
cargo test --test async_tests -- --test-threads=1

# Test async code
cargo make test
```
