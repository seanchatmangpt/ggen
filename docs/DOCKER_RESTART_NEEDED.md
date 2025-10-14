# Docker Restart Required

**Date**: 2025-10-13
**Status**: ⚠️ Docker daemon is hung and needs restart

---

## Symptoms

- Docker commands timeout after 2+ minutes
- Cannot stop/remove containers
- 6+ postgres and redis containers stuck running
- Test cleanup fails

## Quick Fix

### macOS:
```bash
# Restart Docker Desktop
killall Docker && open /Applications/Docker.app

# Wait 30 seconds, then verify
docker ps
```

### Linux:
```bash
# Restart Docker daemon
sudo systemctl restart docker

# Verify
docker ps
```

## Cleanup After Restart

```bash
# Remove all stopped containers
docker container prune -f

# If needed, force remove all
docker stop $(docker ps -aq) 2>/dev/null
docker rm $(docker ps -aq) 2>/dev/null
```

## Why It Happened

The CleanroomGuard panicked during Drop, causing:
1. Tests to abort with SIGABRT
2. Containers left running
3. Docker daemon overload (18+ orphaned containers)
4. System instability

**Fixed**: CleanroomGuard now never panics (cleanroom/src/cleanroom.rs:1018-1029)

## Alternative: Use CLI Tests Without Docker

While Docker is restarting, use the CLI tests that don't require Docker:

```bash
# Test simple CLI (no Docker)
cd cleanroom
cargo test --test cli_test

# Test ggen CLI (no Docker)
cargo test --test cli_integration_cleanroom
```

These tests are faster (~15s vs 5min) and don't require Docker!

---

**Next**: Once Docker restarts, the orphaned containers will be gone and cleanroom will work normally.
