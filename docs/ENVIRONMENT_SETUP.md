# Environment Parity Setup - Order of Operations

## Goal
Create a **local Docker/testcontainers environment** that exactly mirrors Claude Code web, so you can iterate SessionStart hooks locally before deploying to production.

---

## What We Know About Claude Code Web

From environment research (date: 2026-01-03):

| Component | Version | Status |
|-----------|---------|--------|
| OS | Ubuntu 24.04.3 LTS | ✅ |
| Rust | 1.91.1 | ✅ |
| cargo-make | Not pre-installed | ⚠️ (installs on demand) |
| Node.js | 22.21.1 | ✅ |
| Python | 3.11.14 | ✅ |
| Go | 1.24.7 | ✅ |
| Java | OpenJDK 21 | ✅ |
| Ruby | 3.3.6 | ✅ |
| GCC | 13.3.0 | ✅ |
| Clang | 18.1.3 | ✅ |
| PostgreSQL client | 16.11 | ✅ |
| Redis client | 7.0.15 | ✅ |
| Docker | NOT AVAILABLE | ❌ |
| testcontainers | In Cargo.toml | ✅ |

---

## The Problem

You want **local parity** so you can:
1. **Iterate SessionStart hooks** locally without committing to Claude Code web
2. **Test environment verification** before deployment
3. **Add Docker support** that works locally but uses testcontainers in Claude Code web

---

## Order of Operations

### Phase 1: Research & Documentation (DONE ✅)
- [x] Environment discovery script (`scripts/check-testcontainers.sh`)
- [x] Parity check script (`.claude/hooks/session-start-parity-check.sh`)
- [x] Document environment differences

### Phase 2: Local Docker Mirror Setup (YOUR NEXT STEP)

**Goal**: Create Docker Compose or Dockerfile locally that matches Claude Code web

**Steps**:
1. **Create `docker-compose.yml`** in repo root:
   ```yaml
   # Include: Ubuntu 24.04, all languages, PostgreSQL 16, Redis 7.0
   # This is what you iterate locally
   ```

2. **Test locally**:
   ```bash
   # Start containers
   docker-compose up -d

   # Run parity check inside container
   docker-compose exec dev bash .claude/hooks/session-start-parity-check.sh
   ```

3. **Verify complete parity**:
   - All checks pass (22 passed, 0 failed)
   - cargo-make installs successfully
   - Docker available inside container

### Phase 3: SessionStart Hook Iteration (CURRENT STATE)

**Current hook**: `.claude/hooks/session-start.sh` (basic version)
**New hook for iteration**: `.claude/hooks/session-start-parity-check.sh` (comprehensive)

**Workflow**:
```bash
# 1. Locally, inside Docker container:
bash .claude/hooks/session-start-parity-check.sh

# 2. See what's missing or broken
# 3. Adjust Docker setup or hook
# 4. Re-test until all checks pass
# 5. Once working locally, update production hook:
cp .claude/hooks/session-start-parity-check.sh .claude/hooks/session-start.sh
```

### Phase 4: Claude Code Web Deployment

Once your local environment is perfect:

1. **Commit the finalized hook** to your branch:
   ```bash
   git add .claude/hooks/session-start.sh
   git commit -m "feat: Add environment parity check to SessionStart hook"
   git push -u origin claude/setup-docker-testcontainers-H7grF
   ```

2. **Update `.claude/settings.json`** if needed:
   - Already points to `session-start.sh` ✅
   - Hook runs at SessionStart ✅

3. **Next Claude Code web session will**:
   - Run the hook automatically
   - Check all environment requirements
   - Install cargo-make if missing
   - Report any issues before work begins

---

## Current Status

### What You Have ✅
- Parity check script ready to iterate
- Git branch set up (`claude/setup-docker-testcontainers-H7grF`)
- testcontainers crate in Cargo.toml
- ggen-e2e tests ready to run (when Docker available)

### What's Missing ❌ (Local Setup)
- Docker installation (install locally)
- Docker Compose configuration
- Dockerized development environment matching Claude Code web
- SessionStart hook refinement based on local testing

### What Works in Claude Code Web ✅
- All 22 environment checks pass
- Polyglot languages available
- Build tools present
- Database clients available
- testcontainers configured

---

## Next Steps for You

### Option A: Quick Path (Just Docker Install)
```bash
# Install Docker locally
# Run parity check
bash .claude/hooks/session-start-parity-check.sh

# See what fails, install those tools
# Update hook to handle missing pieces
# Commit to branch
```

### Option B: Complete Path (Docker Compose)
```bash
# Create docker-compose.yml matching Claude Code web
# Test hook inside container
# Iterate until perfect
# Commit to branch
```

### Option C: Minimal Path (Just Copy Existing)
```bash
# Use parity check as-is
# Commit as SessionStart hook
# Let Claude Code web use it as-is
# Iterate via Claude Code web feedback
```

---

## Files Reference

| File | Purpose | Status |
|------|---------|--------|
| `scripts/check-testcontainers.sh` | Check E2E infrastructure | ✅ Ready |
| `.claude/hooks/session-start-parity-check.sh` | Comprehensive parity check | ✅ Ready for iteration |
| `.claude/hooks/session-start.sh` | Current production hook | ⚠️ Needs update |
| `.claude/settings.json` | Hook configuration | ✅ Already configured |
| `Dockerfile` | Single-stage build image | ✅ Exists |
| `Dockerfile.binary` | Minimal binary image | ✅ Exists |
| `docker-compose.yml` | Local dev environment | ❌ MISSING |

---

## Success Criteria

When you're done:
- [ ] Parity check script runs successfully locally
- [ ] All 22 environment checks pass (or all critical checks pass)
- [ ] cargo-make installs and works
- [ ] Docker is available (locally)
- [ ] testcontainers can spin up test containers
- [ ] SessionStart hook is finalized and committed
- [ ] Claude Code web can run your code
