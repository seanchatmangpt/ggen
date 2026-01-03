# Order of Operations: Local → Claude Code Web

## The Problem You're Solving

You want your **local development environment** to be exactly like **Claude Code web**, so that:
1. SessionStart hooks work identically in both places
2. You can iterate/test hooks locally before deploying
3. Tests pass locally because your Docker/testcontainers setup matches what Claude Code web expects

---

## The Three Environments

```
┌─────────────────────┐
│  Claude Code Web    │  (gVisor sandbox, NO Docker)
│  - Ubuntu 24.04     │
│  - All languages    │
│  - testcontainers   │
│  - NO Docker        │  ← Uses external Docker from CI
└─────────────────────┘
         ↑
         │ (What you mirror locally)
         │
┌─────────────────────┐
│  Your Local Env     │  (Your machine with Docker)
│  - Ubuntu/macOS/etc │
│  - Same languages   │
│  - testcontainers   │
│  - Docker installed │  ← Uses your local Docker daemon
└─────────────────────┘
         ↑
         │ (What we're designing)
         │
┌─────────────────────┐
│  Docker Container   │  (Optional: dev environment)
│  - Ubuntu 24.04     │
│  - Mirror of Claude │
│  - Fully controlled │
└─────────────────────┘
```

---

## Step-by-Step Workflow

### STEP 1: Know What You Have Right Now (Research Phase)
**Time**: 5 minutes
**Action**: Run environment discovery
```bash
# See what's installed on YOUR machine
bash scripts/check-testcontainers.sh

# OR run the comprehensive parity check
bash .claude/hooks/session-start-parity-check.sh
```

**Output**: Tells you what's missing locally compared to Claude Code web

**Example**:
```
✓ 15 Passed
✗ 7 Failed (languages, build tools)
⚠ 3 Warnings
```

---

### STEP 2: Create Local Docker Mirror (Setup Phase)
**Time**: 30 minutes to 2 hours
**Action**: Create `docker-compose.yml` matching Claude Code web specs

**Option A: Minimal** (Just what ggen needs)
```yaml
version: '3.9'
services:
  dev:
    image: ubuntu:24.04
    container_name: ggen-dev
    volumes:
      - .:/workspace
    working_dir: /workspace
    stdin_open: true
    tty: true
    command: /bin/bash
    environment:
      RUST_BACKTRACE: "1"
      RUST_LOG: "info"
```

**Option B: Complete** (Full polyglot setup matching Claude Code web)
```yaml
version: '3.9'
services:
  dev:
    image: ubuntu:24.04  # Or use ggen's existing Dockerfile
    container_name: ggen-dev
    volumes:
      - .:/workspace
      - /var/run/docker.sock:/var/run/docker.sock  # For nested Docker
    working_dir: /workspace
    stdin_open: true
    tty: true
    command: /bin/bash
    environment:
      RUST_BACKTRACE: "1"
      RUST_LOG: "info"
      CARGO_TERM_COLOR: "always"
      NODE_ENV: "development"
    # Build script to install all languages
    build:
      context: .
      dockerfile: Dockerfile.dev  # Create this
```

**Create `Dockerfile.dev`**:
```dockerfile
FROM ubuntu:24.04

# Rust
RUN apt-get update && apt-get install -y \
    rustup curl git build-essential pkg-config libssl-dev \
    && rustup update stable \
    && rustup toolchain install stable

# Node.js
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y nodejs

# Python
RUN apt-get install -y python3.11 python3-pip

# Go
RUN curl -L https://golang.org/dl/go1.24.7.linux-amd64.tar.gz | tar -C /usr/local -xz

# Java
RUN apt-get install -y openjdk-21-jdk

# Ruby
RUN apt-get install -y ruby3.3

# PostgreSQL & Redis clients
RUN apt-get install -y postgresql-client redis-tools

# Cleanup
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
```

---

### STEP 3: Test Environment Locally (Iteration Phase)
**Time**: 10-15 minutes per iteration
**Action**: Start container and run parity check inside

```bash
# Start your Docker container
docker-compose up -d dev

# Enter the container
docker-compose exec dev bash

# Inside container, run the parity check
bash .claude/hooks/session-start-parity-check.sh

# Expected output:
# ✓ 22 Passed
# ✗ 0 Failed
# ⚠ 1-2 Warnings (Docker daemon, optional tools)
```

**If checks fail**:
1. Note what failed (e.g., "Rust not found")
2. Update your Dockerfile.dev to install it
3. Rebuild: `docker-compose build dev`
4. Test again: `docker-compose up -d && docker-compose exec dev bash`

**Repeat until**:
- All critical checks pass (22/22 or close)
- cargo-make installs successfully
- testcontainers is available

---

### STEP 4: Test SessionStart Hook Locally (Refinement Phase)
**Time**: 5-10 minutes
**Action**: Run the actual SessionStart hook in your local container

```bash
# Inside container:
export CLAUDE_PROJECT_DIR=/workspace
bash .claude/hooks/session-start.sh

# Expected: Hook runs, installs cargo-make, sets env vars, no errors
```

**If hook fails**:
1. Check error message
2. Update hook to handle edge cases
3. Retest in container
4. Commit changes to `.claude/hooks/session-start.sh`

---

### STEP 5: Deploy to Claude Code Web (Deployment Phase)
**Time**: 2 minutes
**Action**: Push your tested hook to the branch

```bash
# Make sure hook is finalized and tested
git add .claude/hooks/session-start.sh

# Commit (if you modified it)
git commit -m "refactor: Finalize SessionStart hook after local iteration

Tested in local Docker environment. All parity checks pass:
- 22/22 environment checks pass
- cargo-make installs automatically
- testcontainers configured and ready
- All required languages available"

# Push to your feature branch
git push -u origin claude/setup-docker-testcontainers-H7grF
```

---

### STEP 6: Claude Code Web Runs It (Production Phase)
**Time**: Automatic
**Action**: Claude Code web runs SessionStart hook on next session start

```bash
# What happens automatically in Claude Code web:
# 1. Session starts
# 2. SessionStart hook runs: .claude/hooks/session-start.sh
# 3. Hook checks environment
# 4. cargo-make installs (if not present)
# 5. All env vars set
# 6. You're ready to work
```

---

## Quick Reference: The Workflow Loop

```
1. Run parity check locally
   ↓
2. See what's missing
   ↓
3. Update Docker setup
   ↓
4. Rebuild container
   ↓
5. Test again in container
   ↓
6. When all checks pass:
   ├─ Commit hook to git
   └─ Push to branch
       ↓
7. Claude Code web runs it
```

---

## Files Involved

### Read-Only (Don't Modify)
- `Cargo.toml` - Already has testcontainers
- `crates/ggen-e2e/` - E2E test suite (ready to use)
- `Makefile.toml` - Build system (will work once cargo-make installs)

### You'll Create/Modify
- `docker-compose.yml` - Your local dev environment
- `Dockerfile.dev` - Instructions to build the container
- `.claude/hooks/session-start.sh` - SessionStart hook (iterate then finalize)
- `docs/ENVIRONMENT_SETUP.md` - Instructions (reference)
- `docs/ENVIRONMENT_DELTA.md` - Analysis (reference)

### Already Ready
- `scripts/check-testcontainers.sh` - Infrastructure check ✅
- `.claude/hooks/session-start-parity-check.sh` - Comprehensive check ✅
- `.claude/settings.json` - Hook configuration ✅

---

## Success Criteria

- [ ] Local environment discovery shows what's missing
- [ ] Docker container created with all tools
- [ ] Parity check runs successfully in container (22/22)
- [ ] SessionStart hook runs without errors
- [ ] cargo-make installs and works
- [ ] testcontainers is available in container
- [ ] Hook is committed to your branch
- [ ] Claude Code web can run it in next session

---

## Estimated Time

| Phase | Time | Status |
|-------|------|--------|
| 1. Research | 5 min | ✅ DONE |
| 2. Local Setup | 1-2 hrs | ⏳ YOUR TURN |
| 3. Iteration | 15-30 min | ⏳ YOUR TURN |
| 4. Refinement | 5-10 min | ⏳ YOUR TURN |
| 5. Deployment | 2 min | ⏳ YOUR TURN |
| 6. Production | Auto | ⏳ NEXT SESSION |
| **Total** | **2-3 hrs** | — |

---

## Rollback/Troubleshooting

**If hook breaks Claude Code web**:
1. Revert hook to basic version: `git revert <commit-hash>`
2. Push: `git push`
3. Next session will use basic hook
4. Iterate more locally before re-committing

**If Docker container is slow/broken**:
1. Rebuild: `docker-compose build dev --no-cache`
2. Or restart fresh: `docker-compose down && docker-compose up -d dev`

**If tests fail in Claude Code web**:
1. Check: Is testcontainers trying to use Docker?
2. In Claude Code web: Docker doesn't exist, testcontainers should fail gracefully
3. Tests should pass locally (where Docker exists)
4. Update test code to skip E2E if Docker unavailable

---

## Key Insight

**Your local environment ≠ Claude Code web environment**

But **both can run the same code** because:
- Rust code is identical
- testcontainers works with both local Docker and remote Docker
- SessionStart hook is agnostic about Docker availability
- Tests pass where Docker exists, fail where it doesn't (expected)

The goal isn't perfect parity, it's **knowing the differences** and **handling them gracefully**.
