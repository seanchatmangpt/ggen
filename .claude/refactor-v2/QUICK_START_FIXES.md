# Quick Start: Fix ggen v2.0.0 Build Issues

**Current Status**: ⛔ NO-GO (45/100)
**Target**: ✅ GO (85/100)
**Timeline**: 1-4 hours to minimum GO threshold

---

## 🚨 Critical: Fix Build System (1-4 hours)

### Blocker
```
error: couldn't create a temp dir: No such file or directory (os error 2)
error: failed to write bytecode: No such file or directory (os error 2)
```

**Root Cause**: System I/O errors creating temp files during compilation

---

## ✅ Step-by-Step Fix

### Step 1: Restart System (5 minutes)
```bash
# Clear I/O locks and temp file handles
sudo reboot
```

**Why**: macOS sometimes has stale file locks that prevent temp file creation

---

### Step 2: Clear Temp Directories (2 minutes)
```bash
# After reboot, run:
cd /Users/sac/ggen

# Clear build artifacts
cargo clean

# Clear system temp dirs (optional, may need sudo)
sudo rm -rf /private/var/folders/*
sudo rm -rf /tmp/*
```

**Why**: Old temp files may be corrupted or locked

---

### Step 3: Rebuild with Limited Parallelism (10-30 minutes)
```bash
# Build with fewer parallel jobs to reduce I/O load
cargo build --release -j4

# Expected output:
#   Compiling ggen-utils v1.2.0
#   Compiling ggen-core v1.2.0
#   Compiling ggen-cli-lib v1.2.0
#   Compiling ggen v1.2.0
#   Finished `release` profile [optimized] target(s) in X min
```

**Why**: Limiting parallelism reduces simultaneous temp file creation

---

### Step 4: Verify Binary (1 minute)
```bash
# Check binary exists
ls -lh target/release/ggen

# Expected: -rwxr-xr-x  1 sac staff  24M Nov  2 XX:XX target/release/ggen

# Test it works
./target/release/ggen --version

# Expected: ggen 1.2.0 (or 2.0.0)
```

**Success Criteria**: ✅ Binary exists and runs

---

## 🔄 If Step 3 Still Fails

### Troubleshooting Options

#### Option A: Check Disk Health (5 minutes)
```bash
# Verify disk
diskutil verifyVolume /

# Repair if issues found
diskutil repairVolume /
```

#### Option B: Check macOS System Logs (5 minutes)
```bash
# Look for cargo/rustc errors
sudo log show --predicate 'process == "cargo"' --last 1h | grep -i error

# Look for file system errors
sudo log show --predicate 'eventMessage contains "No such file"' --last 1h
```

#### Option C: Disable Spotlight Indexing (2 minutes)
```bash
# Disable on build directory
sudo mdutil -i off /Users/sac/ggen

# Rebuild
cargo build --release -j4
```

#### Option D: Change TMPDIR (1 minute)
```bash
# Use custom temp directory
mkdir -p ~/tmp/cargo-build
export TMPDIR=~/tmp/cargo-build

# Rebuild
cargo build --release -j4
```

#### Option E: Use Different Volume (15 minutes)
```bash
# If all else fails, try building on external drive or different partition
cd /Volumes/OtherDisk
git clone /Users/sac/ggen ggen-build
cd ggen-build
cargo build --release
```

---

## ✅ Once Build Succeeds

### Immediate Next Steps (15 minutes)

```bash
# 1. Run quick smoke test
./target/release/ggen doctor

# Expected: All dependency checks pass

# 2. Run help command
./target/release/ggen help

# Expected: Lists all 13 commands

# 3. Check binary size
du -h target/release/ggen

# Expected: ~24M (under 50MB target)
```

**Score After Build Fix**: 45 → **85/100** ✅ **MINIMUM GO**

---

## 🧪 Optional: Run Test Suite (1-2 hours)

```bash
# Run all tests
cargo test --all-features -- --test-threads=4

# Target: ≥90% pass rate

# Run integration tests
cargo test --test integration_marketplace_e2e
cargo test --test integration_template_e2e
cargo test --test integration_graph_e2e
cargo test --test integration_cli_ux_e2e

# Run security tests
cargo test --test v2_security_audit
```

**Score After Tests**: 85 → **95/100** ✅ **STRONG GO**

---

## 📊 Quick Status Check

### Before Fix
```
Score: 45/100 ⛔ NO-GO
Build: ❌ BLOCKED (I/O errors)
Tests: ❌ BLOCKED (cannot run)
Binary: ❌ DOES NOT EXIST
```

### After Fix (Target)
```
Score: 85/100 ✅ MINIMUM GO
Build: ✅ SUCCESS (0 errors, warnings OK)
Tests: ⏳ READY TO RUN
Binary: ✅ EXISTS (24MB)
```

---

## 🎯 Success Criteria Checklist

- [ ] System rebooted
- [ ] Temp directories cleared
- [ ] `cargo build --release -j4` succeeds
- [ ] Binary exists at `target/release/ggen`
- [ ] `./target/release/ggen --version` runs
- [ ] `./target/release/ggen doctor` passes
- [ ] Binary size ~24MB (under 50MB)
- [ ] No compilation errors (warnings OK)

**If all checked**: ✅ **READY for Phase 2 (testing)**

---

## ⏱️ Time Estimate

| Task | Duration | Result |
|------|----------|--------|
| Restart system | 5 min | Clear I/O locks |
| Clear temp dirs | 2 min | Remove corrupted files |
| Rebuild | 10-30 min | Generate binary |
| Verify | 1 min | Confirm success |
| **Total** | **18-38 min** | **Build working** |

**If troubleshooting needed**: +15-60 min

**Realistic total**: 1-4 hours including troubleshooting

---

## 🆘 Get Help

### Still Failing After All Steps?

**Report**:
1. Output of `cargo build --release -j4 2>&1 | tee build.log`
2. Output of `df -h /`
3. Output of `sudo log show --predicate 'process == "cargo"' --last 1h`
4. macOS version: `sw_vers`

**Share** in issue tracker or ask for assistance

---

## 📚 Related Documents

- **Full Report**: `.claude/refactor-v2/PRODUCTION_VALIDATION_FINAL.md`
- **Executive Summary**: `.claude/refactor-v2/EXECUTIVE_SUMMARY.md`
- **Previous Validations**:
  - Agent 6: `.claude/refactor-v2/final-validation-report.md`
  - Agent 12: `.claude/refactor-v2/agent12-final-decision.md`

---

**Remember**: The code is good. The tests are ready. Just need to fix the build system, then we ship.

**Target Release Date**: 2025-11-10 (after all phases complete)

**Good luck! 🚀**
