# Andon Signal Validation Framework - Troubleshooting Guide

## Common Issues and Solutions

### Issue 1: "ggen binary not found" when running verify-cli

**Symptom**:
```bash
$ cargo make verify-cli
Error: ggen binary not found
  Build ggen first: cargo make build-release
```

**Solution**:
The framework now auto-builds the binary if missing. If you still see this error:

```bash
# Manual build
cargo make build-release

# Then verify
cargo make verify-cli
```

**Prevention**: The `verify-cli` task now automatically builds the binary if it's missing.

---

### Issue 2: Act not installed

**Symptom**:
```bash
$ cargo make act-validation
❌ Error: act is not installed
```

**Solution**:
```bash
# macOS
brew install act

# Linux
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash

# Verify installation
cargo make act-status
```

---

### Issue 3: Docker not running

**Symptom**:
```bash
$ cargo make act-validation
❌ Error: Docker is not running
```

**Solution**:
```bash
# Start Docker Desktop (macOS/Windows)
# Or start Docker daemon (Linux)
sudo systemctl start docker

# Verify
docker ps
```

---

### Issue 4: Validation report shows failures

**Symptom**:
```bash
$ cargo make validation-report
❌ Compilation: FAILED
⚠️  Unit Tests: FAILED
❌ CLI Verification: FAILED
```

**Solution**:

1. **Check which layer failed**:
   ```bash
   cargo make validation-report
   ```

2. **Fix Layer 1 (Compile-Time)**:
   ```bash
   cargo make check    # See compilation errors
   cargo make lint     # See linting errors
   ```

3. **Fix Layer 2 (Test-Time)**:
   ```bash
   cargo make test-unit    # See test failures
   cargo make test-clnrm   # See integration test failures
   ```

4. **Fix Layer 3 (Runtime)**:
   ```bash
   cargo make verify-cli   # See CLI command failures
   ```

5. **Re-run validation**:
   ```bash
   cargo make validation-report
   ```

---

### Issue 5: GitHub Actions workflow fails

**Symptom**: Workflow fails in CI/CD with action errors.

**Solution**:

1. **Test locally first**:
   ```bash
   cargo make act-validation DRYRUN=true
   ```

2. **Check workflow syntax**:
   ```bash
   cargo make act-validate
   ```

3. **Review workflow file**:
   - Check action references
   - Verify required secrets
   - Check platform compatibility

---

### Issue 6: Pre-commit hook fails

**Symptom**:
```bash
$ git commit
❌ Pre-commit validation FAILED
```

**Solution**:

1. **Run validation manually**:
   ```bash
   cargo make pre-commit
   ```

2. **Fix failing checks** (see Issue 4)

3. **Re-run pre-commit**:
   ```bash
   cargo make pre-commit
   ```

4. **If needed, skip hooks** (not recommended):
   ```bash
   git commit --no-verify
   ```

---

### Issue 7: Validation takes too long

**Symptom**: Validation runs but takes >5 minutes.

**Solution**:

1. **Check for lock contention**:
   ```bash
   # Check if other cargo processes are running
   ps aux | grep cargo
   ```

2. **Run individual layers**:
   ```bash
   cargo make check        # Layer 1 only
   cargo make test-unit    # Layer 2 only
   cargo make verify-cli   # Layer 3 only
   ```

3. **Use incremental validation**:
   - Only run changed layers
   - Use `cargo make check` for quick feedback

---

### Issue 8: Act workflow fails locally but works on GitHub

**Symptom**: `cargo make act-validation` fails but GitHub Actions succeeds.

**Possible Causes**:
1. Missing secrets (create `.secrets` file)
2. Missing environment variables
3. Action compatibility issues
4. Platform differences (Linux vs macOS)

**Solution**:

1. **Create `.secrets` file** (if needed):
   ```bash
   # .secrets
   GITHUB_TOKEN=your_token_here
   ```

2. **Check action compatibility**:
   ```bash
   act --dryrun -W .github/workflows/andon-validation.yml
   ```

3. **Compare environments**:
   - Check Rust version
   - Check Docker version
   - Check platform (Linux vs macOS)

---

### Issue 9: Monitoring script shows false alerts

**Symptom**: `cargo make monitor-validation` alerts but validation actually passed.

**Solution**:

1. **Regenerate report**:
   ```bash
   cargo make validation-report
   ```

2. **Check report file**:
   ```bash
   cat validation-report.txt
   ```

3. **Re-run monitoring**:
   ```bash
   cargo make monitor-validation validation-report.txt
   ```

---

### Issue 10: Validation passes but CLI commands still fail

**Symptom**: All validation layers pass, but CLI commands fail in production.

**Possible Causes**:
1. Environment differences
2. Missing dependencies
3. Path issues
4. Permission issues

**Solution**:

1. **Test in production-like environment**:
   ```bash
   docker run -it rust:1.90 bash
   # Test commands inside container
   ```

2. **Check dependencies**:
   ```bash
   # Verify all required tools are installed
   which ggen
   ggen --version
   ```

3. **Check paths and permissions**:
   ```bash
   ls -la target/release/ggen
   ./target/release/ggen --version
   ```

---

## Debug Mode

### Enable Verbose Output

```bash
# Validation report with verbose output
RUST_BACKTRACE=1 cargo make validation-report

# Act with verbose output
act -v -W .github/workflows/andon-validation.yml

# CLI verification with debug
bash -x scripts/verify-cli-commands.sh
```

### Check Logs

```bash
# Build logs
cat /tmp/ggen-build.log

# Validation logs
cat validation-report.txt

# Act logs
act -W .github/workflows/andon-validation.yml 2>&1 | tee act.log
```

---

## Getting Help

### Check Status

```bash
# Framework status
cargo make act-status
cargo make validation-report
cargo make monitor-validation
```

### Review Documentation

- Quick Start: `docs/innovation/QUICK_START.md`
- Framework Design: `docs/innovation/ANDON_VALIDATION_FRAMEWORK.md`
- Implementation: `docs/innovation/PHASE*_IMPLEMENTATION.md`
- User Findings: `docs/innovation/USER_FINDINGS.md`

### Common Commands

```bash
# Validation
cargo make verify-cli              # Verify CLI commands
cargo make validation-report       # Generate report
cargo make monitor-validation      # Monitor and alert

# Testing
cargo make act-validation          # Test with act
cargo make act-validation DRYRUN=true  # Dry-run mode
cargo make act-status              # Check act installation

# Debugging
cargo make check                   # Compile-time check
cargo make lint                    # Linting
cargo make test-unit               # Unit tests
```

---

## Still Having Issues?

1. **Check framework status**: `docs/innovation/STATUS.md`
2. **Review user findings**: `docs/innovation/USER_FINDINGS.md`
3. **Check GitHub Actions**: View workflow runs in GitHub
4. **Review logs**: Check validation-report.txt and build logs

---

**Last Updated**: 2025-12-12  
**Framework Version**: v1.0.0

