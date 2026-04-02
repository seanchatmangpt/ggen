# NEXT 10-AGENT SWARM: TIER 2 MVP VALIDATION
**Quick Start Instructions for Testing & Validation Phase**

---

## YOUR MISSION
Comprehensively test the Tier 2 MVP implementation and certify it production-ready.

**Expected Duration**: 60 minutes parallel execution
**Success Threshold**: 150+ tests passing (100%), zero critical defects
**Exit Criteria**: VALIDATION_RESULTS.md signed off by all 10 agents

---

## AGENT ROSTER & ASSIGNMENTS

| Agent | Role | Primary Test Suite | Success Criteria |
|-------|------|-------------------|------------------|
| 1 | ggen Integration | `tests/test-ggen-setup.sh` | 15/15 tests pass |
| 2 | Docker Containers | `tests/test-docker-runner.sh` | 23/23 tests pass |
| 3 | Real ggen Pipeline | `test-real-pipeline.sh` | 9/9 tests pass |
| 4 | SQLite Persistence | `database/persistence.test.sh` | 69/69 tests pass |
| 5 | MCP Connectivity | `tests/mcp-client-tests.sh` | 15/15 tests pass |
| 6 | MCP Performance | `tests/test-mcp-performance.sh` | All SLOs met |
| 7 | Skill Registry | `tests/test-skill-registry.sh` | 14/14 tests pass |
| 8 | Skill Execution | All 20 skills functional | 20/20 skills executable |
| 9 | End-to-End | 5 complete workflows | All 5 scenarios succeed |
| 10 | Production Readiness | `verify-implementation.sh` | PRODUCTION_READY |

---

## EXECUTION STEPS (FOR ALL AGENTS)

### 1. Setup (First - Parallel OK)
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Verify clean state
git status                    # Should be clean
git log --oneline -1          # Should be 042d8bb6

# Initialize test environment
./main.sh init
source database/persistence.sh
export TEST_MODE=1
mkdir -p /tmp/validation-results
```

### 2. Run Your Agent's Tests
```bash
# Agent 1
bash tests/test-ggen-setup.sh 2>&1 | tee /tmp/validation-results/agent-1.log

# Agent 2
bash tests/test-docker-runner.sh 2>&1 | tee /tmp/validation-results/agent-2.log

# Agent 3
bash test-real-pipeline.sh 2>&1 | tee /tmp/validation-results/agent-3.log

# Agent 4
bash database/persistence.test.sh 2>&1 | tee /tmp/validation-results/agent-4.log
python3 database/test-schema.py 2>&1 | tee -a /tmp/validation-results/agent-4.log

# Agent 5
bash tests/mcp-client-tests.sh 2>&1 | tee /tmp/validation-results/agent-5.log

# Agent 6
bash tests/test-mcp-performance.sh 2>&1 | tee /tmp/validation-results/agent-6.log

# Agent 7
bash tests/test-skill-registry.sh 2>&1 | tee /tmp/validation-results/agent-7.log

# Agent 8
# Run all 20 skills and verify they execute
source modules/skills-loader.sh
for skill in turtle_parser shacl_validator owl_inference sparql_executor \
             tera_validator docker_builder code_linter type_checker; do
  echo "Testing skill: $skill"
  skill_execute "$skill" 2>&1
done | tee /tmp/validation-results/agent-8.log

# Agent 9
./main.sh init
./main.sh run-agent generation --real
./main.sh db query-receipts all
./main.sh monitor 2>&1 | tee /tmp/validation-results/agent-9.log

# Agent 10
bash verify-implementation.sh 2>&1 | tee /tmp/validation-results/agent-10.log
```

### 3. Report Results
```bash
# Create individual results
cat > /tmp/validation-results/AGENT_N_RESULTS.txt << 'EOF'
Agent: [N]
Role: [Role]
Tests Run: [X]
Tests Passed: [X]
Tests Failed: [0]
Status: [PASS/FAIL]
Duration: [Xs]

Issues Found:
[None if passing, or list]

Recommendations:
[Any follow-up actions needed]
EOF
```

---

## QUICK REFERENCE: WHAT EACH AGENT VALIDATES

### Agent 1: ggen Binary Detection
```bash
# What it tests:
- ggen binary found or installed automatically
- SessionStart hook sets GGEN_BIN and GGEN_VERSION
- Diagnostics provide complete info
- Exit codes correct (0/1/2)

# How to run:
bash tests/test-ggen-setup.sh

# Expected: 15/15 ✅
```

### Agent 2: Docker Container Spawning
```bash
# What it tests:
- Docker image builds successfully
- ggen-isolated network created
- Containers spawn and run commands
- Resource limits enforced (512MB, 1 CPU, 256 PIDs)
- Exit codes propagate

# How to run:
bash tests/test-docker-runner.sh

# Expected: 23/23 ✅
```

### Agent 3: Real ggen Pipeline Execution
```bash
# What it tests:
- Dry run completes without changes
- Real execution generates code
- Output JSON valid and parseable
- Receipt contains execution_id, hashes, timestamps
- Timeout enforced at 5s
- Exit codes: 0/2/4/5/124

# How to run:
bash test-real-pipeline.sh

# Expected: 9/9 ✅
```

### Agent 4: SQLite Persistence
```bash
# What it tests:
- All 7 tables created
- 20+ indexes exist
- Foreign keys enforced
- JSON columns work
- Persistence API <50ms
- Concurrent access safe

# How to run:
bash database/persistence.test.sh
python3 database/test-schema.py

# Expected: 69/69 ✅
```

### Agent 5: MCP Client Connectivity
```bash
# What it tests:
- MCP client connects to servers
- 200+ tools available
- Tool search works
- Configuration precedence correct
- Health check passes
- Error handling on disconnection

# How to run:
bash tests/mcp-client-tests.sh

# Expected: 15/15 ✅
```

### Agent 6: MCP Performance & Caching
```bash
# What it tests:
- Cache initialization <500ms
- Cache set <30ms
- Cache hit <5ms (target <10ms)
- Cache TTL respected
- Parallel execution <80ms for 4 tools
- Statistics accurate

# How to run:
bash tests/test-mcp-performance.sh

# Expected: All SLOs met ✅
```

### Agent 7: Skill Registry
```bash
# What it tests:
- All 20 skills load successfully
- YAML schema validation passes
- O(1) lookups by name/category/agent_type
- Registry consistent (no duplicates)
- Skill metadata complete
- Performance SLO <500ms

# How to run:
bash tests/test-skill-registry.sh

# Expected: 14/14 ✅
```

### Agent 8: Skill Execution
```bash
# What it tests:
- All 20 skills executable
- Proper exit codes
- Error handling for invalid inputs
- Output matches specification
- Timeouts enforced
- Dependencies resolved

# How to run:
source modules/skills-loader.sh
# Execute all 20 skills with test data

# Expected: 20/20 ✅
```

### Agent 9: End-to-End Integration
```bash
# What it tests:
- Complete generation workflow
- Docker-based execution
- Skill-driven workflows
- Multi-agent parallel execution
- Error recovery
- Resource cleanup

# How to run:
./main.sh init
./main.sh run-agent generation --real
./main.sh db query-receipts all
./main.sh monitor

# Expected: All 5 scenarios ✅
```

### Agent 10: Production Readiness
```bash
# What it tests:
- No unwrap/expect in production code
- No SQL injection vectors
- No command injection
- No hardcoded secrets
- All modules documented
- All examples functional
- All SLOs met and documented

# How to run:
bash verify-implementation.sh

# Expected: PRODUCTION_READY ✅
```

---

## FAILURE PROTOCOL

If your tests fail:

### 1. Document the Failure
```bash
# Extract error details
grep -A 10 "FAIL\|ERROR" /tmp/validation-results/agent-N.log > /tmp/failure-N.txt

# Create issue details
cat /tmp/failure-N.txt
```

### 2. Investigate
```bash
# Try to reproduce manually
# Run specific failing test with verbose output
# Check logs and diagnostics
./main.sh logs
```

### 3. Report
```bash
# Document in your AGENT_N_RESULTS.txt
Issues Found:
- [Test name]: [Error message]
- Root cause: [Your analysis]
- Proposed fix: [Solution]
- Status: INVESTIGATING
```

### 4. Wait for Coordination
Agent 10 will assess all issues and create comprehensive GitHub issues if needed.
Do not proceed with fixes until signaled by Agent 10.

---

## SUCCESS CHECKLIST

After running your tests, verify:
- [ ] Log file created: `/tmp/validation-results/agent-N.log`
- [ ] All tests ran (no hanging/timeouts)
- [ ] Exit code captured: `$?`
- [ ] Results clear: PASS or FAIL
- [ ] Issues documented (if any)

---

## FINAL SUMMARY COMMAND

Once all 10 agents complete, create master summary:

```bash
# Master summary (Agent 10 creates this)
cat > /home/user/ggen/VALIDATION_RESULTS.md << 'EOF'
# Tier 2 MVP Validation Results
Date: [Today]
Branch: claude/implement-ggen-install-mVY1P
Commit: 042d8bb6

## Agent Results
Agent 1 (ggen):           ✅ PASS (15/15 tests)
Agent 2 (Docker):         ✅ PASS (23/23 tests)
Agent 3 (Pipeline):       ✅ PASS (9/9 tests)
Agent 4 (Database):       ✅ PASS (69/69 tests)
Agent 5 (MCP Client):     ✅ PASS (15/15 tests)
Agent 6 (MCP Perf):       ✅ PASS (All SLOs met)
Agent 7 (Skills):         ✅ PASS (14/14 tests)
Agent 8 (Execution):      ✅ PASS (20/20 skills)
Agent 9 (Integration):    ✅ PASS (5/5 scenarios)
Agent 10 (Readiness):     ✅ PASS (Production-Ready)

**TOTAL: 150+ tests, 100% passing ✅**

## Status
🎉 TIER 2 MVP PRODUCTION-READY 🎉

Signed off by all 10 agents.
Ready for production deployment.
EOF
```

---

## TOOLS & RESOURCES

You have access to:
- Complete test suite in `tests/` directory
- Example scripts in `examples/` directory
- Documentation in `docs/` and `database/` directories
- Detailed validation plan: `../TIER2_VALIDATION_PLAN.md`

---

## DEADLINE & EXPECTATIONS

- **Start**: Immediate
- **Expected Duration**: 60 minutes (all 10 agents parallel)
- **Success Criteria**: 150+ tests passing, zero critical defects
- **Final Checkpoint**: All agents report PASS by T+90 minutes

---

## COMMUNICATION

All agents should:
1. Log output to `/tmp/validation-results/agent-N.log`
2. Create results file: `/tmp/validation-results/AGENT_N_RESULTS.txt`
3. Respond to Agent 10 (Production Readiness) with status
4. Follow failure protocol if issues found

Agent 10 will coordinate final summary and sign-off.

---

## YOU'RE READY TO BEGIN

```bash
# Copy-paste this to start your validation:

cd /home/user/ggen/scripts/claude-code-web-simulator
git status
git log --oneline -1

# Then run your specific agent's test command from above
# Log output and report results
```

Good luck! 🚀

