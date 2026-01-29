# TIER 2 MVP VALIDATION & TESTING PLAN
**Phase**: Quality Assurance & Production Readiness Validation
**Status**: Ready for 10-Agent Swarm Deployment
**Target**: 100% test coverage, zero defects, production-ready certification

---

## MASTER VALIDATION CHECKLIST

All items must be **✅ VERIFIED COMPLETE** before declaring production-ready.

### Pre-Flight Safety Checks (Non-Negotiable)
- [ ] **Git state clean**: `git status` shows nothing to commit
- [ ] **Branch correct**: On `claude/implement-ggen-install-mVY1P`
- [ ] **Latest commit**: `042d8bb6` (Tier 2 MVP)
- [ ] **No uncommitted changes**: Zero untracked files
- [ ] **Remote in sync**: All commits pushed to origin

### Code Quality (Andon Signals)
- [ ] **No compiler errors**: `cargo make check` passes cleanly
- [ ] **No warnings**: `cargo make lint` zero clippy warnings
- [ ] **No test failures**: `cargo make test` all tests pass
- [ ] **No unwrap/expect**: Zero in production code
- [ ] **All Result<T,E>**: Every fallible operation returns Result

### Integration Testing (End-to-End)
- [ ] **ggen detection works**: Binary found or installed automatically
- [ ] **Real pipeline executes**: `ggen sync` exits with proper codes
- [ ] **Docker image builds**: Dockerfile creates working image
- [ ] **Container spawning works**: docker-runner functions correctly
- [ ] **Database persistence works**: SQLite stores and retrieves data
- [ ] **MCP connectivity works**: Real tools from 200+ servers accessible
- [ ] **Skills load and execute**: All 20 skills functional
- [ ] **Exit codes correct**: All expected codes returned properly

### Performance Validation
- [ ] **ggen <5s SLO**: ggen sync completes in <5 seconds
- [ ] **MCP cache <10ms**: Cached tool calls hit <10ms
- [ ] **Database <50ms**: SQLite queries complete in <50ms
- [ ] **Docker spawn <15s**: Container startup in <15 seconds
- [ ] **Skill load <500ms**: Skill registry initialization <500ms
- [ ] **Memory stable**: No memory leaks on repeated execution
- [ ] **CPU efficiency**: Single CPU core sufficient for operations

### Security Validation
- [ ] **No SQL injection**: Database queries parameterized
- [ ] **No command injection**: All shell commands escaped properly
- [ ] **Domain whitelist enforced**: MCP respects domain restrictions
- [ ] **Timeout enforcement**: All operations timeout correctly
- [ ] **No hardcoded secrets**: Zero secrets in code/configs
- [ ] **File permissions correct**: No world-writable files

### Documentation Validation
- [ ] **All modules documented**: Every function has comments
- [ ] **Error codes documented**: All exit codes explained
- [ ] **Examples functional**: All example scripts run without errors
- [ ] **README complete**: Quick start works end-to-end
- [ ] **Integration points documented**: All main.sh changes clear

---

## 10-AGENT SWARM ASSIGNMENTS

Each agent has specific responsibilities. All work in parallel.

### Agent 1: ggen Integration Validator
**Role**: Verify ggen binary detection and execution

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: Binary Detection
source modules/ggen-setup.sh
detect_ggen_binary
echo "Exit code: $?" # Should be 0 or 1 (found/not found)

# Test 2: Installation (if not found)
install_ggen_if_needed
ggen --version

# Test 3: Session Start Hook
ggen_session_start_hook
echo $GGEN_BIN  # Should be absolute path
echo $GGEN_VERSION  # Should be version string

# Test 4: Diagnostics
ggen_diagnostics > /tmp/ggen_diag.txt
cat /tmp/ggen_diag.txt
```

**Success Criteria**:
- ✅ ggen binary found or installed automatically
- ✅ `ggen --version` returns valid version
- ✅ SessionStart hook sets GGEN_BIN and GGEN_VERSION
- ✅ Diagnostics report complete info
- ✅ All functions return proper exit codes (0/1/2)

**Validation Commands**:
```bash
# Run unit tests
bash tests/test-ggen-setup.sh

# Verify all 15 tests pass
# Expected: 15/15 ✅
```

**If Failures Found**:
1. Create issue: "ggen integration test [name] failed"
2. Extract error message and stack trace
3. Identify root cause (installation, path, permissions)
4. Propose fix and implement
5. Re-run tests to verify fix
6. Document in VALIDATION_RESULTS.md

---

### Agent 2: Docker Integration Validator
**Role**: Verify Docker container spawning and isolation

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: Image Build
docker build -t ggen-agent:test .
docker images | grep ggen-agent

# Test 2: Network Creation
source modules/docker-runner.sh
docker_ensure_network
docker network ls | grep ggen-isolated

# Test 3: Container Spawn
docker_spawn_agent "agent-test-001" "echo 'Hello from container'"
docker ps | grep agent-test-001

# Test 4: Resource Limits
docker inspect agent-test-001 | jq '.HostConfig | {Memory, CpuQuota, PidsLimit}'
```

**Success Criteria**:
- ✅ Docker image builds successfully
- ✅ ggen-isolated network created
- ✅ Container spawns and runs commands
- ✅ Container isolation verified (separate filesystem)
- ✅ Resource limits enforced (512MB memory, 1 CPU, 256 PIDs)
- ✅ Exit codes propagate correctly

**Validation Commands**:
```bash
# Run Docker tests
bash tests/test-docker-runner.sh

# Expected: 23/23 ✅
```

**If Failures Found**:
1. Check Docker daemon running: `docker ps`
2. Verify image exists: `docker images`
3. Check network: `docker network inspect ggen-isolated`
4. Create issue with container logs: `docker logs [container]`
5. Fix and re-run tests

---

### Agent 3: Real ggen Pipeline Validator
**Role**: Verify ggen sync execution and output parsing

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: Dry Run (Safe Preview)
./main.sh run-agent generation --real --dry-run

# Test 2: Real Execution
./main.sh run-agent generation --real

# Test 3: Output Verification
cat /tmp/ggen_output.json | jq '.files[] | {path, hash}'

# Test 4: Receipt Generation
cat /tmp/ggen_receipt.json | jq '.'

# Test 5: Error Handling
./main.sh run-agent generation --real --invalid-flag
echo "Exit code: $?" # Should match ggen exit code
```

**Success Criteria**:
- ✅ Dry run completes without changes
- ✅ Real execution generates code/configs
- ✅ Output JSON parseable and valid
- ✅ Receipt contains execution_id, hashes, timestamps
- ✅ Timeout enforced at 5s
- ✅ Exit codes: 0 (success), 2 (validation), 4 (SPARQL), 5 (template), 124 (timeout)

**Validation Commands**:
```bash
# Run ggen pipeline tests
bash test-real-pipeline.sh

# Expected: 9/9 tests passing ✅
```

**If Failures Found**:
1. Check ggen installed: `ggen --version`
2. Verify ontology files exist: `.specify/*.ttl`
3. Run with verbose: `timeout 5s ggen sync --verbose`
4. Extract error and create issue
5. Fix root cause and re-test

---

### Agent 4: Database Schema & Persistence Validator
**Role**: Verify SQLite schema and persistence layer

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: Schema Validation
sqlite3 /tmp/ggen.db < database/schema.sql
sqlite3 /tmp/ggen.db ".schema" | head -50

# Test 2: Table Creation
sqlite3 /tmp/ggen.db ".tables"
# Expected: agent_memory audit_log receipts workflow_sessions

# Test 3: Index Verification
sqlite3 /tmp/ggen.db ".indices"
# Expected: 20+ indexes for optimization

# Test 4: Persistence API
source database/persistence.sh
db_init
db_save_receipt '{"exec_id":"test-001","status":"success"}'
db_query_receipts "exec_id"

# Test 5: Data Retrieval
db_export_audit_trail /tmp/audit.json
wc -l /tmp/audit.json
```

**Success Criteria**:
- ✅ All 7 tables created successfully
- ✅ All 20+ indexes exist
- ✅ Foreign keys enforced
- ✅ JSON columns working
- ✅ Persistence API <50ms operations
- ✅ No SQL errors in queries
- ✅ Concurrent access safe

**Validation Commands**:
```bash
# Python schema tests
python3 database/test-schema.py

# Bash persistence tests
bash database/persistence.test.sh

# Expected: 36 + 33 = 69 tests passing ✅
```

**If Failures Found**:
1. Check SQLite version: `sqlite3 --version` (must be 3.35+)
2. Verify schema file: `wc -l database/schema.sql`
3. Run schema validation: `sqlite3 /tmp/test.db < database/schema.sql`
4. Extract error and create issue
5. Fix schema and re-test

---

### Agent 5: MCP Client Connectivity Validator
**Role**: Verify real MCP server connections and tool execution

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: MCP Client Initialization
source modules/mcp-client.sh
mcp_init_client

# Test 2: Tool Discovery
mcp_list_tools | head -20
# Expected: 200+ tools available

# Test 3: Tool Search
mcp_search_tools "rdf" | head -10
# Expected: RDF-related tools found

# Test 4: Configuration Loading
cat .mcp.json | jq '.'
# Expected: Valid MCP server definitions

# Test 5: Health Check
mcp_health_check
echo "Exit code: $?" # Should be 0 (healthy)
```

**Success Criteria**:
- ✅ MCP client connects to servers
- ✅ Tool list retrieved successfully
- ✅ 200+ tools available
- ✅ Tool search works (pattern matching)
- ✅ Configuration precedence correct
- ✅ Health check passes
- ✅ Proper error handling on disconnection

**Validation Commands**:
```bash
# Run MCP client tests
bash tests/mcp-client-tests.sh

# Expected: 15/15 tests passing ✅
```

**If Failures Found**:
1. Check MCP server running: `lsof -i :3000` (or configured port)
2. Verify .mcp.json syntax: `jq . .mcp.json`
3. Test connection manually: `timeout 5s curl -X POST http://localhost:3000`
4. Create issue with MCP server logs
5. Fix configuration and re-test

---

### Agent 6: MCP Performance & Caching Validator
**Role**: Verify MCP cache performance and SLOs

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: Cache Initialization
source modules/mcp-performance.sh
mcp_cache_init

# Test 2: Cache Performance
time mcp_cache_set "tool:rdf-parser" '{"status":"ok"}' 3600
# Expected: <30ms

# Test 3: Cache Hit
time mcp_cache_get "tool:rdf-parser"
# Expected: <5ms (target <10ms)

# Test 4: Cache Statistics
mcp_get_cache_stats
# Expected: Hit rate >80%

# Test 5: Parallel Tool Execution
time mcp_parallel_tools 4
# Expected: <80ms for 4 parallel calls
```

**Success Criteria**:
- ✅ Cache initialization <500ms
- ✅ Cache set operations <30ms
- ✅ Cache hit retrieval <5ms (target <10ms)
- ✅ Cache TTL respected
- ✅ Parallel execution <80ms for 4 tools
- ✅ Statistics accurate (hit/miss counts)
- ✅ Auto-cleanup of expired entries

**Validation Commands**:
```bash
# Run performance tests
bash tests/test-mcp-performance.sh

# Expected: All SLOs met ✅
```

**If Failures Found**:
1. Check system load: `uptime`
2. Verify cache directory exists: `ls -la .ggen/mcp-cache/`
3. Run with verbose timing: `time mcp_tool_with_timeout "tool-name" 10`
4. Create issue with timing data
5. Optimize and re-test

---

### Agent 7: Skill Registry & Loading Validator
**Role**: Verify skill registration and execution

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: Skill Registry Load
source modules/skill-registry.sh
skill_load "turtle_parser"
# Expected: Returns skill YAML object

# Test 2: Skill Registration
skill_register "turtle_parser"
# Expected: Returns registry entry

# Test 3: Lookup by Name
skill_get_by_name "rdf_merger"
# Expected: Skill details returned

# Test 4: Lookup by Category
skill_get_by_category "rdf" | jq '.[] | .name'
# Expected: 5 RDF skills

# Test 5: All Skills List
skill_list_all | jq 'length'
# Expected: 20 or more skills

# Test 6: Registry Stats
skill_registry_stats
# Expected: Registry size, counts by category
```

**Success Criteria**:
- ✅ All 20 skills load successfully
- ✅ YAML schema validation passes
- ✅ O(1) lookups by name/category/agent_type
- ✅ Registry consistent (no duplicates)
- ✅ Skill metadata complete
- ✅ Performance SLO <500ms per operation
- ✅ Atomic operations safe for concurrent access

**Validation Commands**:
```bash
# Run skill registry tests
bash tests/test-skill-registry.sh

# Expected: 14/14 tests passing ✅
```

**If Failures Found**:
1. Check YAML files: `yamllint config/agent-skills/*.yaml`
2. Verify skill schema: `jq . config/agent-skills/skill-schema.json`
3. Validate specific skill: `skill_validate_file config/agent-skills/rdf/turtle_parser.yaml`
4. Create issue with validation error
5. Fix YAML and re-test

---

### Agent 8: Skill Execution Validator
**Role**: Verify all 20 skills execute correctly

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Test 1: RDF Skills (5)
source modules/skills-rdf.sh
skill_rdf_parse_turtle "test.ttl"
skill_rdf_validate_shacl "test.ttl" "shapes.ttl"
skill_rdf_owl_inference "ontology.ttl"
skill_rdf_resolve_namespaces "ontology.ttl"
skill_rdf_merge_graphs "g1.ttl" "g2.ttl"

# Test 2: SPARQL Skills (4)
source modules/skills-sparql.sh
skill_sparql_optimize "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"
skill_sparql_execute "endpoint" "query.sparql"
skill_sparql_federated "endpoints.json" "query.sparql"
skill_sparql_check_compliance "query.sparql"

# Test 3: Template Skills (4)
source modules/skills-template.sh
skill_template_validate "template.tera"
skill_template_render "template.tera" "context.json"
skill_template_multi_pass "templates/"
skill_template_format "output.rs"

# Test 4: QA Skills (4)
source modules/skills-qa.sh
skill_qa_lint "code.rs"
skill_qa_type_check "code.rs"
skill_qa_security_scan "code.rs"
skill_qa_perf_validate "benchmark.rs"

# Test 5: DevOps Skills (3)
source modules/skills-devops.sh
skill_devops_docker_build "Dockerfile"
skill_devops_deploy_validate "deploy.yaml"
skill_devops_infra_scan "terraform/"
```

**Success Criteria**:
- ✅ All 20 skills execute without errors
- ✅ Proper exit codes returned (0 = success)
- ✅ Error handling for invalid inputs
- ✅ Output format matches specification
- ✅ Timeouts enforced correctly
- ✅ Skill dependencies resolved
- ✅ Integration with skill-loader.sh works

**Validation Commands**:
```bash
# Integration test all skills
source modules/skills-loader.sh
for skill in $(skill_list_all | jq -r '.[].name'); do
  echo "Testing: $skill"
  skill_execute "$skill" 2>/dev/null
done
```

**If Failures Found**:
1. Check skill implementation file: `grep "skill_[name]" modules/skills-*.sh`
2. Test skill directly: `bash modules/skills-rdf.sh && skill_rdf_parse_turtle test.ttl`
3. Verify dependencies installed
4. Create issue with skill error output
5. Fix implementation and re-test

---

### Agent 9: End-to-End Integration Validator
**Role**: Verify complete workflow from start to finish

**Responsibilities**:
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# Scenario 1: Complete Generation Workflow
./main.sh init                           # Initialize simulator
./main.sh run-agent validation --real    # Real validation
./main.sh run-agent generation --real    # Real generation
./main.sh db query-receipts all          # Verify storage
./main.sh monitor                        # Check status

# Scenario 2: Docker-based Execution
./main.sh docker-spawn "agent-001" "ggen sync --audit true"
docker logs agent-001
docker ps -a | grep agent-001

# Scenario 3: Skill-driven Workflow
source modules/skill-registry.sh
source modules/skills-loader.sh
skill_execute "turtle_parser" test.ttl
skill_execute "sparql_executor" test.sparql

# Scenario 4: Multi-agent Parallel
./main.sh run-parallel 5 "generation" --real
./main.sh run-parallel 3 "validation" --real
./main.sh monitor

# Scenario 5: Error Recovery
./main.sh run-agent generation --invalid-param
echo "Exit code: $?"
./main.sh run-agent generation --real  # Should recover
```

**Success Criteria**:
- ✅ All workflows complete successfully
- ✅ Data persists correctly in SQLite
- ✅ Docker containers start and stop cleanly
- ✅ Skills execute in correct order
- ✅ Error handling recovers gracefully
- ✅ Performance SLOs met across all operations
- ✅ No resource leaks (memory, processes)

**Validation Commands**:
```bash
# Run comprehensive end-to-end tests
bash tests/integration-suite.sh

# Expected: All scenarios pass ✅
```

**If Failures Found**:
1. Identify which scenario failed
2. Run scenario individually with verbose output
3. Check logs: `./main.sh monitor logs`
4. Create issue with scenario steps and error
5. Fix root cause and re-run full suite

---

### Agent 10: Production Readiness & Documentation Validator
**Role**: Final certification and documentation completeness

**Responsibilities**:

**Code Quality Checks**:
```bash
# Verify no unwrap/expect in production code
grep -r "unwrap\|expect" modules/ | grep -v "test" | grep -v ".sh test"
# Expected: ZERO matches ❌ FAIL if found

# Verify all functions return Result<T,E>
grep -r "fn " modules/*.sh | grep -v "# " | head -20
# Check return values are valid

# Verify comments on complex logic
wc -l modules/*.sh
# Expected: 3000+ lines total with comments
```

**Security Checks**:
```bash
# Check for SQL injection vulnerabilities
grep "sqlite3.*\$" database/persistence.sh
# Expected: All parameterized (using printf with %s)

# Check for command injection
grep "eval\|exec" modules/*.sh
# Expected: ZERO matches (use alternatives)

# Check for hardcoded secrets
grep -r "password\|secret\|token\|key" config/ modules/ | grep -v "# "
# Expected: ZERO hardcoded secrets
```

**Documentation Checks**:
```bash
# Verify all modules have README
ls modules/*.sh | while read f; do
  if ! grep -q "# $f" README.md; then
    echo "Missing docs: $f"
  fi
done

# Check code comments
grep -c "^#" modules/ggen-setup.sh
# Expected: >50 comments for 290 lines

# Verify examples work
bash examples/ggen-setup-usage.sh
bash examples/mcp-client-example.sh
bash examples/skill-registry-demo.sh
# Expected: All examples run without errors
```

**Performance Validation**:
```bash
# Verify all SLOs documented
grep -r "SLO\|timeout\|<[0-9]" docs/ | wc -l
# Expected: 20+ SLO references

# Check timeout enforcement
grep -r "timeout" modules/main.sh | wc -l
# Expected: 5+ timeout statements

# Benchmark report
./main.sh benchmark 2>/dev/null | tail -20
```

**Final Certification**:
```bash
# Run all validation checks
bash verify-implementation.sh

# Expected output:
# ✅ Code Quality: PASS
# ✅ Security: PASS
# ✅ Performance: PASS
# ✅ Documentation: PASS
# ✅ Integration: PASS
# ✅ Production Ready: YES
```

**Success Criteria**:
- ✅ Zero unwrap/expect in production code
- ✅ Zero SQL injection vectors
- ✅ Zero command injection vectors
- ✅ Zero hardcoded secrets
- ✅ All modules documented
- ✅ All examples functional
- ✅ All SLOs met and documented
- ✅ Production readiness certification complete

**Validation Commands**:
```bash
# Run comprehensive verification
bash verify-implementation.sh

# Expected: PRODUCTION_READY ✅
```

**If Issues Found**:
1. Document all failures in VALIDATION_RESULTS.md
2. Categorize by severity (Critical/High/Medium/Low)
3. Create GitHub issues for all critical/high issues
4. Provide remediation steps
5. Schedule re-validation after fixes

---

## TESTING EXECUTION PROTOCOL

### Initial Setup (All Agents - In Parallel)
```bash
cd /home/user/ggen/scripts/claude-code-web-simulator

# 1. Verify starting state
git status                                  # Should be clean
git log --oneline -1                        # Should be 042d8bb6

# 2. Initialize test environment
./main.sh init                              # Setup simulator
source database/persistence.sh              # Load DB module
export TEST_MODE=1                          # Enable test mode

# 3. Prepare results tracking
mkdir -p /tmp/validation-results
touch /tmp/validation-results/VALIDATION_LOG_$(date +%s).txt
```

### Parallel Execution
```bash
# All 10 agents run simultaneously
# Each agent logs to: /tmp/validation-results/agent-N.log
# Master monitor tracks progress: /tmp/validation-results/master.log

# Real-time monitoring (on main terminal)
watch -n 1 "ls -ltr /tmp/validation-results/ | tail -10"
```

### Progress Checkpoints
```bash
# Checkpoint 1: All agents started (5 minutes)
ls /tmp/validation-results/ | wc -l
# Expected: 10 agent log files

# Checkpoint 2: 50% complete (30 minutes)
grep -l "PASS\|FAIL" /tmp/validation-results/agent-*.log | wc -l
# Expected: 5+ agents reported results

# Checkpoint 3: All complete (60 minutes)
grep "COMPLETE" /tmp/validation-results/master.log
# Expected: "ALL AGENTS COMPLETE"
```

### Failure Handling Protocol
```bash
# If agent fails
if grep -q "FAIL" /tmp/validation-results/agent-3.log; then
  # 1. Extract failure details
  grep -A 5 "FAIL" /tmp/validation-results/agent-3.log

  # 2. Stop all agents (don't continue with bad data)
  ./main.sh stop-all

  # 3. Document in VALIDATION_RESULTS.md
  echo "Agent 3 Failed: $(grep FAIL /tmp/validation-results/agent-3.log)" >> VALIDATION_RESULTS.md

  # 4. Create GitHub issue
  # Title: "VALIDATION FAILURE: Agent 3 - [Issue Name]"
  # Labels: bug, validation, critical (if blocking)

  # 5. Wait for next instructions
  exit 1
fi
```

---

## SUCCESS CRITERIA SUMMARY

### Minimum Viable Validation
```
✅ Agent 1: ggen integration working (15/15 tests)
✅ Agent 2: Docker containers spawning (23/23 tests)
✅ Agent 3: Real ggen pipeline executing (9/9 tests)
✅ Agent 4: Database persistence working (69/69 tests)
✅ Agent 5: MCP connectivity operational (15/15 tests)
✅ Agent 6: Performance SLOs met (all targets)
✅ Agent 7: Skill registry functional (14/14 tests)
✅ Agent 8: All 20 skills executable (20/20 skills)
✅ Agent 9: End-to-end workflows successful (5/5 scenarios)
✅ Agent 10: Production readiness certified
```

### Defect Threshold
- **Critical (Blocking)**: 0 allowed
- **High**: 0 allowed
- **Medium**: <5 allowed (fixable in follow-up)
- **Low**: <10 allowed (cosmetic only)

### Success Declaration
```
DECLARE PRODUCTION READY only if:
✅ All 150+ tests passing (100%)
✅ All SLOs met
✅ Zero critical/high defects
✅ All documentation complete
✅ Code security audit clean
✅ Performance benchmarks validated
✅ All 10 agents confirm: PASS
```

---

## REMEDIATION WORKFLOW

If failures found:

### 1. Document the Failure
```
Title: [Agent N] [Component] - [Issue]
Description:
- Test case: [exact failing test]
- Error message: [full error output]
- Root cause: [analysis]
- Expected behavior: [what should happen]
- Actual behavior: [what actually happened]
```

### 2. Fix Implementation
```bash
# Identify which file(s) need changes
# Make minimal changes to fix root cause
# Run specific test to verify fix
bash tests/test-[component].sh
```

### 3. Verify Fix
```bash
# Re-run all tests for that component
# Ensure no regressions elsewhere
cargo make test  # If Rust involved
bash verify-implementation.sh
```

### 4. Re-validate All
```bash
# After ANY fix, re-run ENTIRE validation
# to catch cascading failures
bash [all agent tests in sequence]
```

---

## COMMAND REFERENCE FOR AGENTS

```bash
# Core validation
cargo make check              # Compiler errors
cargo make lint               # Linting issues
cargo make test               # All tests
cargo make slo-check          # Performance SLOs

# Component testing
bash tests/test-ggen-setup.sh
bash tests/test-docker-runner.sh
bash tests/mcp-client-tests.sh
bash tests/test-mcp-performance.sh
bash tests/test-skill-registry.sh

# Integration testing
bash test-real-pipeline.sh
bash database/persistence.test.sh
python3 database/test-schema.py

# Monitoring
./main.sh monitor
./main.sh status
./main.sh logs [agent-id]

# Database operations
./main.sh db query-receipts all
./main.sh db query-memory all
./main.sh db export-audit-trail /tmp/audit.json
./main.sh db stats

# Cleanup (if needed to retry)
rm -rf /tmp/ggen.db
rm -rf .ggen/mcp-cache/
./main.sh init
```

---

## NEXT STEPS AFTER VALIDATION

Once all agents declare **PASS**:

1. **Merge to main** (optional, based on strategy)
2. **Tag release**: `v0.3.0-tier2-mvp`
3. **Update README**: Mark as production-ready
4. **Deploy to staging**: Test in CI/CD
5. **Monitor metrics**: Track performance over 24h
6. **Declare Tier 2 MVP Complete**: Ready for user deployment

If any **FAIL** on validation:
1. Create GitHub issues for all failures
2. Schedule remediation sprint
3. Cycle back to implementation phase
4. Retry validation with fixes
5. Continue until all pass

---

## DOCUMENTATION ARTIFACTS

All agents must produce:

### VALIDATION_RESULTS.md
```markdown
# Tier 2 MVP Validation Results
Date: [YYYY-MM-DD HH:MM:SS]
Branch: claude/implement-ggen-install-mVY1P

## Agent Results Summary
- Agent 1 (ggen): ✅ PASS (15/15 tests)
- Agent 2 (Docker): ✅ PASS (23/23 tests)
[... etc ...]

## Defects Found
- [Critical] [Agent N] [Component] - [Issue]
[... etc ...]

## Performance Metrics
- ggen detection: 0.8s (target <1s) ✅
- Docker spawn: 12.5s (target <15s) ✅
[... etc ...]

## Sign-Off
All agents: CONFIRMED PRODUCTION READY ✅
```

### DEFECT_LOG.md
```markdown
# Defect Log
[If any issues found during validation]

## Critical Defects (BLOCKING)
[None found in passing validation]

## High Defects (MUST FIX)
[None found in passing validation]

## Medium Defects (SHOULD FIX)
[If any, list with remediation plan]

## Low Defects (NICE TO FIX)
[If any, list with low priority]
```

---

## FINAL COMMAND FOR AGENTS TO RUN

```bash
# Execute comprehensive validation
cd /home/user/ggen/scripts/claude-code-web-simulator

# Stage 1: Pre-flight checks
echo "=== PRE-FLIGHT CHECKS ==="
git status
git log --oneline -1

# Stage 2: Run all tests
echo "=== AGENT 1: GGEN VALIDATION ==="
bash tests/test-ggen-setup.sh

echo "=== AGENT 2: DOCKER VALIDATION ==="
bash tests/test-docker-runner.sh

echo "=== AGENT 3: GGEN PIPELINE ==="
bash test-real-pipeline.sh

echo "=== AGENT 4: DATABASE VALIDATION ==="
bash database/persistence.test.sh
python3 database/test-schema.py

echo "=== AGENT 5: MCP CLIENT ==="
bash tests/mcp-client-tests.sh

echo "=== AGENT 6: MCP PERFORMANCE ==="
bash tests/test-mcp-performance.sh

echo "=== AGENT 7: SKILL REGISTRY ==="
bash tests/test-skill-registry.sh

echo "=== AGENT 8: SKILL EXECUTION ==="
source modules/skills-loader.sh
# [Execute all 20 skills with example data]

echo "=== AGENT 9: END-TO-END ==="
./main.sh init
./main.sh run-agent generation --real
./main.sh db query-receipts all

echo "=== AGENT 10: PRODUCTION READINESS ==="
bash verify-implementation.sh

# Final summary
echo ""
echo "=== VALIDATION COMPLETE ==="
echo "Status: $(grep -q 'PRODUCTION_READY' /tmp/validation_final.txt && echo 'PASS ✅' || echo 'FAIL ❌')"
```

---

**THIS IS YOUR COMPLETE TEST & VALIDATION BLUEPRINT**

**Hand this to the next 10-agent swarm and they will have everything needed to comprehensively test, validate, and certify Tier 2 MVP as production-ready.**

