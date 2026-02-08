# Definition of Done: MCP & A2A Integration with GLM Intelligence

**Version**: 1.0.0
**Status**: Active
**Last Updated**: 2026-02-08
**Quality Standard**: Lean Six Sigma (99.99966% defect-free)

---

## Executive Summary

This Definition of DoD (DoD) framework establishes production-ready delivery criteria for the Model Context Protocol (MCP), Agent-to-Agent (A2A) communication, and GLM (Zhipu AI) intelligence integration within ggen. It enforces zero-defect quality standards through measurable, verifiable gates across seven critical categories.

**Quality Philosophy**: Poka-Yoke (mistake-proofing) + Chicago TDD + Andon (stop-the-line)

---

## Table of Contents

1. [Code Quality Gates](#1-code-quality-gates)
2. [API Integration Criteria](#2-api-integration-criteria)
3. [MCP/A2A Functionality](#3-mcpa2a-functionality)
4. [Marketplace Deployment](#4-marketplace-deployment)
5. [CLI Intelligence](#5-cli-intelligence)
6. [Performance SLOs](#6-performance-slos)
7. [Security and Documentation](#7-security-and-documentation)
8. [Sign-off Requirements](#8-sign-off-requirements)
9. [Blocking Conditions](#9-blocking-conditions)
10. [Component Checklists](#10-component-checklists)

---

## 1. Code Quality Gates

### 1.1 Compiler Checks

| Criterion | Command | Pass Criteria | Status |
|-----------|---------|--------------|--------|
| **Zero Warnings** | `cargo make check` | 0 warnings, 0 errors | Mandatory |
| **Warnings-as-Errors** | `cargo check --all-targets` | Clean compilation | Mandatory |
| **All Crates** | `cargo check --workspace` | All 30 crates compile | Mandatory |
| **Target Verification** | `cargo check --all-targets` | lib + bin + tests compile | Mandatory |

```bash
# Verification command (ALL must pass)
cargo make check && cargo make check-all-crates
```

**Blocking Condition**: ANY compiler error or warning blocks completion.

### 1.2 Clippy Linting

| Criterion | Command | Pass Criteria | Status |
|-----------|---------|--------------|--------|
| **Strict Mode** | `cargo make lint` | 0 clippy warnings | Mandatory |
| **Warnings as Errors** | `-D warnings` flag set | All lints pass | Mandatory |
| **All Targets** | `--all-targets --all-features` | Full workspace clean | Mandatory |
| **Production Code** | No `unwrap/expect` | Result<T,E> throughout | Mandatory |

```bash
# Verification command
cargo make lint

# Forbidden patterns (automated detection):
# - unwrap(), expect() in production code paths
# - todo!(), unimplemented!() placeholders
# - println!/eprintln! (use tracing instead)
# - Hidden unsafe blocks without justification
```

**Blocking Condition**: ANY clippy warning blocks completion.

### 1.3 Rustfmt Compliance

| Criterion | Command | Pass Criteria | Status |
|-----------|---------|--------------|--------|
| **Format Check** | `cargo fmt --all -- --check` | Zero format differences | Mandatory |
| **Line Length** | 100 char limit enforced | No violations | Mandatory |
| **Consistent Style** | Automated formatting | All files formatted | Mandatory |

```bash
# Verification command
cargo make fmt && git diff --exit-code
```

**Blocking Condition**: ANY format difference blocks commit.

### 1.4 Error Handling Standards

| Requirement | Specification | Verification |
|-------------|---------------|--------------|
| **Result<T,E>** | All fallible operations return Result | Manual code review |
| **No unwrap/expect** | Only in test code or with justification | Clippy enforced |
| **Error Context** | All errors include context | Manual code review |
| **Error Conversion** | `?` operator or `.map_err()` | Automated check |
| **Custom Error Types** | Domain-specific errors | Manual code review |

```bash
# Forbidden pattern detection
grep -r "unwrap()" crates/ --exclude-dir=target | grep -v "test" | grep -v "// allowed"
```

**Blocking Condition**: Production code with unwrap/expect without documented justification.

---

## 2. API Integration Criteria

### 2.1 GLM API Functionality

| Feature | Specification | Test Command | Status |
|---------|---------------|--------------|--------|
| **Authentication** | API key from env/secure storage | `cargo make test-glm-auth` | Mandatory |
| **Chat Completion** | Full streaming + non-streaming | `cargo make test-glm-chat` | Mandatory |
| **Model Selection** | glm-4, glm-4-flash, glm-4-plus | `cargo make test-glm-models` | Mandatory |
| **Token Counting** | Accurate token usage | Integration test | Mandatory |
| **Response Parsing** | Structured output handling | Integration test | Mandatory |

```bash
# GLM Integration Test Suite
cargo test --package ggen-ai --features glm-integration -- --test-threads=1

# Expected behavior:
# - Connection established to https://open.bigmodel.cn/api/paas/v4/
# - JWT token generation from API key
# - Request/response serialization
# - Streaming chunk processing
```

### 2.2 Error Handling for API Failures

| Failure Mode | Handling Strategy | Test Coverage |
|--------------|-------------------|---------------|
| **Network Timeout** | Exponential backoff, retry | Test with mock server |
| **Rate Limiting** | 429 response handling | Test with rate limit simulation |
| **Invalid API Key** | Clear error, guidance | Test with invalid credentials |
| **Server Error (5xx)** | Retry with backoff | Test with 503 response |
| **Malformed Response** | Graceful degradation | Test with invalid JSON |
| **Token Exhaustion** | Early detection, clear message | Test with max tokens |

```bash
# Error handling verification
cargo test --test glm_error_scenarios -- --nocapture
```

### 2.3 Retry Logic with Exponential Backoff

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| **Initial Delay** | 1s | Standard exponential backoff |
| **Max Delay** | 32s | Prevents excessive wait |
| **Max Retries** | 3 | Balances reliability vs latency |
| **Backoff Factor** | 2.0 | Standard exponential growth |
| **Jitter** | +/- 25% | Prevents thundering herd |

```rust
// Expected implementation pattern
 RetryConfig {
     max_retries: 3,
     initial_delay: Duration::from_secs(1),
     max_delay: Duration::from_secs(32),
     backoff_factor: 2.0,
     jitter: true,
 }
```

### 2.4 Rate Limiting and Quota Management

| Feature | Specification | Implementation |
|---------|---------------|----------------|
| **Token Rate Limit** | Track tokens/minute | Sliding window counter |
| **Request Rate Limit** | Track requests/second | Token bucket algorithm |
| **Quota Warning** | Alert at 80% consumption | Preemptive notification |
| **Quota Enforcement** | Stop at 100% or queue | Configurable strategy |
| **Per-Model Limits** | Model-specific quotas | Configuration driven |

```bash
# Rate limiting verification
cargo test --test glm_rate_limiting -- --nocapture
```

### 2.5 Secure API Key Handling

| Requirement | Implementation | Verification |
|-------------|----------------|--------------|
| **No Hardcoded Keys** | Environment variables only | `grep -r "sk-"` check |
| **Key Rotation** | Support rotation without restart | Integration test |
| **Audit Logging** | Log key usage (not the key) | Log verification |
| **Scope Validation** | Validate key permissions | Test with restricted key |
| **Error Messages** | Never leak key in error | Manual review |

```bash
# Security verification
./scripts/security/verify-no-secrets.sh crates/ggen-ai
```

---

## 3. MCP/A2A Functionality

### 3.1 Transport Layer Verification

| Transport | Test Command | Expected Behavior | Status |
|-----------|--------------|-------------------|--------|
| **stdio** | `cargo make test-mcp-stdio` | Full-duplex communication | Mandatory |
| **HTTP** | `cargo make test-mcp-http` | REST endpoint handling | Mandatory |
| **SSE** | `cargo make test-mcp-sse` | Server-Sent Events streaming | Mandatory |
| **A2A** | `cargo make test-a2a-protocol` | Agent-to-Agent messaging | Mandatory |
| **WebSocket** | `cargo make test-mcp-ws` | Bidirectional streaming | Optional |

```bash
# Transport layer test suite
cargo test --test mcp_transports -- --test-threads=1
cargo test --test a2a_protocol -- --test-threads=1
```

### 3.2 Tool Discovery and Registration

| Feature | Specification | Verification |
|---------|---------------|--------------|
| **Tool Listing** | `tools/list` endpoint | Returns all available tools |
| **Tool Schema** | JSON Schema per tool | Valid schemas for all tools |
| **Dynamic Registration** | Runtime tool addition | Test with mock agent |
| **Tool Invocation** | `tools/call` endpoint | Execute tools with args |
| **Batch Operations** | Multiple tool calls | Test concurrent calls |

```bash
# Tool discovery verification
cargo test --test mcp_tool_discovery -- --nocapture
```

### 3.3 Schema Conversion (Bidirectional)

| Conversion Type | From | To | Test Coverage |
|----------------|------|-----|---------------|
| **JSON Schema** | OpenAPI Schema | MCP Tool Schema | 100% |
| **Type System** | Rust types | JSON Schema | 100% |
| **A2A Messages** | A2A protocol | MCP protocol | 100% |
| **Error Mapping** | Domain errors | MCP errors | 100% |
| **Round-trip** | Schema -> Type -> Schema | Identity verified | 100% |

```bash
# Schema conversion verification
cargo test --test schema_conversion -- --test-threads=1
```

### 3.4 Bridge Layer Functionality

| Bridge | Source | Target | Verification |
|--------|--------|--------|--------------|
| **MCP -> A2A** | MCP Tool Call | A2A Task | Test with mock |
| **A2A -> MCP** | A2A Event | MCP Notification | Test with mock |
| **GLM -> Agent** | GLM Response | Agent Action | Integration test |
| **Agent -> GLM** | Agent State | GLM Context | Integration test |

```bash
# Bridge layer verification
cargo test --test bridge_layer -- --nocapture
```

### 3.5 Protocol Translation Correctness

| Test Category | Description | Pass Criteria |
|---------------|-------------|---------------|
| **Message Format** | JSON-RPC 2.0 compliance | 100% valid messages |
| **Error Codes** | Standard error mapping | All errors mapped |
| **Request ID** | Correlation tracking | 100% tracked |
| **Version Negotiation** | Protocol versioning | Backward compatible |
| **Serialization** | Binary/JSON conversion | Round-trip verified |

---

## 4. Marketplace Deployment

### 4.1 Automated Deployment Pipeline

| Stage | Command | Artifact | Status |
|-------|---------|----------|--------|
| **Package Build** | `cargo make marketplace-build` | WASM/binary | Mandatory |
| **Schema Export** | `cargo make marketplace-schema` | .ttl files | Mandatory |
| **Metadata Generation** | `cargo make marketplace-meta` | package.json | Mandatory |
| **Validation** | `cargo make marketplace-validate` | Validation report | Mandatory |
| **Publish** | `cargo make marketplace-publish` | Published package | Mandatory |

```bash
# Full deployment pipeline
cargo make marketplace-full-pipeline
```

### 4.2 Template Generation from Specs

| Template | Source | Output | Verification |
|----------|--------|--------|--------------|
| **Rust Code** | .ttl ontology | .rs files | Compiles |
| **TypeScript** | .ttl ontology | .ts files | Type checks |
| **Documentation** | .ttl ontology | .md files | Links valid |
| **Tests** | .ttl ontology | _test.rs files | Pass |

```bash
# Template generation verification
cargo make speckit-render
cargo test --package ggen-core --lib template_tests
```

### 4.3 CI/CD Workflow Generation

| Workflow | Triggers | Jobs | Status |
|----------|----------|------|--------|
| **Test** | Pull request | Unit, integration, property | Mandatory |
| **Lint** | Push, PR | Clippy, fmt, docs | Mandatory |
| **Security** | Nightly | Audit, secrets scan | Mandatory |
| **Release** | Tag | Build, publish, notify | Mandatory |
| **Performance** | Weekly | Benchmarks, SLO check | Mandatory |

```yaml
# Generated .github/workflows/package-name.yml
# Must include all jobs with proper dependencies
```

### 4.4 Publishing Process Automation

| Step | Command | Verification |
|------|---------|--------------|
| **Version Bump** | `cargo make version-bump` | Cargo.toml updated |
| **Changelog** | `cargo make changelog` | CHANGELOG.md entry |
| **Git Tag** | `cargo make git-tag` | Tag created |
| **Registry Push** | `cargo make publish` | Package available |
| **Notification** | `cargo make notify` | Release announced |

### 4.5 Documentation Generation

| Artifact | Command | Location |
|----------|---------|----------|
| **README.md** | `cargo make docs-readme` | Package root |
| **API Docs** | `cargo make docs-api` | docs/api/ |
| **Examples** | `cargo make docs-examples` | examples/ |
| **Migration Guide** | `cargo make docs-migration` | docs/migration/ |

---

## 5. CLI Intelligence

### 5.1 Chat Command with Streaming

| Feature | Specification | Test Command |
|---------|---------------|--------------|
| **Command** | `ggen chat <prompt>` | Integration test |
| **Streaming** | Real-time token output | Verify with --stream |
| **History** | Session context management | Test multi-turn |
| **Model Selection** | `--model` flag | Test all models |
| **Exit Handling** | Graceful cleanup on interrupt | Test with SIGINT |

```bash
# Chat command verification
cargo run --bin ggen -- chat "Hello, GLM!" --model glm-4 --stream
```

### 5.2 Context-Aware Help

| Component | Behavior | Verification |
|-----------|----------|--------------|
| **Command Discovery** | `ggen --help` shows all | Manual check |
| **Subcommand Help** | `ggen <cmd> --help` detailed | Manual check |
| **Error Suggestions** | "Did you mean?" on typo | Integration test |
| **Examples** | Usage examples in help | Manual review |
| **Related Commands** | "See also" sections | Manual review |

### 5.3 Error Intelligence

| Error Type | Intelligent Response | Verification |
|------------|---------------------|--------------|
| **API Key Missing** | Show setup command | Test with no key |
| **Network Failure** | Suggest retry/cancel | Test with blocked network |
| **Rate Limit** | Show wait time, queue | Test rate limit |
| **Invalid Input** | Show valid options | Test with bad input |
| **Model Unavailable** | Suggest alternatives | Test with invalid model |

### 5.4 Natural Language Configuration

| Feature | Specification | Test |
|---------|---------------|-----|
| **Config Parsing** | YAML/TOML/JSON support | All formats |
| **Env Var Expansion** | `${VAR}` substitution | Test |
| **Validation** | Schema validation | Invalid config test |
| **Defaults** | Sensible defaults | No config test |
| **Merge Strategy** | Multiple config files | Test precedence |

```bash
# Config verification
ggen config validate
ggen config show
```

### 5.5 Progress Monitoring

| Operation | Progress Display | Verification |
|-----------|-----------------|--------------|
| **File Operations** | Progress bar | Test with large file |
| **Network Requests** | Spinner/percentage | Test with slow request |
| **Task Execution** | Step-by-step output | Test multi-step task |
| **Cancellations** | Clean stop on Ctrl+C | Test interrupt |

---

## 6. Performance SLOs

### 6.1 Build Performance

| Metric | Target | Measurement | Status |
|--------|--------|-------------|--------|
| **First Build** | <= 15s | `time cargo make check` | Mandatory |
| **Incremental** | <= 2s | `time cargo make check` (no change) | Mandatory |
| **Full Release** | <= 120s | `time cargo build --release` | Recommended |
| **Doc Build** | <= 30s | `time cargo doc` | Recommended |

```bash
# Performance verification
hyperfine 'cargo make check' --warmup 1 --runs 5
```

### 6.2 RDF Processing

| Metric | Target | Test | Status |
|--------|--------|------|--------|
| **Ontology Load** | < 1s | Load .specify/*.ttl | Mandatory |
| **SHACL Validation** | < 100ms | Validate shapes | Mandatory |
| **Template Render** | < 5s per file | Code generation | Mandatory |
| **1k Triples** | < 5s processing | Benchmark | Mandatory |

```bash
# RDF performance verification
cargo test --package ggen-core --lib rdf::bench -- --nocapture
```

### 6.3 GLM API Latency

| Operation | P50 | P95 | P99 | Measurement |
|-----------|-----|-----|-----|-------------|
| **Chat (simple)** | < 500ms | < 1s | < 2s | Integration test |
| **Chat (complex)** | < 2s | < 5s | < 10s | Integration test |
| **Streaming First Token** | < 300ms | < 500ms | < 1s | Integration test |
| **Embedding** | < 200ms | < 500ms | < 1s | Integration test |

```bash
# Latency verification
cargo test --test glm_performance -- --nocapture
```

### 6.4 Memory Usage

| Context | Limit | Measurement | Action |
|---------|-------|-------------|--------|
| **CLI Commands** | < 100MB RSS | `memory_profiling` benchmark | Optimize if exceeded |
| **Long-Running Agent** | < 500MB RSS | 24h soak test | Monitor for leaks |
| **GLM Streaming** | < 50MB growth | Per-request delta | Check for leaks |
| **Workspace Build** | < 4GB peak | Full build | Document baseline |

```bash
# Memory verification
cargo test --test memory_profile -- --nocapture
```

### 6.5 Memory Leak Prevention

| Test | Duration | Pass Criteria |
|------|----------|---------------|
| **Soak Test** | 24h | No growth > 10MB/hr |
| **Request Cycle** | 10k iterations | Return to baseline |
| **Connection Pool** | Open/close cycles | No FD leaks |
| **Buffer Reuse** | Large message cycles | No accumulation |

```bash
# Leak detection
valgrind --leak-check=full --show-leak-kinds=all ./target/debug/ggen
```

---

## 7. Security and Documentation

### 7.1 No Hardcoded Secrets

| Check | Command | Pass Criteria |
|-------|---------|---------------|
| **API Keys** | `grep -rE "sk-|api_key\s*="` | Zero matches |
| **Passwords** | `grep -rE "password\s*=\s*\""` | Zero matches |
| **Tokens** | `grep -rE "token\s*=\s*\"[^\"]+\""` | Only placeholders |
| **Endpoints** | `grep -rE "https://.*\.api"` | Documented only |

```bash
# Secrets verification
./scripts/security/audit-secrets.sh
```

### 7.2 Security Scan

| Tool | Command | Status |
|------|---------|--------|
| **cargo-audit** | `cargo make audit` | Zero advisories |
| **cargo-deny** | `cargo make audit-deny` | Zero violations |
| **Bandit** | `bandit -r crates/` | Zero issues |
| **Secrets Scan** | `gitleaks detect` | Zero leaks |

```bash
# Full security scan
cargo make audit-all
```

### 7.3 Test Coverage

| Metric | Target | Tool | Status |
|--------|--------|------|--------|
| **Line Coverage** | >= 80% | tarpaulin | Mandatory |
| **Branch Coverage** | >= 70% | tarpaulin | Recommended |
| **Critical Path** | 100% | Manual review | Mandatory |
| **Error Paths** | 100% | Manual review | Mandatory |

```bash
# Coverage verification
cargo tarpaulin --workspace --timeout 120 --out Html
```

### 7.4 Documentation Standards

| Component | Requirement | Verification |
|-----------|-------------|--------------|
| **Public APIs** | 100% documented | `cargo doc --no-deps` |
| **Examples** | Per public function | Manual review |
| **Module Docs** | All modules have `//!` | `cargo doc` check |
| **Error Docs** | All error variants documented | Manual review |
| **Type Docs** | All public types documented | `cargo doc` check |

```bash
# Documentation verification
cargo make docs-check
cargo make test-doc
```

### 7.5 User Guides

| Guide | Location | Contents | Status |
|-------|----------|----------|--------|
| **Quick Start** | docs/quickstart.md | Installation, first use | Mandatory |
| **GLM Integration** | docs/glm-integration.md | Setup, usage, examples | Mandatory |
| **MCP Guide** | docs/mcp-guide.md | Server, client, tools | Mandatory |
| **A2A Guide** | docs/a2a-guide.md | Agents, messaging | Mandatory |
| **Troubleshooting** | docs/troubleshooting.md | Common issues | Mandatory |

---

## 8. Sign-off Requirements

### 8.1 Developer Sign-off

```bash
# Pre-signoff verification (all must pass)
cargo make pre-commit

# Signoff command
git commit -m "feat: MCP & A2A with GLM integration

[Receipt] cargo make pre-commit: PASSED
[Receipt] cargo make test: PASSED (347/347 tests, 28.3s)
[Receipt] cargo make lint: PASSED
[Receipt] cargo make docs-check: PASSED
[Receipt] cargo make audit-all: PASSED
[Receipt] Coverage: 82.3% line, 75.1% branch

Verified:
- Zero compiler warnings
- Zero clippy warnings
- All tests passing
- 80%+ coverage
- No security issues
- Documentation complete"
```

### 8.2 Reviewer Sign-off

| Checklist | Item | Status |
|-----------|------|--------|
| **Code Review** | All PRs reviewed by 2+ people | Required |
| **Architecture** | Design patterns validated | Required |
| **Security** | Security review completed | Required |
| **Performance** | SLOs verified | Required |
| **Documentation** | All docs reviewed | Required |

### 8.3 QA Sign-off

| Test Suite | Result | Notes |
|------------|--------|-------|
| **Unit Tests** | PASS | 100% pass rate |
| **Integration Tests** | PASS | All scenarios covered |
| **Property Tests** | PASS | 100k iterations |
| **Performance Tests** | PASS | All SLOs met |
| **Security Tests** | PASS | Zero vulnerabilities |

---

## 9. Blocking Conditions

### 9.1 Critical Blockers (STOP IMMEDIATELY)

| Condition | Impact | Resolution Required |
|-----------|--------|-------------------|
| **Compiler Error** | Cannot build | Fix immediately |
| **Test Failure** | Unknown behavior | Fix or mark known issue |
| **Security Vulnerability** | Exploitable | Patch immediately |
| **SLO Violation** | Performance regression | Optimize or document |
| **Data Loss Bug** | User impact | Fix immediately |

### 9.2 High Priority (FIX BEFORE MERGE)

| Condition | Impact | Resolution Required |
|-----------|--------|-------------------|
| **Clippy Warning** | Code quality | Fix or justify |
| **Coverage < 80%** | Test gaps | Add tests |
| **Missing Doc** | Usability | Add documentation |
| **Format Issue** | Consistency | Run formatter |
| **TODO Comment** | Incomplete work | Complete or file issue |

### 9.3 Medium Priority (ACKNOWLEDGE)

| Condition | Impact | Resolution Required |
|-----------|--------|-------------------|
| **Deprecated API Usage** | Future maintenance | Plan migration |
| **Complex Function** | Maintainability | Add doc or refactor |
| **Duplicate Code** | Maintainability | File refactoring issue |
| **Performance Warning** | Non-critical path | Document or optimize |

---

## 10. Component Checklists

### 10.1 GLM Integration Checklist

```markdown
## GLM Integration DoD

### Authentication
- [ ] API key loaded from environment
- [ ] JWT token generation working
- [ ] Token refresh mechanism
- [ ] Invalid key error handling

### Chat Completion
- [ ] Non-streaming mode working
- [ ] Streaming mode working
- [ ] Message history support
- [ ] System prompt support
- [ ] Temperature parameter
- [ ] Max tokens parameter

### Error Handling
- [ ] Network timeout handling
- [ ] Rate limit handling
- [ ] Invalid response handling
- [ ] Token limit handling
- [ ] User-friendly error messages

### Testing
- [ ] Unit tests for all functions
- [ ] Integration tests with mock server
- [ ] Error scenario tests
- [ ] Performance benchmarks
- [ ] Memory leak checks

### Documentation
- [ ] API documentation complete
- [ ] Usage examples provided
- [ ] Error codes documented
- [ ] Setup guide complete
```

### 10.2 MCP Integration Checklist

```markdown
## MCP Integration DoD

### Transport Layers
- [ ] stdio transport working
- [ ] HTTP transport working
- [ ] SSE transport working
- [ ] WebSocket transport (if applicable)

### Tool Management
- [ ] Tool discovery working
- [ ] Tool invocation working
- [ ] Schema generation working
- [ ] Error handling complete

### Protocol Compliance
- [ ] JSON-RPC 2.0 compliant
- [ ] Request ID tracking
- [ ] Error code mapping
- [ ] Version negotiation

### Testing
- [ ] Unit tests for all transports
- [ ] Integration tests with real servers
- [ ] Protocol compliance tests
- [ ] Interoperability tests

### Documentation
- [ ] Protocol documentation
- [ ] Server setup guide
- [ ] Client usage guide
- [ ] Tool development guide
```

### 10.3 A2A Integration Checklist

```markdown
## A2A Integration DoD

### Agent Management
- [ ] Agent registration working
- [ ] Agent discovery working
- [ ] Agent lifecycle management
- [ ] Health monitoring

### Message Passing
- [ ] Message sending working
- [ ] Message receiving working
- [ ] Message routing working
- [ ] Error handling complete

### Protocol Implementation
- [ ] A2A protocol compliance
- [ ] Message serialization
- [ ] Error code mapping
- [ ] Version compatibility

### Testing
- [ ] Unit tests for all components
- [ ] Integration tests with agents
- [ ] Multi-agent scenarios
- [ ] Performance tests

### Documentation
- [ ] Agent development guide
- [ ] Message format reference
- [ ] Protocol specification
- [ ] Deployment guide
```

### 10.4 CLI Intelligence Checklist

```markdown
## CLI Intelligence DoD

### Chat Command
- [ ] Basic chat working
- [ ] Streaming output working
- [ ] History management working
- [ ] Model selection working
- [ ] Error messages helpful

### Configuration
- [ ] Config file parsing working
- [ ] Environment variable expansion
- [ ] Validation working
- [ ] Default values sensible
- [ ] Merge strategy correct

### Help System
- [ ] Command help complete
- [ ] Error suggestions working
- [ ] Examples provided
- [ ] "See also" references
- [ ] Interactive help (if applicable)

### Testing
- [ ] Unit tests for all commands
- [ ] Integration tests for CLI
- [ ] Error scenario tests
- [ ] User experience tests

### Documentation
- [ ] Command reference complete
- [ ] Usage examples provided
- [ ] Configuration documented
- [ ] Troubleshooting guide
```

### 10.5 Marketplace Checklist

```markdown
## Marketplace Deployment DoD

### Package Structure
- [ ] All required files present
- [ ] Package.json valid
- [ ] Schema files included
- [ ] Documentation complete

### Build Pipeline
- [ ] Automated build working
- [ ] Template generation working
- [ ] Validation passing
- [ ] Publishing automated

### CI/CD
- [ ] Test workflow configured
- [ ] Lint workflow configured
- [ ] Release workflow configured
- [ ] All workflows passing

### Quality Gates
- [ ] Code coverage >= 80%
- [ ] Zero security issues
- [ ] Documentation complete
- [ ] Examples working

### Documentation
- [ ] README.md complete
- [ ] API docs generated
- [ ] Usage examples provided
- [ ] Migration guide (if applicable)
```

---

## Appendix A: Verification Commands

### Full Verification Suite

```bash
#!/bin/bash
# Full DoD verification for MCP & A2A with GLM

set -euo pipefail

echo "Running DoD Verification..."
echo "================================"

# 1. Code Quality Gates
echo "1. Code Quality Gates"
echo "-------------------"
cargo make timeout-check
cargo make check
cargo make check-all-crates
cargo make lint
cargo fmt --all -- --check

# 2. API Integration (if credentials available)
echo "2. API Integration"
echo "-----------------"
if [ -n "${GLM_API_KEY:-}" ]; then
    cargo test --package ggen-ai --features glm-integration
else
    echo "⚠️  GLM_API_KEY not set, skipping live tests"
fi

# 3. MCP/A2A Functionality
echo "3. MCP/A2A Functionality"
echo "-----------------------"
cargo test --test mcp_transports
cargo test --test a2a_protocol
cargo test --test bridge_layer
cargo test --test schema_conversion

# 4. Marketplace
echo "4. Marketplace"
echo "-------------"
cargo make marketplace-validate
cargo make marketplace-full-pipeline

# 5. CLI Intelligence
echo "5. CLI Intelligence"
echo "------------------"
cargo test --package ggen-cli --bin ggen
cargo run --bin ggen -- --help

# 6. Performance
echo "6. Performance"
echo "-------------"
cargo test --test performance_tests
hyperfine 'cargo make check' --warmup 1 --runs 3

# 7. Security and Documentation
echo "7. Security and Documentation"
echo "----------------------------"
cargo make audit-all
cargo make docs-check
cargo tarpaulin --workspace --timeout 120 --out Html

echo "================================"
echo "DoD Verification Complete!"
```

### Quick Verification (Pre-commit)

```bash
#!/bin/bash
# Quick pre-commit verification

cargo make fmt
cargo make check
cargo make lint
cargo make test-unit
cargo make test-doc
```

---

## Appendix B: Template Receipts

### Commit Message Template

```
feat(component): brief description

[Receipt] cargo make pre-commit: PASSED
[Receipt] cargo make test: PASSED (X/Y tests, Zs)
[Receipt] cargo make lint: PASSED
[Receipt] cargo make docs-check: PASSED
[Receipt] Coverage: X% line, Y% branch

Changes:
- Bullet point describing main change
- Another bullet point

Tests:
- Test coverage added for new functionality
- All existing tests still passing

Docs:
- Documentation updated for public APIs
- Examples added for new features

Related: #[issue_number]
```

### Pull Request Template

```
## Summary
Brief description of changes

## DoD Verification
- [ ] All code quality gates passing
- [ ] All tests passing (unit + integration)
- [ ] Coverage >= 80%
- [ ] Documentation complete
- [ ] Security scan clean
- [ ] Performance SLOs met

## Test Results
```
[Insert test output here]
```

## Breaking Changes
None / List any breaking changes

## Checklist
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] Changelog updated
- [ ] Milestone linked
```

---

## Change History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0.0 | 2026-02-08 | Initial DoD for MCP & A2A with GLM | ggen-team |

---

**This DoD is enforced via Git hooks, CI/CD pipelines, and manual review.**
**Any deviation requires explicit approval with documented justification.**
