# 7-Agent Validation System

A novel validation approach combining ggen's quality gates with Byzantine consensus for autonomous, fault-tolerant code validation.

## Overview

Traditional CI/CD validation is one-way: code → validate → pass/fail. This system creates a **closed-loop feedback system** where validation results continuously improve the generation process.

```
ggen generation (μ₁-μ₅)
        ↓
7-agent validation (parallel, autonomous)
        ↓
Consensus aggregation (5-of-7 quorum)
        ↓
Feedback to ggen (kaizen PDCA)
        ↓
Improved generation
```

## Architecture

### 7 Specialized Agents

Each agent validates one quality dimension:

| Agent | Gate | Validates |
|-------|------|-----------|
| **Agent 1** | Compiler | Code compiles without errors |
| **Agent 2** | Test | All tests pass (Chicago TDD) |
| **Agent 3** | Lint | Zero lint errors/warnings |
| **Agent 4** | SHACL | RDF ontology conformance |
| **Agent 5** | OTEL | Spans exist in Jaeger |
| **Agent 6** | Security | No vulnerabilities |
| **Agent 7** | Performance | SLO compliance |

### Consensus Layer (PBFT)

- **5-of-7 quorum**: Prevents malicious/failed agents from blocking
- **Priority ordering**: RED (safety) > GREEN (progress) > YELLOW (warning)
- **Byzantine fault tolerance**: Up to 2 agents can fail/misbehave

### Supervisor Tree (Armstrong)

- **Let-it-crash**: Fail fast, restart cleanly
- **One-for-one supervision**: Each agent independently supervised
- **Budget constraints**: Time/resource limits per validation
- **No shared state**: Message passing only

## Usage

### As a Library

```rust
use validation_system::ValidationSystem;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize system
    let system = ValidationSystem::new().await?;

    // Validate ggen generation
    let decision = system.validate_generation(
        "/path/to/package",
        "blake3-receipt-123"
    ).await?;

    match decision {
        ConsensusDecision::Approve { .. } => {
            println!("Code approved!");
        }
        ConsensusDecision::Reject { reasons, .. } => {
            println!("Code rejected: {:?}", reasons);
        }
        ConsensusDecision::Defer { suggestion, .. } => {
            println!("Deferred: {}", suggestion);
        }
    }

    Ok(())
}
```

### Run Demo

```bash
cd /Users/sac/ggen
cargo run --example 7-agent-validation
```

## Integration with ggen

### Input: ggen Receipt

After ggen completes μ₁-μ₅ pipeline, it emits a BLAKE3 receipt:

```bash
ggen sync --audit true
# Emits: blake3-hash-abc123
```

This receipt proves the generation is deterministic and tamper-evident.

### Validation Loop

```bash
# 1. Generate code with ggen
ggen sync --from .specify/specs/ --to src/generated/

# 2. Validate with 7-agent system
validation-system validate src/ blake3-hash-abc123

# 3. Check consensus
validation-system status blake3-hash-abc123
# Output: APPROVED (5 GREEN, 2 YELLOW)

# 4. If APPROVED, merge to main
# If REJECTED/DEFER, fix issues and retry
```

### Output: Feedback to ggen

Validation decisions are stored in ggen's kaizen cycle (PDCA):

- **Plan**: What we want to validate
- **Do**: Run the validation
- **Check**: Did agents approve?
- **Act**: Fix issues or improve process

## Innovation vs. Traditional CI/CD

| Traditional CI/CD | 7-Agent System |
|------------------|----------------|
| Sequential validation | Parallel (7 agents run simultaneously) |
| Single point of failure | Byzantine fault tolerance (5-of-7) |
| Pass/fail binary | APPROVE/REJECT/DEFER with reasons |
| No feedback loop | Closed-loop kaizen improvement |
| Pipeline blocks on failure | Agents continue independently |
| No supervision | Armstrong supervision tree |

## Implementation Status

### ✅ Completed

- [x] Core validation system architecture
- [x] 7 agent definitions with gates
- [x] PBFT consensus layer (5-of-7 quorum)
- [x] Agent registry (A2A pattern)
- [x] Supervisor tree (Armstrong principles)
- [x] Demo/example program

### 🔨 In Progress

- [ ] Actual gate implementations (CompilerGate, TestGate, etc.)
- [ ] OpenTelemetry span emission
- [ ] ggen receipt integration
- [ ] Kaizen feedback loop persistence

### 📋 Planned

- [ ] CLI command: `validation-system validate <package> <receipt>`
- [ ] ggen integration: `ggen sync --validate-with validation-system`
- [ ] Web UI: Real-time validation status dashboard
- [ ] Metrics: Validation success rate, agent health, consensus patterns

## References

- **Toyota Production System**: Jidoka (stop-the-line), Andon signals
- **Joe Armstrong**: Let-it-crash, supervision trees (Erlang/OTP)
- **Byzantine Fault Tolerance**: PBFT consensus algorithm
- **Chicago TDD**: Real implementation testing, not mocks
- **Wil van der Aalst**: Soundness (deadlock-free, liveness, boundedness)

## License

MIT License - See ggen LICENSE file for details.
