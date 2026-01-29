# Claude Code Web Simulator - Quick Start Guide

Get up and running with the Claude Code Web simulation environment in 5 minutes.

## Prerequisites

- Bash 4.0+
- Basic CLI tools (date, mkdir, cat, echo)
- Optional: python3 (for JSON pretty-printing)

## 1. Initialize the Simulator

```bash
cd /home/user/ggen/scripts/claude-code-web-simulator
./main.sh start
```

This will:
- Create workspace directories
- Initialize configuration files
- Set up agent memory system
- Display status

**Expected output:**
```
[INFO] Initializing Claude Code Web simulation environment...
[✓] Environment initialized
[✓] Simulation environment ready
```

## 2. Run Your First Agent

### Validation Agent (Specification Checking)
```bash
./main.sh run-agent validation --spec myspec.ttl
```

**What it does:**
- Simulates RDF ontology parsing (μ₁ Normalize)
- Executes SPARQL queries (μ₂ Extract)
- Validates SHACL shapes
- Generates deterministic receipt

**Output includes:**
- Pipeline stage timings
- Validation results
- Cryptographic receipt SHA-256 hash

### Generation Agent (Code Generation)
```bash
./main.sh run-agent generation --ontology ont.ttl
```

**What it does:**
- Runs full μ₁-μ₅ pipeline
- Simulates Tera template rendering
- Shows file generation count
- Produces deterministic receipt

### Watch Agent (Continuous Mode)
```bash
./main.sh run-agent watch
```

**What it does:**
- Monitors for file changes (simulated)
- Triggers 3 regeneration cycles
- Shows continuous mode operation

### Dry-Run Agent (Preview Only)
```bash
./main.sh run-agent dry-run
```

**What it does:**
- Previews changes without committing
- Shows what files would be generated
- Produces receipt without side effects

## 3. Run Multi-Agent Workflows

### Parallel Code Generation (4 Agents)
```bash
./main.sh run-workflow multi-gen --parallel 4
```

**What it shows:**
- 4 agents executing in parallel
- Collision detection (checking for overlaps)
- Convergence synthesis (combining outputs)
- Unified result

### Parallel Validation (4 Validators)
```bash
./main.sh run-workflow parallel-validation
```

**What it shows:**
- SHACL validation agent
- SPARQL validation agent
- Schema validation agent
- Consistency validation agent
- All running in parallel

### Watch Continuous (Monitoring Loop)
```bash
./main.sh run-workflow watch-continuous
```

**What it shows:**
- Continuous monitoring (5 iterations)
- Regeneration cycles
- File change detection

## 4. View Results

### Check Deterministic Receipts
```bash
./main.sh view-receipts
```

**Shows:**
- Execution ID (unique per run)
- Manifest and ontology SHA-256 hashes
- Files generated and modified
- Pipeline stage timings
- Total execution duration
- Determinism guarantee status

### View Audit Trail
```bash
./main.sh view-audit-trail
```

**Shows:**
- Timestamped log of all operations
- Agent IDs and operations
- Status and duration for each run

### Monitor Status
```bash
./main.sh monitor
```

**Shows:**
- Simulator status
- Count of sandboxes created
- Count of receipts generated
- Audit log size

## 5. Run Examples

### Simple Validation Example
```bash
./main.sh run-example simple-validation
```

### Multi-Agent Generation Example
```bash
./main.sh run-example multi-agent-gen
```

### Watch Mode Example
```bash
./main.sh run-example watch-mode
```

### Error Recovery Example
```bash
./main.sh run-example error-recovery
```

**Shows:**
- Error detection (exit code 4)
- Automatic retry mechanism
- Recovery success

## 6. Run Test Suite

### Test Everything
```bash
./main.sh test all
```

### Individual Tests
```bash
./main.sh test sandbox          # Sandbox isolation
./main.sh test mcp-proxy        # MCP server integration
./main.sh test multi-agent      # Multi-agent coordination
./main.sh test determinism      # Receipt verification
```

## 7. Clean Up

```bash
./main.sh clean
```

Removes all simulation data (sandboxes, receipts, audit logs).

## Understanding the Output

### Color Codes

- 🔵 `[INFO]` - Informational message
- 🟢 `[✓]` - Success message
- 🟡 `[WARN]` - Warning message
- 🔴 `[ERROR]` - Error message

### Exit Codes

- `0` - Success
- `1` - General error
- `2` - Validation failed
- `4` - SPARQL error
- `5` - Template error

## Key Concepts Demonstrated

### 1. Sandbox Isolation
Each agent runs in an isolated workspace with:
- Unique agent ID
- Separate workspace directory
- No cross-agent filesystem access
- Network restrictions via proxy

### 2. ggen Pipeline (μ₁-μ₅)
The deterministic 5-stage transformation:
- **μ₁ Normalize**: RDF validation (400ms)
- **μ₂ Extract**: SPARQL queries (500ms)
- **μ₃ Emit**: Template rendering (600ms)
- **μ₄ Canonicalize**: Format & hash (300ms)
- **μ₅ Receipt**: Cryptographic proof (200ms)

### 3. Deterministic Receipts
Each run produces:
- SHA-256 manifest hash
- SHA-256 ontology hash
- File-by-file content hashes
- Timestamped audit trail
- Proves reproducibility

### 4. Multi-Agent Coordination
Agents operate in parallel with:
- Memory synchronization
- Collision detection
- Convergence synthesis
- Unified output

### 5. Error Handling
Exit codes for automation:
- `0` = Success
- `2` = Validation failed
- `4` = SPARQL error
- `5` = Template error

## Typical Session

```bash
# 1. Start simulator
./main.sh start

# 2. Run a validation agent
./main.sh run-agent validation --spec example.ttl

# 3. Check the receipt
./main.sh view-receipts

# 4. Run a multi-agent workflow
./main.sh run-workflow multi-gen --parallel 4

# 5. View audit trail
./main.sh view-audit-trail

# 6. Check status
./main.sh monitor

# 7. Clean up
./main.sh clean
```

## Troubleshooting

### "Permission denied" on main.sh
```bash
chmod +x /home/user/ggen/scripts/claude-code-web-simulator/main.sh
```

### No output or slow execution
- Check available disk space
- Verify Python3 is installed for JSON formatting
- Increase simulated delays (edit main.sh sleep times)

### Receipts not generating
- Check workspace directory permissions
- Verify `receipts/` directory exists
- Try `./main.sh clean` then `./main.sh start`

## Next Steps

1. **Study the Code**: Review `main.sh` to understand architecture
2. **Extend Examples**: Create custom agent types
3. **Test Stress**: Run many agents in parallel
4. **Real Integration**: Connect to actual ggen binary
5. **Docker Testing**: Verify Docker sandbox behavior

## Architecture Overview

```
Simulator
├── Sandbox Isolation
│   └── Per-agent isolated workspaces
├── MCP Proxy
│   └── Tool access with domain whitelist
├── Agent Orchestrator
│   ├── SessionStart hooks
│   ├── Agent bootstrap
│   └── Multi-agent coordination
├── ggen Pipeline (μ₁-μ₅)
│   ├── RDF Normalization
│   ├── SPARQL Extraction
│   ├── Template Emission
│   ├── Format Canonicalization
│   └── Receipt Generation
├── Receipt Generator
│   └── SHA-256 deterministic proofs
└── Error Handler
    └── Exit code propagation
```

## For More Information

- **Full Documentation**: See `README.md` in simulator directory
- **ggen Docs**: See `/home/user/ggen/docs/`
- **Agent Guide**: See `/home/user/ggen/docs/installation/CLAUDE_CODE_WEB_GUIDE.md`
- **Research Findings**: See `README.md` research section (Docker, MCP, Sandbox)

---

**Happy simulating! 🚀**
