# Claude Code Web Simulator - File Index

Quick reference guide to all files and directories.

## 📍 Getting Started

### Start Here
1. **README.md** - Overview of simulator, research findings, and architecture
2. **QUICKSTART.md** - 5-minute getting started guide
3. **main.sh** - The actual simulator executable

### For Deep Understanding
1. **ARCHITECTURE.md** - Comprehensive technical documentation with ASCII diagrams
2. **IMPLEMENTATION_SUMMARY.md** - What was built, features implemented, test results

## 📂 Directory Structure

```
/home/user/ggen/scripts/claude-code-web-simulator/
│
├─ 📄 Documentation (Read These First)
│  ├─ README.md                      ← START HERE (research + overview)
│  ├─ QUICKSTART.md                  ← Quick 5-min guide
│  ├─ ARCHITECTURE.md                ← Deep technical design
│  ├─ IMPLEMENTATION_SUMMARY.md      ← What was built
│  └─ INDEX.md                       ← This file
│
├─ 🚀 Main Executable
│  └─ main.sh                        ← Run all simulator commands
│
├─ ⚙️ Configuration
│  ├─ config/environment.json        ← Simulator version + SLOs
│  ├─ config/mcp-servers.json        ← 7 MCP server definitions
│  └─ config/security-policy.json    ← Network/filesystem rules
│
├─ 📦 Module Stubs (Not Yet Implemented)
│  ├─ modules/sandbox-simulator.sh
│  ├─ modules/mcp-proxy.sh
│  ├─ modules/hooks-engine.sh
│  ├─ modules/agent-orchestrator.sh
│  ├─ modules/ggen-pipeline.sh
│  ├─ modules/receipt-generator.sh
│  ├─ modules/invocation-patterns.sh
│  ├─ modules/error-handler.sh
│  └─ modules/memory-integrator.sh
│
├─ 📝 Example Scripts (Stubs)
│  ├─ examples/simple-validation.sh
│  ├─ examples/multi-agent-gen.sh
│  ├─ examples/watch-mode.sh
│  └─ examples/error-recovery.sh
│
├─ 🧪 Test Suite (Integrated)
│  ├─ tests/test-sandbox.sh
│  ├─ tests/test-mcp-proxy.sh
│  ├─ tests/test-multi-agent.sh
│  └─ tests/test-determinism.sh
│
└─ 💾 Runtime Workspace (Created on First Run)
   ├─ workspace/agent-memory/        ← Agent memory JSON files
   ├─ workspace/sandboxes/           ← Per-agent isolated workspaces
   ├─ workspace/receipts/            ← Deterministic receipts
   └─ workspace/audit-logs/          ← Timestamped operation logs
```

## 📖 Documentation Files

### README.md (11 KB)
**What**: Research findings + simulator overview + feature list
**Read if**: You want to understand what the simulator does and why
**Contains**:
- Research on Claude Code Web (Docker, MCP, Sandbox)
- Simulation architecture overview
- List of 10 features implemented
- Usage guide and examples
- Testing information

**Read time**: 10 minutes

### QUICKSTART.md (7.3 KB)
**What**: 5-minute getting started guide
**Read if**: You want to try the simulator immediately
**Contains**:
- Prerequisites (Bash 4.0+)
- 5 quick steps to run first agent
- Running examples
- Running tests
- Troubleshooting common issues

**Read time**: 5 minutes

### ARCHITECTURE.md (46 KB)
**What**: Complete technical architecture with detailed diagrams
**Read if**: You want to understand how simulator works internally
**Contains**:
- System overview with ASCII diagrams
- Component interactions
- Agent startup sequence (10 steps)
- Multi-agent workflow sequence
- Error recovery flow
- Data flow diagrams (receipts, memory)
- Performance characteristics
- Docker container integration
- Security boundaries

**Read time**: 30 minutes (technical deep-dive)

### IMPLEMENTATION_SUMMARY.md (25+ KB)
**What**: Complete summary of what was built
**Read if**: You want to know exactly what features are implemented
**Contains**:
- Mission accomplished summary
- Research findings (3 major areas)
- Simulator architecture
- Deliverables (files, config, runtime)
- 10 features implemented (detailed)
- Usage guide
- Current status (test results)
- Key achievements
- Integration points
- Next steps (optional enhancements)

**Read time**: 20 minutes

## 🚀 Main Executable

### main.sh (24 KB, ~800 lines)
**What**: The actual simulator - do everything from here
**How to use**: `./main.sh [COMMAND] [OPTIONS]`

**Key Commands**:
```bash
./main.sh start                           # Initialize simulator
./main.sh run-agent validation            # Run validation agent
./main.sh run-agent generation            # Run generation agent
./main.sh run-workflow multi-gen --parallel 4  # 4 parallel agents
./main.sh run-example multi-agent-gen     # Run demo
./main.sh test all                        # Run all tests
./main.sh monitor                         # Show status
./main.sh view-receipts                   # Show receipts
./main.sh clean                           # Clean data
./main.sh help                            # Show help
```

**Structure**:
- Color-coded logging functions
- Environment initialization
- Agent execution (validation, generation, watch, dry-run)
- Multi-agent workflows (multi-gen, parallel-validation, watch-continuous)
- Receipt generation (SHA-256 hashing, timestamps)
- Test suite (4 test types)
- Monitoring and display functions

## ⚙️ Configuration Files

### config/environment.json
**What**: Simulator configuration and version info
**Contains**:
- Simulator version (1.0.0)
- Sandbox settings (OS-level isolation)
- MCP configuration (timeout, token limits)
- ggen pipeline settings (5 stages, determinism)
- Agent settings (max parallel, bootstrap timeout)

**Edit if**: You want to change SLO targets or feature flags

### config/mcp-servers.json
**What**: MCP server definitions
**Contains**: 7 MCP servers
- GitHub (proxy, GitHub domains)
- Research/Perplexity (proxy, documentation domains)
- Sequential Thinking (LLM-based)
- Context7 (documentation)
- Docker (container management, restricted)
- Plus 2 additional servers

**Edit if**: You want to add/remove MCP servers

### config/security-policy.json
**What**: Network, filesystem, and process security rules
**Contains**:
- Network: whitelist mode, allowed domains, DNS/SSH
- Filesystem: sandbox mode, restrictions
- Process: permission enforcement, subprocess timeout
- Docker: socket access policy, network isolation

**Edit if**: You want to modify security constraints

## 📦 Module Stubs

All modules in `/modules/` are currently stubs (empty files).
They're placeholders for future implementation:
- `sandbox-simulator.sh` - OS-level sandbox simulation
- `mcp-proxy.sh` - MCP server proxy implementation
- `hooks-engine.sh` - Hook system execution
- `agent-orchestrator.sh` - Multi-agent coordination
- `ggen-pipeline.sh` - Pipeline stage execution
- `receipt-generator.sh` - Receipt generation
- `invocation-patterns.sh` - Agent invocation patterns
- `error-handler.sh` - Error handling and recovery
- `memory-integrator.sh` - Memory system integration

**Note**: All functionality is currently in `main.sh`. These files are for future modularization.

## 📝 Example Scripts

All examples in `/examples/` are currently stubs.
The examples are run from `main.sh`:
```bash
./main.sh run-example simple-validation     # Single agent validation
./main.sh run-example multi-agent-gen       # 4 parallel agents
./main.sh run-example watch-mode            # Watch mode (3 cycles)
./main.sh run-example error-recovery        # Error handling + recovery
```

## 🧪 Test Suite

All tests are integrated into `main.sh`. Run with:
```bash
./main.sh test all              # All 4 test suites
./main.sh test sandbox          # Sandbox isolation
./main.sh test mcp-proxy        # MCP server proxy
./main.sh test multi-agent      # Multi-agent coordination
./main.sh test determinism      # Receipt reproducibility
```

**Test Results**: All 4 suites passing (100%) ✅

## 💾 Runtime Workspace

Created automatically on first run (`./main.sh start`):

### workspace/agent-memory/
**What**: Per-agent memory storage (JSON)
**Contains**:
- `index.json` - Master memory index
- Agent-specific memory files

**Persists**: Between runs (unless cleaned)

### workspace/sandboxes/
**What**: Per-agent isolated workspaces
**Contains**: One directory per agent execution
- `{agent_id}/` - Sandbox workspace

**Cleaned by**: `./main.sh clean`

### workspace/receipts/
**What**: Deterministic execution receipts (JSON)
**Contains**: One receipt per agent execution
- `{agent_id}.json` - SHA-256 hashes, timestamps, pipeline timings

**View with**: `./main.sh view-receipts`

**Example**:
```json
{
  "receipt": {
    "execution_id": "exec-1769707527040909752",
    "agent_id": "agent-1769707524971877739-1",
    "operation": "generation",
    "status": "passed",
    "timestamp": "2026-01-29T17:25:27Z",
    "hashes": {
      "manifest": "60f717681aef7aba3f89256927ccbbb8...",
      "ontology": "4cf923408df3e0a1186785e7ec8a271a..."
    },
    "files_generated": 47,
    "files_modified": 12,
    "pipeline_stages": {
      "μ₁_normalize": {"status": "completed", "duration_ms": 400},
      "μ₂_extract": {"status": "completed", "duration_ms": 500},
      "μ₃_emit": {"status": "completed", "duration_ms": 600},
      "μ₄_canonicalize": {"status": "completed", "duration_ms": 300},
      "μ₅_receipt": {"status": "completed", "duration_ms": 200}
    },
    "total_duration_ms": 2000,
    "determinism_guarantee": true
  }
}
```

### workspace/audit-logs/
**What**: Timestamped operation log
**Contains**:
- `audit.log` - Append-only audit trail

**View with**: `./main.sh view-audit-trail` or `tail -20 workspace/audit-logs/audit.log`

**Format**:
```
[2026-01-29T17:25:15Z] Agent: validator-1769707514154396967 | Operation: validation | Status: passed | Duration: 2000ms
[2026-01-29T17:25:27Z] Agent: agent-1769707524971877739-1 | Operation: generation | Status: passed | Duration: 2000ms
```

## 🗺️ Navigation Guide

### If you want to...

**Get started immediately (5 minutes)**
1. Read: `QUICKSTART.md`
2. Run: `./main.sh start`
3. Run: `./main.sh run-agent validation`
4. Run: `./main.sh view-receipts`

**Understand the simulator (30 minutes)**
1. Read: `README.md` (overview + research)
2. Read: `ARCHITECTURE.md` (technical deep-dive)
3. Review: Key commands in `main.sh --help`

**Use the simulator for research (60+ minutes)**
1. Read: `IMPLEMENTATION_SUMMARY.md` (what was built)
2. Run: `./main.sh run-example multi-agent-gen` (see it in action)
3. Examine: `workspace/receipts/` (see receipt format)
4. Explore: `config/` (see configuration)

**Extend the simulator**
1. Study: `main.sh` (understand implementation)
2. Add: New agent type (edit `run_agent()` function)
3. Test: `./main.sh run-agent my-new-agent`

**Integrate with real ggen**
1. Read: Integration points in `IMPLEMENTATION_SUMMARY.md`
2. Replace: Simulated pipeline with `ggen sync` calls
3. Test: `./main.sh run-agent generation` (now using real ggen)

## 📊 File Statistics

```
Documentation:
  ├─ README.md: 11 KB
  ├─ QUICKSTART.md: 7.3 KB
  ├─ ARCHITECTURE.md: 46 KB
  ├─ IMPLEMENTATION_SUMMARY.md: 25+ KB
  └─ INDEX.md (this file): ~7 KB
  Total: ~96 KB

Code:
  ├─ main.sh: 24 KB (~800 lines)
  ├─ config/*.json: 7 KB (3 files)
  └─ Total: 31 KB

Workspace (runtime):
  ├─ agent-memory/: Created on first run
  ├─ sandboxes/: Created per agent
  ├─ receipts/: ~2 KB per receipt (JSON)
  └─ audit-logs/: ~1 KB per 20 operations

Total Deliverable: ~127 KB (documentation + code)
```

## 🔗 Quick Links

- **Start**: `./main.sh start`
- **Help**: `./main.sh help`
- **Quick Start Guide**: `cat QUICKSTART.md`
- **Architecture**: `cat ARCHITECTURE.md`
- **Status**: `./main.sh monitor`
- **Examples**: `./main.sh run-example [name]`
- **Tests**: `./main.sh test all`
- **Clean**: `./main.sh clean`

## ✅ Verification

To verify everything is working:

```bash
# 1. Start simulator
./main.sh start

# 2. Run all tests
./main.sh test all

# Expected output: 4/4 test suites passing ✅

# 3. Run example
./main.sh run-example multi-agent-gen

# 4. View receipts
./main.sh view-receipts

# 5. Check status
./main.sh monitor
```

All should complete successfully with green checkmarks (✓).

---

## 📞 Support

- **Getting Started**: Read `QUICKSTART.md`
- **Technical Questions**: Read `ARCHITECTURE.md`
- **What Was Built**: Read `IMPLEMENTATION_SUMMARY.md`
- **Research Findings**: Read `README.md`
- **All Commands**: Run `./main.sh help`

---

**Location**: `/home/user/ggen/scripts/claude-code-web-simulator/`

**Status**: ✅ Complete and Ready for Use

**Last Updated**: 2026-01-29
