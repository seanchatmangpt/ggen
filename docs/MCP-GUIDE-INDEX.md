<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP Integration with ggen - Complete Documentation Index](#mcp-integration-with-ggen---complete-documentation-index)
  - [Quick Navigation](#quick-navigation)
    - [🚀 I Want to...](#-i-want-to)
  - [Documentation Structure](#documentation-structure)
    - [📚 <a name="zero-to-hero"></a> MCP Zero-to-Hero Guide](#-a-namezero-to-heroa-mcp-zero-to-hero-guide)
    - [🛠️ <a name="how-to-build"></a> How-To: Build Your First MCP Tool with ggen](#-a-namehow-to-builda-how-to-build-your-first-mcp-tool-with-ggen)
    - [💡 <a name="explanation"></a> Why MCP and ggen Work Together](#-a-nameexplanationa-why-mcp-and-ggen-work-together)
  - [<a name="examples"></a> Real-World Examples](#a-nameexamplesa-real-world-examples)
    - [From ggen's Own Packages](#from-ggens-own-packages)
    - [In ggen Tests](#in-ggen-tests)
  - [<a name="production"></a> Production Patterns](#a-nameproductiona-production-patterns)
    - [Pattern 1: Simple Tool Wrapper](#pattern-1-simple-tool-wrapper)
    - [Pattern 2: Multi-Step Workflow](#pattern-2-multi-step-workflow)
    - [Pattern 3: Learning Agent System](#pattern-3-learning-agent-system)
    - [Pattern 4: Agent Swarms (EPIC 9)](#pattern-4-agent-swarms-epic-9)
  - [MCP + ggen Capabilities Summary](#mcp--ggen-capabilities-summary)
    - [Available ggen Verbs (44 Total)](#available-ggen-verbs-44-total)
  - [<a name="troubleshooting"></a> Troubleshooting Guide](#a-nametroubleshootinga-troubleshooting-guide)
    - [Common Issues](#common-issues)
  - [Configuration & Setup](#configuration--setup)
    - [MCP Server Configuration (.mcp.json)](#mcp-server-configuration-mcpjson)
    - [Capability Discovery Flags](#capability-discovery-flags)
  - [Integration Checklist](#integration-checklist)
  - [Resources](#resources)
    - [Official Documentation](#official-documentation)
    - [ggen Examples](#ggen-examples)
    - [Community](#community)
  - [Summary](#summary)
    - [The Three Documents](#the-three-documents)
    - [Your Learning Path](#your-learning-path)
    - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCP Integration with ggen - Complete Documentation Index

**Everything you need to know about using ggen in Model Context Protocol servers**

---

## Quick Navigation

### 🚀 I Want to...

| Goal | Start Here |
|------|-----------|
| Understand what MCPs + ggen can do | [Why MCPs and ggen Work Together](#explanation) |
| Learn zero-to-hero from scratch | [MCP Zero-to-Hero Guide](#zero-to-hero) |
| Build my first MCP tool | [How-To: Build MCP Tool with ggen](#how-to-build) |
| See real production code | [Examples & Real Implementations](#examples) |
| Deploy to production | [Production Patterns](#production) |
| Troubleshoot issues | [Troubleshooting](#troubleshooting) |

---

## Documentation Structure

### 📚 <a name="zero-to-hero"></a> [MCP Zero-to-Hero Guide](mcp-zero-to-hero-guide.md)

**Complete learning journey from "What is ggen?" to production MCP tools**

**Covers:**
- What ggen does (code generation, RDF, ontologies)
- Why MCPs want ggen (capabilities, composition, consistency)
- How to discover ggen's 44 capabilities
- Building your first ggen tool (simple code generation)
- Building complex multi-step workflows
- Real-world examples from ggen's own packages
- Common patterns and anti-patterns
- Troubleshooting guide

**Estimated Time:** 2-3 hours
**Best For:** Complete beginners to MCPs and ggen

**Key Sections:**
1. Part 1: Understanding ggen's 3 core capabilities
2. Part 2: Discovery pattern (Phase 1 & 2 of EPIC 9)
3. Part 3: Your first tool (100 lines of code)
4. Part 4: Complete workflows (error recovery, streaming)
5. Part 5: Real examples from agent-reasoning-mcp and rig-mcp

---

### 🛠️ <a name="how-to-build"></a> [How-To: Build Your First MCP Tool with ggen](how-to-build-mcp-tool-with-ggen.md)

**Step-by-step practical guide to building a complete, production-ready MCP tool**

**What You'll Build:**
An `api-generator` MCP tool that takes a domain spec and generates:
- OpenAPI specification
- Zod validation schemas
- TypeScript type definitions
- Next.js API handler example

**Covers:**
- Project setup (Cargo.toml, directory structure)
- Type definitions for your tool
- Core generation logic (spec → RDF → artifacts)
- MCP tool interface
- Testing and validation
- Integration with ggen CLI
- Deployment preparation

**Estimated Time:** 45-60 minutes (hands-on)
**Best For:** Learning by doing

**Key Deliverables:**
1. Working Rust library
2. Tool interface definitions
3. Integration tests
4. Example usage
5. Deployment-ready code

---

### 💡 <a name="explanation"></a> [Why MCP and ggen Work Together](why-mcp-and-ggen-work-together.md)

**Understanding the philosophical and architectural alignment**

**Explains:**
- Core problems MCPs solve (composition)
- Core problems ggen solves (consistency)
- Why they fit together perfectly
- The 4 levels of integration (simple to autonomous swarms)
- Comparison to alternatives
- When to use this pattern
- Key principles and insights

**Estimated Time:** 20-30 minutes (reading)
**Best For:** Understanding the "why"

**Key Concepts:**
1. MCPs provide interface, ggen provides engine
2. Synergy: standardized discovery + consistent output
3. Four integration levels (wrapper → learning → swarms)
4. Why this beats alternatives
5. Specification-driven generation is mandatory

---

## <a name="examples"></a> Real-World Examples

### From ggen's Own Packages

**agent-reasoning-mcp:**
- Location: `/home/user/ggen/marketplace/packages/agent-reasoning-mcp/`
- Shows: SPARQL-based reasoning with MCP protocol integration
- Pattern: Agents discover capabilities, learn verb metadata, build workflows
- RDF Ontology: 52 classes for reasoning and MCP integration
- Integration Tests: 8-phase agent workflow simulation
- Key Learning: How agents adapt based on ggen capabilities

**rig-mcp:**
- Location: `/home/user/ggen/marketplace/packages/rig-mcp/`
- Shows: Multi-provider LLM support + MCP tool loading
- Pattern: Dynamic tool loading with embedding-based selection
- Architecture: Support for 20+ LLM providers
- Key Learning: How to build intelligent tool selection

### In ggen Tests

**mcp_agent_workflows.rs:**
- Location: `/home/user/ggen/.archive/crate-tests-backup/crates-ggen-cli-tests/`
- Shows: Complete 8-phase agent lifecycle
- Phases: Discovery → Learning → Planning → Execution → Adaptation → Error Recovery → Improvement → Audit
- Key Learning: How real systems handle failure and learning

---

## <a name="production"></a> Production Patterns

### Pattern 1: Simple Tool Wrapper

```rust
// Minimal implementation (~100 lines)
pub fn handle_generate_api(spec: String) -> Result<Artifacts> {
    let ontology = ggen::ai::generate_ontology(&spec)?;
    ggen_generate_all(&ontology)
}
```

**Best For:** Single artifact types
**Complexity:** Low
**Scalability:** Medium

### Pattern 2: Multi-Step Workflow

```rust
// Coordinates multiple ggen steps (~200-300 lines)
pub async fn execute_workflow(spec: String) -> Result<Artifacts> {
    let ontology = ggen::ai::generate_ontology(&spec)?;
    ggen::graph::validate(&ontology)?;

    let (openapi, zod, ts) = tokio::join!(
        ggen_openapi(&ontology),
        ggen_zod(&ontology),
        ggen_typescript(&ontology),
    );

    Ok(Artifacts { openapi, zod, ts })
}
```

**Best For:** Complex specs with validation
**Complexity:** Medium
**Scalability:** High

### Pattern 3: Learning Agent System

```rust
// Learns from execution results (~500+ lines)
pub async fn execute_with_learning(
    &mut self,
    task: Task,
) -> Result<Artifacts> {
    let tool = self.select_best_tool(&task);
    match tool.execute(&task) {
        Ok(result) => {
            self.success_rate[&tool.id] += 0.1;
            Ok(result)
        }
        Err(e) => {
            self.success_rate[&tool.id] -= 0.1;
            Err(e)
        }
    }
}
```

**Best For:** Production systems with optimization
**Complexity:** High
**Scalability:** Very High

### Pattern 4: Agent Swarms (EPIC 9)

```rust
// Full parallel agent coordination (~1000+ lines)
pub async fn execute_epic_task(
    &self,
    task: EpicTask,
) -> Result<Artifacts> {
    let plan = self.queen.plan(&task)?;

    let results = futures::future::join_all(
        self.colony_leaders.iter().map(|leader| {
            leader.execute_with_ggen(&plan)
        })
    ).await;

    self.queen.synthesize(results)
}
```

**Best For:** Massive scale with full parallelism
**Complexity:** Very High
**Scalability:** Extreme

---

## MCP + ggen Capabilities Summary

### Available ggen Verbs (44 Total)

**AI Module (6 verbs):**
- `generate-ontology` - Create RDF ontologies from specs
- `analyze-model` - Understand domain relationships
- `generate-schema` - Create validation schemas
- `validate-model` - Check model consistency
- `export-types` - Generate type definitions
- `infer-relationships` - Discover implicit connections

**Template Module (7 verbs):**
- `generate` - Transform data to code
- `validate` - Check template syntax
- `list` - Show available templates
- `preview` - See output before generation
- `export` - Save templates
- `import` - Load templates
- `generate-rdf` - Create RDF from templates

**Graph Module (5 verbs):**
- `load` - Read RDF graphs
- `query` - Execute SPARQL queries
- `export` - Output graph data
- `validate` - Check RDF integrity
- `merge` - Combine graphs

**Ontology Module (4 verbs):**
- `extract` - Get ontologies from code
- `generate` - Create from specs
- `validate` - Check structure
- `init` - Start new ontologies

**Project Module (4 verbs):**
- `new` - Create projects
- `generate` - Build from ontologies
- `plan` - Design implementations
- `validate` - Check structure

**CI Module (4 verbs):**
- `workflow` - Generate CI workflows
- `action` - Create CI actions
- `pipeline` - Build pipelines
- `validate` - Check configs

**Workflow Module (2 verbs):**
- `generate` - Create workflows
- `execute` - Run workflows

**FMEA Module (5 verbs):**
- `report` - Generate risk reports
- `pareto` - Analyze failures
- `list` - List failure modes
- `show` - Show details
- `export` - Export data

**Utils Module (2 verbs):**
- `doctor` - System diagnostics
- `env` - Environment info

---

## <a name="troubleshooting"></a> Troubleshooting Guide

### Common Issues

| Issue | Symptom | Solution |
|-------|---------|----------|
| ggen not found | `command not found: ggen` | Ensure ggen in PATH: `which ggen` |
| Capability discovery fails | `Unknown capability` | Update ggen: `cargo install ggen@latest` |
| Template generation empty | Output file exists but is empty | Validate template: `ggen template validate` |
| Ontology validation fails | RDF errors | Check syntax: `ggen graph validate --file` |
| Performance degradation | Takes 30+ seconds | Cache metadata, use async/parallelism |
| Integration test failures | Tests pass locally, fail in CI | Check environment variables, ggen version |

**Full troubleshooting in:** [MCP Zero-to-Hero Guide - Troubleshooting Section](mcp-zero-to-hero-guide.md#troubleshooting)

---

## Configuration & Setup

### MCP Server Configuration (.mcp.json)

ggen's default configuration includes 4 MCP servers:

```json
{
  "mcpServers": {
    "claude-code-guide": {
      "command": "npx",
      "args": ["@anthropic-ai/claude-code-guide"]
    },
    "git": {
      "command": "git",
      "args": ["mcp-server"],
      "env": { "GIT_REPO": "/home/user/ggen" }
    },
    "bash": {
      "command": "bash",
      "args": [".claude/helpers/bash-init.sh"]
    },
    "rdf-tools": {
      "command": "oxigraph",
      "args": ["serve", "--bind", "127.0.0.1", "--port", "7878"]
    }
  }
}
```

### Capability Discovery Flags

```bash
# List all capabilities
ggen --graph
# Output: CommandGraph with 44 verbs

# Get verb metadata
ggen --capabilities <noun> <verb>
# Output: VerbMetadata with arguments and types

# Get introspection data
ggen --introspect <noun> <verb>
# Output: Detailed type information
```

---

## Integration Checklist

Before deploying an MCP tool with ggen:

- ✅ Capability discovery works (`ggen --graph`)
- ✅ Argument validation implemented
- ✅ Error recovery patterns in place
- ✅ Caching implemented (if needed)
- ✅ Tests pass (unit + integration)
- ✅ Performance acceptable (<30s for typical tasks)
- ✅ Documentation clear and examples work
- ✅ Deployment ready (config, dependencies)

---

## Resources

### Official Documentation
- [ggen Core Repository](https://github.com/seanchatmangpt/ggen)
- [MCP Specification](https://modelcontextprotocol.io/)
- [RDF/SPARQL Resources](https://www.w3.org/RDF/)

### ggen Examples
- [examples/openapi/](../examples/openapi/) - API generation from RDF
- [marketplace/packages/agent-reasoning-mcp/](../marketplace/packages/agent-reasoning-mcp/) - Agent reasoning
- [marketplace/packages/rig-mcp/](../marketplace/packages/rig-mcp/) - Multi-LLM support

### Community
- GitHub Issues: Report problems and ideas
- Discussions: Ask questions and share patterns
- Marketplace: Publish your MCP tools

---

## Summary

### The Three Documents

1. **[MCP Zero-to-Hero Guide](mcp-zero-to-hero-guide.md)** - Learn everything
   - What ggen does
   - How to discover capabilities
   - How to build tools
   - Common patterns
   - Troubleshooting

2. **[How-To: Build MCP Tool](how-to-build-mcp-tool-with-ggen.md)** - Build it
   - Project setup
   - Complete working code
   - Testing
   - Deployment

3. **[Why MCP and ggen Work Together](why-mcp-and-ggen-work-together.md)** - Understand it
   - Philosophy and design
   - Integration levels
   - Comparisons
   - Key principles

### Your Learning Path

```
Start → Read Why (philosophy)
     → Read Zero-to-Hero (overview)
     → Read How-To (hands-on)
     → Build tool
     → Test
     → Deploy
     → Contribute patterns
```

### Next Steps

1. **Read:** [Why MCP and ggen Work Together](why-mcp-and-ggen-work-together.md) (20 min)
2. **Learn:** [MCP Zero-to-Hero Guide](mcp-zero-to-hero-guide.md) (2-3 hours)
3. **Build:** [How-To Guide](how-to-build-mcp-tool-with-ggen.md) (45-60 min)
4. **Deploy:** Use as template for your own MCP

---

**Last Updated:** 2026-01-06
**Framework Version:** ggen 5.2.0
**MCP Protocol:** Version 1.0
**Maintained By:** ggen Core Team
