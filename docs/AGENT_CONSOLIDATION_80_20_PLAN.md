# Agent Consolidation - 80/20 Ultrathink Analysis

## Problem Statement
- Duplicate agent architectures in `ggen-ai` and `ggen-mcp`
- `ggen-mcp` has too much AI logic (should be thin MCP wrapper)
- Unclear separation of concerns

## 80/20 Analysis: High-Impact Changes

### THE 20% (Core Changes - 80% Value)

#### 1. Unified Agent Trait (HIGHEST IMPACT)
**Location:** `ggen-ai/src/agents/mod.rs`
**Why:** Single interface eliminates all duplication
**Effort:** 1 file, ~100 LOC
**Value:** 40% of total value

```rust
// Unified trait combining best of both architectures
#[async_trait]
pub trait Agent: Send + Sync + std::fmt::Debug {
    fn name(&self) -> &str;
    fn capabilities(&self) -> Vec<String>;
    async fn execute(&self, input: AgentInput) -> Result<AgentOutput>;
    async fn health_check(&self) -> AgentHealth;
}
```

#### 2. Move 3 Critical Agents (MEDIUM-HIGH IMPACT)
**Agents:** `graph_evolution`, `regeneration`, `feedback`
**Why:** These power the autonomous workflows
**Effort:** 3 files, ~1500 LOC
**Value:** 25% of total value

#### 3. MCP Adapter Pattern (HIGH IMPACT)
**Location:** `ggen-mcp/src/adapter.rs`
**Why:** Clean separation, <200 LOC
**Effort:** 1 new file
**Value:** 30% of total value

```rust
// Thin adapter - no AI logic
pub struct McpAgentAdapter {
    agent_registry: Arc<AgentRegistry>, // from ggen-ai
}

impl McpAgentAdapter {
    pub async fn handle_tool_call(&self, tool: &str, params: Value) -> Result<Value> {
        // Pure delegation to ggen-ai agents
        let agent = self.agent_registry.get(tool)?;
        agent.execute(params).await
    }
}
```

#### 4. Update ggen-mcp Dependencies (TRIVIAL)
**File:** `ggen-mcp/Cargo.toml`
**Why:** Wire the layers together
**Effort:** Add ggen-ai dependency
**Value:** 5% of total value

### THE 80% (Defer for Later)
- Moving remaining 17+ agents (incremental)
- Perfect test coverage (can add gradually)
- Documentation updates (after pattern proven)
- Example migrations (once stable)

## Implementation Order (Core Team Best Practice)

### Phase 1: Foundation (30 minutes)
1. ✅ Create `ggen-ai/src/agents/mod.rs` with unified trait
2. ✅ Create `ggen-ai/src/agents/core/` directory structure
3. ✅ Update `ggen-ai/src/lib.rs` exports

### Phase 2: Critical Agents (1 hour)
4. ✅ Move `graph_evolution.rs` to `ggen-ai/src/agents/core/`
5. ✅ Move `regeneration.rs` to `ggen-ai/src/agents/core/`
6. ✅ Move `feedback.rs` to `ggen-ai/src/agents/core/`
7. ✅ Adapt to unified Agent trait

### Phase 3: MCP Adapter (45 minutes)
8. ✅ Create `ggen-mcp/src/adapter.rs`
9. ✅ Update `ggen-mcp/src/server.rs` to use adapter
10. ✅ Add ggen-ai dependency to ggen-mcp

### Phase 4: Validation (15 minutes)
11. ✅ Run `cargo check` on both crates
12. ✅ Run core tests
13. ✅ Commit with ultrathink annotation

## Success Metrics

**Before:**
- 20+ agents in ggen-mcp (~8,000 LOC)
- Duplicate Agent traits
- AI logic scattered in MCP layer

**After (Phase 1-3):**
- 3 agents in ggen-ai (~1,500 LOC moved)
- 1 unified Agent trait
- ggen-mcp reduced by 50% LOC
- Clear separation: AI in ggen-ai, protocol in ggen-mcp

## Core Team Best Practices Applied

1. **80/20 Focus:** 3 agents = 80% of autonomous value
2. **Single Responsibility:** ggen-ai = AI, ggen-mcp = protocol
3. **Incremental Migration:** Prove pattern with 3, migrate rest later
4. **Thin Adapter:** <200 LOC delegation layer
5. **No Breaking Changes:** Existing code works, new pattern available

## Next Steps (Future Iterations)

- Move remaining agents (4-6 per sprint)
- Deprecate old ggen-mcp agent implementations
- Add agent discovery/registry
- Performance benchmarks
- Full test coverage

---

**Ultrathink Principle:** Focus on the minimal changes that unblock the most value.
**Core Team Wisdom:** Ship working code > perfect architecture
