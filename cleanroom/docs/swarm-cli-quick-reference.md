# Cleanroom Swarm CLI - Quick Reference Card

## Essential Commands

### Initialization
```bash
# Mesh topology (collaborative)
cleanroom swarm init --topology mesh --agents 5

# Hierarchical (scaled coordination)
cleanroom swarm init --topology hierarchical --agents 10 --auto-scale

# Ring (pipeline processing)
cleanroom swarm init --topology ring --agents 6

# Star (centralized control)
cleanroom swarm init --topology star --agents 8
```

### Agent Management
```bash
# Spawn coder
cleanroom swarm spawn --type coder --name "agent-1" --capabilities "rust,backend"

# Spawn tester
cleanroom swarm spawn --type tester --name "agent-2" --capabilities "testing,coverage"

# Spawn researcher
cleanroom swarm spawn --type researcher --name "agent-3" --capabilities "research,docs"

# List agents
cleanroom swarm list

# Filter by status
cleanroom swarm list --filter active
```

### Task Orchestration
```bash
# High priority, parallel execution
cleanroom swarm orchestrate \
    --task "Build and test project" \
    --priority high \
    --strategy parallel \
    --max-agents 3

# Medium priority, adaptive execution
cleanroom swarm orchestrate \
    --task "Performance optimization" \
    --priority medium \
    --strategy adaptive \
    --max-agents 2

# Low priority, sequential execution
cleanroom swarm orchestrate \
    --task "Generate documentation" \
    --priority low \
    --strategy sequential
```

### Monitoring
```bash
# Basic status
cleanroom swarm status

# Detailed status
cleanroom swarm status --detailed

# Agent metrics
cleanroom swarm metrics --agent-id "agent-001"

# Swarm-wide metrics
cleanroom swarm metrics --metric all
```

### Command Execution
```bash
# Simple command
cleanroom run echo "hello"

# With network isolation
cleanroom run curl https://example.com --network-isolation

# With resource limits
cleanroom run python3 script.py --max-memory 512 --max-cpu 50.0

# With policy file
cleanroom run ./test.sh --policy security.toml
```

### Environment Management
```bash
# Create
cleanroom env create --name "test-env"

# List
cleanroom env list

# Show details
cleanroom env show "test-env"

# Delete
cleanroom env delete "test-env" --force

# Cleanup all
cleanroom env cleanup
```

### Cleanup
```bash
# Stop swarm
cleanroom swarm stop

# Force stop
cleanroom swarm stop --force
```

## Output Formats

```bash
# JSON output (machine-readable)
cleanroom swarm status --output json

# Text output (human-readable, default)
cleanroom swarm status --output text

# Quiet mode (errors only)
cleanroom swarm status --output quiet
```

## Topologies Comparison

| Topology | Best For | Pattern | Agents |
|----------|----------|---------|--------|
| **Mesh** | Collaboration | Peer-to-peer | 3-10 |
| **Hierarchical** | Large-scale | Tree structure | 10+ |
| **Ring** | Pipelines | Circular chain | 4-8 |
| **Star** | Simple tasks | Centralized | 5-15 |

## Agent Types

| Type | Capabilities | Use Case |
|------|--------------|----------|
| **Coder** | Code generation, refactoring | Implementation |
| **Tester** | Testing, coverage analysis | QA |
| **Researcher** | Research, documentation | Planning |
| **Analyst** | Performance analysis | Optimization |
| **Optimizer** | Code optimization | Performance |
| **Coordinator** | Task orchestration | Management |

## Strategies

| Strategy | Description | Best For |
|----------|-------------|----------|
| **Balanced** | Equal distribution | Uniform tasks |
| **Specialized** | Capability-based routing | Heterogeneous tasks |
| **Adaptive** | Dynamic load balancing | Variable load |

## Priority Levels

| Priority | Use Case | Execution |
|----------|----------|-----------|
| **Critical** | Urgent fixes | Immediate |
| **High** | Core features | Fast |
| **Medium** | Normal tasks | Standard |
| **Low** | Nice-to-have | When available |

## Execution Strategies

| Strategy | Description | Agents |
|----------|-------------|--------|
| **Parallel** | Concurrent execution | 2+ |
| **Sequential** | One after another | 1 |
| **Adaptive** | Smart distribution | Auto |

## Quick Workflow

```bash
# 1. Initialize
cleanroom swarm init --topology mesh --agents 5

# 2. Spawn agents
cleanroom swarm spawn --type coder --name "dev-1"
cleanroom swarm spawn --type tester --name "test-1"

# 3. Orchestrate
cleanroom swarm orchestrate --task "Build project" --priority high

# 4. Monitor
cleanroom swarm status --detailed
cleanroom swarm metrics

# 5. Cleanup
cleanroom swarm stop
```

## Security Policy Template

`security-policy.toml`:
```toml
[security]
enable_network_isolation = true
enable_filesystem_isolation = true
blocked_commands = ["rm", "format"]
allowed_ports = [80, 443]

[resources]
max_memory_mb = 512
max_cpu_percent = 50.0
max_disk_mb = 1000
max_network_mbps = 10
```

## Benchmarking

```bash
# Container performance
cleanroom bench --bench-type container --iterations 10

# Swarm performance
cleanroom bench --bench-type swarm --iterations 5

# Full benchmark
cleanroom bench --bench-type all --iterations 20
```

## System Status

```bash
# Overall status
cleanroom status

# Version info
cleanroom version

# Help
cleanroom --help
cleanroom swarm --help
```

## Common Patterns

### Full Development Cycle
```bash
cleanroom swarm init --topology mesh --agents 8
cleanroom swarm spawn --type coder --name "backend-dev"
cleanroom swarm spawn --type tester --name "test-engineer"
cleanroom swarm orchestrate --task "Implement feature X" --priority high
cleanroom swarm status --detailed
cleanroom swarm stop
```

### CI/CD Integration
```bash
cleanroom swarm init --topology hierarchical --agents 10
cleanroom swarm orchestrate --task "Run CI pipeline" --strategy parallel
cleanroom swarm metrics --output json > metrics.json
cleanroom swarm stop
```

### Performance Testing
```bash
cleanroom env create --name "perf-test"
cleanroom run ./benchmark.sh --max-memory 2048 --max-cpu 100
cleanroom bench --bench-type all --iterations 50
cleanroom env delete "perf-test" --force
```

## Troubleshooting

```bash
# Check system health
cleanroom status

# View detailed swarm status
cleanroom swarm status --detailed

# List all agents
cleanroom swarm list

# Check agent metrics
cleanroom swarm metrics --agent-id "agent-001"

# Force cleanup
cleanroom swarm stop --force
cleanroom env cleanup
```

## Environment Variables

```bash
export CLEANROOM_LOG_LEVEL=debug
export CLEANROOM_MAX_AGENTS=20
export CLEANROOM_AUTO_SCALE=true
export CLEANROOM_TOPOLOGY=mesh
```

## Tips

1. **Use JSON output for automation**
   ```bash
   cleanroom swarm status --output json | jq '.agents.active'
   ```

2. **Enable verbose logging for debugging**
   ```bash
   cleanroom --verbose swarm status
   ```

3. **Save metrics to file**
   ```bash
   cleanroom swarm metrics --output json > metrics.json
   ```

4. **Chain commands**
   ```bash
   cleanroom swarm init --topology mesh --agents 5 && \
   cleanroom swarm spawn --type coder --name "dev-1" && \
   cleanroom swarm orchestrate --task "Build"
   ```

5. **Use auto-scaling for variable loads**
   ```bash
   cleanroom swarm init --auto-scale
   ```

## For More Information

- Full Guide: `docs/swarm-cli-guide.md`
- Examples: `examples/swarm_cli_usage.sh`
- Help: `cleanroom --help`
