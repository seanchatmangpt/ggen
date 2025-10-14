# 🐝 Cleanroom Swarm CLI

> Multi-agent coordination and orchestration for deterministic testing environments

## Quick Start

```bash
# Build the CLI
cargo build --release --bin cleanroom

# Initialize a swarm
cleanroom swarm init --topology mesh --agents 5

# Spawn agents
cleanroom swarm spawn --type coder --name "agent-1"
cleanroom swarm spawn --type tester --name "agent-2"

# Orchestrate tasks
cleanroom swarm orchestrate --task "Build and test project" --priority high

# Monitor status
cleanroom swarm status --detailed
```

## Features

### 🎯 Swarm Topologies

| Topology | Pattern | Best For | Agents |
|----------|---------|----------|--------|
| **Mesh** | Peer-to-peer | Collaboration | 3-10 |
| **Hierarchical** | Tree structure | Large-scale | 10+ |
| **Ring** | Circular chain | Pipelines | 4-8 |
| **Star** | Centralized | Simple tasks | 5-15 |

### 🤖 Agent Types

- **Coder**: Code generation, refactoring, optimization
- **Tester**: Test creation, coverage analysis
- **Researcher**: Pattern analysis, documentation
- **Analyst**: Performance analysis
- **Optimizer**: Code optimization
- **Coordinator**: Task orchestration

### ⚡ Execution Strategies

- **Parallel**: Concurrent execution
- **Sequential**: One after another
- **Adaptive**: Smart distribution

## Commands

### Initialize Swarm

```bash
cleanroom swarm init \
    --topology mesh \
    --agents 5 \
    --strategy balanced \
    --auto-scale
```

### Spawn Agent

```bash
cleanroom swarm spawn \
    --type coder \
    --name "backend-dev" \
    --capabilities "rust,backend,api-design"
```

### Orchestrate Task

```bash
cleanroom swarm orchestrate \
    --task "Build and test the entire project" \
    --priority high \
    --strategy parallel \
    --max-agents 3 \
    --timeout 300
```

### Monitor Swarm

```bash
# Basic status
cleanroom swarm status

# Detailed status
cleanroom swarm status --detailed --output json

# Agent metrics
cleanroom swarm metrics --agent-id "agent-001"

# List agents
cleanroom swarm list --filter active
```

### Stop Swarm

```bash
cleanroom swarm stop
```

## Complete Workflow Example

```bash
#!/bin/bash
# Complete development workflow

# 1. Initialize swarm
cleanroom swarm init --topology mesh --agents 8 --strategy adaptive

# 2. Spawn specialized agents
cleanroom swarm spawn --type coder --name "backend-dev"
cleanroom swarm spawn --type coder --name "frontend-dev"
cleanroom swarm spawn --type tester --name "test-engineer"
cleanroom swarm spawn --type researcher --name "researcher"
cleanroom swarm spawn --type optimizer --name "optimizer"

# 3. Orchestrate development tasks
cleanroom swarm orchestrate \
    --task "Implement REST API with authentication" \
    --priority high \
    --strategy parallel \
    --max-agents 3

cleanroom swarm orchestrate \
    --task "Create comprehensive test suite" \
    --priority high \
    --strategy parallel \
    --max-agents 2

# 4. Monitor progress
cleanroom swarm status --detailed
cleanroom swarm metrics

# 5. Cleanup
cleanroom swarm stop
```

## Output Formats

### Text (Human-Readable)

```bash
cleanroom swarm status --output text
```

### JSON (Machine-Parsable)

```bash
cleanroom swarm status --output json | jq .
```

### Quiet (Errors Only)

```bash
cleanroom swarm status --output quiet
```

## Security & Resource Management

### Run with Network Isolation

```bash
cleanroom run curl https://example.com --network-isolation
```

### Run with Resource Limits

```bash
cleanroom run python3 script.py \
    --max-memory 512 \
    --max-cpu 50.0 \
    --timeout 60
```

### Run with Policy File

```bash
cleanroom run ./test.sh --policy security-policy.toml
```

Example `security-policy.toml`:

```toml
[security]
enable_network_isolation = true
enable_filesystem_isolation = true
blocked_commands = ["rm", "format"]
allowed_ports = [80, 443]

[resources]
max_memory_usage_bytes = 536870912  # 512 MB
max_cpu_usage_percent = 50.0
max_disk_usage_bytes = 1073741824   # 1 GB
```

## Integration with Claude Flow

The CLI automatically integrates with Claude Flow for enhanced coordination:

```bash
# Swarm state is automatically stored in memory
cleanroom swarm init --topology mesh --agents 5
# Executes: npx claude-flow@alpha hooks post-edit --memory-key "hive/swarm/{swarm_id}"
```

## Documentation

- **Complete Guide**: [swarm-cli-guide.md](./swarm-cli-guide.md)
- **Quick Reference**: [swarm-cli-quick-reference.md](./swarm-cli-quick-reference.md)
- **Implementation Summary**: [swarm-cli-implementation-summary.md](./swarm-cli-implementation-summary.md)
- **Examples**: [../examples/swarm_cli_usage.sh](../examples/swarm_cli_usage.sh)

## Architecture

```
┌─────────────────────────────────────────────────────┐
│              Cleanroom Swarm CLI                    │
│  ┌───────────┐  ┌───────────┐  ┌──────────────┐   │
│  │   Swarm   │  │   Agent   │  │    Task      │   │
│  │   Init    │  │   Spawn   │  │ Orchestrate  │   │
│  └───────────┘  └───────────┘  └──────────────┘   │
│  ┌───────────┐  ┌───────────┐  ┌──────────────┐   │
│  │  Status   │  │  Metrics  │  │     Stop     │   │
│  │ Monitor   │  │  Collect  │  │   Cleanup    │   │
│  └───────────┘  └───────────┘  └──────────────┘   │
└─────────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────┐
│           Cleanroom Environment                     │
│  ┌───────────────────────────────────────────────┐ │
│  │   Hermetic Execution + Security Policies      │ │
│  └───────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────┐
│           Container Backend                         │
│     Docker / Podman / Kubernetes                    │
└─────────────────────────────────────────────────────┘
```

## Benefits

### For Developers

- ⚡ **Parallel Testing**: Run tests across multiple agents
- 🎯 **Task Coordination**: Orchestrate complex workflows
- 📊 **Monitoring**: Real-time performance metrics
- 🔒 **Security**: Isolated execution environments

### For Teams

- 🤝 **Collaboration**: Mesh topology for team work
- 📈 **Scalability**: Hierarchical topology for large projects
- 🔄 **Flexibility**: Multiple strategies and topologies
- 👁️ **Observability**: Comprehensive monitoring

### For CI/CD

- 🤖 **Automation**: JSON output for scripting
- ⚡ **Parallelization**: Speed up CI pipelines
- 🎛️ **Resource Control**: Fine-grained limits
- 🛡️ **Security**: Policy-based execution

## Examples

See [examples/swarm_cli_usage.sh](../examples/swarm_cli_usage.sh) for comprehensive usage examples including:

1. Basic swarm initialization
2. Hierarchical swarm with auto-scaling
3. Spawning specialized agents
4. Orchestrating complex tasks
5. Monitoring swarm status
6. Listing agents
7. Getting performance metrics
8. Running commands with security policies
9. Environment management
10. Performance benchmarking
11. System status
12. Cleanup operations
13. Complete development workflow

## Troubleshooting

### Swarm initialization fails

```bash
# Check Docker/Podman availability
docker ps

# Check system resources
cleanroom status
```

### Agent spawn failures

```bash
# Check swarm status
cleanroom swarm status --detailed

# Verify agent name is unique
cleanroom swarm list
```

### Task orchestration timeouts

```bash
# Increase timeout
cleanroom swarm orchestrate --task "..." --timeout 600

# Check agent health
cleanroom swarm metrics
```

## Contributing

We welcome contributions! Please see our [Contributing Guide](../../CONTRIBUTING.md) for details.

## License

MIT License - see [LICENSE](../../LICENSE) for details.

---

**Built with ❤️ by the Cleanroom Team**

For more information:
- 📚 [Documentation](./swarm-cli-guide.md)
- 💬 [Discussions](https://github.com/sac/ggen/discussions)
- 🐛 [Issues](https://github.com/sac/ggen/issues)
