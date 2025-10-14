# Cleanroom Swarm CLI Guide

## Overview

The Cleanroom Swarm CLI provides comprehensive agent coordination and orchestration capabilities inspired by the Hive Queen architecture. It enables parallel test execution, distributed task orchestration, and sophisticated resource management for deterministic testing environments.

## Table of Contents

1. [Installation](#installation)
2. [Core Concepts](#core-concepts)
3. [Command Reference](#command-reference)
4. [Usage Examples](#usage-examples)
5. [Advanced Features](#advanced-features)
6. [Best Practices](#best-practices)

## Installation

### Building from Source

```bash
# Clone the repository
git clone https://github.com/sac/ggen
cd ggen/cleanroom

# Build the CLI
cargo build --release

# Install globally
cargo install --path .
```

### Verify Installation

```bash
cleanroom --version
cleanroom --help
```

## Core Concepts

### Swarm Topologies

The CLI supports four swarm topologies for different coordination patterns:

#### 1. Mesh Topology
- **Best for**: Peer-to-peer collaboration, flexible coordination
- **Pattern**: Every agent can communicate with every other agent
- **Use case**: Small to medium teams, high collaboration needs

```bash
cleanroom swarm init --topology mesh --agents 5
```

#### 2. Hierarchical Topology
- **Best for**: Large-scale coordination, clear task delegation
- **Pattern**: Tree structure with coordinator agents managing workers
- **Use case**: Large projects, clear separation of concerns

```bash
cleanroom swarm init --topology hierarchical --agents 10
```

#### 3. Ring Topology
- **Best for**: Sequential processing, pipeline workflows
- **Pattern**: Agents arranged in a circular chain
- **Use case**: Data processing pipelines, sequential operations

```bash
cleanroom swarm init --topology ring --agents 6
```

#### 4. Star Topology
- **Best for**: Centralized control, simple coordination
- **Pattern**: Single coordinator agent manages all workers
- **Use case**: Simple task distribution, centralized monitoring

```bash
cleanroom swarm init --topology star --agents 8
```

### Agent Types

Each agent type specializes in specific development tasks:

| Agent Type | Capabilities | Best For |
|------------|--------------|----------|
| **Coder** | Code generation, refactoring, optimization | Implementation work |
| **Tester** | Test creation, coverage analysis, validation | Quality assurance |
| **Researcher** | Pattern analysis, best practices, documentation | Research and planning |
| **Analyst** | Performance analysis, bottleneck detection | Optimization |
| **Optimizer** | Code optimization, performance tuning | Performance work |
| **Coordinator** | Task orchestration, resource management | Swarm management |

### Distribution Strategies

#### Balanced Strategy
- Equal distribution of tasks across all agents
- Best for homogeneous workloads
- Predictable resource usage

```bash
cleanroom swarm init --strategy balanced
```

#### Specialized Strategy
- Tasks routed to agents with matching capabilities
- Best for heterogeneous workloads
- Optimal expertise utilization

```bash
cleanroom swarm init --strategy specialized
```

#### Adaptive Strategy
- Dynamic task distribution based on agent load and performance
- Best for varying workloads
- Automatic load balancing

```bash
cleanroom swarm init --strategy adaptive
```

## Command Reference

### Global Options

```bash
--output <FORMAT>    # Output format: text, json, quiet (default: text)
--verbose           # Enable verbose logging
--help              # Show help information
--version           # Show version information
```

### Swarm Commands

#### Initialize Swarm

```bash
cleanroom swarm init [OPTIONS]

Options:
  -t, --topology <TYPE>      Swarm topology: mesh, hierarchical, ring, star (default: mesh)
  -a, --agents <NUM>         Maximum number of agents (default: 5)
  -s, --strategy <TYPE>      Distribution strategy: balanced, specialized, adaptive (default: balanced)
      --auto-scale          Enable auto-scaling based on load
  -o, --output <FORMAT>     Output format: text, json, quiet
```

#### Spawn Agent

```bash
cleanroom swarm spawn [OPTIONS]

Options:
  -t, --type <TYPE>          Agent type: researcher, coder, tester, analyst, optimizer, coordinator
  -n, --name <NAME>          Agent name (must be unique)
  -c, --capabilities <LIST>  Comma-separated capabilities
  -s, --swarm-id <ID>        Assign to specific swarm
  -o, --output <FORMAT>      Output format
```

#### Orchestrate Task

```bash
cleanroom swarm orchestrate [OPTIONS]

Options:
  -t, --task <DESCRIPTION>   Task description or instructions
  -m, --max-agents <NUM>     Maximum agents to use
  -p, --priority <LEVEL>     Priority: low, medium, high, critical (default: medium)
  -e, --strategy <TYPE>      Execution strategy: parallel, sequential, adaptive (default: adaptive)
      --timeout <SECONDS>    Timeout in seconds
  -o, --output <FORMAT>      Output format
```

#### Show Status

```bash
cleanroom swarm status [OPTIONS]

Options:
  -d, --detailed            Show detailed status information
  -s, --swarm-id <ID>       Specific swarm ID
  -o, --output <FORMAT>     Output format
```

#### List Agents

```bash
cleanroom swarm list [OPTIONS]

Options:
  -f, --filter <STATUS>     Filter by status: active, idle, busy, all (default: all)
  -d, --detailed           Show detailed agent information
  -o, --output <FORMAT>    Output format
```

#### Get Metrics

```bash
cleanroom swarm metrics [OPTIONS]

Options:
  -a, --agent-id <ID>      Specific agent ID
  -m, --metric <TYPE>      Metric type: cpu, memory, tasks, performance, all (default: all)
  -o, --output <FORMAT>    Output format
```

#### Stop Swarm

```bash
cleanroom swarm stop [OPTIONS]

Options:
  -s, --swarm-id <ID>      Specific swarm ID to stop
  -f, --force             Force stop without cleanup
  -o, --output <FORMAT>    Output format
```

### Run Commands

```bash
cleanroom run [OPTIONS] <COMMAND>

Options:
  -p, --policy <FILE>            Security policy file (TOML)
      --network-isolation        Enable network isolation
      --filesystem-isolation     Enable filesystem isolation
      --max-memory <MB>          Maximum memory in MB
      --max-cpu <PERCENT>        Maximum CPU percentage
  -t, --timeout <SECONDS>        Timeout in seconds
  -o, --output <FORMAT>          Output format
```

### Environment Commands

```bash
# Create environment
cleanroom env create --name <NAME> [--config <FILE>]

# List environments
cleanroom env list

# Show environment details
cleanroom env show <NAME>

# Delete environment
cleanroom env delete <NAME> [--force]

# Cleanup all environments
cleanroom env cleanup
```

### Benchmark Commands

```bash
cleanroom bench [OPTIONS]

Options:
  -t, --bench-type <TYPE>   Benchmark type: container, swarm, all (default: all)
  -i, --iterations <NUM>    Number of iterations (default: 10)
  -o, --output <FORMAT>     Output format
```

## Usage Examples

### Example 1: Simple Swarm Initialization

```bash
# Initialize a mesh swarm with 5 agents
cleanroom swarm init --topology mesh --agents 5 --output json

# Output:
# {
#   "swarm_id": "swarm-abc123",
#   "topology": "mesh",
#   "max_agents": 5,
#   "strategy": "balanced",
#   "status": "initialized",
#   "health": "healthy"
# }
```

### Example 2: Spawn Specialized Agents

```bash
# Spawn a coder agent
cleanroom swarm spawn \
    --type coder \
    --name "backend-dev-1" \
    --capabilities "rust,backend,api-design"

# Spawn a tester agent
cleanroom swarm spawn \
    --type tester \
    --name "test-engineer-1" \
    --capabilities "testing,tdd,integration-tests"

# Spawn a researcher agent
cleanroom swarm spawn \
    --type researcher \
    --name "research-analyst-1" \
    --capabilities "research,documentation,patterns"
```

### Example 3: Orchestrate Complex Tasks

```bash
# High-priority parallel task
cleanroom swarm orchestrate \
    --task "Build and test the entire project with coverage analysis" \
    --priority high \
    --strategy parallel \
    --max-agents 3

# Medium-priority adaptive task
cleanroom swarm orchestrate \
    --task "Analyze performance bottlenecks and suggest optimizations" \
    --priority medium \
    --strategy adaptive \
    --max-agents 2 \
    --timeout 300

# Low-priority sequential task
cleanroom swarm orchestrate \
    --task "Generate comprehensive API documentation" \
    --priority low \
    --strategy sequential \
    --max-agents 1
```

### Example 4: Monitor Swarm Performance

```bash
# Basic status
cleanroom swarm status

# Detailed status with metrics
cleanroom swarm status --detailed --output json

# Agent-specific metrics
cleanroom swarm metrics --agent-id "agent-001"

# Swarm-wide performance metrics
cleanroom swarm metrics --metric performance
```

### Example 5: Complete Development Workflow

```bash
#!/bin/bash
# Complete development workflow with swarm coordination

# 1. Initialize development swarm
cleanroom swarm init --topology mesh --agents 8 --strategy adaptive

# 2. Spawn specialized agents
cleanroom swarm spawn --type coder --name "backend-dev" --capabilities "rust,backend"
cleanroom swarm spawn --type coder --name "frontend-dev" --capabilities "typescript,react"
cleanroom swarm spawn --type tester --name "test-engineer" --capabilities "testing,tdd"
cleanroom swarm spawn --type researcher --name "researcher" --capabilities "research,docs"
cleanroom swarm spawn --type optimizer --name "optimizer" --capabilities "performance"

# 3. Orchestrate development tasks
cleanroom swarm orchestrate \
    --task "Implement REST API with authentication and authorization" \
    --priority high \
    --strategy parallel \
    --max-agents 3

cleanroom swarm orchestrate \
    --task "Create comprehensive test suite with 90% coverage" \
    --priority high \
    --strategy parallel \
    --max-agents 2

cleanroom swarm orchestrate \
    --task "Optimize database queries and API performance" \
    --priority medium \
    --strategy sequential \
    --max-agents 1

# 4. Monitor progress
cleanroom swarm status --detailed
cleanroom swarm metrics

# 5. Cleanup
cleanroom swarm stop
```

## Advanced Features

### Auto-Scaling

Enable automatic scaling based on workload:

```bash
cleanroom swarm init --topology mesh --agents 5 --auto-scale

# Swarm will automatically scale up to handle increased load
# and scale down during idle periods
```

### Security Policies

Run commands with fine-grained security controls:

```bash
# Network isolation
cleanroom run curl https://example.com --network-isolation

# Resource limits
cleanroom run python3 script.py \
    --max-memory 512 \
    --max-cpu 50.0 \
    --timeout 60

# Policy file
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
max_memory_mb = 512
max_cpu_percent = 50.0
max_disk_mb = 1000
```

### Performance Benchmarking

```bash
# Container performance
cleanroom bench --bench-type container --iterations 10

# Swarm performance
cleanroom bench --bench-type swarm --iterations 5

# Comprehensive benchmark
cleanroom bench --bench-type all --iterations 20
```

### Environment Management

```bash
# Create isolated test environment
cleanroom env create --name "integration-tests"

# List all environments
cleanroom env list

# Show environment details
cleanroom env show "integration-tests"

# Delete environment
cleanroom env delete "integration-tests" --force

# Cleanup all environments
cleanroom env cleanup
```

## Best Practices

### 1. Topology Selection

- **Mesh**: Use for collaborative work where agents need to share context frequently
- **Hierarchical**: Use for large-scale projects with clear task hierarchies
- **Ring**: Use for pipeline processing where tasks flow sequentially
- **Star**: Use for simple task distribution with centralized control

### 2. Agent Specialization

```bash
# Good: Specialized agents with clear capabilities
cleanroom swarm spawn --type coder --name "rust-expert" --capabilities "rust,systems-programming"
cleanroom swarm spawn --type tester --name "integration-tester" --capabilities "integration-tests,e2e"

# Avoid: Generic agents with too many capabilities
cleanroom swarm spawn --type coder --name "generic" --capabilities "everything"
```

### 3. Task Orchestration

```bash
# Good: Clear task descriptions with appropriate priority and strategy
cleanroom swarm orchestrate \
    --task "Implement user authentication with JWT tokens and refresh logic" \
    --priority high \
    --strategy parallel \
    --max-agents 2

# Avoid: Vague tasks without context
cleanroom swarm orchestrate --task "do stuff" --priority low
```

### 4. Resource Management

```bash
# Good: Set appropriate resource limits
cleanroom run intensive-task.sh \
    --max-memory 1024 \
    --max-cpu 75.0 \
    --timeout 300

# Monitor resource usage
cleanroom swarm metrics --metric all
```

### 5. Monitoring and Observability

```bash
# Regular status checks
cleanroom swarm status --detailed

# Agent-specific monitoring
cleanroom swarm metrics --agent-id "agent-001"

# List agent states
cleanroom swarm list --filter active
```

### 6. Graceful Shutdown

```bash
# Good: Allow cleanup
cleanroom swarm stop

# Only use force when necessary
cleanroom swarm stop --force
```

## Integration with Claude Flow

The Cleanroom CLI integrates with Claude Flow for enhanced coordination:

```bash
# Initialize swarm (automatically stores state via hooks)
cleanroom swarm init --topology mesh --agents 5

# Coordination state stored in memory:
# npx claude-flow@alpha hooks post-edit --memory-key "hive/swarm/{swarm_id}"
```

## Troubleshooting

### Common Issues

1. **Swarm initialization fails**
   ```bash
   # Check Docker/Podman availability
   docker ps

   # Check system resources
   cleanroom status
   ```

2. **Agent spawn failures**
   ```bash
   # Check swarm status
   cleanroom swarm status --detailed

   # Verify agent name is unique
   cleanroom swarm list
   ```

3. **Task orchestration timeouts**
   ```bash
   # Increase timeout
   cleanroom swarm orchestrate --task "..." --timeout 600

   # Check agent health
   cleanroom swarm metrics
   ```

## Performance Tuning

### Optimal Swarm Sizes

- **Small projects**: 3-5 agents
- **Medium projects**: 5-10 agents
- **Large projects**: 10-20 agents
- **Enterprise**: 20+ agents with hierarchical topology

### Strategy Selection

| Workload Type | Recommended Strategy |
|---------------|---------------------|
| Uniform tasks | Balanced |
| Specialized tasks | Specialized |
| Variable load | Adaptive |
| Mixed workload | Adaptive + Auto-scale |

## Additional Resources

- [Cleanroom Documentation](../README.md)
- [Architecture Overview](./architecture-overview.md)
- [Development Guide](./development/README.md)
- [Example Scripts](../examples/swarm_cli_usage.sh)

## Contributing

We welcome contributions! See our [Contributing Guide](../../CONTRIBUTING.md) for details.

## License

MIT License - see [LICENSE](../../LICENSE) for details.
