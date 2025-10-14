# Cleanroom CLI Command Tree

**Visual hierarchy of all CLI commands**

```
cleanroom (root)
│
├── Global Flags
│   ├── --config <file>       Configuration file path
│   ├── --output <format>     Output format (json|yaml|table|wide)
│   ├── --quiet               Suppress non-essential output
│   ├── --verbose             Enable verbose logging
│   ├── --debug               Enable debug mode
│   ├── --no-color           Disable colored output
│   ├── --timeout <duration>  Command timeout
│   └── --context <name>      Use specific context
│
├── environment (Cleanroom Environments)
│   ├── create <name> [--config <file>]
│   ├── start <name>
│   ├── stop <name>
│   ├── restart <name>
│   ├── delete <name> [--force]
│   ├── list [--status <filter>]
│   ├── show <name>
│   ├── describe <name>
│   ├── logs <name> [--tail <n>] [--follow]
│   ├── exec <name> -- <command> [args...]
│   ├── inspect <name>
│   ├── validate <name>
│   └── cleanup <name>
│
├── container (Container Management)
│   ├── create <name> [--type <type>] [--image <image>]
│   ├── start <type> [--name <name>] [options...]
│   │   ├── postgres [--db <db>] [--user <user>] [--password <pass>] [--port <port>]
│   │   ├── redis [--password <pass>] [--port <port>]
│   │   └── generic [--image <image>] [--port <mapping>] [--env <key=value>] [--volume <mapping>]
│   ├── stop <name>
│   ├── restart <name>
│   ├── delete <name> [--force]
│   ├── list [--type <filter>] [--status <filter>]
│   ├── show <name>
│   ├── describe <name>
│   ├── logs <name> [--tail <n>] [--follow]
│   ├── exec <name> -- <command> [args...]
│   ├── inspect <name>
│   ├── ps [filters...]
│   ├── port <name>
│   └── stats <name> [--interval <duration>]
│
├── test (Test Execution)
│   ├── run <test-file> [--timeout <duration>] [--parallel <n>]
│   ├── run <pattern> [--coverage] [--min-coverage <percent>]
│   ├── list [--status <filter>] [--pattern <glob>]
│   ├── show <test-id>
│   ├── describe <test-id>
│   ├── validate <test-file>
│   ├── watch <test-file> [--interval <duration>]
│   ├── report [--format <html|json|yaml>] [--output <file>]
│   ├── coverage [--file <file>] [--min <percent>]
│   └── results <test-id> [--format <format>]
│
├── metrics (Performance Metrics)
│   ├── show [--timeframe <duration>]
│   ├── list [--filter <key>]
│   ├── describe <metric-key>
│   ├── export [--file <file>] [--format <json|yaml|csv>]
│   ├── reset [--confirm]
│   ├── watch [--interval <duration>] [--keys <list>]
│   └── report [--timeframe <duration>] [--output <file>]
│
├── swarm (Swarm Coordination)
│   ├── init [--topology <mesh|hierarchical|ring|star>] [--agents <n>] [--strategy <balanced|specialized|adaptive>]
│   ├── spawn agent [--type <type>] [--name <name>] [--capabilities <list>]
│   ├── orchestrate task [--description <text>] [--priority <low|medium|high|critical>] [--max-agents <n>]
│   ├── list [--status <active|idle|destroyed>]
│   ├── show <swarm-id>
│   ├── describe <swarm-id>
│   ├── status <swarm-id>
│   ├── scale <swarm-id> [--agents <n>]
│   ├── destroy <swarm-id> [--cleanup]
│   └── health <swarm-id>
│
├── agent (Agent Management)
│   ├── spawn [--type <coder|researcher|tester|analyst|optimizer|coordinator>] [--name <name>]
│   ├── list [--status <active|idle|busy>] [--type <filter>]
│   ├── show <agent-id>
│   ├── describe <agent-id>
│   ├── status <agent-id>
│   ├── logs <agent-id> [--tail <n>] [--follow]
│   ├── kill <agent-id> [--force]
│   └── health <agent-id>
│
├── task (Task Management)
│   ├── create [--name <name>] [--agent <agent-id>] [--timeout <duration>]
│   ├── start <task-id>
│   ├── stop <task-id>
│   ├── cancel <task-id>
│   ├── list [--status <pending|running|completed|failed>]
│   ├── show <task-id>
│   ├── describe <task-id>
│   ├── status <task-id>
│   ├── logs <task-id> [--tail <n>] [--follow]
│   ├── wait <task-id> [--timeout <duration>]
│   └── results <task-id> [--format <json|yaml>]
│
├── config (Configuration Management)
│   ├── get <key>
│   ├── set <key> <value>
│   ├── unset <key>
│   ├── list [--scope <global|environment|user>]
│   ├── show [--scope <filter>]
│   ├── validate [--config <file>]
│   ├── export [--file <file>] [--format <toml|yaml|json>]
│   ├── import [--file <file>] [--merge]
│   └── reset [--confirm] [--scope <filter>]
│
├── service (Service Management)
│   ├── start <type> [--name <name>] [options...]
│   │   ├── postgres [options...]
│   │   ├── redis [options...]
│   │   └── custom [--image <image>] [options...]
│   ├── stop <name>
│   ├── restart <name>
│   ├── list [--type <filter>] [--status <filter>]
│   ├── show <name>
│   ├── describe <name>
│   ├── status <name>
│   ├── logs <name> [--tail <n>] [--follow]
│   └── health <name>
│
└── snapshot (Snapshot Management)
    ├── create [--name <name>] [--environment <name>]
    ├── restore <snapshot-id> [--environment <name>]
    ├── delete <snapshot-id> [--force]
    ├── list [--environment <filter>]
    ├── show <snapshot-id>
    ├── describe <snapshot-id>
    └── compare <snapshot-id1> <snapshot-id2> [--diff]
```

## Command Categories

### Core Operations (Most Frequently Used)
```
cleanroom environment create/start/stop
cleanroom container start/list/logs
cleanroom test run/report/coverage
cleanroom metrics show/export
```

### Swarm Operations (Hive Mind Coordination)
```
cleanroom swarm init/orchestrate/status
cleanroom agent spawn/list/health
cleanroom task create/wait/results
```

### Management Operations
```
cleanroom config get/set/list
cleanroom service start/health
cleanroom snapshot create/restore
```

## Command Aliases (Future Enhancement)

```bash
# Short aliases for common operations
cleanroom env     → cleanroom environment
cleanroom ct      → cleanroom container
cleanroom svc     → cleanroom service
cleanroom cfg     → cleanroom config

# Convenience aliases
cleanroom up      → cleanroom environment start
cleanroom down    → cleanroom environment stop
cleanroom ps      → cleanroom container list
cleanroom logs    → cleanroom container logs (default container)
```

## Context-Aware Commands (Future Enhancement)

When inside an environment context:
```bash
# Automatically use current environment
cleanroom exec -- <command>          # Execute in current environment
cleanroom logs                       # Show logs for current environment
cleanroom container start postgres   # Start container in current environment
```

## Pipeline Support

```bash
# Pipe container list to jq
cleanroom container list --output json | jq '.containers[].name'

# Chain commands
cleanroom environment create test && cleanroom container start postgres

# Use in scripts
if cleanroom test run test.rs; then
  cleanroom environment cleanup
fi
```

## Exit Code Reference

```bash
cleanroom environment create myenv
echo $?  # 0 = success

cleanroom environment create myenv  # Already exists
echo $?  # 5 = already exists

cleanroom container show nonexistent
echo $?  # 3 = not found

cleanroom test run --timeout 1s long_test
echo $?  # 4 = timeout
```

---

**Note:** This command tree represents the complete CLI surface area. Implementation will proceed in phases as outlined in CLI_ARCHITECTURE.md.
