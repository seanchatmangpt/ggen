# Cleanroom CLI Architecture Summary

**Quick Reference for Hive Mind Development**

## Command Pattern

```bash
cleanroom <noun> <verb> [flags] [args]
```

## Core Resources (Nouns)

1. **environment** - Cleanroom execution environments
2. **container** - Individual containers (postgres, redis, generic)
3. **test** - Test execution and management
4. **metrics** - Performance and resource metrics
5. **swarm** - Distributed agent swarms
6. **agent** - Individual agents within swarms
7. **task** - Concurrent tasks executed by agents
8. **config** - Configuration management
9. **service** - Service management (postgres, redis)
10. **snapshot** - Snapshot management

## Universal Verbs

- `create` - Create new resource
- `delete` - Delete resource
- `list` - List all resources
- `show` - Show basic info
- `describe` - Show verbose info
- `start` - Start resource
- `stop` - Stop resource
- `restart` - Restart resource
- `exec` - Execute command
- `logs` - View logs
- `inspect` - Inspect configuration
- `validate` - Validate configuration
- `status` - Show status
- `health` - Check health
- `watch` - Watch in real-time

## Output Formats

```bash
--output table  # Human-readable (default)
--output json   # Machine-readable
--output yaml   # Configuration-friendly
--output wide   # Extended table
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General Error |
| 2 | Misuse |
| 3 | Not Found |
| 4 | Timeout |
| 5 | Already Exists |
| 6 | Validation Error |
| 7 | Permission Denied |
| 8 | Backend Error |
| 9 | Network Error |
| 10 | Resource Error |

## Quick Examples

### Environment
```bash
cleanroom environment create myenv --config env.toml
cleanroom environment start myenv
cleanroom environment exec myenv -- echo "Hello"
cleanroom environment cleanup myenv
```

### Container
```bash
cleanroom container start postgres --db testdb --user test --password pass
cleanroom container list --output json
cleanroom container exec postgres -- psql -U test -d testdb -c "SELECT 1"
cleanroom container stop postgres
```

### Test
```bash
cleanroom test run test.rs --timeout 30s
cleanroom test run --coverage --min-coverage 80
cleanroom test report --format html --output report.html
```

### Swarm (Hive Mind)
```bash
cleanroom swarm init --topology mesh --agents 5
cleanroom swarm spawn agent --type coder --name worker-1
cleanroom swarm orchestrate task --description "Build project" --priority high
cleanroom swarm status --format json
```

### Metrics
```bash
cleanroom metrics show --output json
cleanroom metrics watch --interval 1s
cleanroom metrics export --file metrics.json
```

## Configuration

### File Locations (Precedence Order)
1. Command-line flags
2. Environment variables (`CLEANROOM_*`)
3. Project config (`./cleanroom.toml`)
4. User config (`~/.cleanroom/config.toml`)
5. System config (`/etc/cleanroom/config.toml`)
6. Built-in defaults

### Sample Configuration
```toml
[global]
output_format = "table"
timeout = "5m"

[environment]
default_backend = "docker"
enable_singleton = true

[test]
execution_timeout = "5m"
coverage_enabled = true

[swarm]
default_topology = "mesh"
max_agents = 10
```

## Cross-Language Integration

### Python
```python
from cleanroom_cli import CleanroomCLI

cli = CleanroomCLI()
env = cli.environment_create("myenv")
results = cli.test_run("test.rs")
```

### Node.js
```javascript
const { CleanroomCLI } = require('cleanroom-cli');

const cli = new CleanroomCLI();
const env = cli.environmentCreate('myenv');
const results = cli.testRun('test.rs');
```

### Bash
```bash
# JSON parsing with jq
containers=$(cleanroom container list --output json)
echo "$containers" | jq '.containers[] | select(.status == "running")'
```

## Implementation Phases

1. **Phase 1** (Week 1-2): Core infrastructure, output formats, config
2. **Phase 2** (Week 3-4): Environment & container commands
3. **Phase 3** (Week 5-6): Test & metrics commands
4. **Phase 4** (Week 7-8): Swarm & agent commands
5. **Phase 5** (Week 9-10): Polish & documentation

## Key Design Principles

1. **Scriptability First** - No interactive prompts, proper exit codes
2. **Consistency** - Universal verbs across resources
3. **Discoverability** - Built-in help at every level
4. **Performance** - Fast command execution (< 100ms local ops)
5. **Cross-Language** - JSON/YAML output for easy parsing

## Success Metrics

- Command execution < 100ms (local operations)
- Container operations < 5s
- Test execution < 30s
- Memory usage < 50MB
- Test coverage > 90%

---

For complete details, see [CLI_ARCHITECTURE.md](./CLI_ARCHITECTURE.md)
