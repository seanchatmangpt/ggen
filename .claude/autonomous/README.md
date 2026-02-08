# Autonomous System - User Guide

Simple autonomous execution system using Claude Code Task tool. Run 20 concurrent agents processing a task queue with automatic persistence and crash recovery.

## Quick Start

### 1. Installation

```bash
# Clone and build
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make build-autonomous

# Or install from release
curl -L https://github.com/seanchatmangpt/ggen/releases/latest/download/ggen-autonomous-linux -o ggen-autonomous
chmod +x ggen-autonomous
sudo mv ggen-autonomous /usr/local/bin/
```

### 2. Initialize State

Create initial state file:

```bash
cat > .state.json <<EOF
{
  "tasks": [
    {"id": 1, "status": "pending", "desc": "Analyze codebase structure", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 2, "status": "pending", "desc": "Run test suite", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null}
  ],
  "checkpoint": "2026-02-08T12:00:00Z",
  "progress": {
    "completed": 0,
    "total": 2,
    "failed": 0
  }
}
EOF
```

### 3. Run Locally

```bash
# Development mode
RUST_LOG=info ./ggen-autonomous

# Production mode
RUST_LOG=warn AGENT_COUNT=20 ./ggen-autonomous
```

### 4. Install as Service

**Linux (systemd):**

```bash
sudo cp ggen-autonomous /opt/ggen/
sudo cp systemd/ggen-autonomous.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable ggen-autonomous
sudo systemctl start ggen-autonomous
```

**macOS (launchd):**

```bash
cp ggen-autonomous /usr/local/bin/
cp launchd/io.ggen.autonomous.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/io.ggen.autonomous.plist
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `AGENT_COUNT` | `20` | Number of concurrent agents |
| `SAVE_INTERVAL` | `300` | State save interval (seconds) |
| `STATE_FILE` | `.state.json` | State file path |
| `MAX_RETRIES` | `3` | Max task retry attempts |
| `RUST_LOG` | `info` | Log level (error/warn/info/debug/trace) |

### Example

```bash
# Custom configuration
export AGENT_COUNT=50
export SAVE_INTERVAL=600
export STATE_FILE=/var/lib/ggen/.state.json
export RUST_LOG=debug

./ggen-autonomous
```

## Usage

### Adding Tasks

Edit `.state.json` and add tasks to the `tasks` array:

```json
{
  "tasks": [
    {
      "id": 3,
      "status": "pending",
      "desc": "Refactor authentication module",
      "created_at": "2026-02-08T14:00:00Z",
      "updated_at": "2026-02-08T14:00:00Z",
      "retry_count": 0,
      "result": null,
      "error": null
    }
  ],
  "checkpoint": "2026-02-08T14:00:00Z",
  "progress": {
    "completed": 2,
    "total": 3,
    "failed": 0
  }
}
```

The system will automatically pick up new tasks on the next state load cycle (every 5 minutes) or restart.

### Task Lifecycle

```
pending → in_progress → completed
                     ↘ failed (after 3 retries)
```

**Status Meanings:**
- `pending`: Waiting to be processed
- `in_progress`: Currently being executed by an agent
- `completed`: Successfully finished
- `failed`: Failed after max retries

### Monitoring Progress

**View logs:**
```bash
# Systemd
journalctl -u ggen-autonomous -f

# Launchd
tail -f /tmp/ggen-autonomous.log

# Local
# Logs to stdout/stderr
```

**Check state:**
```bash
# Pretty-print current state
cat .state.json | jq '.'

# Check progress
cat .state.json | jq '.progress'

# List pending tasks
cat .state.json | jq '.tasks[] | select(.status == "pending")'

# List failed tasks
cat .state.json | jq '.tasks[] | select(.status == "failed")'
```

**Metrics (if enabled):**
```bash
curl http://localhost:9090/metrics
```

## Common Operations

### Pause System

```bash
# Systemd
sudo systemctl stop ggen-autonomous

# Launchd
launchctl stop io.ggen.autonomous

# Local: Send SIGTERM
kill -TERM $(pgrep ggen-autonomous)
```

State is automatically saved on shutdown.

### Resume System

```bash
# Systemd
sudo systemctl start ggen-autonomous

# Launchd
launchctl start io.ggen.autonomous
```

### Reset State

```bash
# Backup current state
cp .state.json .state.json.backup

# Create fresh state
cat > .state.json <<EOF
{
  "tasks": [],
  "checkpoint": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "progress": {"completed": 0, "total": 0, "failed": 0}
}
EOF

# Restart service
sudo systemctl restart ggen-autonomous
```

### View Completed Tasks

```bash
cat .state.json | jq '.tasks[] | select(.status == "completed") | {id, desc, result}'
```

### Retry Failed Tasks

```bash
# Reset failed tasks to pending
jq '.tasks |= map(if .status == "failed" then .status = "pending" | .retry_count = 0 else . end)' .state.json > .state.json.tmp
mv .state.json.tmp .state.json

# Restart to pick up changes
sudo systemctl restart ggen-autonomous
```

## Troubleshooting

### System Won't Start

**Check logs:**
```bash
journalctl -u ggen-autonomous -n 50 --no-pager
```

**Common issues:**
1. **State file corrupted**: Restore from backup (`.state.json.1`, `.state.json.2`, `.state.json.3`)
2. **Permissions**: Ensure user has read/write access to state file
3. **Missing dependencies**: Verify Rust runtime installed

**Validate state file:**
```bash
jq empty .state.json  # Should output nothing if valid
# Or
cat .state.json | jq '.' > /dev/null && echo "Valid JSON" || echo "Invalid JSON"
```

### Tasks Not Processing

**Check agent count:**
```bash
ps aux | grep ggen-autonomous | wc -l
```

**Verify pending tasks:**
```bash
cat .state.json | jq '.tasks[] | select(.status == "pending") | .id'
```

**Check logs for errors:**
```bash
journalctl -u ggen-autonomous -f | grep ERROR
```

### High Resource Usage

**Check memory:**
```bash
systemctl status ggen-autonomous | grep Memory
```

**Reduce agent count:**
```bash
# Edit systemd service
sudo systemctl edit ggen-autonomous

# Add:
[Service]
Environment="AGENT_COUNT=10"

# Reload
sudo systemctl daemon-reload
sudo systemctl restart ggen-autonomous
```

### State File Growing Too Large

**Archive completed tasks:**
```bash
# Extract completed tasks
jq '.tasks[] | select(.status == "completed")' .state.json > completed-$(date +%Y%m%d).json

# Remove from state
jq '.tasks = [.tasks[] | select(.status != "completed")]' .state.json > .state.json.tmp
mv .state.json.tmp .state.json

# Update progress
jq '.progress.total = (.tasks | length)' .state.json > .state.json.tmp
mv .state.json.tmp .state.json
```

## Examples

### Example 1: Code Analysis Pipeline

```json
{
  "tasks": [
    {"id": 1, "status": "pending", "desc": "Scan codebase for TODO comments", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 2, "status": "pending", "desc": "Identify unused dependencies", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 3, "status": "pending", "desc": "Generate API documentation", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 4, "status": "pending", "desc": "Check test coverage", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null}
  ],
  "checkpoint": "2026-02-08T12:00:00Z",
  "progress": {"completed": 0, "total": 4, "failed": 0}
}
```

### Example 2: Continuous Monitoring

```json
{
  "tasks": [
    {"id": 1, "status": "pending", "desc": "Monitor system health metrics", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 2, "status": "pending", "desc": "Check for security vulnerabilities", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 3, "status": "pending", "desc": "Validate configuration files", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 4, "status": "pending", "desc": "Update dependency versions", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null}
  ],
  "checkpoint": "2026-02-08T12:00:00Z",
  "progress": {"completed": 0, "total": 4, "failed": 0}
}
```

### Example 3: Batch Processing

```json
{
  "tasks": [
    {"id": 1, "status": "pending", "desc": "Process data file batch-001.csv", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 2, "status": "pending", "desc": "Process data file batch-002.csv", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 3, "status": "pending", "desc": "Process data file batch-003.csv", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null},
    {"id": 4, "status": "pending", "desc": "Generate summary report", "created_at": "2026-02-08T12:00:00Z", "updated_at": "2026-02-08T12:00:00Z", "retry_count": 0, "result": null, "error": null}
  ],
  "checkpoint": "2026-02-08T12:00:00Z",
  "progress": {"completed": 0, "total": 4, "failed": 0}
}
```

## Best Practices

1. **Task Descriptions**: Be specific and actionable
   - Good: "Run cargo test on crates/ggen-core"
   - Bad: "Test stuff"

2. **State Backups**: Regularly backup `.state.json`
   ```bash
   # Daily backup via cron
   0 0 * * * cp /var/lib/ggen/.state.json /backups/state-$(date +\%Y\%m\%d).json
   ```

3. **Monitoring**: Set up alerts for failures
   ```bash
   # Check for failures every hour
   */60 * * * * [ $(jq '.progress.failed' /var/lib/ggen/.state.json) -gt 0 ] && notify-admin
   ```

4. **Resource Limits**: Start conservative, scale up
   - Start: 10 agents
   - Monitor: CPU/Memory usage
   - Scale: Increase if resources available

5. **Graceful Shutdown**: Always stop service properly
   ```bash
   # Good
   sudo systemctl stop ggen-autonomous

   # Bad
   sudo kill -9 $(pgrep ggen-autonomous)  # May corrupt state
   ```

## Advanced Usage

### Custom Task Queue

Instead of editing `.state.json` manually, create a task generator:

```bash
#!/bin/bash
# generate-tasks.sh

STATE_FILE=".state.json"
START_ID=$(jq '.tasks | map(.id) | max + 1' $STATE_FILE)

for file in src/*.rs; do
  TASK=$(jq -n \
    --arg id "$START_ID" \
    --arg desc "Analyze file: $file" \
    --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    '{
      id: ($id | tonumber),
      status: "pending",
      desc: $desc,
      created_at: $ts,
      updated_at: $ts,
      retry_count: 0,
      result: null,
      error: null
    }')

  jq ".tasks += [$TASK]" $STATE_FILE > tmp.json && mv tmp.json $STATE_FILE
  START_ID=$((START_ID + 1))
done

# Update total
jq '.progress.total = (.tasks | length)' $STATE_FILE > tmp.json && mv tmp.json $STATE_FILE
```

### Integration with CI/CD

```yaml
# .github/workflows/autonomous.yml
name: Autonomous Tasks

on:
  schedule:
    - cron: '0 */6 * * *'  # Every 6 hours

jobs:
  process:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run autonomous system
        run: |
          cargo build --release
          timeout 30m ./target/release/ggen-autonomous
      - name: Archive results
        uses: actions/upload-artifact@v2
        with:
          name: state
          path: .state.json
```

## Support

- **Documentation**: `/home/user/ggen/docs/AUTONOMOUS_SIMPLE_DESIGN.md`
- **Schema**: `/home/user/ggen/.claude/autonomous/state.schema.json`
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions

---

**Version**: 1.0.0
**Last Updated**: 2026-02-08
