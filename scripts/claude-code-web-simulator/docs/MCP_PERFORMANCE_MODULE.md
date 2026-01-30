# MCP Performance Module - Complete Guide

## Overview

The MCP Performance Module (`mcp-performance.sh`) provides intelligent caching, timeout management, and performance monitoring for MCP server access. It enables efficient tool execution with automatic result caching, deterministic timeout enforcement, and detailed performance metrics tracking.

**Module Location**: `/scripts/claude-code-web-simulator/modules/mcp-performance.sh`

**Version**: 1.0.0

---

## Features

### 1. Intelligent Caching
- **TTL-based result caching** with automatic expiration
- **Tool definition caching** (30-minute default)
- **Automatic cleanup** of expired entries (>24 hours)
- **Cache statistics** tracking (hits/misses per tool)
- **Cache invalidation** on-demand

### 2. Timeout Management
- **Configurable timeouts** per tool (10s default)
- **Timeout enforcement** via `timeout` command
- **Graceful error handling** with proper exit codes
- **Buffer time** for cleanup operations (1s default)

### 3. Parallel Execution
- **Concurrent tool execution** (up to 4 parallel by default)
- **Process tracking** with automatic job management
- **Failure counting** across parallel operations

### 4. Performance Monitoring
- **Execution timing** (millisecond precision)
- **Slow tool alerting** (>5s threshold)
- **Performance logging** to audit trail
- **Cache hit rate** calculations

---

## Installation & Setup

### 1. Source the Module

```bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh
```

### 2. Initialize Cache

```bash
# Initialize cache directories and logging
mcp_cache_init
```

This creates:
- `.ggen/mcp-cache/results/` - Result cache storage
- `.ggen/mcp-cache/definitions/` - Tool definition cache
- `.ggen/mcp-cache/metrics/` - Performance metrics
- `.ggen/mcp-cache/metadata.json` - Cache metadata
- `.ggen/mcp-cache/performance.log` - Performance audit trail

---

## Core Functions

### Cache Operations

#### `mcp_cache_set(tool_name, cache_key, result, ttl_seconds)`

Store a result in cache with TTL.

**Parameters:**
- `tool_name` (string) - Name of the tool
- `cache_key` (string) - Cache key (usually MD5 of parameters)
- `result` (JSON string) - Result to cache
- `ttl_seconds` (integer) - Time-to-live in seconds (default: 300s)

**Example:**

```bash
result='{"status":"success","data":"value"}'
mcp_cache_set "list_files" "params_hash_abc123" "$result" 600
```

**Output:**
```
[Cache set successful]
```

---

#### `mcp_cache_get(tool_name, cache_key)`

Retrieve cached result if valid.

**Parameters:**
- `tool_name` (string) - Name of the tool
- `cache_key` (string) - Cache key to lookup

**Returns:**
- Cached result if valid (echo to stdout)
- Exit code 1 if cache miss

**Example:**

```bash
if cached=$(mcp_cache_get "list_files" "params_hash_abc123"); then
    echo "Cache hit: $cached"
else
    echo "Cache miss"
fi
```

---

#### `mcp_cache_invalidate(tool_name, [cache_key])`

Invalidate one or all cache entries for a tool.

**Parameters:**
- `tool_name` (string) - Tool name
- `cache_key` (string, optional) - Specific key to invalidate (default: all)

**Example:**

```bash
# Invalidate specific key
mcp_cache_invalidate "list_files" "params_hash_abc123"

# Invalidate all entries for tool
mcp_cache_invalidate "list_files"
```

---

### Tool Execution with Timeout

#### `mcp_tool_with_timeout(tool_name, timeout_ms, parameters)`

Execute MCP tool with timeout enforcement and caching.

**Parameters:**
- `tool_name` (string) - Tool name to execute
- `timeout_ms` (integer) - Timeout in milliseconds (default: 10000)
- `parameters` (string) - Tool parameters (JSON or query string)

**Returns:**
- Tool result (echo to stdout)
- Exit code 124 if timeout
- Exit code from tool otherwise

**Features:**
- Automatic result caching
- Timeout enforcement
- Performance timing
- Slow tool alerting (>5s)

**Example:**

```bash
# Execute with 5s timeout
result=$(mcp_tool_with_timeout "search" "5000" "query=test" 2>&1)
exit_code=$?

if [[ $exit_code -eq 124 ]]; then
    echo "Tool timeout"
elif [[ $exit_code -eq 0 ]]; then
    echo "Result: $result"
fi
```

---

### Parallel Execution

#### `mcp_parallel_tools(max_parallel, spec1, spec2, ...)`

Execute multiple tools in parallel with job management.

**Parameters:**
- `max_parallel` (integer) - Maximum concurrent jobs (default: 4)
- `spec_n` (string) - Tool specification: `tool_name|parameters|timeout_ms`

**Returns:**
- Exit code = number of failed tools
- Results echoed to stdout

**Example:**

```bash
# Execute 3 tools in parallel (max 2 concurrent)
mcp_parallel_tools 2 \
    "search|query=test|5000" \
    "list|pattern=*.txt|3000" \
    "stat|path=/tmp|2000"
```

---

### Tool Definition Caching

#### `mcp_cache_tool_definitions(tool_list_json)`

Cache tool definitions (list of available tools/parameters).

**Parameters:**
- `tool_list_json` (JSON string) - Array of tool definitions

**Example:**

```bash
tools='[{"name":"search","version":"1.0"},{"name":"list","version":"1.0"}]'
mcp_cache_tool_definitions "$tools"
```

---

#### `mcp_get_cached_tool_definitions()`

Retrieve cached tool definitions if valid (30-minute TTL).

**Returns:**
- Tool definitions JSON if cached
- Exit code 1 if cache miss/expired

**Example:**

```bash
if defs=$(mcp_get_cached_tool_definitions); then
    echo "Using cached definitions"
    echo "$defs" | jq '.[] | .name'
else
    echo "No cached definitions"
fi
```

---

### Performance Monitoring

#### `mcp_get_cache_stats()`

Display cache statistics and hit rates.

**Output:**
```
=== MCP Cache Statistics ===

Total Requests: 42
Cache Hits: 38 (90%)
Cache Misses: 4

=== Per-Tool Statistics ===
  search: 20 hits, 2 misses (90% hit rate)
  list: 18 hits, 2 misses (90% hit rate)

Cache Size: 2.3M
```

---

#### `mcp_get_performance_report()`

Display comprehensive performance analysis.

**Output:**
```
=== MCP Performance Report ===
Generated: [timestamp]

=== Recent Performance Events ===
[Last 20 log entries]

=== Slow Tool Alerts (>5s) ===
[Recent slow tool warnings]

=== Timeout Events ===
[Recent timeout occurrences]
```

---

#### `mcp_cache_status()`

Display current cache status summary.

**Output:**
```
=== MCP Cache Status ===

Cache Directory: .ggen/mcp-cache
Initialized: 1

Cached Items: 42
Metadata: [timestamp]

[Full cache statistics]
```

---

### Cache Management

#### `mcp_cache_clear()`

Clear all cached results.

**Output:**
```
[✓] Cleared 42 cache entries
```

---

#### `mcp_cache_cleanup_old_entries()`

Remove cache entries older than 24 hours.

**Automatically called** during `mcp_cache_init()`.

---

## Performance Characteristics

### SLO Targets

| Operation | Target | Notes |
|-----------|--------|-------|
| Cache hit | <10ms | In-memory lookup |
| Cache set | <50ms | Disk I/O, JSON serialization |
| Timeout check | <1ms | Process signal check |
| Parallel spawn | <100ms | Job creation overhead |
| Tool definition cache | <20ms | Cached JSON lookup |

### Resource Usage

- **Memory**: <100MB for 10,000 cache entries
- **Disk**: ~1KB per cache entry (average)
- **CPU**: Minimal (<1% idle monitoring)

### Throughput

- **Cache operations**: ~1000 operations/second
- **Parallel tools**: Up to 4 concurrent executions
- **Tool invocations**: Limited by tool performance, not caching layer

---

## Usage Examples

### Example 1: Simple Tool Call with Caching

```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# Call tool (will cache if successful)
result=$(mcp_tool_with_timeout "search" "5000" "query=important" 2>&1)

if [[ $? -eq 0 ]]; then
    echo "Result: $result"
    # Second call will use cache
    cached=$(mcp_tool_with_timeout "search" "5000" "query=important" 2>&1)
    echo "Cached: $cached"
fi
```

---

### Example 2: Parallel Tool Execution

```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# Execute 3 tools in parallel (max 2 at a time)
echo "Starting parallel execution..."

mcp_parallel_tools 2 \
    "search|query=test|10000" \
    "list_files|path=/data|5000" \
    "validate_schema|file=schema.json|3000"

if [[ $? -eq 0 ]]; then
    echo "All tools succeeded"
else
    echo "Some tools failed"
fi

# Show statistics
mcp_get_cache_stats
```

---

### Example 3: Tool Definition Caching

```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# First request - fetch and cache tool definitions
fetch_tools() {
    # Simulate tool discovery
    echo '[{"name":"search","timeout":5000},{"name":"list","timeout":3000}]'
}

tools=$(fetch_tools)
mcp_cache_tool_definitions "$tools"
echo "Tool definitions cached"

# Later request - use cached definitions
if cached_defs=$(mcp_get_cached_tool_definitions); then
    echo "Using cached tool definitions:"
    echo "$cached_defs" | jq '.'
else
    echo "Definitions expired, fetching fresh copy"
    tools=$(fetch_tools)
    mcp_cache_tool_definitions "$tools"
fi
```

---

### Example 4: Performance Monitoring

```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# Execute various tools
for i in {1..10}; do
    mcp_tool_with_timeout "search" "5000" "query=test_$i" >/dev/null 2>&1
done

# Get statistics
echo "=== After 10 executions ==="
mcp_get_cache_stats

# Get detailed performance report
echo ""
mcp_get_performance_report

# View current status
echo ""
mcp_cache_status
```

---

### Example 5: Error Handling with Timeout

```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# Execute tool with timeout
result=$(mcp_tool_with_timeout "slow_operation" "3000" "param=value" 2>&1)
exit_code=$?

case $exit_code in
    0)
        echo "Success: $result"
        ;;
    124)
        echo "ERROR: Operation timed out after 3 seconds"
        mcp_log_perf "ERROR" "slow_operation timeout"
        exit 1
        ;;
    *)
        echo "ERROR: Operation failed with code $exit_code"
        echo "Output: $result"
        exit $exit_code
        ;;
esac
```

---

## Configuration

### Environment Variables

Override cache behavior via environment variables:

```bash
# Custom cache directory (set before sourcing module)
export MCP_CACHE_DIR="/custom/cache/path"

# Source module after setting override
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh
```

### Built-in Constants

Modify by editing module before sourcing:

```bash
MCP_DEFAULT_TTL=300                 # Default cache TTL (seconds)
MCP_TOOL_DEF_TTL=1800               # Tool definition TTL (30 minutes)
MCP_CACHE_CLEANUP_AGE=86400         # Old cache cleanup threshold (24 hours)
MCP_SLOW_TOOL_THRESHOLD=5000        # Slow tool alert threshold (ms)
MCP_MAX_PARALLEL=4                  # Max concurrent tool executions
MCP_TIMEOUT_BUFFER=1000             # Timeout buffer for cleanup (ms)
```

---

## Performance Tuning

### 1. Optimize Cache Hit Rate

```bash
# Increase TTL for frequently accessed tools
# Reduces cache misses, increases staleness risk
mcp_cache_set "tool_name" "$key" "$result" 1800  # 30 minutes instead of 5
```

### 2. Adjust Timeout Values

```bash
# Conservative timeout (handles slow networks)
mcp_tool_with_timeout "tool" "30000" "params"  # 30 seconds

# Aggressive timeout (fail fast)
mcp_tool_with_timeout "tool" "1000" "params"   # 1 second
```

### 3. Control Parallel Concurrency

```bash
# Conservative (sequential)
mcp_parallel_tools 1 spec1 spec2 spec3

# Aggressive (max parallelism)
mcp_parallel_tools 8 spec1 spec2 spec3
```

### 4. Cache Cleanup Strategy

```bash
# Monthly cleanup
0 0 1 * * /scripts/claude-code-web-simulator/modules/mcp-performance.sh
# Call mcp_cache_cleanup_old_entries within script
```

---

## Troubleshooting

### Problem: Cache Misses on Valid Entries

**Symptom**: `mcp_cache_get` returns miss for recently cached entries

**Solutions**:
1. Check TTL value (may have expired)
2. Verify cache directory exists: `ls -la .ggen/mcp-cache/`
3. Check file permissions: `ls -la .ggen/mcp-cache/results/`
4. Clear cache and retry: `mcp_cache_clear`

---

### Problem: Timeouts Occurring

**Symptom**: Tools frequently timeout with `Exit code: 124`

**Solutions**:
1. Increase timeout: `mcp_tool_with_timeout "tool" "15000" "params"`
2. Check system load: `top`, `ps aux | wc -l`
3. Review performance log: `tail -20 .ggen/mcp-cache/performance.log`
4. Profile tool execution: `time mcp_call_tool ...`

---

### Problem: Cache Directory Issues

**Symptom**: Permission denied or directory not found errors

**Solutions**:
1. Create directory manually: `mkdir -p .ggen/mcp-cache/{results,definitions,metrics}`
2. Fix permissions: `chmod 755 .ggen/mcp-cache`
3. Verify write access: `touch .ggen/mcp-cache/test.txt`
4. Set custom cache directory: `export MCP_CACHE_DIR="/tmp/mcp-cache"`

---

## Integration with Automation

### Shell Script Integration

```bash
#!/bin/bash
set -euo pipefail

source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

# Initialize
mcp_cache_init

# Use in script
tool_result=$(mcp_tool_with_timeout "tool_name" "5000" "params")
echo "Tool output: $tool_result"

# Report stats before exit
mcp_get_cache_stats >> "report.txt"
```

### CI/CD Pipeline Integration

```yaml
# GitHub Actions example
- name: Run MCP with Caching
  run: |
    source scripts/claude-code-web-simulator/modules/mcp-performance.sh
    mcp_cache_init

    # Execute parallel tools
    mcp_parallel_tools 4 \
      "validate|spec.ttl|5000" \
      "generate|template.tera|10000" \
      "test|suite.feature|30000"

    # Generate performance report
    mcp_get_performance_report > performance.txt

  - name: Upload Performance Report
    uses: actions/upload-artifact@v2
    with:
      name: mcp-performance
      path: performance.txt
```

---

## Performance Metrics Reference

### Cache Efficiency

```
Hit Rate = (Hits / (Hits + Misses)) × 100

Optimal Range: 85-95%
- <70%: Consider increasing TTL
- >98%: May indicate overly long TTL (stale data risk)
```

### Tool Performance

```
Execution Time = Median execution across same parameters

SLO Compliance: Check .ggen/mcp-cache/performance.log
- Tools >5s: Investigate tool implementation
- Tools <500ms: Good baseline performance
```

---

## Migration & Deprecation

### Version 1.0.0 (Current)

- Initial release
- TTL-based caching
- Timeout enforcement
- Performance monitoring

### Future Enhancements

- Redis-based distributed caching
- Compression for large results
- Cache warming strategies
- Advanced metrics (percentiles, histograms)

---

## License

Part of the ggen project. See main LICENSE file.

---

## Support

For issues or questions:
1. Check troubleshooting section
2. Review `.ggen/mcp-cache/performance.log` for detailed diagnostics
3. Run `mcp_get_performance_report` for comprehensive analysis

---

**Last Updated**: January 2026
**Module Version**: 1.0.0
