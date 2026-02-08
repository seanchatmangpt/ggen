# MCP Performance Module - Quick Start Guide

## Installation (5 minutes)

### 1. Source the Module
```bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh
```

### 2. Initialize Cache
```bash
mcp_cache_init
# Creates:
# - .ggen/mcp-cache/results/ (cache storage)
# - .ggen/mcp-cache/definitions/ (tool defs)
# - .ggen/mcp-cache/performance.log (metrics)
```

---

## Common Tasks

### Cache a Tool Result
```bash
result='{"status":"success","data":"value"}'
mcp_cache_set "tool_name" "cache_key" "$result" 600  # 10 minute TTL
```

### Get Cached Result
```bash
result=$(mcp_cache_get "tool_name" "cache_key")
if [[ $? -eq 0 ]]; then
    echo "Cache hit: $result"
else
    echo "Cache miss"
fi
```

### Execute Tool with Timeout & Caching
```bash
# Auto-caches successful results
result=$(mcp_tool_with_timeout "search" "5000" "query=test")
```

### Run Tools in Parallel
```bash
# Execute 3 tools concurrently (max 2 at a time)
mcp_parallel_tools 2 \
    "search|query=test|5000" \
    "list|path=/data|3000" \
    "stat|file=config|2000"
```

### View Cache Statistics
```bash
mcp_get_cache_stats
# Shows: hits/misses per tool, hit rates, cache size
```

### View Performance Report
```bash
mcp_get_performance_report
# Shows: recent events, slow tools, timeouts
```

### Clear All Cache
```bash
mcp_cache_clear
# Removes all cached results
```

---

## Configuration

### Custom Cache Directory
```bash
export MCP_CACHE_DIR="/custom/path"
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh
```

### Adjust Performance Thresholds
Edit module constants (before sourcing):
- `MCP_DEFAULT_TTL=300` - Cache TTL (seconds)
- `MCP_SLOW_TOOL_THRESHOLD=5000` - Slow alert threshold (ms)
- `MCP_MAX_PARALLEL=4` - Max concurrent jobs

---

## Performance Characteristics

| Operation | Target | Notes |
|-----------|--------|-------|
| Cache hit | <10ms | In-memory |
| Cache set | <50ms | Disk I/O |
| Timeout | <1ms | Signal check |
| Parallel (4x) | <100ms | Job spawn |

---

## Troubleshooting

**Cache misses on valid entries?**
- Check TTL has not expired
- Verify `.ggen/mcp-cache/results/` exists and is writable
- Run `mcp_cache_clear` to reset

**Tools timing out?**
- Increase timeout: `mcp_tool_with_timeout "tool" "15000" "params"`
- Check system load: `top`
- Review logs: `tail -20 .ggen/mcp-cache/performance.log`

**Permission denied?**
- Create directory: `mkdir -p .ggen/mcp-cache/{results,definitions,metrics}`
- Fix permissions: `chmod 755 .ggen/mcp-cache`

---

## Examples

### Simple Integration
```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# First call - executes and caches
result=$(mcp_tool_with_timeout "search" "5000" "query=important" 2>&1)

# Second call - cache hit
cached=$(mcp_tool_with_timeout "search" "5000" "query=important" 2>&1)

mcp_get_cache_stats
```

### Parallel Processing
```bash
#!/bin/bash
source /scripts/claude-code-web-simulator/modules/mcp-performance.sh

mcp_cache_init

# Process 10 items in parallel (max 4)
for i in {1..10}; do
    specs+="search|query=item_${i}|5000 "
done

mcp_parallel_tools 4 $specs
echo "Completed with $? failures"
```

---

## For More Information

See full documentation: `docs/MCP_PERFORMANCE_MODULE.md`

Last Updated: January 2026
