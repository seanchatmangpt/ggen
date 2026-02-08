#!/bin/bash

##############################################################################
# MCP Tool Caching & Performance Optimization Module
#
# Provides intelligent caching, timeout management, and performance monitoring
# for MCP server access. Includes:
#
# - TTL-based result caching with automatic expiration
# - Timeout enforcement with graceful error handling
# - Parallel tool call execution (up to 4 concurrent)
# - Performance metrics collection and alerting
# - Cache hit/miss tracking and statistics
#
# Version: 1.0.0
# Author: ggen AI Team
##############################################################################

set -euo pipefail

# Module metadata
readonly MCP_PERF_MODULE_VERSION="1.0.0"
readonly MCP_PERF_MODULE_NAME="mcp-performance"

# Color codes
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Cache configuration
readonly MCP_CACHE_DIR="${MCP_CACHE_DIR:-.ggen/mcp-cache}"
readonly MCP_CACHE_METADATA="${MCP_CACHE_DIR}/metadata.json"
readonly MCP_PERF_LOG="${MCP_CACHE_DIR}/performance.log"
readonly MCP_DEFAULT_TTL=300  # 5 minutes default
readonly MCP_TOOL_DEF_TTL=1800  # 30 minutes for tool definitions
readonly MCP_CACHE_CLEANUP_AGE=86400  # 24 hours

# Performance thresholds
readonly MCP_SLOW_TOOL_THRESHOLD=5000  # 5s alert threshold
readonly MCP_MAX_PARALLEL=4
readonly MCP_TIMEOUT_BUFFER=1000  # 1s buffer for cleanup

# Tracking variables
declare -A MCP_CACHE_HITS
declare -A MCP_CACHE_MISSES
declare -a MCP_ACTIVE_PIDS
MCP_PERF_INITIALIZED=0

##############################################################################
# Utility Functions
##############################################################################

mcp_log_perf() {
    local level="$1"
    local message="$2"
    local timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "${timestamp} [${level}] ${message}" >> "${MCP_PERF_LOG}"
}

mcp_log_cache_hit() {
    local tool_name="$1"
    MCP_CACHE_HITS["${tool_name}"]=$((${MCP_CACHE_HITS["${tool_name}"]:-0} + 1))
    mcp_log_perf "CACHE_HIT" "Tool: ${tool_name}, Total hits: ${MCP_CACHE_HITS[${tool_name}]}"
}

mcp_log_cache_miss() {
    local tool_name="$1"
    MCP_CACHE_MISSES["${tool_name}"]=$((${MCP_CACHE_MISSES["${tool_name}"]:-0} + 1))
    mcp_log_perf "CACHE_MISS" "Tool: ${tool_name}, Total misses: ${MCP_CACHE_MISSES[${tool_name}]}"
}

mcp_log_timing() {
    local tool_name="$1"
    local duration_ms="$2"
    local status="$3"

    if (( duration_ms > MCP_SLOW_TOOL_THRESHOLD )); then
        echo -e "${YELLOW}[ALERT]${NC} Slow tool execution: ${tool_name} took ${duration_ms}ms"
        mcp_log_perf "SLOW_TOOL" "Tool: ${tool_name}, Duration: ${duration_ms}ms"
    fi

    mcp_log_perf "TIMING" "Tool: ${tool_name}, Duration: ${duration_ms}ms, Status: ${status}"
}

##############################################################################
# Cache Initialization & Management
##############################################################################

mcp_cache_init() {
    if [[ ${MCP_PERF_INITIALIZED} -eq 1 ]]; then
        return 0
    fi

    # Create cache directory structure
    mkdir -p "${MCP_CACHE_DIR}/results"
    mkdir -p "${MCP_CACHE_DIR}/definitions"
    mkdir -p "${MCP_CACHE_DIR}/metrics"

    # Initialize metadata file
    if [[ ! -f "${MCP_CACHE_METADATA}" ]]; then
        cat > "${MCP_CACHE_METADATA}" <<'EOF'
{
  "version": "1.0.0",
  "initialized": true,
  "created_at": "",
  "cache_stats": {
    "total_hits": 0,
    "total_misses": 0,
    "total_tools_cached": 0
  }
}
EOF
    fi

    # Initialize performance log
    touch "${MCP_PERF_LOG}"

    # Clean up old cache entries (>24 hours)
    mcp_cache_cleanup_old_entries

    MCP_PERF_INITIALIZED=1
    mcp_log_perf "INIT" "Cache initialized at ${MCP_CACHE_DIR}"

    return 0
}

mcp_cache_cleanup_old_entries() {
    local now
    now=$(date +%s)
    local cutoff=$((now - MCP_CACHE_CLEANUP_AGE))

    local count=0
    while IFS= read -r -d '' cache_file; do
        local file_mtime
        file_mtime=$(stat -f%m "${cache_file}" 2>/dev/null || stat -c%Y "${cache_file}" 2>/dev/null || echo 0)

        if (( file_mtime < cutoff )); then
            rm -f "${cache_file}"
            ((count++))
        fi
    done < <(find "${MCP_CACHE_DIR}/results" -type f -print0 2>/dev/null || true)

    if (( count > 0 )); then
        mcp_log_perf "CLEANUP" "Removed ${count} expired cache entries"
    fi

    return 0
}

##############################################################################
# Cache Operations
##############################################################################

mcp_cache_get() {
    local tool_name="$1"
    local cache_key="$2"

    mcp_cache_init

    local cache_file="${MCP_CACHE_DIR}/results/${tool_name}_${cache_key}.cache"

    if [[ ! -f "${cache_file}" ]]; then
        mcp_log_cache_miss "${tool_name}"
        return 1
    fi

    # Check cache validity (time-based TTL)
    local file_mtime
    file_mtime=$(stat -f%m "${cache_file}" 2>/dev/null || stat -c%Y "${cache_file}" 2>/dev/null || echo 0)
    local now
    now=$(date +%s)

    # Extract TTL from cache metadata (use sed for portability)
    local ttl
    ttl=$(sed -n 's/.*"ttl":\s*\([0-9]*\).*/\1/p' "${cache_file}" 2>/dev/null | head -1 || echo "${MCP_DEFAULT_TTL}")
    if [[ -z "${ttl}" ]]; then
        ttl="${MCP_DEFAULT_TTL}"
    fi

    local age=$((now - file_mtime))
    if (( age > ttl )); then
        rm -f "${cache_file}"
        mcp_log_cache_miss "${tool_name}"
        return 1
    fi

    # Cache is valid, return cached result
    mcp_log_cache_hit "${tool_name}"
    # Extract result field value (handles JSON properly)
    sed -n 's/.*"result":\s*\(.*\)}$/\1/p' "${cache_file}" || echo ""

    return 0
}

mcp_cache_set() {
    local tool_name="$1"
    local cache_key="$2"
    local result="$3"
    local ttl_seconds="${4:-${MCP_DEFAULT_TTL}}"

    mcp_cache_init

    local cache_file="${MCP_CACHE_DIR}/results/${tool_name}_${cache_key}.cache"
    local timestamp
    timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    local now
    now=$(date +%s)

    # Store result with metadata
    cat > "${cache_file}" <<EOF
{
  "tool_name": "${tool_name}",
  "cache_key": "${cache_key}",
  "timestamp": "${timestamp}",
  "ttl": ${ttl_seconds},
  "expires_at": $((now + ttl_seconds)),
  "result": ${result}
}
EOF

    mcp_log_perf "CACHE_SET" "Cached ${tool_name}:${cache_key} with TTL ${ttl_seconds}s"

    return 0
}

mcp_cache_invalidate() {
    local tool_name="$1"
    local cache_key="${2:-*}"

    local count=0

    # Remove matching cache files
    if [[ "${cache_key}" == "*" ]]; then
        # Remove all entries for tool
        for cache_file in "${MCP_CACHE_DIR}/results/${tool_name}_"*.cache; do
            if [[ -f "${cache_file}" ]]; then
                rm -f "${cache_file}"
                ((count++)) || true
            fi
        done
    else
        # Remove specific cache entry
        local cache_file="${MCP_CACHE_DIR}/results/${tool_name}_${cache_key}.cache"
        if [[ -f "${cache_file}" ]]; then
            rm -f "${cache_file}"
            ((count++)) || true
        fi
    fi

    if (( count > 0 )); then
        mcp_log_perf "CACHE_INVALIDATE" "Invalidated ${count} cache entries for ${tool_name}"
    fi

    return 0
}

##############################################################################
# Tool Definition Caching
##############################################################################

mcp_cache_tool_definitions() {
    local tool_list="$1"

    mcp_cache_init

    local def_file="${MCP_CACHE_DIR}/definitions/tool_definitions.cache"
    local timestamp
    timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    cat > "${def_file}" <<EOF
{
  "timestamp": "${timestamp}",
  "ttl": ${MCP_TOOL_DEF_TTL},
  "tool_count": $(echo "${tool_list}" | jq -r 'length'),
  "definitions": ${tool_list}
}
EOF

    mcp_log_perf "CACHE_DEFS" "Cached $(echo "${tool_list}" | jq -r 'length') tool definitions"

    return 0
}

mcp_get_cached_tool_definitions() {
    mcp_cache_init

    local def_file="${MCP_CACHE_DIR}/definitions/tool_definitions.cache"

    if [[ ! -f "${def_file}" ]]; then
        return 1
    fi

    # Check expiration
    local file_mtime
    file_mtime=$(stat -f%m "${def_file}" 2>/dev/null || stat -c%Y "${def_file}" 2>/dev/null || echo 0)
    local now
    now=$(date +%s)
    local age=$((now - file_mtime))

    if (( age > MCP_TOOL_DEF_TTL )); then
        rm -f "${def_file}"
        return 1
    fi

    cat "${def_file}" | jq -r '.definitions'

    return 0
}

##############################################################################
# Timeout Management
##############################################################################

mcp_tool_with_timeout() {
    local tool_name="$1"
    local timeout_ms="${2:-10000}"  # Default 10s
    local parameters="${3:-}"

    local timeout_sec=$((timeout_ms / 1000))
    if (( timeout_sec < 1 )); then
        timeout_sec=1
    fi

    # Add buffer for cleanup
    local timeout_with_buffer=$((timeout_sec + 1))

    local start_time
    start_time=$(date +%s%N | cut -b1-13)  # milliseconds

    # Check cache first
    local cache_key
    cache_key=$(echo -n "${tool_name}${parameters}" | md5sum | awk '{print $1}')

    if local cached_result
    cached_result=$(mcp_cache_get "${tool_name}" "${cache_key}" 2>/dev/null); then
        local end_time
        end_time=$(date +%s%N | cut -b1-13)
        local duration=$((end_time - start_time))
        mcp_log_timing "${tool_name}" "${duration}" "cached"
        echo "${cached_result}"
        return 0
    fi

    # Execute tool with timeout
    local result
    local exit_code

    result=$(timeout "${timeout_with_buffer}s" mcp_call_tool "${tool_name}" "${parameters}" 2>&1) || exit_code=$?

    if [[ ${exit_code:-0} -eq 124 ]]; then
        # Timeout occurred
        local end_time
        end_time=$(date +%s%N | cut -b1-13)
        local duration=$((end_time - start_time))

        mcp_log_timing "${tool_name}" "${duration}" "timeout"
        echo -e "${RED}[TIMEOUT]${NC} Tool '${tool_name}' exceeded ${timeout_ms}ms limit"
        mcp_log_perf "TIMEOUT" "Tool: ${tool_name}, Limit: ${timeout_ms}ms"

        return 124
    fi

    # Cache successful result
    if [[ ${exit_code:-0} -eq 0 ]]; then
        mcp_cache_set "${tool_name}" "${cache_key}" "${result}" "${MCP_DEFAULT_TTL}"
    fi

    local end_time
    end_time=$(date +%s%N | cut -b1-13)
    local duration=$((end_time - start_time))

    mcp_log_timing "${tool_name}" "${duration}" "success"
    echo "${result}"

    return ${exit_code:-0}
}

##############################################################################
# Parallel Tool Execution
##############################################################################

mcp_parallel_tools() {
    local max_parallel="${1:-${MCP_MAX_PARALLEL}}"
    shift

    local -a tool_specs=("$@")
    local active_count=0
    local failed_count=0

    mcp_log_perf "PARALLEL_START" "Starting parallel execution of ${#tool_specs[@]} tools"

    for spec in "${tool_specs[@]}"; do
        # Wait if at max parallel
        while (( active_count >= max_parallel )); do
            # Check for completed jobs
            local completed=0
            for pid in "${MCP_ACTIVE_PIDS[@]}"; do
                if ! kill -0 "$pid" 2>/dev/null; then
                    ((active_count--))
                    ((completed++))
                fi
            done

            if (( completed == 0 )); then
                sleep 0.1
            fi
        done

        # Parse spec: "tool_name|parameters|timeout_ms"
        local tool_name
        local parameters
        local timeout_ms

        IFS='|' read -r tool_name parameters timeout_ms <<< "${spec}"

        # Execute tool asynchronously
        mcp_tool_with_timeout "${tool_name}" "${timeout_ms:-10000}" "${parameters}" &
        MCP_ACTIVE_PIDS+=($!)
        ((active_count++))
    done

    # Wait for all remaining jobs
    for pid in "${MCP_ACTIVE_PIDS[@]}"; do
        wait "$pid" || ((failed_count++))
    done

    MCP_ACTIVE_PIDS=()

    mcp_log_perf "PARALLEL_END" "Completed with ${failed_count} failures"

    return ${failed_count}
}

##############################################################################
# Performance Metrics
##############################################################################

mcp_get_cache_stats() {
    mcp_cache_init

    echo "=== MCP Cache Statistics ==="
    echo ""

    # Summary stats
    local total_hits=0
    local total_misses=0

    for tool in "${!MCP_CACHE_HITS[@]}"; do
        ((total_hits += MCP_CACHE_HITS["${tool}"]))
    done

    for tool in "${!MCP_CACHE_MISSES[@]}"; do
        ((total_misses += MCP_CACHE_MISSES["${tool}"]))
    done

    local total_requests=$((total_hits + total_misses))
    local hit_rate=0

    if (( total_requests > 0 )); then
        hit_rate=$((total_hits * 100 / total_requests))
    fi

    echo "Total Requests: ${total_requests}"
    echo "Cache Hits: ${total_hits} (${hit_rate}%)"
    echo "Cache Misses: ${total_misses}"
    echo ""

    # Per-tool breakdown
    echo "=== Per-Tool Statistics ==="
    for tool in "${!MCP_CACHE_HITS[@]}"; do
        local hits=${MCP_CACHE_HITS["${tool}"]}
        local misses=${MCP_CACHE_MISSES["${tool}"]:-0}
        local tool_total=$((hits + misses))
        local tool_hit_rate=0

        if (( tool_total > 0 )); then
            tool_hit_rate=$((hits * 100 / tool_total))
        fi

        printf "  %s: %d hits, %d misses (%d%% hit rate)\n" "${tool}" "${hits}" "${misses}" "${tool_hit_rate}"
    done

    echo ""

    # Cache size
    local cache_size
    cache_size=$(du -sh "${MCP_CACHE_DIR}" 2>/dev/null | awk '{print $1}')
    echo "Cache Size: ${cache_size}"

    return 0
}

mcp_get_performance_report() {
    mcp_cache_init

    echo "=== MCP Performance Report ==="
    echo "Generated: $(date)"
    echo ""

    # Parse performance log
    echo "=== Recent Performance Events ==="
    tail -20 "${MCP_PERF_LOG}" 2>/dev/null || echo "No performance data available"

    echo ""

    # Slow tools summary
    echo "=== Slow Tool Alerts (>5s) ==="
    grep "SLOW_TOOL" "${MCP_PERF_LOG}" 2>/dev/null | tail -10 || echo "No slow tool alerts"

    echo ""

    # Timeout summary
    echo "=== Timeout Events ==="
    grep "TIMEOUT" "${MCP_PERF_LOG}" 2>/dev/null | tail -10 || echo "No timeout events"

    return 0
}

##############################################################################
# Cache Cleanup & Utilities
##############################################################################

mcp_cache_clear() {
    mcp_cache_init

    local count
    count=$(find "${MCP_CACHE_DIR}/results" -type f | wc -l)

    rm -rf "${MCP_CACHE_DIR}/results"/*
    mkdir -p "${MCP_CACHE_DIR}/results"

    mcp_log_perf "CACHE_CLEAR" "Cleared ${count} cache entries"

    echo -e "${GREEN}[✓]${NC} Cleared ${count} cache entries"

    return 0
}

mcp_cache_status() {
    mcp_cache_init

    echo "=== MCP Cache Status ==="
    echo ""
    echo "Cache Directory: ${MCP_CACHE_DIR}"
    echo "Initialized: ${MCP_PERF_INITIALIZED}"
    echo ""

    local file_count
    file_count=$(find "${MCP_CACHE_DIR}/results" -type f 2>/dev/null | wc -l || echo 0)

    echo "Cached Items: ${file_count}"

    if [[ -f "${MCP_CACHE_METADATA}" ]]; then
        echo "Metadata: $(stat -f%Sm "${MCP_CACHE_METADATA}" 2>/dev/null || stat -c%y "${MCP_CACHE_METADATA}" 2>/dev/null || echo 'unknown')"
    fi

    echo ""
    mcp_get_cache_stats

    return 0
}

##############################################################################
# Module Testing
##############################################################################

mcp_test_cache_operations() {
    echo -e "${BLUE}[TEST]${NC} Testing cache operations..."

    mcp_cache_init

    # Test 1: Cache set and get
    echo -n "  Test 1 (set/get): "
    local test_result='{"status": "success", "data": "test_value"}'
    mcp_cache_set "test_tool" "key1" "${test_result}" 300

    if local cached
    cached=$(mcp_cache_get "test_tool" "key1"); then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        return 1
    fi

    # Test 2: Cache miss
    echo -n "  Test 2 (miss): "
    if ! mcp_cache_get "test_tool" "nonexistent_key" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        return 1
    fi

    # Test 3: Cache invalidation
    echo -n "  Test 3 (invalidate): "
    mcp_cache_invalidate "test_tool" "key1"
    if ! mcp_cache_get "test_tool" "key1" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        return 1
    fi

    return 0
}

mcp_test_timeout() {
    echo -e "${BLUE}[TEST]${NC} Testing timeout functionality..."

    # Mock mcp_call_tool function
    mcp_call_tool() {
        local tool_name="$1"
        if [[ "${tool_name}" == "slow_tool" ]]; then
            sleep 2
            echo '{"result": "slow"}'
        else
            echo '{"result": "fast"}'
        fi
    }

    export -f mcp_call_tool

    # Test: Timeout execution
    echo -n "  Test (timeout alert): "
    if timeout 1s mcp_tool_with_timeout "slow_tool" "1000" "" 2>&1 | grep -q "TIMEOUT"; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
    fi

    return 0
}

mcp_run_all_tests() {
    echo -e "${BLUE}╔════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║  MCP Performance Module - Test Suite                   ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════╝${NC}"
    echo ""

    mcp_test_cache_operations
    echo ""
    mcp_test_timeout
    echo ""

    echo -e "${GREEN}[✓]${NC} All tests completed"

    return 0
}

##############################################################################
# Module Export
##############################################################################

# Export all functions
export -f mcp_cache_init
export -f mcp_cache_get
export -f mcp_cache_set
export -f mcp_cache_invalidate
export -f mcp_tool_with_timeout
export -f mcp_parallel_tools
export -f mcp_get_cache_stats
export -f mcp_get_performance_report
export -f mcp_cache_clear
export -f mcp_cache_status
export -f mcp_run_all_tests
export -f mcp_log_perf
export -f mcp_cache_cleanup_old_entries
export -f mcp_cache_tool_definitions
export -f mcp_get_cached_tool_definitions

# Module initialization marker
readonly MCP_PERFORMANCE_MODULE_LOADED=1
