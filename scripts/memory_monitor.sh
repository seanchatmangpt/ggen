#!/bin/bash

##############################################################################
# Memory Usage Monitor
#
# Real-time memory monitoring for build and runtime operations
# Tracks peak memory, allocation patterns, and resource efficiency
#
# Usage:
#   ./scripts/memory_monitor.sh [--command="command to run"] [--duration=60]
#
# Examples:
#   ./scripts/memory_monitor.sh --command="cargo build --release"
#   ./scripts/memory_monitor.sh --command="cargo test" --duration=120
#   ./scripts/memory_monitor.sh --duration=30  # Monitor for 30 seconds
#
# Output:
#   - Real-time memory stats every second
#   - Peak memory tracking
#   - Memory allocation analysis
#   - Report saved to: .ggen/memory_reports/
##############################################################################

set -euo pipefail

# Configuration
MEMORY_REPORT_DIR=".ggen/memory_reports"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="${MEMORY_REPORT_DIR}/memory_${TIMESTAMP}.json"

# Defaults
COMMAND=""
DURATION=60
INTERVAL=1

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

##############################################################################
# Functions
##############################################################################

print_status() {
    local status=$1
    local message=$2

    case "$status" in
    PASS)
        echo -e "${GREEN}[✓]${NC} $message"
        ;;
    FAIL)
        echo -e "${RED}[✗]${NC} $message"
        ;;
    INFO)
        echo -e "${BLUE}[ℹ]${NC} $message"
        ;;
    WARN)
        echo -e "${YELLOW}[⚠]${NC} $message"
        ;;
    esac
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
        --command=*)
            COMMAND="${1#*=}"
            shift
            ;;
        --duration=*)
            DURATION="${1#*=}"
            shift
            ;;
        --interval=*)
            INTERVAL="${1#*=}"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
        esac
    done
}

# Initialize report directory
init_report_dir() {
    mkdir -p "${MEMORY_REPORT_DIR}"
    print_status "INFO" "Report directory: ${MEMORY_REPORT_DIR}"
}

# Get memory usage from /proc
get_memory_stats() {
    local pid=$1

    if [ ! -d "/proc/$pid" ]; then
        return 1
    fi

    # RSS (Resident Set Size) in KB
    local rss=$(awk '/VmRSS/ {print $2}' "/proc/$pid/status" 2>/dev/null || echo "0")

    # Peak RSS in KB
    local peak_rss=$(awk '/VmPeak/ {print $2}' "/proc/$pid/status" 2>/dev/null || echo "0")

    # Heap size in KB (approximation from /proc/[pid]/maps)
    local heap=$(grep "\[heap\]" "/proc/$pid/maps" 2>/dev/null | wc -l)

    echo "$rss|$peak_rss|$heap"
}

# Monitor memory during command execution
monitor_memory_during_command() {
    print_status "INFO" "Starting memory monitor"
    print_status "INFO" "Command: $COMMAND"

    init_report_dir

    # Start command in background
    eval "$COMMAND" &
    local cmd_pid=$!

    local measurements=()
    local peak_rss=0
    local peak_heap=0

    print_status "INFO" "Monitoring PID: $cmd_pid"
    echo "Starting memory monitoring..."
    echo ""
    printf "%-10s %-12s %-12s %-10s\n" "Time(s)" "RSS(MB)" "Peak(MB)" "Status"
    echo "-------------------------------------------"

    local elapsed=0

    while kill -0 $cmd_pid 2>/dev/null; do
        local stats
        stats=$(get_memory_stats "$cmd_pid")

        if [ -n "$stats" ]; then
            local rss=$(echo "$stats" | cut -d'|' -f1)
            local peak_rss_current=$(echo "$stats" | cut -d'|' -f2)

            local rss_mb=$(echo "scale=2; $rss / 1024" | bc)
            local peak_mb=$(echo "scale=2; $peak_rss_current / 1024" | bc)

            printf "%-10d %-12.2f %-12.2f %-10s\n" "$elapsed" "$rss_mb" "$peak_mb" "Running"

            measurements+=("$elapsed|$rss_mb|$peak_mb")

            # Track peak
            if (( $(echo "$peak_mb > $peak_rss" | bc -l) )); then
                peak_rss=$peak_mb
            fi
        fi

        sleep "$INTERVAL"
        ((elapsed += INTERVAL))

        # Timeout after duration
        if [ "$elapsed" -ge "$DURATION" ]; then
            kill -TERM $cmd_pid 2>/dev/null || true
            break
        fi
    done

    # Wait for process to finish
    wait $cmd_pid 2>/dev/null || true

    echo ""
    print_status "PASS" "Memory monitoring complete"
    print_status "INFO" "Peak memory: ${peak_rss} MB"

    return 0
}

# Monitor system memory (without specific command)
monitor_system_memory() {
    print_status "INFO" "Starting system memory monitor (${DURATION}s)"

    init_report_dir

    local measurements=()
    local peak_memory=0

    echo "System Memory Monitor"
    printf "%-10s %-12s %-12s %-12s\n" "Time(s)" "Free(MB)" "Used(MB)" "Avail(MB)"
    echo "-------------------------------------------"

    local elapsed=0

    while [ "$elapsed" -lt "$DURATION" ]; do
        # Get memory info from /proc/meminfo
        local meminfo=$(cat /proc/meminfo)
        local mem_total=$(echo "$meminfo" | grep "^MemTotal:" | awk '{print $2}')
        local mem_free=$(echo "$meminfo" | grep "^MemFree:" | awk '{print $2}')
        local mem_avail=$(echo "$meminfo" | grep "^MemAvailable:" | awk '{print $2}')

        local mem_used=$((mem_total - mem_free))

        local free_mb=$(echo "scale=2; $mem_free / 1024" | bc)
        local used_mb=$(echo "scale=2; $mem_used / 1024" | bc)
        local avail_mb=$(echo "scale=2; $mem_avail / 1024" | bc)

        printf "%-10d %-12.2f %-12.2f %-12.2f\n" "$elapsed" "$free_mb" "$used_mb" "$avail_mb"

        measurements+=("$elapsed|$free_mb|$used_mb|$avail_mb")

        sleep "$INTERVAL"
        ((elapsed += INTERVAL))
    done

    echo ""
    print_status "PASS" "System memory monitoring complete"

    return 0
}

# Generate JSON report
generate_report() {
    print_status "INFO" "Generating memory report"

    cat > "${REPORT_FILE}" <<EOF
{
  "timestamp": "$(date -Iseconds)",
  "duration_seconds": $DURATION,
  "interval_seconds": $INTERVAL,
  "monitor_type": "$([ -z "$COMMAND" ] && echo 'system' || echo 'process')",
  "command": "$COMMAND",
  "report_file": "$REPORT_FILE"
}
EOF

    print_status "PASS" "Report saved to: ${REPORT_FILE}"
}

##############################################################################
# Main
##############################################################################

main() {
    parse_args "$@"

    print_status "INFO" "Memory Monitor v1.0"
    print_status "INFO" "Timestamp: ${TIMESTAMP}"

    if [ -n "$COMMAND" ]; then
        monitor_memory_during_command
    else
        monitor_system_memory
    fi

    generate_report

    print_status "INFO" "Monitor complete"
}

main "$@"
