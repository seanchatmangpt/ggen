#!/usr/bin/env python3
"""
OTEL Overhead Calculation Script

Analyzes Criterion benchmark results to calculate OpenTelemetry instrumentation overhead.
Compares baseline (OTEL disabled) vs OTEL-enabled benchmarks.

Usage:
    python3 scripts/calculate_otel_overhead.py

Prerequisites:
    - Baseline benchmarks run with: cargo bench --no-default-features --features p2p -- --save-baseline otel-disabled
    - OTEL benchmarks run with: cargo bench --all-features -- --save-baseline otel-enabled
"""

import json
import sys
from pathlib import Path
from typing import Dict, Any

# Color codes for terminal output
class Colors:
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BLUE = '\033[94m'
    BOLD = '\033[1m'
    END = '\033[0m'

def load_criterion_estimates(baseline_name: str) -> Dict[str, Any]:
    """
    Load Criterion benchmark estimates from JSON files.

    Criterion stores results in:
    target/criterion/<benchmark_name>/<baseline_name>/estimates.json
    """
    criterion_dir = Path('target/criterion')
    estimates = {}

    if not criterion_dir.exists():
        print(f"{Colors.RED}Error: Criterion results directory not found: {criterion_dir}{Colors.END}")
        sys.exit(1)

    # Iterate through all benchmark directories
    for bench_dir in criterion_dir.iterdir():
        if not bench_dir.is_dir():
            continue

        # Skip the 'report' directory
        if bench_dir.name == 'report':
            continue

        baseline_dir = bench_dir / baseline_name
        estimates_file = baseline_dir / 'estimates.json'

        if estimates_file.exists():
            with open(estimates_file) as f:
                data = json.load(f)
                # Store the mean estimate (in nanoseconds)
                estimates[bench_dir.name] = data['mean']['point_estimate']

    return estimates

def calculate_overhead(baseline: Dict[str, float], otel_enabled: Dict[str, float]) -> Dict[str, Dict[str, Any]]:
    """
    Calculate percentage overhead for each benchmark.

    Overhead % = ((otel_time - baseline_time) / baseline_time) * 100
    """
    results = {}

    # Find benchmarks present in both datasets
    common_benchmarks = set(baseline.keys()) & set(otel_enabled.keys())

    if not common_benchmarks:
        print(f"{Colors.RED}Error: No common benchmarks found between baseline and OTEL runs{Colors.END}")
        print(f"Baseline benchmarks: {list(baseline.keys())}")
        print(f"OTEL benchmarks: {list(otel_enabled.keys())}")
        sys.exit(1)

    for benchmark_name in sorted(common_benchmarks):
        baseline_time_ns = baseline[benchmark_name]
        otel_time_ns = otel_enabled[benchmark_name]

        # Convert nanoseconds to milliseconds
        baseline_ms = baseline_time_ns / 1_000_000
        otel_ms = otel_time_ns / 1_000_000

        # Calculate overhead percentage
        overhead_pct = ((otel_time_ns - baseline_time_ns) / baseline_time_ns) * 100

        # Determine status
        if overhead_pct < 5:
            status = 'EXCELLENT'
        elif overhead_pct < 8:
            status = 'GOOD'
        elif overhead_pct < 10:
            status = 'ACCEPTABLE'
        elif overhead_pct < 15:
            status = 'WARNING'
        else:
            status = 'FAIL'

        results[benchmark_name] = {
            'baseline_ms': baseline_ms,
            'otel_ms': otel_ms,
            'overhead_ms': otel_ms - baseline_ms,
            'overhead_pct': overhead_pct,
            'status': status
        }

    return results

def print_results(results: Dict[str, Dict[str, Any]]):
    """
    Pretty-print overhead analysis results with color coding.
    """
    print(f"\n{Colors.BOLD}{'=' * 100}{Colors.END}")
    print(f"{Colors.BOLD}{Colors.BLUE}OpenTelemetry Overhead Analysis{Colors.END}")
    print(f"{Colors.BOLD}{'=' * 100}{Colors.END}\n")

    # Header
    print(f"{'Benchmark':<50} {'Baseline':<12} {'With OTEL':<12} {'Overhead':<15} {'Status':<12}")
    print(f"{'-' * 100}")

    # Sort by overhead percentage (highest first)
    sorted_results = sorted(results.items(), key=lambda x: x[1]['overhead_pct'], reverse=True)

    for benchmark_name, data in sorted_results:
        status = data['status']

        # Color based on status
        if status == 'EXCELLENT':
            status_color = Colors.GREEN
            status_emoji = '✅'
        elif status == 'GOOD':
            status_color = Colors.GREEN
            status_emoji = '✅'
        elif status == 'ACCEPTABLE':
            status_color = Colors.YELLOW
            status_emoji = '⚠️ '
        elif status == 'WARNING':
            status_color = Colors.YELLOW
            status_emoji = '⚠️ '
        else:  # FAIL
            status_color = Colors.RED
            status_emoji = '❌'

        # Truncate long benchmark names
        display_name = benchmark_name[:48] + '..' if len(benchmark_name) > 50 else benchmark_name

        print(f"{display_name:<50} "
              f"{data['baseline_ms']:>10.2f}ms "
              f"{data['otel_ms']:>10.2f}ms "
              f"{status_color}{data['overhead_pct']:>+13.1f}%{Colors.END} "
              f"{status_emoji} {status_color}{status:<10}{Colors.END}")

    print(f"\n{Colors.BOLD}{'=' * 100}{Colors.END}")

def print_summary(results: Dict[str, Dict[str, Any]]):
    """
    Print summary statistics.
    """
    total_benchmarks = len(results)
    overhead_values = [data['overhead_pct'] for data in results.values()]

    avg_overhead = sum(overhead_values) / len(overhead_values)
    max_overhead = max(overhead_values)
    min_overhead = min(overhead_values)

    # Count by status
    status_counts = {
        'EXCELLENT': 0,
        'GOOD': 0,
        'ACCEPTABLE': 0,
        'WARNING': 0,
        'FAIL': 0
    }

    for data in results.values():
        status_counts[data['status']] += 1

    print(f"\n{Colors.BOLD}Summary Statistics{Colors.END}")
    print(f"{'-' * 50}")
    print(f"Total Benchmarks:     {total_benchmarks}")
    print(f"Average Overhead:     {avg_overhead:+.1f}%")
    print(f"Maximum Overhead:     {max_overhead:+.1f}%")
    print(f"Minimum Overhead:     {min_overhead:+.1f}%")
    print()
    print(f"Status Distribution:")
    print(f"  ✅ Excellent (<5%):   {status_counts['EXCELLENT']} benchmarks")
    print(f"  ✅ Good (5-8%):       {status_counts['GOOD']} benchmarks")
    print(f"  ⚠️  Acceptable (8-10%): {status_counts['ACCEPTABLE']} benchmarks")
    print(f"  ⚠️  Warning (10-15%):  {status_counts['WARNING']} benchmarks")
    print(f"  ❌ Fail (>15%):       {status_counts['FAIL']} benchmarks")
    print()

    # Overall verdict
    if status_counts['FAIL'] > 0:
        print(f"{Colors.RED}{Colors.BOLD}❌ OVERALL STATUS: FAIL{Colors.END}")
        print(f"{Colors.RED}   {status_counts['FAIL']} benchmark(s) exceed 15% overhead threshold{Colors.END}")
        return False
    elif status_counts['WARNING'] > 0:
        print(f"{Colors.YELLOW}{Colors.BOLD}⚠️  OVERALL STATUS: WARNING{Colors.END}")
        print(f"{Colors.YELLOW}   {status_counts['WARNING']} benchmark(s) exceed 10% overhead threshold{Colors.END}")
        return False
    elif avg_overhead > 10:
        print(f"{Colors.YELLOW}{Colors.BOLD}⚠️  OVERALL STATUS: WARNING{Colors.END}")
        print(f"{Colors.YELLOW}   Average overhead ({avg_overhead:.1f}%) exceeds 10% threshold{Colors.END}")
        return False
    else:
        print(f"{Colors.GREEN}{Colors.BOLD}✅ OVERALL STATUS: PASS{Colors.END}")
        print(f"{Colors.GREEN}   All benchmarks meet <10% overhead requirement{Colors.END}")
        return True

def print_recommendations(results: Dict[str, Dict[str, Any]]):
    """
    Print optimization recommendations for high-overhead benchmarks.
    """
    high_overhead = {name: data for name, data in results.items() if data['overhead_pct'] > 10}

    if not high_overhead:
        return

    print(f"\n{Colors.BOLD}Optimization Recommendations{Colors.END}")
    print(f"{'-' * 50}")

    for benchmark_name, data in sorted(high_overhead.items(), key=lambda x: x[1]['overhead_pct'], reverse=True):
        print(f"\n{Colors.YELLOW}⚠️  {benchmark_name}{Colors.END}")
        print(f"   Overhead: {data['overhead_pct']:+.1f}% ({data['overhead_ms']:.2f}ms)")

        # Provide specific recommendations based on benchmark type
        if 'dht' in benchmark_name.lower():
            print("   Recommendations:")
            print("   - Reduce DHT operation sampling rate (currently 100%)")
            print("   - Skip tracing of internal DHT routing hops")
            print("   - Use #[tracing::instrument(skip)] on hot paths")
        elif 'search' in benchmark_name.lower():
            print("   Recommendations:")
            print("   - Sample search operations at 10% for metrics")
            print("   - Skip tracing of search result iterations")
            print("   - Batch metric exports (currently per-operation)")
        elif 'gossipsub' in benchmark_name.lower():
            print("   Recommendations:")
            print("   - Reduce gossipsub message tracing (high frequency)")
            print("   - Sample at 5% for message propagation metrics")
            print("   - Skip tracing of duplicate message handling")
        elif 'bootstrap' in benchmark_name.lower():
            print("   Recommendations:")
            print("   - Skip tracing of individual connection attempts")
            print("   - Use single span for entire bootstrap process")
            print("   - Reduce peer discovery event tracing")
        else:
            print("   Recommendations:")
            print("   - Profile with `cargo flamegraph` to identify hot paths")
            print("   - Reduce sampling rate for high-frequency operations")
            print("   - Skip tracing of internal implementation details")

def main():
    """
    Main execution flow.
    """
    print(f"{Colors.BOLD}Loading benchmark results...{Colors.END}")

    # Load baseline results (OTEL disabled)
    try:
        baseline = load_criterion_estimates('otel-disabled')
        print(f"✅ Loaded {len(baseline)} baseline benchmarks (OTEL disabled)")
    except Exception as e:
        print(f"{Colors.RED}Error loading baseline results: {e}{Colors.END}")
        print(f"\n{Colors.YELLOW}Did you run the baseline benchmarks?{Colors.END}")
        print(f"   cargo bench --no-default-features --features p2p -- --save-baseline otel-disabled")
        sys.exit(1)

    # Load OTEL-enabled results
    try:
        otel_enabled = load_criterion_estimates('otel-enabled')
        print(f"✅ Loaded {len(otel_enabled)} OTEL-enabled benchmarks")
    except Exception as e:
        print(f"{Colors.RED}Error loading OTEL-enabled results: {e}{Colors.END}")
        print(f"\n{Colors.YELLOW}Did you run the OTEL-enabled benchmarks?{Colors.END}")
        print(f"   cargo bench --all-features -- --save-baseline otel-enabled")
        sys.exit(1)

    # Calculate overhead
    results = calculate_overhead(baseline, otel_enabled)

    # Print results
    print_results(results)

    # Print summary
    passed = print_summary(results)

    # Print recommendations
    print_recommendations(results)

    # Exit with appropriate status code
    sys.exit(0 if passed else 1)

if __name__ == '__main__':
    main()
