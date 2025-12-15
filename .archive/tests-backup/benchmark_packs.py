#!/usr/bin/env python3
"""Performance benchmarking for ggen packs commands."""

import subprocess
import time
import statistics

def benchmark_command(cmd, runs=3):
    """Benchmark a command multiple times and return timing data."""
    times = []
    for i in range(runs):
        start = time.time()
        result = subprocess.run(cmd, shell=True, capture_output=True)
        elapsed = (time.time() - start) * 1000  # Convert to ms
        times.append(elapsed)
        if result.returncode != 0:
            print(f"  ⚠️  Run {i+1} failed with code {result.returncode}")

    return {
        'times': times,
        'avg': statistics.mean(times) if times else 0,
        'min': min(times) if times else 0,
        'max': max(times) if times else 0
    }

def main():
    commands = {
        'list': 'ggen packs list',
        'show': 'ggen packs show --pack_id startup-essentials',
        'validate': 'ggen packs validate --pack_id startup-essentials',
        'install': 'ggen packs install --pack_id startup-essentials --dry_run'
    }

    print("🔬 PACKS PERFORMANCE BENCHMARKING\n")
    print("=" * 80)

    results = {}
    for name, cmd in commands.items():
        print(f"\n📊 Benchmarking: {name}")
        print(f"   Command: {cmd}")
        result = benchmark_command(cmd)
        results[name] = result

        print(f"   Run 1: {result['times'][0]:.1f}ms")
        print(f"   Run 2: {result['times'][1]:.1f}ms")
        print(f"   Run 3: {result['times'][2]:.1f}ms")
        print(f"   Average: {result['avg']:.1f}ms")

    # Generate performance table
    print("\n" + "=" * 80)
    print("\n📈 PERFORMANCE SUMMARY\n")
    print("| Command  | Time 1 | Time 2 | Time 3 | Avg    | Target  | Status |")
    print("|----------|--------|--------|--------|--------|---------|--------|")

    for name, result in results.items():
        t1, t2, t3 = result['times']
        avg = result['avg']
        status = "✅" if avg < 100 else "❌"
        print(f"| {name:8} | {t1:5.0f}ms | {t2:5.0f}ms | {t3:5.0f}ms | {avg:5.0f}ms | < 100ms | {status:6} |")

    # Check for consistency
    print("\n🎯 CONSISTENCY CHECK\n")
    for name, result in results.items():
        variance = max(result['times']) - min(result['times'])
        print(f"   {name}: {variance:.1f}ms variance", end="")
        if variance > 50:
            print(" ⚠️  HIGH VARIANCE")
        else:
            print(" ✅")

    # Overall status
    print("\n" + "=" * 80)
    all_passed = all(r['avg'] < 100 for r in results.values())
    if all_passed:
        print("\n✅ ALL COMMANDS MEET < 100ms TARGET")
    else:
        print("\n❌ SOME COMMANDS EXCEED 100ms TARGET")
        for name, result in results.items():
            if result['avg'] >= 100:
                print(f"   - {name}: {result['avg']:.0f}ms (exceeds by {result['avg']-100:.0f}ms)")

if __name__ == '__main__':
    main()
