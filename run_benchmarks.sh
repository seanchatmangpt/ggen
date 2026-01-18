#!/bin/bash
set -e

echo "Running ggen 80/20 Benchmark Suite"
echo "===================================="
echo ""
echo "System Information:"
echo "  OS: $(uname -s)"
echo "  Kernel: $(uname -r)"
echo "  Date: $(date)"
echo ""
echo "Rust Information:"
rustc --version
cargo --version
echo ""

cd /home/user/ggen/crates/ggen-core

echo "Benchmark 1: Error Handling (focus on actual measurements)"
echo "============================================================"
cargo bench --bench error_handling_benchmarks -- --verbose 2>&1 | tee -a /tmp/bench_error.txt

echo ""
echo "Benchmark 2: Disk I/O Operations"
echo "================================"
cargo bench --bench disk_io_benchmarks -- --verbose 2>&1 | tee -a /tmp/bench_disk.txt

echo ""
echo "Benchmark 3: Configuration Loading"
echo "===================================="
cargo bench --bench config_loading_benchmarks -- --verbose 2>&1 | tee -a /tmp/bench_config.txt

echo ""
echo "Benchmark 4: Memory Stability"
echo "============================="
cargo bench --bench stability_benchmarks -- --verbose 2>&1 | tee -a /tmp/bench_stability.txt

echo ""
echo "All benchmarks completed. Results:"
ls -lh /tmp/bench_*.txt
