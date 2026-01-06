#!/bin/bash
cd /home/user/ggen/crates/ggen-core

echo "=== ERROR HANDLING BENCHMARKS ==="
timeout 180 cargo bench --bench error_handling_benchmarks 2>&1 | grep -E "time:|    create_|allocate_|propagate_|result_|match_"

echo ""
echo "=== CONFIG LOADING BENCHMARKS ==="
timeout 180 cargo bench --bench config_loading_benchmarks 2>&1 | grep -E "time:|parse_|validate_"

echo ""
echo "=== DISK I/O BENCHMARKS ==="
timeout 180 cargo bench --bench disk_io_benchmarks 2>&1 | grep -E "time:|single_file|multiple_file|directory|permission"

echo ""
echo "=== STABILITY BENCHMARKS ==="
timeout 180 cargo bench --bench stability_benchmarks 2>&1 | grep -E "time:|memory|stability|cache|fairness"
