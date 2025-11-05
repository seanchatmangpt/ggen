#!/bin/bash
echo "=== Synthetic Data (4096 triples) ==="
echo "ASK averages:"
for i in {1..10}; do ./knhk_8tick_poc 2>&1 | grep "ASK" | awk '{print $3}'; done | awk '{sum+=$1; count++} END {printf "Average: %.3f ns/op (%.1f ticks @ 250 ps)\n", sum/count, (sum/count)/0.25}'

echo ""
echo "COUNT averages:"
for i in {1..10}; do ./knhk_8tick_poc 2>&1 | grep "COUNT" | awk '{print $3}'; done | awk '{sum+=$1; count++} END {printf "Average: %.3f ns/op (%.1f ticks @ 250 ps)\n", sum/count, (sum/count)/0.25}'

echo ""
echo "=== RDF File (3 triples) ==="
echo "ASK averages:"
for i in {1..10}; do ./knhk_8tick_poc test_rdf.ttl 2>&1 | grep "ASK" | awk '{print $3}'; done | awk '{sum+=$1; count++} END {printf "Average: %.3f ns/op (%.1f ticks @ 250 ps)\n", sum/count, (sum/count)/0.25}'

echo ""
echo "COUNT averages:"
for i in {1..10}; do ./knhk_8tick_poc test_rdf.ttl 2>&1 | grep "COUNT" | awk '{print $3}'; done | awk '{sum+=$1; count++} END {printf "Average: %.3f ns/op (%.1f ticks @ 250 ps)\n", sum/count, (sum/count)/0.25}'
