#!/bin/bash
results_file="test_results.txt"
echo "Starting example tests..." > "$results_file"

# Get list of example directories
examples=$(find examples -maxdepth 1 -type d -not -path "examples" -not -path "examples/_*" -not -path "examples/.*" | sort)

for ex in $examples; do
    echo "Testing $ex..."
    cd "$ex"
    if [ -f "ggen.toml" ]; then
        if /Users/sac/.local/bin/ggen sync > /dev/null 2>&1; then
            echo "$ex: PASSED" >> "../../$results_file"
        else
            echo "$ex: FAILED" >> "../../$results_file"
        fi
    else
        echo "$ex: SKIPPED (no ggen.toml)" >> "../../$results_file"
    fi
    cd - > /dev/null
done
echo "Testing complete. Results in $results_file"
