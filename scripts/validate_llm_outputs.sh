#!/bin/bash
# LLM Output Validation Script
# Runs comprehensive validation of all AI commands with real Ollama outputs

set -e

echo "🤖 LLM Output Validation Suite"
echo "================================"
echo ""
echo "This will test all AI commands with real Ollama and score outputs 0-10"
echo "Estimated time: 2-5 minutes"
echo ""

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/tags > /dev/null 2>&1; then
    echo "❌ Error: Ollama is not running"
    echo "Please start Ollama with: ollama serve"
    exit 1
fi

echo "✅ Ollama is running"
echo ""

# Create temp directory for outputs
TMP_DIR="/tmp/ggen_validation_$$"
mkdir -p "$TMP_DIR"

echo "📁 Output directory: $TMP_DIR"
echo ""

# Test scenarios
declare -a scenarios=(
    "generate:Create a simple hello world web page:500"
    "sparql:Find users over 18 who purchased in last 30 days:300"
    "frontmatter:Blog post about AI ethics with SEO:200"
    "graph:E-commerce product catalog:400"
)

total_score=0
test_count=0

for scenario in "${scenarios[@]}"; do
    IFS=: read -r command description max_tokens <<< "$scenario"
    test_count=$((test_count + 1))

    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "📋 Test $test_count: $command"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Description: $description"
    echo ""

    output_file="$TMP_DIR/${command}_output.txt"

    # Run command
    echo "⏳ Running command..."
    if cargo run --release -- ai $command \
        --description "$description" \
        --max-tokens "$max_tokens" \
        --output "$output_file" 2>&1 | grep -E "(Using|provider|Error)" | head -5; then

        if [ -f "$output_file" ]; then
            output_size=$(wc -c < "$output_file")
            echo "✅ Output generated ($output_size bytes)"

            # Simple scoring (0-10)
            score=8.0

            # Check for errors
            if grep -qi "error" "$output_file"; then
                score=3.0
                echo "⚠️  Output contains errors"
            fi

            # Check for empty
            if [ "$output_size" -lt 50 ]; then
                score=2.0
                echo "⚠️  Output too short"
            fi

            # Check for format-specific requirements
            case "$command" in
                sparql)
                    if ! grep -qi "SELECT" "$output_file" || ! grep -qi "WHERE" "$output_file"; then
                        score=4.0
                        echo "⚠️  Missing required SPARQL keywords"
                    fi
                    ;;
                graph)
                    if ! grep -q "@prefix" "$output_file"; then
                        score=4.0
                        echo "⚠️  Missing @prefix declarations"
                    fi
                    ;;
                frontmatter)
                    if ! grep -qi "title" "$output_file"; then
                        score=5.0
                        echo "⚠️  Missing title field"
                    fi
                    ;;
            esac

            echo ""
            echo "📊 Score: $score/10"
            total_score=$(echo "$total_score + $score" | bc)
        else
            echo "❌ Failed to generate output"
            score=0.0
        fi
    else
        echo "❌ Command failed"
        score=0.0
    fi

    echo ""
done

# Calculate average
avg_score=$(echo "scale=2; $total_score / $test_count" | bc)

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 FINAL RESULTS"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Average Score: $avg_score/10"
echo "Tests Run: $test_count"
echo ""

if (( $(echo "$avg_score >= 8.0" | bc -l) )); then
    echo "✅ PASS: Excellent quality"
    exit_code=0
elif (( $(echo "$avg_score >= 6.0" | bc -l) )); then
    echo "⚠️  PASS: Good quality with room for improvement"
    exit_code=0
else
    echo "❌ FAIL: Quality needs improvement"
    exit_code=1
fi

echo ""
echo "📄 Output files saved to: $TMP_DIR"
echo ""
echo "Recommendations:"
echo "  - Review outputs in $TMP_DIR"
echo "  - Adjust prompts for better results"
echo "  - Experiment with different max_tokens values"
echo "  - Check Ollama model performance"

exit $exit_code
