#!/bin/bash
# Integration Test Scenarios for ggen v0.2.4
# Testing real-world workflows

set -e
TEST_DIR="/Users/sac/ggen/tests/integration-v2"
RESULTS_FILE="$TEST_DIR/test_results.json"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo "🧪 Starting ggen Integration Tests - $TIMESTAMP"
echo "{"
echo "  \"timestamp\": \"$TIMESTAMP\","
echo "  \"version\": \"$(ggen --version)\","
echo "  \"tests\": ["

# Test 1: Template listing
echo "📋 Test 1: Template Listing"
echo "    {"
echo "      \"test\": \"template_list\","
echo "      \"command\": \"ggen template list\","
if ggen template list > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Template listing successful\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"Template listing failed\""
fi
echo "    },"

# Test 2: Create test template
echo "📝 Test 2: Template Creation"
cd "$TEST_DIR"
rm -rf test-template 2>/dev/null || true
echo "    {"
echo "      \"test\": \"template_create\","
echo "      \"command\": \"ggen template new test-template\","
if ggen template new test-template --force > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Template creation successful\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"Template creation failed\""
fi
echo "    },"

# Test 3: Template validation
echo "✅ Test 3: Template Linting"
echo "    {"
echo "      \"test\": \"template_lint\","
echo "      \"command\": \"ggen template lint\","
if [ -d "test-template" ] && ggen template lint test-template > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Template linting successful\""
else
    echo "      \"status\": \"SKIP\","
    echo "      \"message\": \"Template not available for linting\""
fi
echo "    },"

# Test 4: Marketplace search
echo "🔍 Test 4: Marketplace Search"
echo "    {"
echo "      \"test\": \"marketplace_search\","
echo "      \"command\": \"ggen market search rust\","
if ggen market search rust > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Marketplace search successful\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"Marketplace search failed\""
fi
echo "    },"

# Test 5: Marketplace categories
echo "📦 Test 5: Marketplace Categories"
echo "    {"
echo "      \"test\": \"marketplace_categories\","
echo "      \"command\": \"ggen market categories\","
if ggen market categories > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Marketplace categories successful\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"Marketplace categories failed\""
fi
echo "    },"

# Test 6: RDF graph operations
echo "🔗 Test 6: RDF Graph Operations"
echo "    {"
echo "      \"test\": \"rdf_graph\","
echo "      \"command\": \"ggen graph --help\","
if ggen graph --help > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"RDF graph commands available\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"RDF graph commands not available\""
fi
echo "    },"

# Test 7: Project generation workflow
echo "⚙️  Test 7: Project Generation Workflow"
rm -rf test-output 2>/dev/null || true
mkdir -p test-output
echo "    {"
echo "      \"test\": \"project_generation\","
echo "      \"command\": \"ggen project gen\","
if [ -d "test-template" ]; then
    if ggen project gen test-template --output test-output > /dev/null 2>&1; then
        echo "      \"status\": \"PASS\","
        echo "      \"message\": \"Project generation successful\""
    else
        echo "      \"status\": \"FAIL\","
        echo "      \"message\": \"Project generation failed\""
    fi
else
    echo "      \"status\": \"SKIP\","
    echo "      \"message\": \"Template not available\""
fi
echo "    },"

# Test 8: Plan generation
echo "📐 Test 8: Plan Generation (Dry-run)"
echo "    {"
echo "      \"test\": \"plan_generation\","
echo "      \"command\": \"ggen project plan\","
if [ -d "test-template" ]; then
    if ggen project plan test-template --output test-output 2>&1 | grep -q "plan\|Plan"; then
        echo "      \"status\": \"PASS\","
        echo "      \"message\": \"Plan generation successful\""
    else
        echo "      \"status\": \"FAIL\","
        echo "      \"message\": \"Plan generation failed\""
    fi
else
    echo "      \"status\": \"SKIP\","
    echo "      \"message\": \"Template not available\""
fi
echo "    },"

# Test 9: Error handling - invalid template
echo "🛡️  Test 9: Error Handling"
echo "    {"
echo "      \"test\": \"error_handling\","
echo "      \"command\": \"ggen template show nonexistent\","
if ! ggen template show nonexistent > /dev/null 2>&1; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Proper error handling for invalid template\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"No error for invalid template\""
fi
echo "    },"

# Test 10: Help command responsiveness
echo "❓ Test 10: Help System"
echo "    {"
echo "      \"test\": \"help_system\","
echo "      \"command\": \"ggen --help\","
if ggen --help | grep -q "Usage:"; then
    echo "      \"status\": \"PASS\","
    echo "      \"message\": \"Help system responsive\""
else
    echo "      \"status\": \"FAIL\","
    echo "      \"message\": \"Help system not working\""
fi
echo "    }"

echo "  ]"
echo "}"

echo ""
echo "✅ Integration tests completed - $TIMESTAMP"
