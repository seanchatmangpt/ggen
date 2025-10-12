#!/bin/bash
# Production Readiness: Panic Point Detection
# Scans for .expect() and .unwrap() calls in production code

set -euo pipefail

echo "🔍 Scanning for panic points in production code..."
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Count panic points (excluding tests)
EXPECT_COUNT=0
UNWRAP_COUNT=0

if [ -d "cli/src" ]; then
    EXPECT_COUNT=$(grep -r "\.expect(" --include="*.rs" cli/src ggen-core/src ggen-ai/src 2>/dev/null | \
        grep -v "^tests/" | \
        grep -v "#\[cfg(test)\]" | \
        grep -v "// SAFE:" | \
        wc -l | tr -d ' ')

    UNWRAP_COUNT=$(grep -r "\.unwrap()" --include="*.rs" cli/src ggen-core/src ggen-ai/src 2>/dev/null | \
        grep -v "^tests/" | \
        grep -v "#\[cfg(test)\]" | \
        grep -v "// SAFE:" | \
        wc -l | tr -d ' ')
fi

echo "Results:"
echo "  .expect() calls: ${EXPECT_COUNT}"
echo "  .unwrap() calls: ${UNWRAP_COUNT}"
echo "  Total panic points: $((EXPECT_COUNT + UNWRAP_COUNT))"
echo ""

# Production threshold: 0 for expect, < 5 for unwrap with safety comments
if [ "$EXPECT_COUNT" -gt 0 ]; then
    echo -e "${RED}❌ FAIL: Found ${EXPECT_COUNT} .expect() calls in production code${NC}"
    echo ""
    echo "Top 10 offenders:"
    grep -rn "\.expect(" --include="*.rs" cli/src ggen-core/src ggen-ai/src 2>/dev/null | \
        grep -v "^tests/" | \
        grep -v "#\[cfg(test)\]" | \
        head -10
    exit 1
fi

if [ "$UNWRAP_COUNT" -gt 5 ]; then
    echo -e "${YELLOW}⚠️  WARNING: Found ${UNWRAP_COUNT} .unwrap() calls (threshold: 5)${NC}"
    echo ""
    echo "Top 10 offenders:"
    grep -rn "\.unwrap()" --include="*.rs" cli/src ggen-core/src ggen-ai/src 2>/dev/null | \
        grep -v "^tests/" | \
        grep -v "#\[cfg(test)\]" | \
        grep -v "// SAFE:" | \
        head -10
    exit 1
fi

echo -e "${GREEN}✅ PASS: Production code is panic-safe${NC}"
echo ""
echo "Guidelines:"
echo "  - Never use .expect() in production code"
echo "  - Use ? operator to propagate errors"
echo "  - Use .unwrap_or_default() or .unwrap_or_else() for safe fallbacks"
echo "  - Add '// SAFE: <reason>' comment for unavoidable .unwrap()"
exit 0
