#!/bin/bash
# Pre-Tool Safety Check Hook
# Runs before file Write/Edit operations
# Purpose: Prevent dangerous operations, protect critical files

set -e

# Read stdin for tool input (JSON)
TOOL_INPUT=$(cat)

# Extract operation type and file path
OPERATION=$(echo "$TOOL_INPUT" | jq -r '.operation' 2>/dev/null || echo "unknown")
FILE_PATH=$(echo "$TOOL_INPUT" | jq -r '.file_path' 2>/dev/null || echo "")

# Protected files (never modify)
PROTECTED_PATTERNS=(
    ".git/*"
    ".github/*"
    "Cargo.lock"
    ".env"
    "secrets.json"
    "**/private_keys/*"
    ".anthropic/*"
)

# Critical files (ask before modify)
CRITICAL_PATTERNS=(
    "Makefile.toml"
    "Cargo.toml"
    ".specify/ontology/spec-kit-schema.ttl"
    ".specify/memory/constitution.ttl"
)

# Check if file matches protected patterns
for pattern in "${PROTECTED_PATTERNS[@]}"; do
    if [[ "$FILE_PATH" == "$pattern" || "$FILE_PATH" =~ $pattern ]]; then
        echo "✗ BLOCKED: Protected file - $FILE_PATH"
        exit 2  # Deny operation
    fi
done

# Warn about critical files
for pattern in "${CRITICAL_PATTERNS[@]}"; do
    if [[ "$FILE_PATH" == "$pattern" || "$FILE_PATH" =~ $pattern ]]; then
        echo "⚠ WARNING: Modifying critical file - $FILE_PATH"
        echo "  Verify this is intentional before proceeding"
    fi
done

# Check for path traversal attempts
if [[ "$FILE_PATH" == *".."* ]]; then
    echo "✗ BLOCKED: Path traversal attempt detected"
    exit 2
fi

# Allow operation
echo "✓ Safety check passed"
exit 0
