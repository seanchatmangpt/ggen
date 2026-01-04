#!/bin/bash
# User Prompt Validation Hook
# Runs when user submits prompts
# Purpose: Validate requests, ensure clarity, prevent ambiguous instructions

set -e

# Read stdin for prompt data
PROMPT_DATA=$(cat)

# Extract user prompt
PROMPT=$(echo "$PROMPT_DATA" | jq -r '.prompt' 2>/dev/null || echo "")
CONVERSATION_LENGTH=$(echo "$PROMPT_DATA" | jq -r '.conversation_length' 2>/dev/null || echo "0")

# Color codes
YELLOW='\033[1;33m'
NC='\033[0m'

# Check for vague instructions
VAGUE_WORDS=("maybe" "probably" "might" "could" "possibly" "sort of" "kind of" "seems like")

for word in "${VAGUE_WORDS[@]}"; do
    if [[ "$PROMPT" =~ $word ]]; then
        echo -e "${YELLOW}[HINT] Prompt contains vague language ('$word')${NC}"
        echo "Consider being more specific for clearer implementation"
    fi
done

# Check for missing context
if [[ ! "$PROMPT" =~ "crate" && ! "$PROMPT" =~ "module" && ! "$PROMPT" =~ "file" ]]; then
    if [[ "$PROMPT" =~ "implement\|create\|write\|add" ]]; then
        echo -e "${YELLOW}[HINT] Consider specifying which crate/module/file${NC}"
        echo "Example: 'In ggen-core, implement...'"
    fi
fi

# Check for long conversation (may need context reset)
if [ "$CONVERSATION_LENGTH" -gt 30 ]; then
    echo -e "${YELLOW}[HINT] Conversation length > 30 messages${NC}"
    echo "Consider using /compact to summarize context if noticing degraded quality"
fi

# Check for debugging without evidence
if [[ "$PROMPT" =~ "bug\|error\|fix\|broken" ]] && [[ ! "$PROMPT" =~ "test\|verify\|output\|error message" ]]; then
    echo -e "${YELLOW}[HINT] For bug reports, include error messages or test output${NC}"
    echo "This helps identify root causes faster"
fi

# Allow prompt to proceed
exit 0
