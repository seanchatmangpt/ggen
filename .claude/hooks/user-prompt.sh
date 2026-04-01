#!/bin/bash
# Purpose: Detect doctrine violations in user prompts.
#          SELF-CERT, NARRATION, completion without evidence.

set +H
set -uo pipefail

PROMPT="${1:-}"

[ -z "$PROMPT" ] && exit 0

# SELF-CERT: "looks correct/good/right" without test/OTEL/verify keywords
if echo "$PROMPT" | grep -qiE '(looks?|seems?)\s+(correct|good|right|fine|solid|great)' && \
   ! echo "$PROMPT" | grep -qiE '(test|verify|validate|check|otel|span|cargo make)'; then
  echo "SELF-CERT detected: completion claim without mechanical verification" >&2
  echo "  Run: cargo make check && cargo make test && cargo make lint" >&2
fi

# NARRATION: "should work/have/be" in completion context
if echo "$PROMPT" | grep -qiE '(this|it|that|code|feature)\s+(should|would|probably|likely)\s+(work|be|have|pass)' && \
   ! echo "$PROMPT" | grep -qiE '(test|verify|cargo make)'; then
  echo "NARRATION detected: describing expected outcome without proof" >&2
  echo "  Write the test first. Run it. Show the output." >&2
fi

# Completion claims without evidence
if echo "$PROMPT" | grep -qiE '(done|complete|finished|ready|working)' && \
   ! echo "$PROMPT" | grep -qiE '(test|check|verify|validate|cargo make|receipt|proof)'; then
  echo "REMINDER: Definition of Done requires validation" >&2
  echo "  cargo make check -> lint -> test -> slo-check" >&2
fi

# Skip safety checks
if echo "$PROMPT" | grep -qiE '(skip|ignore|bypass|disable).*(test|check|lint|validation)'; then
  echo "BLOCKED: Request to skip safety checks" >&2
  echo "  All checks are required per CLAUDE.md" >&2
fi

# Direct cargo commands
if echo "$PROMPT" | grep -qE 'cargo\s+(check|test|build|clippy|fmt|run|bench)'; then
  echo "BLOCKED: Direct cargo command. Use: cargo make <target>" >&2
fi

# Spec/markdown confusion
if echo "$PROMPT" | grep -qiE '(spec|ontology|rdf|ttl).*\.(md|markdown)'; then
  echo "REMINDER: RDF (.ttl) is source of truth, .md is generated" >&2
  echo "  Edit .specify/*.ttl, then: cargo make speckit-render" >&2
fi

exit 0
