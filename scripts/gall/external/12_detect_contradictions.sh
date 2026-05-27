#!/usr/bin/env bash
# Compliance script to detect event log contradictions and missing evaluations.
# Path: scripts/gall/external/12_detect_contradictions.sh
# Exit code: 0 if no contradictions, 1 if contradictions found.

set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

OCEL_FILE="${1:-crates/ggen-graph/audit/vision2030.self_audit.ocel.json}"

echo "=== Running Contradiction & Missing Evaluation Scanner ==="
echo "Target OCEL file: $OCEL_FILE"

if [ ! -f "$OCEL_FILE" ]; then
    echo "FAIL: OCEL self-audit log file not found at: $OCEL_FILE"
    exit 1
fi

VIOLATIONS=0

# Helper to run a jq check and increment violations if it outputs errors
run_check() {
    local check_name="$1"
    local jq_query="$2"
    echo -n "Running $check_name... "
    
    local results
    results=$(jq -r "$jq_query" "$OCEL_FILE")
    
    if [ -n "$results" ]; then
        echo "FAIL"
        echo "$results"
        local count
        count=$(echo "$results" | grep -c "^FAIL" || true)
        if [ "$count" -eq 0 ]; then
            count=1
        fi
        VIOLATIONS=$((VIOLATIONS + count))
    else
        echo "PASS"
    fi
}

# Check 1: Conflicting Decisions
run_check "Check 1: Conflicting Decisions" '
  . as $root
  | [ $root.events[] | select(.activity == "CheckpointPromoted") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $promoted
  | [ $root.events[] | select(.activity == "CheckpointRefused") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $refused
  | ($promoted | map(select(. as $x | $refused | index($x) != null)))
  | if length > 0 then "FAIL: Checkpoint " + join(", ") + " has both CheckpointPromoted and CheckpointRefused events" else empty end
'

# Check 2: Decision without Evaluation
run_check "Check 2: Decision without Evaluation" '
  . as $root
  | [ $root.events[] | select(.activity == "CheckpointPromoted" or .activity == "CheckpointRefused") ] as $adjs
  | [ $root.events[] | select(.activity == "CheckpointEvaluated") ] as $evals
  | $adjs[] | . as $adj
  | ($adj.objects[] | select(.type == "GALLCheckpoint") | .id) as $cp_id
  | [ $evals[] | select(.objects[] | select(.type == "GALLCheckpoint" and .id == $cp_id)) ] as $cp_evals
  | if ($cp_evals | length) == 0 then
      "FAIL: Checkpoint \($cp_id) adjudicated in event \($adj.id) but has no corresponding CheckpointEvaluated event"
    else
      [ $cp_evals[] | select(.timestamp < $adj.timestamp) ] as $prior_evals
      | if ($prior_evals | length) == 0 then
          "FAIL: Checkpoint \($cp_id) adjudicated in event \($adj.id) at \($adj.timestamp) but has no chronologically preceding CheckpointEvaluated event"
        else empty
        end
    end
'

# Check 3: Evaluation without Adjudication
run_check "Check 3: Evaluation without Adjudication" '
  . as $root
  | [ $root.events[] | select(.activity == "CheckpointEvaluated") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $evaluated
  | [ $root.events[] | select(.activity == "CheckpointPromoted" or .activity == "CheckpointRefused") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $adjudicated
  | ($evaluated | map(select(. as $x | $adjudicated | index($x) == null)))
  | if length > 0 then "FAIL: Checkpoint " + join(", ") + " was evaluated but has no promotion or refusal decision" else empty end
'

# Check 4: Declared but never Evaluated
run_check "Check 4: Declared but never Evaluated" '
  . as $root
  | [ $root.objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $declared
  | [ $root.events[] | select(.activity == "CheckpointEvaluated") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $evaluated
  | ($declared | map(select(. as $x | $evaluated | index($x) == null)))
  | if length > 0 then "FAIL: Declared checkpoint " + join(", ") + " has never been evaluated in the event log" else empty end
'

# Check 5: Promotion despite un-remediated failure
run_check "Check 5: Promotion despite un-remediated failure" '
  . as $root
  | $root.events[] | select(.activity == "CheckpointPromoted") | . as $promo
  | [ $root.events[] | select(.activity == "TestPassed" and .timestamp < $promo.timestamp) ] | last as $last_pass
  | [ $root.events[] | select(.activity == "TestFailed" and .timestamp < $promo.timestamp) ] | last as $last_fail
  | if ($last_fail != null) and ($last_pass == null or $last_fail.timestamp > $last_pass.timestamp) then
      "FAIL: Checkpoint promoted in event \($promo.id) despite preceding un-remediated test failure in event \($last_fail.id) at \($last_fail.timestamp)"
    else empty end
'

# Check 6: Redundant Decisions
run_check "Check 6: Redundant Decisions" '
  . as $root
  | [ $root.events[] | select(.activity == "CheckpointPromoted") | .objects[] | select(.type == "GALLCheckpoint") | .id ]
  | group_by(.) | map(select(length > 1)) | map(.[0])
  | if length > 0 then "FAIL: Multiple promotion decision events found for checkpoints: " + join(", ") else empty end
'

# Check 7: PromotionDecision ID reuse
run_check "Check 7: PromotionDecision ID reuse" '
  . as $root
  | [ $root.events[] | select(.activity == "CheckpointPromoted" or .activity == "CheckpointRefused") | {decision: (.objects[] | select(.type == "PromotionDecision").id), checkpoint: (.objects[] | select(.type == "GALLCheckpoint").id)} ]
  | group_by(.decision) | .[] | select(map(.checkpoint) | unique | length > 1)
  | if length > 0 then "FAIL: PromotionDecision ID " + .[0].decision + " is reused across multiple checkpoints: " + ([.[].checkpoint] | unique | join(", ")) else empty end
'

echo "=== Contradiction Scan Summary ==="
if [ "$VIOLATIONS" -eq 0 ]; then
    echo "PASS: Zero contradictions detected in OCEL log."
    exit 0
else
    echo "FAIL: $VIOLATIONS contradiction(s) found in OCEL log."
    exit 1
fi
