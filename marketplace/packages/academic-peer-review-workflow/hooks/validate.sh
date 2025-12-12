#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"

require_file() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "missing required file: $path" >&2
    exit 1
  fi
}

require_stage() {
  local stage="$1"
  local workflow_file="$2"
  if ! grep -q "\"${stage}\"" "$workflow_file"; then
    echo "workflow is missing required stage: ${stage}" >&2
    exit 1
  fi
}

workflow_json="${root}/workflows/peer_review.workflow.json"
require_file "$workflow_json"
require_file "${root}/paper.rdf"
require_file "${root}/reviewers.json"

require_stage "reviewer-assignment" "$workflow_json"
require_stage "review-period" "$workflow_json"
require_stage "decision-compilation" "$workflow_json"

echo "validation ok: required files and stages present"
