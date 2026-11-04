#!/usr/bin/env bash
set -euo pipefail
# Usage: pr-view-mergeable.sh <pr-number>
pr="${1:?usage: pr-view-mergeable.sh <pr-number>}"
gh pr view "$pr" --json mergeable,mergeStateStatus

