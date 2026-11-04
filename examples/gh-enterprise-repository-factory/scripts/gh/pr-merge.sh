#!/usr/bin/env bash
set -euo pipefail
# Usage: pr-merge.sh <pr-number>
pr="${1:?usage: pr-merge.sh <pr-number>}"
gh pr merge "$pr" --squash --delete-branch
