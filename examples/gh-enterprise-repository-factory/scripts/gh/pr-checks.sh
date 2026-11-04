#!/usr/bin/env bash
set -euo pipefail
# Usage: pr-checks.sh <pr-number>
pr="${1:?usage: pr-checks.sh <pr-number>}"
gh pr checks "$pr"
