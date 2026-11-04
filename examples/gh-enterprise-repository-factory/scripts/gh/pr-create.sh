#!/usr/bin/env bash
set -euo pipefail
# Usage: pr-create.sh [--title TITLE] [--body BODY]
gh pr create --fill "$@"
