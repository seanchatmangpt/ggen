#!/usr/bin/env bash
set -euo pipefail
# Usage: accept-verify.sh --repo <owner/name>
repo=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] || { echo "usage: accept-verify.sh --repo <owner/name>" >&2; exit 2; }
labels=$(gh api "repos/${repo}/labels" --jq '.[].name')
echo "$labels" | grep -q "異常" || { echo "FAIL: label 異常 missing" >&2; exit 1; }
dbm=$(gh api "repos/${repo}" --jq '.delete_branch_on_merge')
[ "$dbm" = "true" ] || { echo "FAIL: delete_branch_on_merge=${dbm}" >&2; exit 1; }
ctx=$(gh api "repos/${repo}/branches/main/protection" --jq '.required_status_checks.contexts | length')
[ "${ctx:-0}" -ge 4 ] || { echo "FAIL: protection contexts=${ctx}" >&2; exit 1; }
labelcount=$(echo "$labels" | grep -c . || true)
echo '{"verify":"pass","repo":"'"$repo"'","labels":'"$labelcount"',"delete_branch_on_merge":true,"protection_contexts":'"$ctx"'}'
