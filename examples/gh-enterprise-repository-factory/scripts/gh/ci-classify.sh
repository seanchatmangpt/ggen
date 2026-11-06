#!/usr/bin/env bash
set -euo pipefail
# Usage: ci-classify.sh --repo <owner/repo> --pr <n>
# 第二十九章 condition 9: mechanized step 2 of CI異常対応.
# Buckets: 製品異常 (product defect -> fix-product),
#          設備異常 (equipment: runner/network/timeout/... -> run-rerun.sh),
#          標準異常 (required check not wired/not reported -> drift-report.sh).
repo="" ; pr=""
while [ $# -gt 0 ]; do
  case "$1" in
    --repo) repo="$2"; shift 2 ;;
    --pr) pr="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$repo" ] && [ -n "$pr" ] || { echo "usage: ci-classify.sh --repo <owner/repo> --pr <n>" >&2; exit 2; }

checks_json="$(gh pr checks "$pr" --repo "$repo" --json name,state,bucket,link 2>/dev/null || true)"
if ! echo "$checks_json" | jq -e 'type == "array"' >/dev/null 2>&1; then
  plain="$(gh pr checks "$pr" --repo "$repo" 2>/dev/null || true)"
  checks_json="$(echo "$plain" | while IFS= read -r line; do
    [ -n "$line" ] || continue
    n="$(echo "$line" | cut -f1)"
    s="$(echo "$line" | cut -f2)"
    jq -cn --arg name "$n" --arg state "$s" --arg link "$line" '{name: $name, state: $state, bucket: $state, link: $link}'
  done | jq -cs '.')"
fi

results="$(echo "$checks_json" | jq -c '.[]' | while IFS= read -r c; do
  state="$(echo "$c" | jq -r '(.state // "") | ascii_upcase')"
  case "$state" in SUCCESS|PASS|NEUTRAL|SKIPPED|SKIPPING) continue ;; esac
  name="$(echo "$c" | jq -r '.name // ""')"
  required=0
  case "$name" in Check|Test|Build|Doctest|"CI Status") required=1 ;; esac
  bucket="製品異常" ; recommend="fix-product"
  if [ "$required" -eq 1 ] && { [ -z "$state" ] || [ "$state" = "MISSING" ] || [ "$state" = "NOT_REPORTED" ] || [ "$state" = "EXPECTED" ] || [ "$state" = "PENDING" ]; }; then
    bucket="標準異常" ; recommend="drift-report.sh"
  elif echo "$c" | grep -qiE 'runner|network|timeout|cancelled|infrastructure|rate.limit'; then
    bucket="設備異常" ; recommend="run-rerun.sh"
  fi
  jq -cn --arg check "$name" --arg bucket "$bucket" --arg recommend "$recommend" '{check: $check, bucket: $bucket, recommend: $recommend}'
done)"

if [ -n "$results" ]; then
  echo "$results"
fi
echo "$results" | jq -cs 'map(select(type == "object")) | {ci_classify: "done", counts: (group_by(.bucket) | map({(.[0].bucket): length}) | add // {})}'

