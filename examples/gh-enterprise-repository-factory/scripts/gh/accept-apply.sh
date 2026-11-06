#!/usr/bin/env bash
set -euo pipefail
# Usage: accept-apply.sh --dir <tf-dir> --owner <owner> --repo <name> [--secret-value <v>] [--approve-deletions]
dir="" ; owner="" ; repo="" ; secret="sandbox-dummy-value" ; approve_deletions=0
while [ $# -gt 0 ]; do
  case "$1" in
    --dir) dir="$2"; shift 2 ;;
    --owner) owner="$2"; shift 2 ;;
    --repo) repo="$2"; shift 2 ;;
    --secret-value) secret="$2"; shift 2 ;;
    --approve-deletions) approve_deletions=1; shift ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -n "$dir" ] && [ -n "$owner" ] && [ -n "$repo" ] || { echo "usage: accept-apply.sh --dir <tf-dir> --owner <owner> --repo <name> [--secret-value <v>] [--approve-deletions]" >&2; exit 2; }
cd "$dir"
planfile=$(mktemp)
set +e
terraform plan -detailed-exitcode -no-color -input=false "-var=github_owner=${owner}" "-var=repository=${repo}" "-var=ggen_signing_key_value=${secret}" >"$planfile" 2>&1
plan_rc=$?
set -e
if [ "$plan_rc" -ne 0 ] && [ "$plan_rc" -ne 2 ]; then
  cat "$planfile" >&2
  rm -f "$planfile"
  exit "$plan_rc"
fi
if grep -q "will be destroyed" "$planfile" && [ "$approve_deletions" -ne 1 ]; then
  echo "ANDON RED: terraform plan destroys resources; refusing apply without --approve-deletions (第二十六章第四段階 mechanized deletion approval)" >&2
  grep "will be destroyed" "$planfile" >&2
  rm -f "$planfile"
  exit 3
fi
rm -f "$planfile"
terraform apply -auto-approve -input=false "-var=github_owner=${owner}" "-var=repository=${repo}" "-var=ggen_signing_key_value=${secret}"

