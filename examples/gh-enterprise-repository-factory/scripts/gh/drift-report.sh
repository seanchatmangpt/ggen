#!/usr/bin/env bash
set -euo pipefail
# Usage: drift-report.sh [--dir <terraform-dir>] [--owner <o>] [--repo <name>] [--secret-value <v>]
dir="infra/terraform/github" ; owner="" ; repo="" ; secret=""
while [ $# -gt 0 ]; do
  case "$1" in
    --dir) dir="$2"; shift 2 ;;
    --owner) owner="$2"; shift 2 ;;
    --repo) repo="$2"; shift 2 ;;
    --secret-value) secret="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done
[ -d "$dir" ] || { echo "terraform dir not found: $dir" >&2; exit 2; }
cd "$dir"
vars=()
[ -n "$owner" ] && vars+=("-var=github_owner=${owner}")
[ -n "$repo" ] && vars+=("-var=repository=${repo}")
[ -n "$secret" ] && vars+=("-var=ggen_signing_key_value=${secret}")
plan_out=$(mktemp)
set +e
terraform plan -detailed-exitcode -no-color -input=false ${vars[@]+"${vars[@]}"} > "$plan_out" 2>&1
code=$?
set -e
case "$code" in
  0) andon="GREEN" ; drift="none" ;;
  2) andon="YELLOW" ; drift="detected" ;;
  *) andon="RED" ; drift="plan-error" ;;
esac
deletes=$(grep -c "will be destroyed" "$plan_out" || true)
if [ "${deletes:-0}" -gt 0 ]; then andon="YELLOW"; fi
if grep -qE "github_branch_protection.*destroyed|github_actions_secret.*destroyed|required_status_checks" "$plan_out" && [ "${deletes:-0}" -gt 0 ]; then
  andon="RED"
fi
creates=$(grep -c "will be created" "$plan_out" || true)
updates=$(grep -c "will be updated in-place" "$plan_out" || true)
replaces=$(grep -c "must be replaced" "$plan_out" || true)
echo '{"andon":"'"$andon"'","drift":"'"$drift"'","create":'"${creates:-0}"',"update":'"${updates:-0}"',"replace":'"${replaces:-0}"',"delete":'"${deletes:-0}"'}'
rm -f "$plan_out"
[ "$andon" = "GREEN" ]

