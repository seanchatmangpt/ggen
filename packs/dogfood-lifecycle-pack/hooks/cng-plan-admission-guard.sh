#!/usr/bin/env bash
# cng-plan-admission-guard.sh — PreToolUse(Bash) guard wiring `cng plan
# check` (crates/cng/src/plan_approval.rs) as the real admission backend.
# Canonical copy; the live install lives in .claude/hooks/ (gitignored,
# see packs/dogfood-lifecycle-pack/README.md "Installation (local)").
#
# ============================================================================
# DESIGN DISCLOSURE — read before assuming this gates every Bash call
# ============================================================================
# `cng plan check` admits (exit 0) iff the proposed `--action` string is
# byte-exact equal to the single next unexecuted step of a plan already
# disclosed via `cng plan present` (see plan_approval.rs doc comments on
# plan_present/check_action/execute_approved_step). That models a
# structured PDDL8 plan under presented/approved-step discipline — it does
# NOT model "is this ad-hoc Bash command line safe/allowed", and nothing in
# today's interactive Claude Code lifecycle calls `cng plan present` to
# seed a plan at the start of an ordinary session. Building that
# integration (presenting a real plan for every session before it starts)
# is separate, larger work this script does not attempt.
#
# So the honest, smallest-lawful behavior implemented here is:
#
#   NO-OP BY DEFAULT. If this session has no plan ledger, this hook is a
#   pure passthrough (exit 0, always) — the overwhelmingly common case
#   today. The socket is wired and provably works end-to-end (see
#   VERIFIED LIVE below), but it is dormant until something deliberately
#   opts a session in. This is NOT yet default-on admission control for
#   arbitrary Bash.
#
#   REAL ADMISSION ONLY WHEN OPTED IN. If (and only if) a plan ledger file
#   exists for the CURRENT session at:
#     .cargo-cicd/lifecycle/session-<session_id>/plan-ledger/plan-ledger.jsonl
#   — the same `.cargo-cicd/lifecycle/session-<id>` naming convention
#   `dogfood-lifecycle-capture.sh` already uses for its per-session Turtle
#   capture log, just a sibling directory instead of a `.ttl` file — this
#   hook reads the most recent `"presented"` event's `plan_digest` straight
#   out of that ledger (no separate marker file needed; `cng plan present`
#   already durably records the digest in the ledger file it writes) and
#   calls `cng plan check --plan-digest <digest> --action <literal Bash
#   command text> --ledger-dir <that dir>` for real, enforcing its verdict
#   exactly as documented: exit 0 → allow, any nonzero → block. This guard
#   never writes to the ledger — read-only, same as `cng plan check` itself.
#
#   Opting a session in today is manual and deliberate — nothing does it
#   automatically yet (a named follow-up, not silently claimed as already
#   live):
#     cng plan present --dir <plan-dir> \
#       --ledger-dir /Users/sac/praxis/.cargo-cicd/lifecycle/session-<THIS SESSION's id>/plan-ledger
#
# ============================================================================
# EXIT-CODE DISCIPLINE — mirrors block-direct-cargo.sh / block-overclaim-commit.sh
# exactly; read both before touching this file
# ============================================================================
# 0 = ALLOW. 2 = BLOCK (stderr names why). Any OTHER exit code is treated by
# Claude Code as a HOOK FAILURE and FAILS OPEN (the tool call proceeds
# anyway) per this repo's documented hook semantics — so an internal error
# in THIS script (missing jq, missing/broken cng binary, unreadable or
# malformed ledger, unexpected cng output shape) must resolve to exit 0,
# never bubble up as some other exit code. `set -e` is deliberately NOT
# used at top level for exactly this reason: every risky step below is
# individually guarded so a failure there falls through to the fail-open
# path, and this script has exactly one `exit` per terminal branch, chosen
# explicitly — nothing here is allowed to end the script via an unguarded
# command's own raw exit status.
#
# A refusal FROM cng plan check itself (a real, opted-in ledger says the
# action is not the lawful next step) is different from an internal error
# IN this wrapper — the former is deliberately mapped to exit 2 (block),
# the latter is deliberately mapped to exit 0 (fail open), and the two
# paths never merge.
#
# ============================================================================
# VERIFIED LIVE (end-to-end, against a real presented plan) — see the
# installing session's report for the exact transcript. Summary: rendered
# crates/cng/tests/fixtures/pddl-pair.template.ttl into a scratch dir (a
# 2-action linear STRIPS plan, prep-widget(widget) -> ship-widget(widget)),
# ran `cng plan present` (exit 0, printed plan_digest + recorded it in
# plan-ledger.jsonl), then confirmed the three cng-level outcomes this
# script depends on: exact-next-step action -> cng exit 0; wrong action ->
# cng exit 1 with CNG_R31 + expected_next in stderr; unpresented digest ->
# cng exit 1 with CNG_R30 in stderr. Then ran THIS script against synthetic
# stdin payloads pointed at that same ledger and confirmed it maps: no
# ledger present -> exit 0 (passthrough); ledger present + matching action
# -> exit 0; ledger present + mismatched action -> exit 2 with the cng
# refusal text relayed on stderr.
# ============================================================================
set -uo pipefail

payload=$(cat 2>/dev/null)
[ -n "${payload:-}" ] || exit 0

command -v jq >/dev/null 2>&1 || exit 0

sid=$(printf '%s' "$payload" | jq -r '.session_id // empty' 2>/dev/null)
cmd=$(printf '%s' "$payload" | jq -r '.tool_input.command // empty' 2>/dev/null)
[ -n "${sid:-}" ] && [ -n "${cmd:-}" ] || exit 0

repo_root="/Users/sac/praxis"
ledger_dir="$repo_root/.cargo-cicd/lifecycle/session-${sid}/plan-ledger"
ledger_file="$ledger_dir/plan-ledger.jsonl"

# NO-OP BY DEFAULT: no ledger for this session -> nothing opted this
# session into plan-bound admission -> pure passthrough. This is the
# common case for every ordinary interactive session today.
[ -f "$ledger_file" ] || exit 0

# Most recent "presented" event's plan_digest. `-s` (slurp) treats the
# JSONL file as one array so `last` picks the newest record; this
# tolerates a ledger re-presented against a newer plan later in the same
# session. A malformed/foreign ledger yields empty here, which falls
# through to fail-open below rather than crashing this script.
plan_digest=$(jq -rs '[.[] | select(.event == "presented")] | last | .plan_digest // empty' "$ledger_file" 2>/dev/null)
[ -n "${plan_digest:-}" ] || exit 0

cng_bin="$HOME/.cargo/bin/cng"
[ -x "$cng_bin" ] || cng_bin=$(command -v cng 2>/dev/null || true)
[ -n "${cng_bin:-}" ] || exit 0

# Capture stderr only (the refusal message), discard stdout (--format
# quiet already suppresses it on the success path).
check_err=$("$cng_bin" plan check \
  --plan-digest "$plan_digest" \
  --action "$cmd" \
  --ledger-dir "$ledger_dir" \
  --format quiet 2>&1 1>/dev/null)
check_status=$?

[ "$check_status" -eq 0 ] && exit 0

echo "Blocked: 'cng plan check' refused this Bash command against the presented plan for this session." >&2
echo "  session_id:  $sid" >&2
echo "  plan_digest: $plan_digest" >&2
echo "  ledger_dir:  $ledger_dir" >&2
echo "  action:      $cmd" >&2
echo "$check_err" >&2
echo "This session has an active plan ledger (opted in via 'cng plan present'), so" >&2
echo "every Bash command must be the literal next unexecuted plan step." >&2
exit 2
