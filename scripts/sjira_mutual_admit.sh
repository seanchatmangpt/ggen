#!/usr/bin/env bash
# sjira_mutual_admit.sh — GAP19 / WP9 Elixir↔Rust golden mutual-admit gate.
#
# Proves one committed fixture admits on BOTH kernels:
#   Rust:   ggen_sjira::admit_work_order  (shared_fixture_admission_test)
#   Elixir: GgenIgniter.SemanticJira.admit_work_order/1  (sibling checkout)
#
# Usage:
#   scripts/sjira_mutual_admit.sh [auto|dry-run|strict]
#   just sjira-mutual-admit
#   just sjira-mutual-admit dry-run
#   just sjira-mutual-admit strict
#
# Modes:
#   auto    (default) — always run Rust; run Elixir when env present;
#                       if Elixir env missing, skip that leg with DRY-RUN and exit 0
#                       (hands-free gate must not rely on ad-hoc commands, but
#                       a ggen-only checkout is still a lawful soft pass).
#   dry-run — fixture + path preflight only; print the commands that would run.
#   strict  — fail closed if Elixir env (GGEN_IGNITER + mix) is missing.
#
# Env:
#   GGEN_IGNITER — absolute path to ggen_igniter checkout
#                  (default: $REPO/../ggen_igniter when that dir exists)
#   SJIRA_GOLDEN — override golden fixture path (tests only)
#
# Exit 0 = Rust admit OK and (Elixir admit OK | Elixir dry-run skip | dry-run mode)
# Exit 1 = fixture missing, Rust refuse, or Elixir present-and-refuses / strict miss
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
GOLDEN="${SJIRA_GOLDEN:-$REPO/crates/ggen-sjira/tests/fixtures/golden_admit.json}"
MODE="${1:-auto}"
case "$MODE" in
  auto|dry-run|strict) ;;
  *)
    echo "[MUTUAL-ADMIT-FAIL] unknown mode='$MODE' (want auto|dry-run|strict)" >&2
    exit 1
    ;;
esac

fail() { echo "[MUTUAL-ADMIT-FAIL] $*" >&2; exit 1; }
info() { echo "[MUTUAL-ADMIT] $*"; }

resolve_igniter() {
  # Only return a path that looks like a Mix project. A set-but-missing
  # GGEN_IGNITER is treated as absent so auto mode can dry-run-skip cleanly.
  if [[ -n "${GGEN_IGNITER:-}" ]]; then
    if [[ -f "${GGEN_IGNITER}/mix.exs" ]]; then
      (cd "$GGEN_IGNITER" && pwd)
      return
    fi
    printf ''
    return
  fi
  local sibling="$REPO/../ggen_igniter"
  if [[ -f "$sibling/mix.exs" ]]; then
    (cd "$sibling" && pwd)
    return
  fi
  printf ''
}

IGNITER="$(resolve_igniter)"
HAS_MIX=0
command -v mix >/dev/null 2>&1 && HAS_MIX=1
HAS_CARGO=0
command -v cargo >/dev/null 2>&1 && HAS_CARGO=1

[[ -f "$GOLDEN" ]] || fail "golden fixture missing: $GOLDEN"
info "golden=$GOLDEN"
info "mode=$MODE cargo=$HAS_CARGO mix=$HAS_MIX igniter=${IGNITER:-<missing>}"

if [[ "$MODE" == "dry-run" ]]; then
  info "DRY-RUN: would run:"
  echo "  (cd $REPO && cargo test -p ggen-sjira --test shared_fixture_admission_test -- --exact golden_json_admits_on_rust)"
  if [[ -n "$IGNITER" && "$HAS_MIX" -eq 1 ]]; then
    echo "  (cd $IGNITER && SJIRA_GOLDEN=$GOLDEN mix run -e '<admit_work_order golden; print ELIXIR_ADMIT_OK>')"
  else
    echo "  # Elixir leg skipped in live auto (igniter/mix missing)"
  fi
  info "DRY-RUN ok (preflight only)"
  exit 0
fi

# ── Rust leg (always required in auto/strict) ────────────────────────────────
[[ "$HAS_CARGO" -eq 1 ]] || fail "cargo not on PATH (cannot prove Rust admit)"
info "Rust: cargo test -p ggen-sjira --test shared_fixture_admission_test"
(
  cd "$REPO"
  cargo test -p ggen-sjira --test shared_fixture_admission_test -- --exact golden_json_admits_on_rust
) || fail "Rust shared_fixture_admission_test refused golden"
info "RUST_ADMIT_OK"

# ── Elixir leg ───────────────────────────────────────────────────────────────
if [[ -z "$IGNITER" || "$HAS_MIX" -eq 0 ]]; then
  if [[ "$MODE" == "strict" ]]; then
    fail "Elixir env missing (set GGEN_IGNITER to a checkout with mix on PATH); mode=strict"
  fi
  info "DRY-RUN: Elixir leg skipped (GGEN_IGNITER/mix missing) — Rust admit stands alone"
  info "MUTUAL_ADMIT_PARTIAL rust=ok elixir=dry-run-skip"
  exit 0
fi

info "Elixir: admit_work_order via mix run ($IGNITER)"

# Export absolute golden so the -e snippet does not depend on cwd-relative paths.
export SJIRA_GOLDEN="$GOLDEN"
ELIXIR_OUT="$(
  cd "$IGNITER"
  mix run -e '
path = System.fetch_env!("SJIRA_GOLDEN")
{:ok, wo} =
  path
  |> File.read!()
  |> Jason.decode!()
  |> GgenIgniter.SemanticJira.admit_work_order()
id = Map.fetch!(wo, "identity")
digest = Map.fetch!(wo, "definition_digest")
IO.puts("ELIXIR_ADMIT_OK " <> id <> " " <> digest)
'
)" || fail "Elixir admit_work_order refused golden (mix run failed)"

echo "$ELIXIR_OUT" | grep -q '^ELIXIR_ADMIT_OK ' || \
  fail "Elixir probe ran but did not print ELIXIR_ADMIT_OK: $ELIXIR_OUT"

echo "$ELIXIR_OUT" | grep '^ELIXIR_ADMIT_OK ' || true
info "MUTUAL_ADMIT_OK rust=ok elixir=ok"
exit 0
