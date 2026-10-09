#!/usr/bin/env bash
# pre-push.sh - Full Tier Git Hook (ref-validating)
# Validates the PUSHED REF, not the shared working tree.
#
# Root cause (2026-10-09, lane R35): the previous version ran `just check` /
# `just lint` in the working tree. When the canonical checkout sits on a
# shared branch (e.g. spec-integration) carrying other lanes' lint debt,
# pushes of main could never pass while the tree was dirty with other
# branches' content. Fix: export the pushed commit via `git archive` to a
# scratch dir and run the same gates there. Gate strength identical
# (same commands, same clippy flags) — only WHAT is validated changed.

set -e
cd "$(git rev-parse --show-toplevel)"

# Only run validation when pushing to the default branch (main), and only
# for non-delete updates (local_sha of all zeros).
IS_DEFAULT_BRANCH=false
ZERO=0000000000000000000000000000000000000000
declare -a PUSHED_SHAS=()
while read local_ref local_sha remote_ref remote_sha; do
    if [[ "$remote_ref" == "refs/heads/main" && "$local_sha" != "$ZERO" ]]; then
        IS_DEFAULT_BRANCH=true
        PUSHED_SHAS+=("$local_sha")
    fi
done

if [ "$IS_DEFAULT_BRANCH" = false ]; then
    exit 0
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BOLD='\033[1m'
NC='\033[0m'

PASSED=0
FAILED=0

# Validate each pushed sha in a scratch export. (In practice one sha per
# push to main; multiple refs are validated sequentially.)
STATUS=0
for SHA in "${PUSHED_SHAS[@]}"; do
    # Persistent per-sha scratch (root-cause fix 2026-10-09, lane R68): the
    # previous mktemp+rm -rf scratch baked a DIFFERENT deleted directory into
    # env!("CARGO_MANIFEST_DIR") on every push; the per-sha cargo cache then
    # reused test binaries whose baked paths no longer existed, so
    # praxis-core's committed-receipt test failed NotFound on every reuse.
    # A stable, surviving path keeps baked paths valid across pushes. Gates
    # and commands are unchanged.
    SCRATCH="${XDG_CACHE_HOME:-$HOME/.cache}/ggen-pre-push/${SHA:0:12}-src"
    mkdir -p "$SCRATCH"
    find "$SCRATCH" -mindepth 1 -delete
    git archive "$SHA" | tar -x -C "$SCRATCH"

    # Persistent per-ref cargo target cache so the scratch build is not
    # always cold; keyed by sha prefix, never touches the shared tree's
    # target/ or working files.
    export CARGO_TARGET_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/ggen-pre-push/${SHA:0:12}"

    echo ""
    echo -e "${BOLD}Pre-Push Validation${NC} (Full Tier, ref ${SHA:0:12})"
    echo "  scratch: $SCRATCH"
    echo ""

    run_gate() {
        local name="$1"; shift
        echo -n "  ${name}... "
        if (cd "$SCRATCH" && "$@") >/dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            return 0
        fi
        echo -e "${RED}FAIL${NC}"
        echo ""
        (cd "$SCRATCH" && "$@") 2>&1 | head -40
        return 1
    }

    ok=0
    run_gate "[1/4] Check"      just check      || { ok=1; }
    run_gate "[2/4] Lint"       just lint       || { ok=1; }
    run_gate "[3/4] Format"     just fmt-check  || { ok=1; }
    run_gate "[4/4] Unit tests" just test-lib   || { ok=1; }


    if [ "$ok" != 0 ]; then
        echo ""
        echo -e "${RED}${BOLD}BLOCKED: pushed ref ${SHA:0:12} failed gates. Push refused.${NC}"
        STATUS=1
        break
    fi
done

if [ "$STATUS" = 0 ]; then
    echo ""
    echo -e "${GREEN}${BOLD}All gates passed on pushed ref. Push will proceed.${NC}"
fi
exit $STATUS
