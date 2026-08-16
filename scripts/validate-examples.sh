#!/usr/bin/env bash
# Validate every live ggen example against the current sync contract.
#
# Default mode is verification: run every live ggen.toml and then require the
# examples tree to remain byte-clean.  --write intentionally leaves regenerated
# projections in place for maintainers.
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="check"
TIMEOUT_SECONDS="${GGEN_EXAMPLE_TIMEOUT_SECONDS:-60}"

usage() {
    cat <<'EOF'
Usage: scripts/validate-examples.sh [--check|--write]

  --check  Run every live ggen.toml and fail if generation changes tracked or
           untracked files under examples/ (default).
  --write  Run every live ggen.toml and leave regenerated projections in place.

Environment:
  GGEN_BIN                       Path to the ggen executable.
  GGEN_EXAMPLE_TIMEOUT_SECONDS   Per-manifest timeout (default: 60).
EOF
}

case "${1:-}" in
    ""|"--check") MODE="check" ;;
    "--write") MODE="write" ;;
    "-h"|"--help") usage; exit 0 ;;
    *) echo "REFUSED: unknown argument: $1" >&2; usage >&2; exit 64 ;;
esac

if [[ -n "${2:-}" ]]; then
    echo "REFUSED: unexpected argument: $2" >&2
    usage >&2
    exit 64
fi

if ! [[ "$TIMEOUT_SECONDS" =~ ^[1-9][0-9]*$ ]]; then
    echo "REFUSED: GGEN_EXAMPLE_TIMEOUT_SECONDS must be a positive integer" >&2
    exit 64
fi

if [[ -n "${GGEN_BIN:-}" ]]; then
    if [[ ! -x "$GGEN_BIN" ]]; then
        echo "BLOCKED: GGEN_BIN is not executable: $GGEN_BIN" >&2
        exit 127
    fi
    GGEN_CMD=("$GGEN_BIN")
elif command -v ggen >/dev/null 2>&1; then
    GGEN_CMD=("$(command -v ggen)")
elif [[ -x "$ROOT_DIR/target/debug/ggen" ]]; then
    GGEN_CMD=("$ROOT_DIR/target/debug/ggen")
else
    cat >&2 <<'EOF'
BLOCKED: no ggen executable found.
Build the canonical CLI first:
  cargo build --locked -p ggen-cli-lib --bin ggen
Then rerun this script, or set GGEN_BIN to an executable path.
EOF
    exit 127
fi

if command -v timeout >/dev/null 2>&1; then
    TIMEOUT_CMD=(timeout "${TIMEOUT_SECONDS}s")
elif command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_CMD=(gtimeout "${TIMEOUT_SECONDS}s")
else
    echo "BLOCKED: timeout (or gtimeout) is required for bounded example execution" >&2
    exit 127
fi

is_excluded_top_level() {
    case "$1" in
        .ggen|_archive|archive|archive_2025|archive_ggen_core) return 0 ;;
        *) return 1 ;;
    esac
}

declare -a ACTIVE_DIRS=()
declare -a MANIFESTS=()
declare -a STRUCTURAL_FAILURES=()

while IFS= read -r -d '' dir; do
    name="$(basename "$dir")"
    if is_excluded_top_level "$name"; then
        continue
    fi

    ACTIVE_DIRS+=("$dir")
    found=0
    while IFS= read -r -d '' manifest; do
        MANIFESTS+=("$manifest")
        found=1
    done < <(find "$dir" -type f -name ggen.toml -print0 | sort -z)

    if [[ "$found" -eq 0 ]]; then
        STRUCTURAL_FAILURES+=("$dir: no ggen.toml")
    fi
done < <(find examples -mindepth 1 -maxdepth 1 -type d -print0 | sort -z)

if [[ "${#ACTIVE_DIRS[@]}" -eq 0 ]]; then
    echo "BUILD_BROKEN: no live example directories discovered" >&2
    exit 1
fi

if [[ "${#STRUCTURAL_FAILURES[@]}" -gt 0 ]]; then
    echo "BUILD_BROKEN: live example directories without a ggen.toml:" >&2
    printf '  - %s\n' "${STRUCTURAL_FAILURES[@]}" >&2
    exit 1
fi

if [[ "${#MANIFESTS[@]}" -eq 0 ]]; then
    echo "BUILD_BROKEN: no live ggen.toml manifests discovered" >&2
    exit 1
fi

echo "Observed ${#ACTIVE_DIRS[@]} live top-level example directories."
echo "Admitted ${#MANIFESTS[@]} ggen.toml manifests for execution."
echo "Mode: $MODE"
echo

failures=0
for manifest in "${MANIFESTS[@]}"; do
    example_dir="$(dirname "$manifest")"
    relative_manifest="${manifest#"$ROOT_DIR"/}"
    relative_manifest="${relative_manifest#./}"

    printf 'VERIFY %-70s ' "$relative_manifest"
    if (
        cd "$example_dir"
        "${TIMEOUT_CMD[@]}" "${GGEN_CMD[@]}" sync run >/dev/null
    ); then
        echo "ALIVE"
    else
        status=$?
        echo "BUILD_BROKEN(exit=$status)"
        failures=$((failures + 1))
    fi
done

if [[ "$failures" -ne 0 ]]; then
    echo
    echo "BUILD_BROKEN: $failures/${#MANIFESTS[@]} manifests failed ggen sync run." >&2
    exit 1
fi

if [[ "$MODE" == "check" ]]; then
    if ! git diff --quiet -- examples/; then
        echo
        echo "BUILD_BROKEN: ggen sync run changed tracked example projections:" >&2
        git status --short -- examples/ >&2
        exit 1
    fi

    untracked="$(git ls-files --others --exclude-standard -- examples/)"
    if [[ -n "$untracked" ]]; then
        echo
        echo "BUILD_BROKEN: ggen sync run created untracked example projections:" >&2
        printf '%s\n' "$untracked" >&2
        exit 1
    fi
else
    echo
    echo "WRITE mode complete. Review generated changes before committing."
    git status --short -- examples/ || true
fi

echo
echo "ALIVE: ${#MANIFESTS[@]}/${#MANIFESTS[@]} live manifests executed successfully."
