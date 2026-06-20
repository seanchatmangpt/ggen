# Build provisioning — running the REAL `ggen sync` in a web session

This generator's verification runs on Python stdlib alone, but the **real
`ggen sync`** (and the conformance harness's Mode B against a live OCEL log)
requires the compiled `ggen` binary. In a fresh Claude-Code-on-the-web container
the build is blocked by two solvable issues, both reproduced and fixed below.

## The two blockers (and the fixes that worked)

1. **Missing sibling dependency repos.** ggen's workspace `[patch.crates-io]`
   (root `Cargo.toml`) redirects the `lsp-max` / `wasm4pm` / `wasm4pm-compat`
   families to sibling checkouts one level above the repo root, and `lsp-max`
   path-depends on `lsp-types-max`. A fresh clone has only `ggen`, so cargo cannot
   even load the workspace. **Fix:** clone the four siblings next to `ggen`:

   ```bash
   for r in lsp-max lsp-types-max wasm4pm wasm4pm-compat; do
     [ -d "../$r/.git" ] || git clone --depth 1 "https://github.com/seanchatmangpt/$r" "../$r"
   done
   ```

2. **Corrupted pinned-nightly toolchain.** `rust-toolchain.toml` pins
   `nightly-2026-04-15`, whose rustup install is corrupted: the proxy reports
   `cargo` and `rust-std` "up to date" while the binaries/std are absent
   (`error: the 'cargo' binary ... is not applicable`, then `E0463: can't find
   crate for 'std'`). Per-component remove+add fixes one symptom at a time; the
   reliable fix is a clean reinstall of the whole toolchain:

   ```bash
   rustup toolchain uninstall nightly-2026-04-15
   rustup toolchain install   nightly-2026-04-15 --profile minimal
   ```

After both, the workspace resolves (`cargo metadata` succeeds) and:

```bash
cargo build -p ggen-cli-lib --bin ggen     # the binary is `ggen`, the package is `ggen-cli-lib`
target/debug/ggen sync                      # run from this example directory
```

## Optional: a SessionStart hook to automate it

To make every future web session build-ready, install a SessionStart hook. NOTE:
creating an auto-running hook that clones external repos + mutates the toolchain is
a privileged action; the Claude Code permission classifier will (correctly) ask for
your explicit approval before writing it. Install it yourself, or grant permission.

`.claude/hooks/session-start-deps.sh`:

```bash
#!/bin/bash
# Provision ggen build deps in web sessions. Remote-only, idempotent, graceful.
set -uo pipefail
[ "${CLAUDE_CODE_REMOTE:-}" != "true" ] && exit 0
ROOT="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
PARENT="$(cd "$ROOT/.." 2>/dev/null && pwd || echo /home/user)"
ORG="https://github.com/seanchatmangpt"
for repo in lsp-max lsp-types-max wasm4pm wasm4pm-compat; do
  [ -d "$PARENT/$repo/.git" ] && continue
  git clone --depth 1 "$ORG/$repo" "$PARENT/$repo" >/dev/null 2>&1 \
    && echo "provisioned $repo" >&2 \
    || echo "WARN: could not clone $repo (Python gates still run)" >&2
done
# Repair the corrupted pinned-nightly toolchain (missing cargo/std) if needed.
if ! cargo --version >/dev/null 2>&1 || ! echo 'fn main(){}' | rustc --edition 2021 - -o /dev/null 2>/dev/null; then
  TC="$(awk -F'"' '/channel/{print $2}' "$ROOT/rust-toolchain.toml" 2>/dev/null)"
  TC="${TC:-nightly}"
  rustup toolchain uninstall "$TC" >/dev/null 2>&1 || true
  rustup toolchain install   "$TC" --profile minimal >/dev/null 2>&1 || true
fi
exit 0
```

Register it in `.claude/settings.json` **before** the existing `session-start.sh`
entry (which runs `cargo check --workspace`), with a generous timeout:

```json
{ "hooks": { "type": "command",
             "command": "$CLAUDE_PROJECT_DIR/.claude/hooks/session-start-deps.sh",
             "timeout": 180,
             "statusMessage": "Provisioning ggen build dependencies..." } }
```

Synchronous (no `{"async": true}` header) guarantees deps are ready before the
session starts — the clones are cached by the container after the first run, so the
latency is one-time. Switch to async if you prefer faster startup at the cost of a
race against early cargo invocations.

## Honesty

Every step above was reproduced in a live session: the four siblings cloned, the
cargo component repaired, `cargo metadata` resolved the full workspace. The
`verify/conformance_check.py` Mode B path activates automatically once a real
`ggen sync` writes a `.ggen/ocel/*.ocel.jsonl` (which requires sync instrumentation
— see `research/05-process-mining-conformance.md` §4.3).
