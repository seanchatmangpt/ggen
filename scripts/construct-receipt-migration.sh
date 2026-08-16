#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

subject="${MIGRATION_SUBJECT:-$(git rev-parse HEAD)}"
observed="$(git rev-parse HEAD)"
if [[ "$observed" != "$subject" ]]; then
  printf 'REFUSED: migration subject mismatch expected=%s observed=%s\n' "$subject" "$observed" >&2
  exit 2
fi

python3 - <<'PY'
from pathlib import Path

p = Path('crates/ggen-engine/src/sync.rs')
s = p.read_text()
old = '''        // CARGO_MANIFEST_DIR is crates/ggen-engine; workspace root is two levels up.
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(std::path::Path::parent)
            .expect("workspace root")
            .to_path_buf();'''
new = '''        // Default to the historical workspace-root migration target, but allow an
        // operator to bind the destructive migration to an exact admitted project root.
        // This preserves the existing F1 reseal algorithm while making it reusable for
        // project-scoped legacy chains such as examples/interview-sandbox.
        let root = std::env::var_os("GGEN_RECEIPT_MIGRATION_ROOT")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|| {
                std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .parent()
                    .and_then(std::path::Path::parent)
                    .expect("workspace root")
                    .to_path_buf()
            });'''
if old in s:
    s = s.replace(old, new, 1)
assert 'std::env::var_os("GGEN_RECEIPT_MIGRATION_ROOT")' in s
s = s.replace(
    '#[ignore = "one-time destructive migration against the real repo-root .ggen-v2/; run explicitly with --ignored"]',
    '#[ignore = "one-time destructive receipt migration; bind GGEN_RECEIPT_MIGRATION_ROOT for a project-scoped legacy chain"]',
    1,
)
p.write_text(s)
PY

# Bound formatting to the one Rust source this constructor modifies. The repository
# currently has unrelated rustfmt debt in other crates; that is not evidence about
# this migration and must not block its admission.
rustfmt --edition 2021 --check crates/ggen-engine/src/sync.rs
cargo build --locked -p ggen-cli-lib --bin ggen

example_root="$repo_root/examples/interview-sandbox"
ggen_bin="$repo_root/target/debug/ggen"

if (cd "$example_root" && "$ggen_bin" receipt history > /tmp/pre-history.out 2> /tmp/pre-history.err); then
  echo 'REFUSED: admitted legacy chain unexpectedly verified before F1 migration' >&2
  exit 3
fi
grep -Eq 'FM-CHAIN-(007|009|014)|chain hash mismatch' /tmp/pre-history.err

export GGEN_RECEIPT_MIGRATION_ROOT="$example_root"
cargo test -p ggen-engine --lib reseal_receipt_log_under_post_f1_chain_hash_formula -- --ignored --nocapture
(cd "$example_root" && "$ggen_bin" receipt history)
sha256sum \
  examples/interview-sandbox/.ggen-v2/receipt-log.jsonl \
  examples/interview-sandbox/.ggen-v2/receipt.json \
  > /tmp/migration-first.sha256

cargo test -p ggen-engine --lib reseal_receipt_log_under_post_f1_chain_hash_formula -- --ignored --nocapture
(cd "$example_root" && "$ggen_bin" receipt history)
sha256sum \
  examples/interview-sandbox/.ggen-v2/receipt-log.jsonl \
  examples/interview-sandbox/.ggen-v2/receipt.json \
  > /tmp/migration-second.sha256
diff -u /tmp/migration-first.sha256 /tmp/migration-second.sha256

git status --short > /tmp/migration-status.txt
python3 - <<'PY'
import subprocess

allowed = {
    'crates/ggen-engine/src/sync.rs',
    'examples/interview-sandbox/.ggen-v2/receipt-log.jsonl',
    'examples/interview-sandbox/.ggen-v2/receipt.json',
}
changed = set(subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines())
untracked = set(subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines())
unexpected = (changed | untracked) - allowed
if unexpected:
    raise SystemExit('REFUSED: unexpected migration fallout: ' + ', '.join(sorted(unexpected)))
missing = allowed - changed
if missing:
    raise SystemExit('REFUSED: expected migration change missing: ' + ', '.join(sorted(missing)))
PY

candidate=/tmp/live-examples-migration-candidate
rm -rf "$candidate"
mkdir -p "$candidate/files/crates/ggen-engine/src"
mkdir -p "$candidate/files/examples/interview-sandbox/.ggen-v2"
cp crates/ggen-engine/src/sync.rs "$candidate/files/crates/ggen-engine/src/sync.rs"
cp examples/interview-sandbox/.ggen-v2/receipt-log.jsonl "$candidate/files/examples/interview-sandbox/.ggen-v2/receipt-log.jsonl"
cp examples/interview-sandbox/.ggen-v2/receipt.json "$candidate/files/examples/interview-sandbox/.ggen-v2/receipt.json"
cp /tmp/migration-first.sha256 "$candidate/"
cp /tmp/migration-second.sha256 "$candidate/"
cp /tmp/migration-status.txt "$candidate/"
git diff --binary "$subject" -- \
  crates/ggen-engine/src/sync.rs \
  examples/interview-sandbox/.ggen-v2 \
  > "$candidate/migration.patch"
printf 'subject=%s\n' "$subject" > "$candidate/identity.txt"

printf 'MIGRATION_ALIVE subject=%s candidate=%s\n' "$subject" "$candidate"
