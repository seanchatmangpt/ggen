#!/usr/bin/env bash
# find-fakes.sh — detect Oracle Gaps in PRODUCTION Rust code (crates/*/src):
# fake-success macros, simulated work, deceptive stubs, and the string-pattern fakes
# clippy cannot see. The Rust-native half (todo!/unimplemented!/panic!/unwrap/expect)
# is owned by clippy — see `[workspace.lints.clippy]` in Cargo.toml (currently INERT:
# no member opts in via `[lints] workspace = true`) and `cargo make find-fakes-clippy`.
#
# Supersedes the STALE scripts/{check-no-panic-points,find-production-panic-points}.sh,
# which scan the pre-restructure layout (`cli/src`, `ggen-ai/src` — now `crates/*/src`,
# and ggen-ai no longer exists).
#
# Exit code: non-zero if any HIGH finding (fake-success macro in live, non-comment code).
set -uo pipefail
cd "$(dirname "$0")/.." || exit 2

SRC='crates/*/src'
FAIL=0
bold() { printf "\n\033[1m== %s ==\033[0m\n" "$1"; }
red()  { printf "\033[0;31m%s\033[0m\n" "$1"; }
grn()  { printf "\033[0;32m%s\033[0m\n" "$1"; }

# DORMANT crates: on-disk under crates/ but NOT Cargo.toml workspace members, so
# they DO NOT compile (see CLAUDE.md "Dormant" list). Findings inside them are not
# live-path and must not be severity-bearing. They are reported separately under
# "INFO — dormant (not compiled)" so they stay visible without being HIGH/MED.
DORMANT_RE='crates/(genesis-construct8|genesis-lockchain|genesis-wasm-shell|ggen-membrane|ggen-projection)/'

# grep production .rs, excluding test files/dirs AND dormant (non-compiled) crates.
# Returns "path:line:content". This is the severity-bearing scan over LIVE
# workspace-member production code only.
prod() {
  grep -rnE "$1" $SRC --include='*.rs' 2>/dev/null \
    | grep -vE '/tests?/|_test\.rs|#\[cfg\(test\)\]|#\[test\]|mod tests' \
    | grep -vE "$DORMANT_RE" || true
}
# Same filters as prod() but KEEPS ONLY dormant-crate hits, for the INFO section.
dormant() {
  grep -rnE "$1" $SRC --include='*.rs' 2>/dev/null \
    | grep -vE '/tests?/|_test\.rs|#\[cfg\(test\)\]|#\[test\]|mod tests' \
    | grep -E "$DORMANT_RE" || true
}
# drop comment/doc lines (content after `path:N:` begins with // or ///), so doctest
# placeholders like `/// unimplemented!()` are not flagged as real gaps.
no_comments() { grep -vE ':[0-9]+:[[:space:]]*//'; }
# drop string-literal mentions: a real `todo!(`/`unimplemented!(` call never has a `"`
# before it on the line, but a printed message (e.g. println!("...unimplemented!()...")) always does.
no_strings() { grep -vE '".*(todo|unimplemented)!\('; }

bold "HIGH — fake-success macros in live production code (todo!/unimplemented!)"
HITS=$(prod 'todo!\(|unimplemented!\(' | no_comments | no_strings)
if [ -n "$HITS" ]; then echo "$HITS"; FAIL=1; else echo "none"; fi

bold "MED — fabricated success / simulated work / fake receipts"
prod 'simulate|simulated|rcpt-simulated|"blake3:0{8}|signature:[[:space:]]*String::new\(\)|signature:[[:space:]]*vec!\[\]|"for now"|// *For now|// *for now' \
  | grep -ivE '// *simulate the' || echo "none"

bold "MED — deceptive 'Real'-named types (verify they are not mocks)"
prod 'struct Real[A-Za-z]+|enum Real[A-Za-z]+' || echo "none"

bold "LOW — stub/fake/placeholder/not-yet markers (review)"
prod 'placeholder|"not yet"|not yet implemented|// *(STUB|FAKE|PLACEHOLDER)|unimplemented stub' || echo "none"

bold "INFO — panic points (unwrap/expect) in production (clippy::unwrap_used/expect_used)"
N=$(prod '\.unwrap\(\)|\.expect\(' | no_comments | wc -l | tr -d ' ')
echo "$N occurrences. The clippy deny-set already covers these but is INERT (no member"
echo "crate has [lints] workspace = true). Inventory non-breaking: cargo make find-fakes-clippy"

bold "INFO — unreachable! (usually safe exhaustiveness, e.g. fmea.rs)"
prod 'unreachable!\(' | no_comments || echo "none"

bold "INFO — dormant (not compiled): hits in 5 non-member crates (no severity)"
echo "These directories are NOT Cargo.toml workspace members and do not compile."
echo "Reported for visibility only; they are excluded from HIGH/MED severity."
DORMANT_HITS=$(dormant 'todo!\(|unimplemented!\(|simulate|simulated|signature:[[:space:]]*String::new\(\)|signature:[[:space:]]*vec!\[\]|// *[Ff]or now|placeholder|not yet implemented' \
  | no_comments)
if [ -n "$DORMANT_HITS" ]; then echo "$DORMANT_HITS"; else echo "none"; fi

echo ""
if [ "$FAIL" -ne 0 ]; then
  red "❌ HIGH: fake-success macro(s) in live production code. Fail loud or implement."
  exit 1
fi
grn "✅ No HIGH fake-success macros in live production code."
