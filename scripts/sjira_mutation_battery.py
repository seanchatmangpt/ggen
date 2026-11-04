#!/usr/bin/env python3
"""DS-7 mutation battery for crates/ggen-sjira (C05: mutation-testing the backlog).

For each acceptance criterion: BASELINE (pinned test passes on the canonical
tree) -> SHIM (minimal mutation) -> pinned test must FLIP (exit non-zero)
-> RESTORE (sha256-exact, fail-closed). Exits 0 iff every criterion flipped.

Disclosed deviation from the /tmp-sandbox charter: the repo is 93 GB with
workspace/path dependencies that do not resolve outside the tree, so shims
are applied IN PLACE on the canonical checkout with byte-exact restore,
mirroring smalt scripts/shim_court/lib.sh. git is never invoked.

Usage: python3 scripts/sjira_mutation_battery.py [--only ID ...]
"""
import hashlib
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CRATE = ROOT / "crates" / "ggen-sjira"

# (id, criterion, file, find, replace, test_filter, target_args)
MUTATIONS = [
    ("EXEC-C01", "ceiling law: only UNKNOWN -> PARTIAL_ALIVE writable; other promotions refuse standing_ceiling",
     "execute.rs",
     "if from == EXECUTION_FROM && to == EXECUTION_CEILING {",
     "if to == EXECUTION_CEILING {",
     "promotion_ceiling_is_typed", ["--test", "execute_test"]),

    ("EXEC-C02", "exhaustion: a no-progress pass refuses typed 'exhausted' instead of spinning",
     "execute.rs",
     "if !progressed {",
     "if false && !progressed {",
     "max_iterations_refuses_typed_exhaustion", ["--test", "execute_test"]),

    ("EXEC-C02c", "nearest-flip probe for C02: mutate the typed exhaustion reason itself",
     "execute.rs",
     '"exhausted",\n        "mu_on_O",',
     '"exhaustedx",\n        "mu_on_O",',
     "max_iterations_refuses_typed_exhaustion", ["--test", "execute_test"]),

    ("EXEC-C02b", "nearest-flip probe for C02: the pinned test DOES pin the iteration budget",
     "execute.rs",
     "for iteration in 1..=opts.max_iterations {",
     "for iteration in 1..=1 {",
     "max_iterations_refuses_typed_exhaustion", ["--test", "execute_test"]),

    ("EXEC-C03", "idempotent replay: an event already recorded is not a second execution",
     "execute.rs",
     "if appended.status == AppendStatus::Appended {",
     "if true {",
     "rerun_is_idempotent_already_recorded", ["--test", "execute_test"]),

    ("LEAS-C04", "multi-party law: >=2 signatures from >=2 DISTINCT signers",
     "lease.rs",
     "if lease.signatures.len() >= MIN_SIGNATURES && distinct.len() >= MIN_SIGNATURES {",
     "if lease.signatures.len() >= MIN_SIGNATURES {",
     "two_signatures_same_signer_refuses_insufficient", ["--test", "lease_epoch_test"]),

    ("LEAS-C05", "minimum-count law: fewer than 2 signature records refuse",
     "lease.rs",
     "if lease.signatures.len() >= MIN_SIGNATURES && distinct.len() >= MIN_SIGNATURES {",
     "if lease.signatures.len() >= 1 {",
     "one_signature_refuses_insufficient", ["--test", "lease_epoch_test"]),

    ("LEAS-C06", "non-monotonic law: lease over unchanged ontology digest refuses",
     "lease.rs",
     "if lease.ontology_digest == pack_ontology_digest {",
     "if false {",
     "unchanged_ontology_digest_refuses_non_monotonic", ["--test", "lease_epoch_test"]),

    ("LEAS-C07", "Ed25519 verification: tampered / unknown-signer records refuse SignatureInvalid",
     "lease.rs",
     "if !signature_record_ok(rec, &msg, public_keys) {",
     "if false && !signature_record_ok(rec, &msg, public_keys) {",
     "tampered_signature_refuses_invalid", ["--test", "lease_epoch_test"]),

    ("LEAS-C08", "signature binds every lease field: granted_at is inside the canonical bytes",
     "lease.rs",
     '        (\n            "granted_at".to_string(),\n            serde_json::to_string(&lease.granted_at).expect("str serializes"),\n        ),\n',
     "",
     "unsigned_field_change_breaks_verification", ["--test", "lease_epoch_test"]),

    ("LEAS-C08b", "nearest-flip probe for C08: same granted_at-out-of-canonical-bytes shim against the canonical-JSON pin",
     "lease.rs",
     '        (\n            "granted_at".to_string(),\n            serde_json::to_string(&lease.granted_at).expect("str serializes"),\n        ),\n',
     "",
     "canonical_json_is_sorted_key_compact_minus_signatures", ["--test", "lease_epoch_test"]),

    ("EPOC-C09", "watermark fail-closed: epoch candidate without a watermark refuses WatermarkUnavailable",
     "epoch.rs",
     "let Some(wm) = watermark else {\n        return Err(Refusal::WatermarkUnavailable);\n    };",
     "let Some(wm) = watermark else {\n        return Ok(());\n    };",
     "missing_watermark_fails_closed", ["--test", "lease_epoch_test"]),

    ("EPOC-C10", "legacy-edit refusal: plan touching a pre-watermark file without a fresh plan refuses",
     "epoch.rs",
     "return Err(Refusal::LegacyEdit);",
     "return Ok(());",
     "legacy_edit_refuses", ["--test", "lease_epoch_test"]),

    ("LEDG-C11", "tamper detection: fetch refuses when any event_digest fails to recompute",
     "ledger.rs",
     "verify(&events)?;",
     "let _ = verify(&events);",
     "dir_mode_tampered_event_refuses_on_fetch_and_append", ["--test", "ledger_test"]),

    ("LEDG-C12", "vc_concurrent law: equal clocks are NOT concurrent; unordered clocks are",
     "ledger.rs",
     "!vc_le(a, b) && !vc_le(b, a)",
     "vc_le(a, b) && vc_le(b, a)",
     "vc_concurrent_law", ["--test", "ledger_test"]),

    ("LEDG-C13", "epoch regression refused before any write (ledger epoch watermark fail-closed)",
     "ledger.rs",
     "if e < last {\n            return Err(LedgerError::EpochRegression);\n        }",
     "if false {\n            return Err(LedgerError::EpochRegression);\n        }",
     "epoch_regression_refused_before_any_write", ["--test", "ledger_test"]),

    ("RECP-C14", "receipts chain: a tampered body/hash is detected at any depth",
     "receipts.rs",
     "if recomputed != t.hash {",
     "if false && recomputed != t.hash {",
     "mid_chain_tamper_detected_at_any_depth", ["--test", "receipts_test"]),

    ("CTRL-C15", "authority fail-closed: unpinned/missing origin_authority refuses at Stage::Authority",
     "controller.rs",
     "if origin_authority.is_empty() || pin.is_none() {",
     "if false && origin_authority.is_empty() && pin.is_none() {",
     "controller_missing_origin_authority_field_refuses_fail_closed", ["--test", "controller_test"]),

    ("CTRL-C16", "critical-path gate: a protected surface inside the blast radius halts before admission",
     "controller.rs",
     "if !violations.is_empty() {",
     "if false && !violations.is_empty() {",
     "controller_critical_path_refusal_halts_before_sjira_admission", ["--test", "controller_test"]),

    ("PROM-C17", "courts-satisfied: every required court needs a passing witness",
     "promote.rs",
     "if !passed {\n            return Err(PromoteRefusal::UnwitnessedCourt { court: court.clone() });",
     "if false {\n            return Err(PromoteRefusal::UnwitnessedCourt { court: court.clone() });",
     "unwitnessed_court_refuses", ["--lib"]),

    ("DESC-C18", "blocked-order refusal: a blocked order refuses NotOnFrontier with its reason",
     "descriptor.rs",
     "return Err(DescriptorRefusal::NotOnFrontier(b.reason.clone()));",
     "return Ok((false, String::new()));",
     "blocked_order_refuses_not_on_frontier_with_reason", ["--test", "descriptor_test"]),
]


def sha(b):
    return hashlib.sha256(b).hexdigest()


def run(cmd, **kw):
    return subprocess.run(cmd, capture_output=True, text=True, **kw)


def test_cmd(mut):
    _id, _crit, _f, _fi, _re, filt, target = mut
    return ["cargo", "test", "-p", "ggen-sjira", filt, *target]


def main():
    args = sys.argv[1:]
    only = set()
    if args and args[0] == "--only":
        only = set(args[1:])
    quiet = "-q" in args

    results = []
    all_flipped = True
    for mut in MUTATIONS:
        mid, crit, fname, find, repl, filt, target = mut
        if only and mid not in only:
            continue
        path = CRATE / "src" / fname
        orig = path.read_bytes()
        orig_sha = sha(orig)
        text = orig.decode()
        if text.count(find) < 1:
            print(f"[UNAPPLICABLE] {mid}: shim target not found in {fname}")
            all_flipped = False
            results.append((mid, "UNAPPLICABLE", "-"))
            continue

        mutated = text.replace(find, repl, 1)

        try:
            # BASELINE: canonical tree passes the pinned test (pristine bytes).
            base = run(["cargo", "test", "-p", "ggen-sjira", filt, *target])
            base_ok = base.returncode == 0
            if "could not compile" in base.stderr:
                print(f"[COMPILE-ABORT] {mid}: baseline cannot compile "
                      f"(concurrent lane edit?) — not a flip, not a pass")
                results.append((mid, "COMPILE-ABORT", filt))
                all_flipped = False
                continue

            path.write_bytes(mutated.encode())
            shim = run(test_cmd(mut))
            if "could not compile" in shim.stderr:
                print(f"[COMPILE-ABORT] {mid}: shim cannot compile — not a flip")
                results.append((mid, "COMPILE-ABORT", filt))
                all_flipped = False
                continue
            flipped = shim.returncode != 0

            results.append((mid, "FLIPPED" if flipped else "VACUOUS", filt))
            if not (base_ok and flipped):
                all_flipped = False
                print(f"[{'VACUOUS' if base_ok and not flipped else 'FAIL'}] {mid} "
                      f"(baseline_ok={base_ok}, shim_exit={shim.returncode}) filter={filt}")
            else:
                if not quiet:
                    print(f"[FLIP] {mid} :: {filt} (baseline pass -> shim exit {shim.returncode})")
        finally:
            path.write_bytes(orig)
            if sha(path.read_bytes()) != orig_sha:
                print(f"[RESTORE-FAILED] {mid}: {fname} did not restore byte-exact")
                all_flipped = False

    print()
    for mid, status, filt in results:
        print(f"  {status:12s} {mid:10s} {filt}")
    print()
    print(f"criteria run: {len(results)}; gate: {'PASS (all flipped)' if all_flipped else 'FAIL'}")
    sys.exit(0 if all_flipped else 1)


if __name__ == "__main__":
    main()
