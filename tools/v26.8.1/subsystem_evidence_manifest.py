#!/usr/bin/env python3
"""Subsystem evidence manifest generator for ggen v26.8.1.

Produces `.ggen/v26.8.1/subsystem-evidence-manifest.json`: one record per
subsystem (governance, system, engine, graph, projection, evidence,
products, verification, economics, legacy). This manifest is DELIBERATELY
NOT the authority for anything -- the external verifier
(`tools/v26.8.1/src/bin/subsystem_verifier.rs`) never trusts a single field
of it without independently re-deriving that field itself (re-hashing
files, re-running tests, re-reading `git rev-parse HEAD`). This generator's
only job is to point the verifier at real evidence and take a first-pass
recording of it; it is not, and must never become, the standing authority.

Critical constraint honoured here: every positive-witness and
negative-falsifier target -- `cargo test` (`run_cargo_test`) or a real
Python `unittest` invocation (`run_python_unittest`, used by `legacy`,
whose real evidence -- `equivalence_runner.py`, `legacy_archaeology.py` --
is Python, not Rust) -- is actually RUN as a subprocess. Pass/fail is read
from the real process exit code, never assumed. `verification`,
`economics`, and `legacy` gained dedicated test targets in the v26.8.1
G-close-unknowns pass (`crates/ggen-cheat-scanner/tests/
verification_subsystem_evidence_test.rs`, `crates/ggen-engine/tests/
economics_measured_evidence_test.rs`, `tools/v26.8.1/
legacy_subsystem_verification_test.py`); where a subsystem still has no
dedicated evidence, this generator emits an honestly empty/insufficient
record -- it does not fabricate coverage.

Usage:
    python3 tools/v26.8.1/subsystem_evidence_manifest.py [--root PATH] [--skip-tests]
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Sequence

SCHEMA = "ggen.v26.8.1.subsystem-evidence-manifest/1"
OUT_REL = Path(".ggen/v26.8.1/subsystem-evidence-manifest.json")
GENERATOR_REL = Path("tools/v26.8.1/subsystem_evidence_manifest.py")


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    h.update(path.read_bytes())
    return h.hexdigest()


def sha256_files(root: Path, rels: Sequence[str]) -> tuple[str, list[str]]:
    """Digest the concatenation of (relpath, contents) for every real file
    matched by `rels` (each a relpath or a glob relative to root). Returns
    (hex_digest, sorted_matched_relpaths). Missing globs contribute nothing
    -- callers should check the returned list length against expectations.
    """
    matched: list[Path] = []
    for pattern in rels:
        if any(ch in pattern for ch in "*?["):
            matched.extend(sorted(root.glob(pattern)))
        else:
            p = root / pattern
            if p.is_file():
                matched.append(p)
    matched = sorted({p.resolve() for p in matched if p.is_file()})
    h = hashlib.sha256()
    out_rels: list[str] = []
    for p in matched:
        relpath = str(p.relative_to(root))
        out_rels.append(relpath)
        h.update(relpath.encode("utf-8"))
        h.update(b"\0")
        h.update(p.read_bytes())
        h.update(b"\0")
    return h.hexdigest(), out_rels


def git_head(root: Path) -> str:
    completed = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=root, capture_output=True, text=True
    )
    if completed.returncode != 0:
        return "UNKNOWN"
    return completed.stdout.strip()


NEGATIVE_CONTROL_PATTERN = re.compile(
    r"refus|fail|reject|blocks?|detects?|tampered|corrupt|missing|escap|absent|wrong"
)


def is_true_negative_control(fn_name: str | None) -> bool:
    """Honest classifier, not an assertion: True only if the fn name itself
    signals a refusal/sabotage-detection test. If False, the fn used as the
    'negative_falsifier' slot is really just a second positive witness --
    the manifest records this explicitly rather than mislabeling it."""
    if not fn_name:
        return False
    return bool(NEGATIVE_CONTROL_PATTERN.search(fn_name))


@dataclass
class TestRun:
    crate: str
    test_target: str
    test_fn: str | None  # None => whole target
    argv: list[str]
    exit_code: int
    passed: bool
    elapsed_ms: int
    stdout_tail: str
    stderr_tail: str
    is_true_negative_control: bool = False


def run_cargo_test(
    root: Path, crate: str, test_target: str, test_fn: str | None, skip: bool
) -> TestRun:
    argv = ["cargo", "test", "-p", crate, "--test", test_target.removesuffix(".rs")]
    if test_fn:
        argv += ["--", test_fn, "--exact"]
    if skip:
        return TestRun(
            crate=crate,
            test_target=test_target,
            test_fn=test_fn,
            argv=argv,
            exit_code=-1,
            passed=False,
            elapsed_ms=0,
            stdout_tail="SKIPPED (--skip-tests)",
            stderr_tail="",
        )
    started = time.monotonic()
    completed = subprocess.run(
        argv, cwd=root, capture_output=True, text=True, timeout=600
    )
    elapsed_ms = int((time.monotonic() - started) * 1000)
    # cargo test's own pass/fail line, not our guess.
    passed = completed.returncode == 0 and "test result: FAILED" not in completed.stdout
    return TestRun(
        crate=crate,
        test_target=test_target,
        test_fn=test_fn,
        argv=argv,
        exit_code=completed.returncode,
        passed=passed,
        elapsed_ms=elapsed_ms,
        stdout_tail=completed.stdout[-2000:],
        stderr_tail=completed.stderr[-2000:],
        is_true_negative_control=is_true_negative_control(test_fn),
    )


def run_python_unittest(
    root: Path, script_rel: str, test_id: str | None, skip: bool
) -> TestRun:
    """Run a single unittest test id (e.g. `SomeTestCase.test_fn`) from a
    real python script via `python3 <script> <test_id> -v` -- the script's
    own `unittest.main()` under `if __name__ == '__main__'` parses argv test
    names directly, no separate test runner shim. `crate`/`test_target`
    fields are repurposed (no real cargo crate here) to keep the same
    TestRun shape the rest of this generator (and the report schema)
    already expects."""
    argv = ["python3", script_rel]
    if test_id:
        argv += [test_id, "-v"]
    if skip:
        return TestRun(
            crate="(python)",
            test_target=script_rel,
            test_fn=test_id,
            argv=argv,
            exit_code=-1,
            passed=False,
            elapsed_ms=0,
            stdout_tail="SKIPPED (--skip-tests)",
            stderr_tail="",
        )
    started = time.monotonic()
    completed = subprocess.run(
        argv, cwd=root, capture_output=True, text=True, timeout=600
    )
    elapsed_ms = int((time.monotonic() - started) * 1000)
    passed = completed.returncode == 0
    return TestRun(
        crate="(python)",
        test_target=script_rel,
        test_fn=test_id,
        argv=argv,
        exit_code=completed.returncode,
        passed=passed,
        elapsed_ms=elapsed_ms,
        stdout_tail=completed.stdout[-2000:],
        stderr_tail=completed.stderr[-2000:],
        is_true_negative_control=is_true_negative_control(test_id),
    )


@dataclass
class SubsystemRecord:
    subsystem: str
    authority_sources: list[str]
    authority_digest: str
    implementation_sources: list[str]
    implementation_digest: str
    positive_witness_reports: list[dict]
    negative_falsifier_reports: list[dict]
    replay_report: dict
    legacy_disposition_report: dict
    insufficient_evidence: bool
    insufficient_evidence_reason: str | None


# --- Per-subsystem declaration: which real files/tests constitute evidence ---
# Each entry: authority globs (docs/v26.8.1/NN-subsystem/*.md), implementation
# globs (crates/*/src/** or scripts/**), and (crate, test_target, positive_fn,
# negative_fn) tuples identifying real #[test] functions already read from
# the actual test files (see report for exact fn names).

SUBSYSTEMS: dict[str, dict] = {
    "governance": {
        "authority": ["docs/v26.8.1/00-governance/*.md"],
        "implementation": ["justfile", "AGENTS.md", "CLAUDE.md"],
        "tests": [
            (
                "ggen-config",
                "governance_precommit_gate_count_test.rs",
                "pre_commit_recipe_exists_and_is_non_empty",
                "no_governance_doc_hardcodes_a_pre_commit_gate_count",
            ),
        ],
    },
    "system": {
        "authority": ["docs/v26.8.1/10-system/*.md"],
        "implementation": ["Cargo.toml", ".specify/repo-facts.ttl"],
        "tests": [
            (
                "ggen-config",
                "system_crate_map_parity_test.rs",
                "repo_facts_ttl_crate_map_matches_cargo_toml_workspace_members",
                "cargo_toml_finds_real_workspace_crate_members",
            ),
        ],
    },
    "engine": {
        "authority": ["docs/v26.8.1/20-engine/*.md"],
        "implementation": ["crates/ggen-engine/src/sync.rs"],
        "tests": [
            (
                "ggen-engine",
                "pipeline_stage_evidence_test.rs",
                "successful_sync_populates_stage_specific_report_fields",
                "missing_ontology_file_refuses_closed_at_resolve_stage",
            ),
            (
                "ggen-engine",
                "manifest_diagnostic_codes_evidence_test.rs",
                "ordered_inline_select_under_strict_mode_syncs_cleanly",
                "unordered_inline_select_under_strict_mode_now_blocks_sync_run",
            ),
        ],
    },
    "graph": {
        "authority": ["docs/v26.8.1/30-graph/*.md"],
        "implementation": [
            "crates/ggen-graph/src/**/*.rs",
            "crates/praxis-graphlaw/src/**/*.rs",
        ],
        "tests": [
            (
                "ggen-graph",
                "graph_hashing_evidence_test.rs",
                "state_hash_is_stable_under_query_observation_and_sensitive_to_content",
                "process_intelligence_boundary_guard_detects_fabricated_violation",
            ),
        ],
    },
    "projection": {
        "authority": ["docs/v26.8.1/40-projection/*.md"],
        "implementation": ["crates/ggen-engine/src/sync.rs", "templates/**/*.tmpl"],
        "tests": [
            (
                "ggen-engine",
                "projection_determinism_test.rs",
                "identical_fixtures_replay_to_byte_identical_output_trees",
                "unless_exists_frontmatter_preserves_hand_edited_scaffold_file",
            ),
        ],
    },
    "evidence": {
        "authority": ["docs/v26.8.1/50-evidence/*.md"],
        "implementation": [
            "crates/ggen-engine/src/sync.rs",
            "crates/praxis-core/src/receipt_record.rs",
        ],
        "tests": [
            (
                "ggen-engine",
                "receipt_signing_evidence_test.rs",
                "second_sync_prev_chain_hash_equals_first_sync_chain_hash",
                "receipt_verify_fails_closed_on_tampered_payload_hash",
            ),
        ],
    },
    "products": {
        "authority": ["docs/v26.8.1/60-products/*.md"],
        "implementation": ["crates/ggen-cli/src/**/*.rs"],
        "tests": [
            (
                "ggen-cli-lib",
                "cli_surface_evidence_test.rs",
                "receipt_default_verb_and_explicit_verb_are_equivalent_at_the_binary_boundary",
                "sync_run_fails_closed_on_corrupt_manifest",
            ),
            (
                "ggen-cli-lib",
                "default_verb_law_test.rs",
                "sync_and_receipt_default_verbs_agree_with_the_live_lib_rs_mapping",
                "graph_and_unknown_nouns_are_not_rewritten_reproduced_against_generated_commands",
            ),
        ],
    },
    "verification": {
        "authority": ["docs/v26.8.1/70-verification/*.md"],
        "implementation": [
            "tools/v26.8.1/src/main.rs",
            "crates/ggen-cheat-scanner/src/lib.rs",
        ],
        "tests": [
            (
                "ggen-cheat-scanner",
                "verification_subsystem_evidence_test.rs",
                "verification_scanner_detects_a_freshly_planted_cheat_pattern",
                "verification_scanner_rejects_a_false_positive_on_clean_code",
            ),
        ],
    },
    "economics": {
        "authority": ["docs/v26.8.1/80-economics/*.md"],
        "implementation": ["justfile", "crates/ggen-engine/tests/receipt_chain_e2e.rs"],
        "tests": [
            (
                "ggen-engine",
                "economics_measured_evidence_test.rs",
                "economics_receipt_chain_wall_clock_measured_under_slo_threshold",
                "economics_measurement_rejects_a_fabricated_zero_duration_reading",
            ),
        ],
    },
    "legacy": {
        "authority": ["docs/v26.8.1/90-legacy/*.md"],
        "implementation": [
            "ontology/v26.8.1/legacy-capabilities.ttl",
            "tools/v26.8.1/equivalence_runner.py",
            "tools/v26.8.1/legacy_archaeology.py",
        ],
        "tests": [],
        # legacy's real evidence is Python (equivalence_runner.py,
        # legacy_archaeology.py are both Python), so this uses the same
        # (positive_fn, negative_fn) shape as "tests" but run via
        # run_python_unittest instead of run_cargo_test.
        "python_tests": [
            (
                "tools/v26.8.1/legacy_subsystem_verification_test.py",
                "LegacyCapabilitiesTtlIsRealTest.test_legacy_capabilities_ttl_is_real_reparseable_and_shacl_conformant",
                "EquivalenceRunnerCatchesFabricatedMismatchTest.test_equivalence_runner_detects_a_fabricated_stdout_mismatch",
            ),
        ],
    },
}


def legacy_disposition_report(root: Path) -> dict:
    """Real, non-fabricated count of LegacyCapability individuals, their
    hasDisposition/hasStanding values, and how many have a matching PASS
    result in the real VERIFIER_REPORT.json (matched by capabilityId ->
    case_id, hyphen-for-underscore -- a literal string transform, not a
    guess)."""
    ttl_path = root / "ontology" / "v26.8.1" / "legacy-capabilities.ttl"
    report_path = (
        root
        / "packs"
        / "legacy-equivalence-verifier-pack"
        / "consumer"
        / "legacy-equivalence"
        / "VERIFIER_REPORT.json"
    )
    text = ttl_path.read_text() if ttl_path.is_file() else ""
    ids = re.findall(r'ggen:capabilityId\s+"([^"]+)"', text)
    dispositions = re.findall(r"ggen:hasDisposition\s+ggen:(\w+)", text)
    subsystems = re.findall(r'ggen:owningSubsystem\s+"([^"]+)"', text)
    passed_case_ids: set[str] = set()
    if report_path.is_file():
        report = json.loads(report_path.read_text())
        for r in report.get("results", []):
            if r.get("status") == "PASS":
                passed_case_ids.add(r["case_id"])

    per_subsystem: dict[str, dict] = {}
    for cap_id, disp, subsystem in zip(ids, dispositions, subsystems):
        case_id = cap_id.replace("_", "-")
        closed = disp != "DISPOSITION_UNKNOWN" and case_id in passed_case_ids
        entry = per_subsystem.setdefault(
            subsystem, {"total": 0, "closed": 0, "unknown": 0, "capability_ids": []}
        )
        entry["total"] += 1
        entry["capability_ids"].append(cap_id)
        if disp == "DISPOSITION_UNKNOWN":
            entry["unknown"] += 1
        if closed:
            entry["closed"] += 1
    return {
        "total_legacy_capabilities": len(ids),
        "total_with_disposition_unknown": dispositions.count("DISPOSITION_UNKNOWN"),
        "total_with_matching_passed_equivalence_case": len(
            [1 for cid, d in zip(ids, dispositions)
             if d != "DISPOSITION_UNKNOWN" and cid.replace("_", "-") in passed_case_ids]
        ),
        "per_subsystem": per_subsystem,
    }


def build_manifest(root: Path, skip_tests: bool) -> dict:
    head = git_head(root)
    subsystem_records: list[dict] = []
    legacy_report = legacy_disposition_report(root)

    for subsystem, decl in SUBSYSTEMS.items():
        authority_digest, authority_matched = sha256_files(root, decl["authority"])
        implementation_digest, impl_matched = sha256_files(root, decl["implementation"])

        positive: list[dict] = []
        negative: list[dict] = []
        for crate, target, pos_fn, neg_fn in decl["tests"]:
            pos_run = run_cargo_test(root, crate, target, pos_fn, skip_tests)
            neg_run = run_cargo_test(root, crate, target, neg_fn, skip_tests)
            positive.append(asdict(pos_run))
            negative.append(asdict(neg_run))
        for script_rel, pos_test_id, neg_test_id in decl.get("python_tests", []):
            pos_run = run_python_unittest(root, script_rel, pos_test_id, skip_tests)
            neg_run = run_python_unittest(root, script_rel, neg_test_id, skip_tests)
            positive.append(asdict(pos_run))
            negative.append(asdict(neg_run))

        replay = {}
        if subsystem in ("projection", "graph", "evidence"):
            replay = {
                "kind": "determinism-test-target",
                "note": (
                    "Determinism/replay evidence for this subsystem is the same "
                    "positive_witness_reports test target re-run twice by the "
                    "external verifier (step 2) -- see that tool's own re-run, "
                    "not a claim recorded here."
                ),
            }
        else:
            replay = {
                "kind": "absent",
                "note": "No determinism/replay-specific test identified for this subsystem yet.",
            }

        legacy_for_subsystem = legacy_report["per_subsystem"].get(
            subsystem, {"total": 0, "closed": 0, "unknown": 0, "capability_ids": []}
        )

        insufficient = len(decl["tests"]) == 0 and len(decl.get("python_tests", [])) == 0
        reason = None
        if insufficient:
            reason = (
                f"No dedicated cargo test target (positive witness) exists yet for "
                f"subsystem '{subsystem}' -- honestly reported as insufficient "
                f"evidence, not fabricated. This is a real, tracked-not-fixed gap."
            )

        record = SubsystemRecord(
            subsystem=subsystem,
            authority_sources=authority_matched,
            authority_digest=authority_digest,
            implementation_sources=impl_matched,
            implementation_digest=implementation_digest,
            positive_witness_reports=positive,
            negative_falsifier_reports=negative,
            replay_report=replay,
            legacy_disposition_report=legacy_for_subsystem,
            insufficient_evidence=insufficient,
            insufficient_evidence_reason=reason,
        )
        subsystem_records.append(asdict(record))

    generator_path = root / GENERATOR_REL
    verifier_identity = {
        "path": str(GENERATOR_REL),
        "content_sha256": sha256_file(generator_path),
        "role": "manifest-generator",
    }

    manifest = {
        "schema": SCHEMA,
        "release": "26.8.1",
        "exact_source_head": head,
        "generated_at_unix": int(time.time()),
        "verifier_identity": verifier_identity,
        "subsystems": subsystem_records,
        "legacy_disposition_summary": legacy_report,
    }
    payload = json.dumps(manifest, sort_keys=True).encode("utf-8")
    manifest["receipt_digest"] = hashlib.sha256(payload).hexdigest()
    return manifest


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument(
        "--skip-tests",
        action="store_true",
        help="Do not actually run cargo test (fast, but produces an UNVERIFIED manifest -- the external verifier will refuse to trust it as ALIVE evidence)",
    )
    args = parser.parse_args(argv)
    root = Path(args.root).resolve()

    manifest = build_manifest(root, args.skip_tests)
    out_path = root / OUT_REL
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    print(f"wrote {out_path}")
    print(f"exact_source_head={manifest['exact_source_head']}")
    print(f"receipt_digest={manifest['receipt_digest']}")
    for rec in manifest["subsystems"]:
        pos_pass = sum(1 for r in rec["positive_witness_reports"] if r["passed"])
        neg_pass = sum(1 for r in rec["negative_falsifier_reports"] if r["passed"])
        print(
            f"  {rec['subsystem']:<14} "
            f"positive={pos_pass}/{len(rec['positive_witness_reports'])} "
            f"negative={neg_pass}/{len(rec['negative_falsifier_reports'])} "
            f"insufficient={rec['insufficient_evidence']}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
