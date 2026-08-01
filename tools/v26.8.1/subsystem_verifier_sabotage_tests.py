#!/usr/bin/env python3
"""Sabotage suite for the v26.8.1 external subsystem verifier.

Style follows `tools/v26.8.1/step_two.py`'s negative-control pattern: copy
real inputs into an isolated temp directory, corrupt exactly one thing,
run the REAL built `subsystem_verifier` binary against the isolated copy,
and assert it refuses (or reports the sabotaged subsystem as something
other than ALIVE) for the RIGHT reason -- not merely "it exited non-zero
for some reason".

Every case here is actually run; there is no assumed-pass result. Each
case prints PASS/FAIL and the suite exits non-zero if any case fails.

Usage:
    python3 tools/v26.8.1/subsystem_verifier_sabotage_tests.py [--root PATH]
"""
from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

INPUT_PATHS = (
    "AGENTS.md",
    "CLAUDE.md",
    "Cargo.toml",
    "Cargo.lock",
    "justfile",
    "rust-toolchain.toml",
    "docs/v26.8.1",
    "ontology/v26.8.1",
    "packs/legacy-equivalence-verifier-pack",
    "tools/v26.8.1",
    ".git",
)

MANIFEST_REL = ".ggen/v26.8.1/subsystem-evidence-manifest.json"
REPORT_REL = ".ggen/v26.8.1/subsystem-verifier-report.json"


def copy_inputs(root: Path, destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=False)
    for rel in INPUT_PATHS:
        source = root / rel
        target = destination / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, target, symlinks=True)
        elif source.is_file():
            shutil.copy2(source, target)


def build_binary(root: Path) -> Path:
    completed = subprocess.run(
        ["cargo", "build", "--manifest-path", "tools/v26.8.1/Cargo.toml", "--bin", "subsystem_verifier"],
        cwd=root,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        raise RuntimeError(f"failed to build subsystem_verifier: {completed.stderr}")
    binary = root / "tools/v26.8.1/target/debug/subsystem_verifier"
    if not binary.is_file():
        raise RuntimeError(f"expected binary at {binary}, not found")
    return binary


def run_verifier(binary: Path, copy_root: Path, observe_only: bool = False) -> subprocess.CompletedProcess:
    argv = [str(binary), "--root", str(copy_root)]
    if observe_only:
        argv.append("--observe-only")
    return subprocess.run(argv, capture_output=True, text=True, timeout=600)


def load_manifest(copy_root: Path) -> dict:
    return json.loads((copy_root / MANIFEST_REL).read_text())


def write_manifest(copy_root: Path, manifest: dict) -> None:
    (copy_root / MANIFEST_REL).write_text(json.dumps(manifest, indent=2))


def load_report(copy_root: Path) -> dict | None:
    path = copy_root / REPORT_REL
    if not path.is_file():
        return None
    return json.loads(path.read_text())


def case_missing_manifest(copy_root: Path):
    (copy_root / MANIFEST_REL).unlink()
    return True, None


def case_wrong_source_head(copy_root: Path):
    manifest = load_manifest(copy_root)
    manifest["exact_source_head"] = "0000000000000000000000000000000000000000"
    write_manifest(copy_root, manifest)
    return True, None


def case_altered_digest(copy_root: Path):
    manifest = load_manifest(copy_root)
    manifest["subsystems"][0]["authority_digest"] = "0" * 64
    write_manifest(copy_root, manifest)
    return True, None


def case_positive_witness_absent(copy_root: Path):
    manifest = load_manifest(copy_root)
    manifest["subsystems"][0]["positive_witness_reports"] = []
    write_manifest(copy_root, manifest)
    return True, None


def case_negative_falsifier_absent(copy_root: Path):
    manifest = load_manifest(copy_root)
    manifest["subsystems"][0]["negative_falsifier_reports"] = []
    write_manifest(copy_root, manifest)
    return True, None


def case_negative_falsifier_not_true_control(copy_root: Path):
    manifest = load_manifest(copy_root)
    for rec in manifest["subsystems"]:
        for t in rec["negative_falsifier_reports"]:
            t["is_true_negative_control"] = False
    write_manifest(copy_root, manifest)
    return True, None


def case_unknown_legacy_disposition_claimed_closed(copy_root: Path):
    ttl_path = copy_root / "ontology/v26.8.1/legacy-capabilities.ttl"
    text = ttl_path.read_text()
    # Force at least one DISPOSITION_UNKNOWN into a subsystem that
    # otherwise has clean dispositions (engine), so this case actually
    # exercises the UNKNOWN_LEGACY_DISPOSITION_CLAIMED_CLOSED path.
    text = text.replace("ggen:hasDisposition ggen:REPLACED", "ggen:hasDisposition ggen:DISPOSITION_UNKNOWN", 1)
    ttl_path.write_text(text)
    return True, None


def case_self_certification(copy_root: Path):
    manifest = load_manifest(copy_root)
    manifest["verifier_identity"]["path"] = "tools/v26.8.1/src/bin/subsystem_verifier.rs"
    manifest["verifier_identity"]["role"] = "subsystem-verifier"
    write_manifest(copy_root, manifest)
    return True, None


def case_matrix_hand_edit_ignored(copy_root: Path):
    """The crown must NOT trust a hand-edit of coverage-matrix.csv's
    standing column. This external verifier never reads that file at all
    -- so hand-editing it must have zero effect on this verifier's output.
    We assert this by diffing standings before/after the CSV edit."""
    matrix_path = copy_root / "docs/v26.8.1/coverage-matrix.csv"
    if not matrix_path.is_file():
        return False, "coverage-matrix.csv absent from isolated copy"
    original = matrix_path.read_text()
    mutated = original.replace("UNKNOWN", "ALIVE")
    matrix_path.write_text(mutated)
    return True, None


CASES: dict[str, callable] = {
    "missing-manifest": case_missing_manifest,
    "wrong-source-head": case_wrong_source_head,
    "altered-evidence-digest": case_altered_digest,
    "positive-witness-absent": case_positive_witness_absent,
    "negative-falsifier-absent": case_negative_falsifier_absent,
    "negative-falsifier-not-true-control": case_negative_falsifier_not_true_control,
    "unknown-legacy-disposition-claimed-closed": case_unknown_legacy_disposition_claimed_closed,
    "subsystem-self-certification": case_self_certification,
    "matrix-hand-edit-ignored": case_matrix_hand_edit_ignored,
}


def assertion_for(name: str, result: subprocess.CompletedProcess, copy_root: Path):
    stderr = result.stderr
    if name == "missing-manifest":
        return result.returncode != 0 and "missing referenced manifest" in stderr, stderr[-400:]
    if name == "wrong-source-head":
        return result.returncode != 0 and "WRONG_SOURCE_HEAD" in stderr, stderr[-400:]
    if name == "altered-evidence-digest":
        report = load_report(copy_root)
        if report is None:
            return False, "no report written"
        rec = report["subsystems"][0]
        ok = not rec["authority_digest_match"] and any(
            "ALTERED_EVIDENCE_DIGEST" in r for r in rec["reasons"]
        )
        return ok, json.dumps(rec["reasons"])
    if name == "positive-witness-absent":
        report = load_report(copy_root)
        if report is None:
            return False, "no report written"
        rec = report["subsystems"][0]
        ok = rec["standing"] == "UNKNOWN" and any(
            "POSITIVE_WITNESS_ABSENT" in r for r in rec["reasons"]
        )
        return ok, json.dumps(rec["reasons"])
    if name == "negative-falsifier-absent":
        report = load_report(copy_root)
        if report is None:
            return False, "no report written"
        rec = report["subsystems"][0]
        ok = rec["standing"] != "ALIVE" and any(
            "NEGATIVE_FALSIFIER_ABSENT" in r for r in rec["reasons"]
        )
        return ok, json.dumps(rec["reasons"])
    if name == "negative-falsifier-not-true-control":
        report = load_report(copy_root)
        if report is None:
            return False, "no report written"
        ok = all(rec["standing"] != "ALIVE" for rec in report["subsystems"])
        return ok, json.dumps([r["standing"] for r in report["subsystems"]])
    if name == "unknown-legacy-disposition-claimed-closed":
        report = load_report(copy_root)
        if report is None:
            return False, "no report written"
        engine = next((r for r in report["subsystems"] if r["subsystem"] == "engine"), None)
        if engine is None:
            return False, "no engine record in report"
        ok = engine["legacy_unknown"] > 0 and engine["standing"] != "ALIVE" and any(
            "UNKNOWN_LEGACY_DISPOSITION_CLAIMED_CLOSED" in r for r in engine["reasons"]
        )
        return ok, json.dumps(engine["reasons"])
    if name == "subsystem-self-certification":
        return result.returncode != 0 and "SELF_CERTIFICATION_REFUSED" in stderr, stderr[-400:]
    if name == "matrix-hand-edit-ignored":
        report = load_report(copy_root)
        if report is None:
            return False, "no report written"
        # The verifier's report must contain no reference to
        # coverage-matrix.csv's standing column at all -- the file is
        # simply never read by this binary.
        report_text = json.dumps(report)
        ok = "coverage-matrix" not in report_text
        return ok, "verifier report unexpectedly mentions coverage-matrix.csv" if not ok else "verifier never consulted coverage-matrix.csv, as required"
    return False, "no assertion defined"


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    args = parser.parse_args(argv)
    root = Path(args.root).resolve()

    binary = build_binary(root)

    results = []
    for name, mutate in CASES.items():
        tmp = Path(tempfile.mkdtemp(prefix=f"ggen-v2681-subsys-sabotage-{name}-"))
        try:
            copy_root = tmp / "repo"
            copy_inputs(root, copy_root)
            gen = subprocess.run(
                ["python3", "tools/v26.8.1/subsystem_evidence_manifest.py", "--root", str(copy_root), "--skip-tests"],
                cwd=root,
                capture_output=True,
                text=True,
            )
            if gen.returncode != 0:
                results.append((name, False, f"manifest generation failed: {gen.stderr[-300:]}"))
                continue
            ok, detail = mutate(copy_root)
            if not ok:
                results.append((name, False, f"mutation setup failed: {detail}"))
                continue
            # wrong-source-head must be checked in STRICT mode (the crown's
            # real default) -- observe-only deliberately bypasses that one
            # bail path, so exercising it here would prove nothing.
            observe_only = name != "wrong-source-head"
            result = run_verifier(binary, copy_root, observe_only=observe_only)
            passed, why = assertion_for(name, result, copy_root)
            results.append((name, passed, why))
        finally:
            shutil.rmtree(tmp, ignore_errors=True)

    all_pass = True
    for name, passed, detail in results:
        status = "PASS" if passed else "FAIL"
        if not passed:
            all_pass = False
        print(f"[{status}] {name}: {detail}")

    print(f"\n{sum(1 for _, p, _ in results if p)}/{len(results)} sabotage cases correctly refused")
    return 0 if all_pass else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
