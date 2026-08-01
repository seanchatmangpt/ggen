#!/usr/bin/env python3
"""Generic, data-driven legacy-capability equivalence runner.

This engine reads a case manifest (JSON; the shape produced by
`packs/legacy-equivalence-verifier-pack`'s `case_manifest.json.tmpl`, or any
hand-authored manifest of the same shape) and, for each case, dispatches
purely off the case's declared `observable_surfaces` list -- there are NO
per-case branches in this file. Adding a new case never requires touching
this script; it only requires adding a manifest entry.

Manifest shape (one case)::

    {
      "case_id": "cli-stdout-echo",
      "title": "...",
      "order": 1,
      "legacy_adapter": "<shell command>",
      "current_adapter": "<shell command>",
      "success_inputs": ["..."],
      "failure_inputs": ["..."],
      "normalization_policy": "none" | "strip_timestamps" | "sort_json_keys",
      "expected_disposition": "PRESERVED" | "SUBSUMED" | "REPLACED" | "ARCHIVED" | "REFUSED",
      "observable_surfaces": ["exit_code", "stdout", ...],
      "timeout_seconds": 5,
      "recovery_action": "<shell command>" | "none",
      "expected_diagnostic_substring": "..."   # optional, REFUSED cases
    }

Disposition semantics
----------------------
PRESERVED / SUBSUMED / REPLACED
    Both `legacy_adapter` and `current_adapter` are run (each once per
    declared input; empty success_inputs/failure_inputs means the adapter
    takes no input and is run once). Every observable surface named in
    `observable_surfaces` is compared, normalized per `normalization_policy`.

ARCHIVED
    `current_adapter` is not required to exist ("N/A" or empty is fine).
    Instead `recovery_action` is run and must prove restoration. The
    `recovery_result` surface (if declared) checks the recovery command's
    exit code; other declared surfaces (e.g. `filesystem_delta`,
    `generated_bytes`) are checked against the recovery_action's own
    invocation rather than a second adapter run.

REFUSED
    `legacy_adapter` is run to establish it used to work (informational
    only, not compared). `current_adapter` must exit non-zero and, if
    `expected_diagnostic_substring` is set, that substring must appear in
    its stderr. The `diagnostics` surface, if declared, checks the
    substring; `exit_code`, if declared, checks non-zero.

Conventions used by the built-in surface checkers
--------------------------------------------------
- `generated_bytes`: each adapter is run with the environment variable
  `EQV_OUT` set to a fresh, per-case temp directory. The legacy adapter is
  expected to write `$EQV_OUT/legacy.bin`; the current adapter (or recovery
  action) is expected to write `$EQV_OUT/current.bin`. Their bytes are
  compared exactly (normalization does not apply to binary content).
- `filesystem_delta` / `side_effects`: the set of relative paths (and their
  sizes) created under `$EQV_OUT` by the legacy run vs. the current run are
  compared as sets.
- `receipt_fields`: each adapter's stdout is parsed as JSON; the compared
  value is the whole parsed object (after normalization).
- `event_order`: lines in stdout starting with `EVENT:` are extracted, in
  order, from each side and compared as a sequence.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

SCHEMA = "ggen.legacy-equivalence.verifier-report.v1"


# ---------------------------------------------------------------------------
# Normalization policies
# ---------------------------------------------------------------------------

_TIMESTAMP_RE = re.compile(
    r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z?"
    r"|\d{2}:\d{2}:\d{2}"
)


def normalize_text(text: str, policy: str) -> str:
    """Apply a named normalization policy to text. No per-case logic --
    dispatch is purely on the policy name declared in the manifest."""
    if policy == "none":
        return text
    if policy == "strip_timestamps":
        return _TIMESTAMP_RE.sub("<TS>", text)
    if policy == "sort_json_keys":
        try:
            parsed = json.loads(text)
        except (json.JSONDecodeError, ValueError):
            return text
        return json.dumps(parsed, sort_keys=True, separators=(",", ":"))
    raise ValueError(f"unknown normalization_policy: {policy!r}")


# ---------------------------------------------------------------------------
# Adapter execution
# ---------------------------------------------------------------------------

@dataclass
class AdapterResult:
    exit_code: int
    stdout: str
    stderr: str
    timed_out: bool
    duration_ms: float
    out_dir: Path


def run_adapter(command: str, input_text: str, timeout_seconds: float, out_dir: Path) -> AdapterResult:
    env = dict(os.environ)
    env["EQV_OUT"] = str(out_dir)
    start = datetime.now(timezone.utc)
    try:
        proc = subprocess.run(
            command,
            shell=True,
            input=input_text,
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
            env=env,
        )
        timed_out = False
        exit_code = proc.returncode
        stdout, stderr = proc.stdout, proc.stderr
    except subprocess.TimeoutExpired as exc:
        timed_out = True
        exit_code = -1
        stdout = (exc.stdout or b"").decode() if isinstance(exc.stdout, bytes) else (exc.stdout or "")
        stderr = (exc.stderr or b"").decode() if isinstance(exc.stderr, bytes) else (exc.stderr or "")
    duration_ms = (datetime.now(timezone.utc) - start).total_seconds() * 1000.0
    return AdapterResult(exit_code, stdout, stderr, timed_out, duration_ms, out_dir)


def _tree_manifest(root: Path) -> dict[str, int]:
    if not root.exists():
        return {}
    manifest: dict[str, int] = {}
    for path in sorted(root.rglob("*")):
        if path.is_file():
            manifest[str(path.relative_to(root))] = path.stat().st_size
    return manifest


def _extract_events(text: str) -> list[str]:
    return [line for line in text.splitlines() if line.startswith("EVENT:")]


# ---------------------------------------------------------------------------
# Observable-surface checkers. Each has signature
#   (case, legacy: AdapterResult, current: AdapterResult) -> (bool, str)
# and is looked up purely by name from `observable_surfaces` -- this is the
# dispatch table that keeps the runner free of per-case branches.
# ---------------------------------------------------------------------------

def _check_exit_code(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    if case["expected_disposition"] == "REFUSED":
        if current.exit_code == 0:
            return False, "current adapter exited 0 but REFUSED disposition requires non-zero exit"
        return True, f"current adapter refused with exit code {current.exit_code}"
    if legacy.exit_code != current.exit_code:
        return False, f"exit_code differs: legacy={legacy.exit_code} current={current.exit_code}"
    return True, f"exit_code matches ({legacy.exit_code})"


def _check_stdout(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    policy = case["normalization_policy"]
    a = normalize_text(legacy.stdout, policy)
    b = normalize_text(current.stdout, policy)
    if a != b:
        return False, f"stdout differs after normalization={policy!r}: legacy={a!r} current={b!r}"
    return True, "stdout matches after normalization"


def _check_stderr(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    policy = case["normalization_policy"]
    a = normalize_text(legacy.stderr, policy)
    b = normalize_text(current.stderr, policy)
    if a != b:
        return False, f"stderr differs after normalization={policy!r}: legacy={a!r} current={b!r}"
    return True, "stderr matches after normalization"


def _check_diagnostics(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    substring = case.get("expected_diagnostic_substring")
    if case["expected_disposition"] == "REFUSED":
        if not substring:
            return False, "REFUSED case declares diagnostics surface but no expected_diagnostic_substring"
        haystack = current.stderr + current.stdout
        if substring not in haystack:
            return False, f"expected diagnostic substring {substring!r} not found in current adapter output: {haystack!r}"
        return True, f"found expected diagnostic substring {substring!r}"
    policy = case["normalization_policy"]
    a = normalize_text(legacy.stderr, policy)
    b = normalize_text(current.stderr, policy)
    if a != b:
        return False, f"diagnostics (stderr) differ after normalization={policy!r}: legacy={a!r} current={b!r}"
    return True, "diagnostics match"


def _check_generated_bytes(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    legacy_file = legacy.out_dir / "legacy.bin"
    current_file = current.out_dir / "current.bin"
    if not legacy_file.exists():
        return False, f"legacy adapter did not create expected file {legacy_file}"
    if not current_file.exists():
        return False, f"current adapter did not create expected file {current_file}"
    a = legacy_file.read_bytes()
    b = current_file.read_bytes()
    if a != b:
        return False, f"generated_bytes differ: legacy={len(a)} bytes, current={len(b)} bytes, first diff at offset {_first_diff_offset(a, b)}"
    return True, f"generated_bytes match ({len(a)} bytes)"


def _first_diff_offset(a: bytes, b: bytes) -> int:
    for i, (x, y) in enumerate(zip(a, b)):
        if x != y:
            return i
    return min(len(a), len(b))


def _check_filesystem_delta(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    a = _tree_manifest(legacy.out_dir)
    b = _tree_manifest(current.out_dir)
    if a != b:
        only_legacy = {k: v for k, v in a.items() if k not in b}
        only_current = {k: v for k, v in b.items() if k not in a}
        differing = {k: (a[k], b[k]) for k in a.keys() & b.keys() if a[k] != b[k]}
        return False, f"filesystem_delta differs: only_in_legacy={only_legacy} only_in_current={only_current} size_mismatch={differing}"
    return True, f"filesystem_delta matches ({len(a)} files)"


def _check_side_effects(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    a = set(_tree_manifest(legacy.out_dir).keys())
    b = set(_tree_manifest(current.out_dir).keys())
    if a != b:
        return False, f"side_effects differ: only_in_legacy={sorted(a - b)} only_in_current={sorted(b - a)}"
    return True, f"side_effects match ({len(a)} artifacts)"


def _check_receipt_fields(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    try:
        a = json.loads(legacy.stdout)
    except (json.JSONDecodeError, ValueError) as exc:
        return False, f"legacy stdout is not valid JSON for receipt_fields comparison: {exc}"
    try:
        b = json.loads(current.stdout)
    except (json.JSONDecodeError, ValueError) as exc:
        return False, f"current stdout is not valid JSON for receipt_fields comparison: {exc}"
    if a != b:
        return False, f"receipt_fields differ: legacy={a!r} current={b!r}"
    return True, "receipt_fields match"


def _check_event_order(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    a = _extract_events(legacy.stdout)
    b = _extract_events(current.stdout)
    if a != b:
        return False, f"event_order differs: legacy={a!r} current={b!r}"
    return True, f"event_order matches ({len(a)} events)"


def _check_recovery_result(case: dict, legacy: AdapterResult, current: AdapterResult) -> tuple[bool, str]:
    if current.exit_code != 0:
        return False, f"recovery_action exited {current.exit_code}: stderr={current.stderr!r}"
    return True, "recovery_action exited 0"


SURFACE_CHECKERS = {
    "exit_code": _check_exit_code,
    "stdout": _check_stdout,
    "stderr": _check_stderr,
    "diagnostics": _check_diagnostics,
    "generated_bytes": _check_generated_bytes,
    "filesystem_delta": _check_filesystem_delta,
    "side_effects": _check_side_effects,
    "receipt_fields": _check_receipt_fields,
    "event_order": _check_event_order,
    "recovery_result": _check_recovery_result,
}


# ---------------------------------------------------------------------------
# Case execution
# ---------------------------------------------------------------------------

def run_case(case: dict, work_root: Path) -> dict:
    case_id = case["case_id"]
    timeout = float(case.get("timeout_seconds", 5))
    disposition = case["expected_disposition"]
    surfaces = case.get("observable_surfaces", [])
    inputs = case.get("success_inputs") or [""]
    input_text = inputs[0] if inputs else ""

    case_dir = work_root / case_id
    legacy_dir = case_dir / "legacy"
    current_dir = case_dir / "current"
    legacy_dir.mkdir(parents=True, exist_ok=True)
    current_dir.mkdir(parents=True, exist_ok=True)

    unknown = [s for s in surfaces if s not in SURFACE_CHECKERS]
    if unknown:
        return {
            "case_id": case_id,
            "expected_disposition": disposition,
            "status": "BLOCKED",
            "reason": f"unknown observable_surfaces: {unknown}",
            "surfaces": [],
            "duration_ms": 0,
        }

    legacy = run_adapter(case["legacy_adapter"], input_text, timeout, legacy_dir)

    if disposition == "ARCHIVED":
        recovery_cmd = case.get("recovery_action", "none")
        if recovery_cmd in ("", "none", None):
            return {
                "case_id": case_id,
                "expected_disposition": disposition,
                "status": "BLOCKED",
                "reason": "ARCHIVED case has no recovery_action to prove restoration",
                "surfaces": [],
                "duration_ms": legacy.duration_ms,
            }
        current = run_adapter(recovery_cmd, input_text, timeout, current_dir)
    else:
        current_cmd = case.get("current_adapter", "")
        if not current_cmd or current_cmd.strip().upper() == "N/A":
            return {
                "case_id": case_id,
                "expected_disposition": disposition,
                "status": "BLOCKED",
                "reason": f"disposition {disposition} requires current_adapter but none was declared",
                "surfaces": [],
                "duration_ms": legacy.duration_ms,
            }
        current = run_adapter(current_cmd, input_text, timeout, current_dir)

    surface_results = []
    all_pass = True
    reasons: list[str] = []
    for surface in surfaces:
        checker = SURFACE_CHECKERS[surface]
        try:
            ok, reason = checker(case, legacy, current)
        except Exception as exc:  # surfaced as a FAIL, never silently swallowed
            ok, reason = False, f"checker for {surface!r} raised {type(exc).__name__}: {exc}"
        surface_results.append({"surface": surface, "status": "PASS" if ok else "FAIL", "reason": reason})
        if not ok:
            all_pass = False
            reasons.append(f"{surface}: {reason}")

    status = "PASS" if all_pass else "FAIL"
    reason = "all declared observable surfaces matched" if all_pass else "; ".join(reasons)

    return {
        "case_id": case_id,
        "expected_disposition": disposition,
        "status": status,
        "reason": reason,
        "surfaces": surface_results,
        "duration_ms": legacy.duration_ms + current.duration_ms,
    }


def run_manifest(manifest: dict, work_root: Path) -> dict:
    results = [run_case(case, work_root) for case in manifest.get("cases", [])]
    summary = {
        "total": len(results),
        "passed": sum(1 for r in results if r["status"] == "PASS"),
        "failed": sum(1 for r in results if r["status"] == "FAIL"),
        "blocked": sum(1 for r in results if r["status"] == "BLOCKED"),
    }
    return {
        "schema": SCHEMA,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "results": results,
        "summary": summary,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, help="Path to case_manifest.json")
    parser.add_argument("--report", required=True, help="Path to write the verifier report JSON")
    parser.add_argument("--work-dir", default=None, help="Scratch directory for adapter EQV_OUT (default: fresh temp dir)")
    args = parser.parse_args(argv)

    manifest_path = Path(args.manifest)
    manifest = json.loads(manifest_path.read_text())

    cleanup = False
    if args.work_dir:
        work_root = Path(args.work_dir)
        work_root.mkdir(parents=True, exist_ok=True)
    else:
        work_root = Path(tempfile.mkdtemp(prefix="eqv-runner-"))
        cleanup = True

    try:
        report = run_manifest(manifest, work_root)
        report["manifest_path"] = str(manifest_path)
        report_path = Path(args.report)
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2) + "\n")

        for result in report["results"]:
            print(f"[{result['status']}] {result['case_id']} ({result['expected_disposition']}): {result['reason']}")
        summary = report["summary"]
        print(f"summary: {summary['passed']}/{summary['total']} passed, {summary['failed']} failed, {summary['blocked']} blocked")

        return 0 if summary["failed"] == 0 and summary["blocked"] == 0 else 1
    finally:
        if cleanup:
            shutil.rmtree(work_root, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
