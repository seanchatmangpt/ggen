#!/usr/bin/env python3
"""Sabotage / negative-control portfolio for the read-only v26.8.1 crown's
coverage-matrix.csv drift check.

Proves, for each of 7 real sabotage cases, that the now-read-only crown
(`tools/v26.8.1/src/main.rs`, via the shared
`v26_8_1_tools::coverage_projection` module) refuses with
`GENERATED_COVERAGE_DRIFT` (or, for case 7, `COVERAGE_PROVENANCE_DRIFT`)
and -- critically -- NEVER rewrites the sabotaged file itself. The crown is
an observer; observers do not repair what they observe.

Unlike `step_two.py`'s existing `crown-sabotage-negative-control` fixture
(which copies only a narrow subset of the crown's inputs into an isolated
temp dir), this suite cannot use that same narrow-copy pattern: the crown
now also shells out to `subsystem_verifier`, which itself shells out to
`cargo test -p <crate> ...` against the full compilable workspace to
re-verify positive/negative witnesses. Reproducing a full buildable copy of
an 18-crate workspace per sabotage case is not a tractable "isolated temp
copy". Instead, each case here mutates exactly ONE real file in the actual
working tree (`docs/v26.8.1/coverage-matrix.csv`, or, for case 7, the
projection receipt), captures its original bytes first, runs the real
crown against the real repository, asserts the expected refusal AND that
the file's on-disk bytes are byte-identical before and after the crown
run (proving the crown did not "fix" it), and restores the original bytes
in a `finally` block regardless of outcome. This is real execution against
the real crown binary -- no mocks -- with the smallest possible, always-
restored blast radius.

Usage:
    python3 tools/v26.8.1/coverage_sabotage_tests.py --root .
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Callable

COVERAGE_REL = "docs/v26.8.1/coverage-matrix.csv"
RECEIPT_REL = ".ggen/v26.8.1/coverage-projection-receipt.json"
CROWN_ARGV = [
    "cargo",
    "run",
    "--quiet",
    "--manifest-path",
    "tools/v26.8.1/Cargo.toml",
    "--bin",
    "ggen-v26-8-1-verifier",
    "--",
]


@dataclass(frozen=True)
class SabotageResult:
    case_id: str
    description: str
    expected_code: str
    exit_code: int
    finding_codes: list[str]
    caught_expected_code: bool
    file_unmodified_by_crown: bool
    passed: bool
    stdout_tail: str
    stderr_tail: str


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def read_rows(csv_bytes: bytes) -> tuple[list[str], list[dict[str, str]]]:
    text = csv_bytes.decode("utf-8")
    reader = csv.DictReader(io.StringIO(text))
    rows = list(reader)
    fieldnames = list(reader.fieldnames or [])
    return fieldnames, rows


def write_rows(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    buf = io.StringIO()
    # Rust's `csv` crate (used by both the manufacturing binary and the
    # crown's in-memory re-serialization) defaults to LF line endings, not
    # CRLF -- match that exactly so a syntactically-legal-but-differently-
    # terminated CSV isn't mistaken for content drift.
    writer = csv.DictWriter(buf, fieldnames=fieldnames, lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    return buf.getvalue().encode("utf-8")


def sabotage_altered_standing(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    rows = [dict(r) for r in rows]
    rows[0]["standing"] = "release-admitted-but-unverified"
    return write_rows(fieldnames, rows)


def sabotage_altered_verifier(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    rows = [dict(r) for r in rows]
    rows[0]["verifier"] = "some/other/verifier.rs"
    return write_rows(fieldnames, rows)


def sabotage_deleted_row(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    rows = [dict(r) for r in rows[1:]]
    return write_rows(fieldnames, rows)


def sabotage_duplicated_row(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    rows = [dict(r) for r in rows]
    rows.append(dict(rows[0]))
    return write_rows(fieldnames, rows)


def sabotage_reordered_rows(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    rows = list(reversed([dict(r) for r in rows]))
    return write_rows(fieldnames, rows)


def sabotage_disposition_without_evidence(fieldnames: list[str], rows: list[dict[str, str]]) -> bytes:
    rows = [dict(r) for r in rows]
    rows[0]["legacy_disposition"] = "PRESERVED" if rows[0]["legacy_disposition"] != "PRESERVED" else "UNKNOWN"
    return write_rows(fieldnames, rows)


def run_crown(root: Path) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        CROWN_ARGV,
        cwd=root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


def extract_finding_codes(root: Path) -> list[str]:
    report_path = root / ".ggen/v26.8.1/verifier-report.json"
    if not report_path.is_file():
        return []
    try:
        report = json.loads(report_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError:
        return []
    return sorted({f["code"] for f in report.get("findings", [])})


def run_csv_case(
    root: Path,
    case_id: str,
    description: str,
    mutate: Callable[[list[str], list[dict[str, str]]], bytes],
    expected_code: str = "GENERATED_COVERAGE_DRIFT",
) -> SabotageResult:
    coverage_path = root / COVERAGE_REL
    original_bytes = coverage_path.read_bytes()
    fieldnames, rows = read_rows(original_bytes)
    sabotaged_bytes = mutate(fieldnames, rows)
    try:
        coverage_path.write_bytes(sabotaged_bytes)
        before_run_bytes = coverage_path.read_bytes()
        completed = run_crown(root)
        after_run_bytes = coverage_path.read_bytes()
        file_unmodified = after_run_bytes == before_run_bytes
        finding_codes = extract_finding_codes(root)
        caught = expected_code in finding_codes
        passed = completed.returncode == 2 and caught and file_unmodified
        return SabotageResult(
            case_id=case_id,
            description=description,
            expected_code=expected_code,
            exit_code=completed.returncode,
            finding_codes=finding_codes,
            caught_expected_code=caught,
            file_unmodified_by_crown=file_unmodified,
            passed=passed,
            stdout_tail=completed.stdout.decode("utf-8", errors="replace")[-2000:],
            stderr_tail=completed.stderr.decode("utf-8", errors="replace")[-2000:],
        )
    finally:
        coverage_path.write_bytes(original_bytes)
        restored = coverage_path.read_bytes() == original_bytes
        if not restored:
            raise RuntimeError(
                f"FAILED TO RESTORE {COVERAGE_REL} after case {case_id} -- manual recovery required"
            )


def run_provenance_case(root: Path) -> SabotageResult:
    """Case 7: stale/mismatched subsystem-verifier receipt used as the
    projection's claimed input. Corrupts
    `.ggen/v26.8.1/coverage-projection-receipt.json`'s
    `subsystem_verifier_report_digest` field (leaving the real CSV and
    subsystem-verifier-report.json untouched) and requires the crown to
    refuse with COVERAGE_PROVENANCE_DRIFT.
    """
    receipt_path = root / RECEIPT_REL
    if not receipt_path.is_file():
        return SabotageResult(
            case_id="7-stale-provenance-receipt",
            description="stale/mismatched subsystem-verifier receipt used as projection input",
            expected_code="COVERAGE_PROVENANCE_DRIFT",
            exit_code=-1,
            finding_codes=[],
            caught_expected_code=False,
            file_unmodified_by_crown=False,
            passed=False,
            stdout_tail="",
            stderr_tail=(
                f"SKIPPED: {RECEIPT_REL} does not exist -- run "
                "`just v26-8-1-project-coverage` first to produce it"
            ),
        )
    original_bytes = receipt_path.read_bytes()
    receipt = json.loads(original_bytes.decode("utf-8"))
    receipt["subsystem_verifier_report_digest"] = "0" * 64
    sabotaged_bytes = (json.dumps(receipt, indent=2) + "\n").encode("utf-8")
    try:
        receipt_path.write_bytes(sabotaged_bytes)
        before_run_bytes = receipt_path.read_bytes()
        completed = run_crown(root)
        after_run_bytes = receipt_path.read_bytes()
        file_unmodified = after_run_bytes == before_run_bytes
        finding_codes = extract_finding_codes(root)
        caught = "COVERAGE_PROVENANCE_DRIFT" in finding_codes
        passed = completed.returncode == 2 and caught and file_unmodified
        return SabotageResult(
            case_id="7-stale-provenance-receipt",
            description="stale/mismatched subsystem-verifier receipt used as projection input",
            expected_code="COVERAGE_PROVENANCE_DRIFT",
            exit_code=completed.returncode,
            finding_codes=finding_codes,
            caught_expected_code=caught,
            file_unmodified_by_crown=file_unmodified,
            passed=passed,
            stdout_tail=completed.stdout.decode("utf-8", errors="replace")[-2000:],
            stderr_tail=completed.stderr.decode("utf-8", errors="replace")[-2000:],
        )
    finally:
        receipt_path.write_bytes(original_bytes)
        restored = receipt_path.read_bytes() == original_bytes
        if not restored:
            raise RuntimeError(
                f"FAILED TO RESTORE {RECEIPT_REL} after provenance case -- manual recovery required"
            )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path.cwd())
    args = parser.parse_args()
    root = args.root.resolve()

    results: list[SabotageResult] = []
    results.append(
        run_csv_case(
            root,
            "1-altered-standing",
            "manually altered `standing` value in one CSV row",
            sabotage_altered_standing,
        )
    )
    results.append(
        run_csv_case(
            root,
            "2-altered-verifier-path",
            "altered verifier path in a CSV row",
            sabotage_altered_verifier,
        )
    )
    results.append(
        run_csv_case(
            root,
            "3-deleted-row",
            "deleted subsystem row (9 of 10 rows remain)",
            sabotage_deleted_row,
        )
    )
    results.append(
        run_csv_case(
            root,
            "4-duplicated-row",
            "duplicated subsystem row (11 rows, one subsystem twice)",
            sabotage_duplicated_row,
        )
    )
    results.append(
        run_csv_case(
            root,
            "5-reordered-rows",
            "non-canonically reordered rows (reversed) -- the projection is "
            "order-sensitive by design (CANONICAL_SUBSYSTEMS fixes row order "
            "independent of any on-disk order), so this IS refused, not "
            "documented-as-accepted",
            sabotage_reordered_rows,
        )
    )
    results.append(
        run_csv_case(
            root,
            "6-disposition-without-evidence",
            "legacy_disposition changed in the CSV without corresponding "
            "evidence (subsystem-verifier report) changing",
            sabotage_disposition_without_evidence,
        )
    )
    results.append(run_provenance_case(root))

    evidence_root = root / ".ggen/v26.8.1"
    evidence_root.mkdir(parents=True, exist_ok=True)
    report = {
        "schema_version": "ggen.v26.8.1.coverage-sabotage-report/1",
        "cases": [asdict(r) for r in results],
        "all_passed": all(r.passed for r in results),
    }
    (evidence_root / "coverage-sabotage-report.json").write_text(
        json.dumps(report, indent=2) + "\n", encoding="utf-8"
    )

    for r in results:
        status = "PASS" if r.passed else "FAIL"
        print(
            f"{status} {r.case_id}: exit={r.exit_code} caught={r.caught_expected_code} "
            f"file_unmodified={r.file_unmodified_by_crown} codes={r.finding_codes}"
        )
        if not r.passed:
            print(f"       stderr_tail={r.stderr_tail!r}")

    all_passed = all(r.passed for r in results)
    print(f"coverage_sabotage_all_passed={str(all_passed).lower()}")
    return 0 if all_passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
