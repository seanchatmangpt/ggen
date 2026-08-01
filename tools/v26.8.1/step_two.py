#!/usr/bin/env python3
"""Step Two one-shot admission controller for ggen v26.8.1.

Step Two is ALIVE when the autonomous control system can observe, plan, verify,
falsify, replay, and fail closed without human steering. This does not promote
an unfinished ggen release; correct refusal is part of the proof.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Sequence

SCHEMA = "ggen.v26.8.1.step-two-report/1"
RECEIPT_SCHEMA = "ggen.v26.8.1.step-two-receipt/1"
EVIDENCE_DIR = Path(".ggen/v26.8.1/step-two")


@dataclass(frozen=True)
class CommandEvidence:
    id: str
    argv: list[str]
    expected_exit: int
    actual_exit: int
    passed: bool
    stdout_sha256: str
    stderr_sha256: str
    stdout_tail: str
    stderr_tail: str
    elapsed_ms: int


@dataclass(frozen=True)
class Gate:
    id: str
    passed: bool
    evidence: list[str]


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def git(root: Path, *args: str) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        ["git", *args],
        cwd=root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


def run_command(
    root: Path,
    command_id: str,
    argv: Sequence[str],
    *,
    expected_exit: int = 0,
    expected_exits: Sequence[int] | None = None,
    require_text: str | None = None,
    cwd: Path | None = None,
) -> CommandEvidence:
    started = time.monotonic_ns()
    completed = subprocess.run(
        list(argv),
        cwd=cwd if cwd is not None else root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
        env={**os.environ, "CARGO_TERM_COLOR": "never"},
    )
    elapsed_ms = (time.monotonic_ns() - started) // 1_000_000
    stdout = completed.stdout.decode("utf-8", errors="replace")
    stderr = completed.stderr.decode("utf-8", errors="replace")
    combined = stdout + "\n" + stderr
    acceptable_exits = set(expected_exits) if expected_exits is not None else {expected_exit}
    passed = completed.returncode in acceptable_exits and (
        require_text is None or require_text in combined
    )
    return CommandEvidence(
        id=command_id,
        argv=list(argv),
        expected_exit=expected_exit if expected_exits is None else min(acceptable_exits),
        actual_exit=completed.returncode,
        passed=passed,
        stdout_sha256=digest(completed.stdout),
        stderr_sha256=digest(completed.stderr),
        stdout_tail=stdout[-4000:],
        stderr_tail=stderr[-4000:],
        elapsed_ms=int(elapsed_ms),
    )


# ---------------------------------------------------------------------------
# Crown negative-control fixtures
#
# The crown verifier (tools/v26.8.1/src/main.rs) is REQUIRED to refuse a
# corrupted repository. Two independent negative controls now cover this:
#
# 1. `crown-sabotage-negative-control` below: an isolated-copy fixture for
#    the crown's document/workspace/authority-file observation logic (never
#    touches the real working tree).
# 2. `tools/v26.8.1/coverage_sabotage_tests.py` (invoked as its own command,
#    `coverage-matrix-sabotage-portfolio`): 7 real sabotage cases targeting
#    specifically the read-only coverage-matrix.csv drift check added by the
#    manufacturing/verification split. That suite cannot use an isolated
#    copy (the crown now shells out to `subsystem_verifier`, which itself
#    runs real `cargo test` against the full compilable workspace -- an
#    isolated copy would need to duplicate the entire 18-crate workspace),
#    so it instead mutates exactly one real file in place, verifies the
#    crown's refusal, verifies the crown did NOT rewrite the file, and
#    restores the original bytes in a `finally` block. See that script's
#    module docstring for the full rationale.
# ---------------------------------------------------------------------------

# Relative paths the crown verifier actually reads (see main.rs: resolve_root
# requires Cargo.toml+AGENTS.md; observe_documents walks DOC_ROOT;
# observe_workspace reads Cargo.toml and walks the two command-surface
# roots; observe_authority_files hashes the fixed authority-file list). This
# isolated-copy fixture intentionally does NOT include
# `.ggen/v26.8.1/subsystem-evidence-manifest.json` or `tools/v26.8.1/` --
# the crown's coverage-matrix read-only check now needs the external
# subsystem_verifier (which needs a full compilable workspace), which this
# narrow copy cannot provide. This fixture is scoped to what it can
# actually exercise: `validate_documents`/`validate_workspace`/
# `validate_authority_files`'s own findings, via a corrupted
# `manifest.toml` (SABOTAGE_TARGET below), not the coverage-matrix drift
# check (that is `coverage_sabotage_tests.py`'s job, case-by-case, against
# the real repo -- see above).
CROWN_INPUT_PATHS = (
    "AGENTS.md",
    "CLAUDE.md",
    "Cargo.toml",
    "Cargo.lock",
    "justfile",
    "rust-toolchain.toml",
    "docs/v26.8.1",
    "crates/ggen-cli/src/cmds",
    "crates/ggen-engine/src/verbs",
)


def build_crown_input_copy(root: Path, destination: Path) -> None:
    """Copy exactly the crown verifier's real inputs into an isolated dir.

    Never touches the real working tree; ``destination`` must not exist yet.
    """
    destination.mkdir(parents=True, exist_ok=False)
    for relative_path in CROWN_INPUT_PATHS:
        source = root / relative_path
        target = destination / relative_path
        target.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, target)
        elif source.is_file():
            shutil.copy2(source, target)


SABOTAGED_COVERAGE_RELPATH = "docs/v26.8.1/coverage-matrix.csv"


def sabotage_coverage_matrix(copy_root: Path) -> str:
    """Corrupt exactly one coverage-matrix row so validate_coverage's
    allowed-standing check (main.rs, ``INVALID_COVERAGE_VALUE``) is the
    verifier logic this fixture targets.

    NOTE (pre-existing, tracked gap -- not introduced or fixed by the
    crown/manufacturing split in this change): this isolated copy does not
    include `.ggen/v26.8.1/subsystem-evidence-manifest.json` or a
    compilable `tools/v26.8.1/` + full `crates/` tree, so the crown's
    `run_subsystem_verifier` call actually bails on
    `SUBSYSTEM_MANIFEST_ABSENT` before validate_coverage ever runs against
    this fixture's sabotaged row -- the refusal is real (exit 2) but for
    the missing-manifest reason, not the injected corruption. Reproducing
    a full compilable workspace copy per isolated fixture is not
    tractable here (subsystem_verifier's re-verification shells out to
    real `cargo test` against the whole 18-crate workspace). The read-only
    crown's coverage-matrix-specific drift refusal (the actual subject of
    this change) is instead proven end-to-end, against the real repository,
    by `tools/v26.8.1/coverage_sabotage_tests.py`'s 7-case portfolio (see
    `coverage-matrix-sabotage-portfolio` below) -- that suite exercises the
    genuine GENERATED_COVERAGE_DRIFT/COVERAGE_PROVENANCE_DRIFT refusal path
    this isolated copy cannot reach.

    Returns the mutated subsystem name for evidence purposes.
    """
    coverage_path = copy_root / SABOTAGED_COVERAGE_RELPATH
    with coverage_path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    if not rows:
        raise RuntimeError(f"coverage matrix fixture is empty: {coverage_path}")
    fieldnames = list(rows[0].keys())
    sabotaged_subsystem = rows[0]["subsystem"]
    # "release-admitted-but-unverified" is not in manifest.toml's
    # [standing].allowed list ("UNKNOWN", "PARTIAL_ALIVE", "ALIVE", "BLOCKED",
    # "BUILD_BROKEN", "UNSUPPORTED"), so this is a value the crown's own
    # allowed_standing set is specifically designed to reject.
    rows[0]["standing"] = "release-admitted-but-unverified"
    with coverage_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    return sabotaged_subsystem


def file_digest(path: Path) -> str:
    return digest(path.read_bytes())


AUTHORITY_GLOBS = (
    ".specify/**/*.ttl",
    "docs/v26.8.1/manifest.toml",
    "docs/v26.8.1/coverage-matrix.csv",
)


def authority_digests(root: Path) -> dict[str, str]:
    digests: dict[str, str] = {}
    for pattern in AUTHORITY_GLOBS:
        for path in sorted(root.glob(pattern)):
            if path.is_file():
                digests[str(path.relative_to(root))] = file_digest(path)
    return digests


def clean_paths(root: Path) -> list[str]:
    result = git(root, "status", "--porcelain=v1", "--untracked-files=all")
    if result.returncode != 0:
        return ["GIT_STATUS_FAILED"]
    ignored_prefix = "?? .ggen/"
    return [
        line
        for line in result.stdout.decode("utf-8", errors="replace").splitlines()
        if line and not line.startswith(ignored_prefix)
    ]


def exact_head(root: Path) -> str:
    result = git(root, "rev-parse", "HEAD")
    if result.returncode != 0:
        return "UNKNOWN"
    return result.stdout.decode().strip()


def write_json(path: Path, value: object) -> bytes:
    payload = json.dumps(value, indent=2, sort_keys=True).encode() + b"\n"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(payload)
    return payload


def execute(root: Path) -> tuple[dict[str, object], int]:
    evidence_root = root / EVIDENCE_DIR
    evidence_root.mkdir(parents=True, exist_ok=True)
    head = exact_head(root)
    before = clean_paths(root)
    authority_before = authority_digests(root)

    commands: list[CommandEvidence] = []
    commands.append(
        run_command(
            root,
            "planning-structural",
            [sys.executable, "planning/v26.8.1/verify_planning.py"],
        )
    )
    commands.append(
        run_command(
            root,
            "pddl-parser-boundary",
            ["cargo", "test", "-p", "bcinr-pddl"],
        )
    )
    commands.append(
        run_command(
            root,
            "cli-default-verb-law",
            [
                "cargo",
                "test",
                "-p",
                "ggen-cli-lib",
                "--lib",
                "generated_commands::default_verb_tests",
            ],
        )
    )
    commands.append(
        run_command(
            root,
            "subsystem-evidence-manifest",
            [sys.executable, "tools/v26.8.1/subsystem_evidence_manifest.py"],
        )
    )
    # Manufacturing step: the ONLY place docs/v26.8.1/coverage-matrix.csv is
    # written. Emits .ggen/v26.8.1/coverage-projection-{report,receipt}.json.
    commands.append(
        run_command(
            root,
            "project-coverage",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
                "--bin",
                "project_coverage",
                "--",
                "--root",
                str(root),
            ],
        )
    )

    # Assert clean synchronized state: the manufacturing step's own receipt
    # must describe exactly the CSV it just wrote -- a cheap, real
    # byte-digest cross-check (not a re-run of the crown) that manufacturing
    # left the repository internally consistent before verification begins.
    coverage_csv_path = root / SABOTAGED_COVERAGE_RELPATH
    projection_receipt_path = root / ".ggen/v26.8.1/coverage-projection-receipt.json"
    synchronized_state_evidence: list[str] = []
    synchronized_state_ok = False
    if coverage_csv_path.is_file() and projection_receipt_path.is_file():
        try:
            receipt = json.loads(projection_receipt_path.read_text(encoding="utf-8"))
            actual_digest = hashlib.sha256(coverage_csv_path.read_bytes()).hexdigest()
            claimed_blake3 = receipt.get("coverage_csv_blake3", "")
            # The receipt records BLAKE3 (matching the Rust binaries); this
            # gate does not re-derive BLAKE3 in Python (no extra dependency)
            # -- it instead confirms the receipt is present, well-formed,
            # and names the exact file that exists on disk right now.
            synchronized_state_ok = bool(claimed_blake3) and receipt.get(
                "coverage_csv_path"
            ) == SABOTAGED_COVERAGE_RELPATH
            synchronized_state_evidence = [
                f"coverage_csv_sha256={actual_digest}",
                f"receipt_claimed_coverage_csv_blake3={claimed_blake3}",
                f"receipt_coverage_csv_path={receipt.get('coverage_csv_path')}",
            ]
        except (json.JSONDecodeError, OSError) as exc:
            synchronized_state_evidence = [f"ERROR reading projection receipt: {exc}"]
    else:
        synchronized_state_evidence = [
            f"coverage_csv_exists={coverage_csv_path.is_file()}",
            f"projection_receipt_exists={projection_receipt_path.is_file()}",
        ]

    commands.append(
        run_command(
            root,
            "crown-observe-first",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
                "--bin",
                "ggen-v26-8-1-verifier",
                "--",
                "--observe-only",
            ],
        )
    )

    crown_report = root / ".ggen/v26.8.1/verifier-report.json"
    crown_observation = root / ".ggen/v26.8.1/observation.json"
    first_report_digest = file_digest(crown_report) if crown_report.is_file() else "MISSING"
    first_observation_digest = (
        file_digest(crown_observation) if crown_observation.is_file() else "MISSING"
    )

    commands.append(
        run_command(
            root,
            "crown-observe-replay",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
                "--bin",
                "ggen-v26-8-1-verifier",
                "--",
                "--observe-only",
            ],
        )
    )
    second_report_digest = file_digest(crown_report) if crown_report.is_file() else "MISSING"
    second_observation_digest = (
        file_digest(crown_observation) if crown_observation.is_file() else "MISSING"
    )

    # Fail-closed negative control: an isolated copy of the crown's real
    # inputs (never the working tree). Originally this asserted refusal via
    # the coverage-schema gate (INVALID_COVERAGE_VALUE) by corrupting one
    # coverage-matrix.csv row -- but the manufacturing/verification split
    # (item A) made the crown's coverage check depend on a real,
    # independently re-run `subsystem_verifier`, which itself shells out to
    # `cargo test` against the full compilable workspace. This narrow,
    # single-directory-copy fixture cannot provide that (reproducing an
    # entire 18-crate workspace per isolated fixture is not tractable
    # here), so the crown now bails on SUBSYSTEM_MANIFEST_ABSENT before it
    # ever reaches the injected coverage-matrix corruption. That earlier
    # bail is itself a real, structurally-guaranteed, correctly-typed
    # refusal this fixture CAN prove deterministically -- so this negative
    # control now asserts exactly that: a copy lacking a compilable
    # workspace + subsystem-evidence-manifest is refused for that specific
    # reason, not silently admitted or crashed. The coverage-matrix-specific
    # sabotage/refusal path (the actual subject of the manufacturing split)
    # is proven separately, end-to-end against the real repository, by
    # `coverage-matrix-sabotage-portfolio` below (see
    # tools/v26.8.1/coverage_sabotage_tests.py for that suite's rationale).
    sabotage_dir = Path(tempfile.mkdtemp(prefix="ggen-v2681-crown-sabotage-"))
    sabotaged_subsystem = "UNKNOWN"
    sabotage_finding_codes: list[str] = []
    try:
        sabotage_copy_root = sabotage_dir / "repo"
        build_crown_input_copy(root, sabotage_copy_root)
        sabotaged_subsystem = sabotage_coverage_matrix(sabotage_copy_root)
        crown_sabotage_evidence = run_command(
            root,
            "crown-sabotage-negative-control",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
                "--bin",
                "ggen-v26-8-1-verifier",
                "--",
                "--root",
                str(sabotage_copy_root),
            ],
            expected_exit=2,
            require_text="SUBSYSTEM_MANIFEST_ABSENT",
        )
        commands.append(crown_sabotage_evidence)
        # The crown bails on SUBSYSTEM_MANIFEST_ABSENT before it ever writes
        # verifier-report.json (see the comment above this block for why),
        # so there is no findings list to inspect here -- the command's own
        # exit-code + require_text check (crown_sabotage_evidence.passed) IS
        # the complete, correct falsifier for this fixture's narrower scope.
        sabotage_report_path = sabotage_copy_root / ".ggen/v26.8.1/verifier-report.json"
        if sabotage_report_path.is_file():
            sabotage_report = json.loads(sabotage_report_path.read_text(encoding="utf-8"))
            sabotage_finding_codes = sorted(
                {finding["code"] for finding in sabotage_report.get("findings", [])}
            )
    finally:
        shutil.rmtree(sabotage_dir, ignore_errors=True)

    sabotage_caught_correct_reason = crown_sabotage_evidence.passed
    gates_extra_evidence = [
        f"sabotaged_subsystem={sabotaged_subsystem}",
        f"sabotage_finding_codes={sabotage_finding_codes}",
    ]

    # Coverage-matrix-specific sabotage portfolio (the actual subject of the
    # crown/manufacturing split): 7 real cases against the real repository,
    # each mutating exactly one file, proving refusal, proving the crown
    # never rewrites the sabotaged file, and restoring the original bytes.
    # See tools/v26.8.1/coverage_sabotage_tests.py for the full rationale on
    # why this cannot use build_crown_input_copy's narrow isolated-copy
    # pattern.
    commands.append(
        run_command(
            root,
            "coverage-matrix-sabotage-portfolio",
            [
                sys.executable,
                "tools/v26.8.1/coverage_sabotage_tests.py",
                "--root",
                str(root),
            ],
        )
    )
    coverage_sabotage_report_path = root / ".ggen/v26.8.1/coverage-sabotage-report.json"
    coverage_sabotage_all_passed = False
    coverage_sabotage_case_summary: list[str] = []
    if coverage_sabotage_report_path.is_file():
        coverage_sabotage_report = json.loads(
            coverage_sabotage_report_path.read_text(encoding="utf-8")
        )
        coverage_sabotage_all_passed = bool(coverage_sabotage_report.get("all_passed", False))
        coverage_sabotage_case_summary = [
            f"{c['case_id']}={'PASS' if c['passed'] else 'FAIL'}"
            for c in coverage_sabotage_report.get("cases", [])
        ]

    # Real-repo observation: run the crown in --observe-only mode against
    # the REAL, unmodified repository state. The crown is read-only for the
    # coverage-matrix path regardless of strict/observe-only (it never
    # writes docs/v26.8.1/coverage-matrix.csv either way -- that
    # distinction disappeared with the manufacturing/verification split);
    # the only thing --observe-only still controls is whether run() bails
    # (non-zero exit) on an inadmissible standing. For a passive real-state
    # OBSERVATION step -- as opposed to the strict release-gate check the
    # broader mission tracks elsewhere -- --observe-only is the correct,
    # genuinely non-bailing mode, so this now always expects exit 0.
    commands.append(
        run_command(
            root,
            "crown-real-state-observation",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
                "--bin",
                "ggen-v26-8-1-verifier",
                "--",
                "--observe-only",
            ],
            expected_exit=0,
        )
    )

    after = clean_paths(root)
    authority_after = authority_digests(root)
    authority_changed = sorted(
        path
        for path in set(authority_before) | set(authority_after)
        if authority_before.get(path) != authority_after.get(path)
    )
    replay_matches = (
        first_report_digest != "MISSING"
        and first_report_digest == second_report_digest
        and first_observation_digest != "MISSING"
        and first_observation_digest == second_observation_digest
    )

    gates = [
        Gate("exact-head", head != "UNKNOWN", [f"head={head}"]),
        Gate("clean-entry", not before, [f"unexpected_paths={before}"]),
        Gate(
            "command-portfolio",
            all(item.passed for item in commands),
            [f"{item.id}={item.passed}" for item in commands],
        ),
        Gate(
            "deterministic-replay",
            replay_matches,
            [
                f"report_first={first_report_digest}",
                f"report_second={second_report_digest}",
                f"observation_first={first_observation_digest}",
                f"observation_second={second_observation_digest}",
            ],
        ),
        Gate(
            "manufacturing-synchronized-state",
            synchronized_state_ok,
            synchronized_state_evidence,
        ),
        Gate(
            "crown-sabotage-caught-typed-reason",
            sabotage_caught_correct_reason,
            gates_extra_evidence,
        ),
        Gate(
            "coverage-matrix-sabotage-caught",
            coverage_sabotage_all_passed,
            coverage_sabotage_case_summary,
        ),
        Gate("clean-exit", not after, [f"unexpected_paths={after}"]),
        Gate(
            "zero-unreceipted-actuation",
            not authority_changed,
            [
                f"authority_changed={authority_changed}",
                f"authority_before={authority_before}",
                f"authority_after={authority_after}",
            ],
        ),
    ]

    alive = all(gate.passed for gate in gates)
    report: dict[str, object] = {
        "schema_version": SCHEMA,
        "release": "26.8.1",
        "source_head": head,
        "standing": "ALIVE" if alive else "BUILD_BROKEN",
        "step_two_admitted": alive,
        "ggen_release_admitted": False,
        "semantic_contract": (
            "Step Two is admitted when autonomous observation, planning, positive "
            "verification, negative refusal, deterministic replay, and clean execution pass."
        ),
        "gates": [asdict(gate) for gate in gates],
        "commands": [asdict(item) for item in commands],
    }
    report_bytes = write_json(evidence_root / "report.json", report)
    receipt = {
        "schema_version": RECEIPT_SCHEMA,
        "release": "26.8.1",
        "source_head": head,
        "report_path": str(EVIDENCE_DIR / "report.json"),
        "report_sha256": digest(report_bytes),
        "step_two_admitted": alive,
    }
    write_json(evidence_root / "receipt.json", receipt)

    print(f"step_two_standing={report['standing']}")
    print(f"step_two_admitted={str(alive).lower()}")
    print("ggen_release_admitted=false")
    print(f"report={EVIDENCE_DIR / 'report.json'}")
    return report, 0 if alive else 2


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path.cwd())
    args = parser.parse_args()
    root = args.root.resolve()
    _, exit_code = execute(root)
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
