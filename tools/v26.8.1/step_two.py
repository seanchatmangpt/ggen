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
# Crown negative-control fixture
#
# The crown verifier (tools/v26.8.1/src/main.rs) is REQUIRED to refuse the
# real, currently-incomplete repository today (most coverage-matrix.csv rows
# are still standing=UNKNOWN). Asserting that fact directly against the real
# repo state made the negative control fragile: once later mission phases
# genuinely admit the release, the same assertion would then require the
# real repo to *keep* refusing -- which would be wrong. Instead we prove
# fail-closed behavior against a deliberately sabotaged, fully isolated copy
# of the crown's real inputs, so the negative control's correctness never
# depends on how far the real corpus has progressed.
# ---------------------------------------------------------------------------

# Relative paths the crown verifier actually reads (see main.rs: resolve_root
# requires Cargo.toml+AGENTS.md; observe_documents walks DOC_ROOT;
# load_coverage reads COVERAGE_PATH; observe_workspace reads Cargo.toml and
# walks the two command-surface roots; observe_authority_files hashes the
# fixed authority-file list).
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

SABOTAGED_COVERAGE_RELPATH = "docs/v26.8.1/coverage-matrix.csv"


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


def sabotage_coverage_matrix(copy_root: Path) -> str:
    """Corrupt exactly one coverage-matrix row so validate_coverage's
    allowed-standing check (main.rs, ``INVALID_COVERAGE_VALUE``) is the
    verifier logic actually exercised by this fixture -- not an arbitrary
    syntax break the crown would reject for an uninteresting reason like a
    missing file.

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
            "crown-observe-first",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
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
                "--",
                "--observe-only",
            ],
        )
    )
    second_report_digest = file_digest(crown_report) if crown_report.is_file() else "MISSING"
    second_observation_digest = (
        file_digest(crown_observation) if crown_observation.is_file() else "MISSING"
    )

    # Fail-closed negative control: sabotage an isolated copy of the crown's
    # real inputs (never the working tree) and require the crown to refuse
    # for the specific, typed reason its own coverage-schema gate is
    # designed to catch (INVALID_COVERAGE_VALUE), not an arbitrary crash.
    sabotage_dir = Path(tempfile.mkdtemp(prefix="ggen-v2681-crown-sabotage-"))
    sabotaged_subsystem = "UNKNOWN"
    sabotage_finding_codes: list[str] = []
    try:
        sabotage_copy_root = sabotage_dir / "repo"
        build_crown_input_copy(root, sabotage_copy_root)
        sabotaged_subsystem = sabotage_coverage_matrix(sabotage_copy_root)
        commands.append(
            run_command(
                root,
                "crown-sabotage-negative-control",
                [
                    "cargo",
                    "run",
                    "--quiet",
                    "--manifest-path",
                    "tools/v26.8.1/Cargo.toml",
                    "--",
                    "--root",
                    str(sabotage_copy_root),
                ],
                expected_exit=2,
                require_text="release admission refused",
            )
        )
        sabotage_report_path = sabotage_copy_root / ".ggen/v26.8.1/verifier-report.json"
        if sabotage_report_path.is_file():
            sabotage_report = json.loads(sabotage_report_path.read_text(encoding="utf-8"))
            sabotage_finding_codes = sorted(
                {finding["code"] for finding in sabotage_report.get("findings", [])}
            )
    finally:
        shutil.rmtree(sabotage_dir, ignore_errors=True)

    sabotage_caught_correct_reason = "INVALID_COVERAGE_VALUE" in sabotage_finding_codes
    gates_extra_evidence = [
        f"sabotaged_subsystem={sabotaged_subsystem}",
        f"sabotage_finding_codes={sabotage_finding_codes}",
    ]

    # Real-repo observation: run the crown strictly against the REAL,
    # unmodified repository state and require only that it complete without
    # crashing (exit 0 admitted, or exit 2 typed refusal -- both are legal
    # outcomes of a working crown; anything else, e.g. a panic, is a real
    # bug). This deliberately does NOT assert whether the real repo is
    # admitted or refused right now -- that is tracked separately by the
    # broader mission's coverage-matrix state, not by this negative control.
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
                "--",
            ],
            expected_exits=(0, 2),
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
            "crown-sabotage-caught-typed-reason",
            sabotage_caught_correct_reason,
            gates_extra_evidence,
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
