#!/usr/bin/env python3
"""Step Two one-shot admission controller for ggen v26.8.1.

Step Two is ALIVE when the autonomous control system can observe, plan, verify,
falsify, replay, and fail closed without human steering. This does not promote
an unfinished ggen release; correct refusal is part of the proof.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
import sys
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
    require_text: str | None = None,
) -> CommandEvidence:
    started = time.monotonic_ns()
    completed = subprocess.run(
        list(argv),
        cwd=root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
        env={**os.environ, "CARGO_TERM_COLOR": "never"},
    )
    elapsed_ms = (time.monotonic_ns() - started) // 1_000_000
    stdout = completed.stdout.decode("utf-8", errors="replace")
    stderr = completed.stderr.decode("utf-8", errors="replace")
    combined = stdout + "\n" + stderr
    passed = completed.returncode == expected_exit and (
        require_text is None or require_text in combined
    )
    return CommandEvidence(
        id=command_id,
        argv=list(argv),
        expected_exit=expected_exit,
        actual_exit=completed.returncode,
        passed=passed,
        stdout_sha256=digest(completed.stdout),
        stderr_sha256=digest(completed.stderr),
        stdout_tail=stdout[-4000:],
        stderr_tail=stderr[-4000:],
        elapsed_ms=int(elapsed_ms),
    )


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

    # Until all coverage rows are admitted, the strict crown MUST refuse.
    commands.append(
        run_command(
            root,
            "crown-fail-closed-negative-control",
            [
                "cargo",
                "run",
                "--quiet",
                "--manifest-path",
                "tools/v26.8.1/Cargo.toml",
                "--",
            ],
            expected_exit=2,
            require_text="release admission refused",
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
