#!/usr/bin/env python3
"""Manufacture ggen's exact-head fast-admission receipt."""
from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
import sys
import time
from pathlib import Path
from typing import Sequence

import errc_router

TAIL_LIMIT = 4000


def run_check(check_id: str, command: Sequence[str], cwd: Path) -> dict[str, object]:
    started = time.monotonic()
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)
    elapsed_ms = round((time.monotonic() - started) * 1000)
    failure = None if result.returncode == 0 else f"BUILD_BROKEN:{check_id.upper().replace('-', '_')}"
    return {
        "id": check_id,
        "command": shlex.join(command),
        "exit_code": result.returncode,
        "elapsed_ms": elapsed_ms,
        "passed": result.returncode == 0,
        "typed_failure": failure,
        "stdout_tail": result.stdout[-TAIL_LIMIT:],
        "stderr_tail": result.stderr[-TAIL_LIMIT:],
    }


def existing_changed_workflows(paths: Sequence[str], repo: Path) -> list[str]:
    """Return changed workflow definitions that still exist at the admitted head.

    Git's name-only diff deliberately reports deleted paths. A deleted workflow
    is an elimination consequence, not YAML that can still be parsed, so the
    fast court must parse every surviving changed workflow without trying to
    reopen a path whose exact-head state is absence.
    """
    return [
        path
        for path in paths
        if path.startswith(".github/workflows/")
        and path.endswith((".yml", ".yaml"))
        and (repo / path).is_file()
    ]


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--repository", required=True)
    result.add_argument("--base", required=True)
    result.add_argument("--head", required=True)
    result.add_argument("--workflow", required=True)
    result.add_argument("--run-id", required=True)
    result.add_argument("--repo", type=Path, default=Path("."))
    result.add_argument("--receipt", type=Path, required=True)
    result.add_argument("--routing-report", type=Path, required=True)
    result.add_argument("--github-output", type=Path)
    return result


def main(argv: Sequence[str] | None = None) -> int:
    args = parser().parse_args(argv)
    repo = args.repo.resolve()
    checks: list[dict[str, object]] = []
    failures: list[str] = []
    routing = errc_router.route([])

    try:
        observed_head = subprocess.check_output(["git", "-C", str(repo), "rev-parse", "HEAD"], text=True).strip()
        head_ok = observed_head == args.head
        checks.append({
            "id": "exact-head",
            "command": "git rev-parse HEAD",
            "exit_code": 0 if head_ok else 1,
            "elapsed_ms": 0,
            "passed": head_ok,
            "typed_failure": None if head_ok else "REFUSED:HEAD_IDENTITY_MISMATCH",
            "stdout_tail": observed_head,
            "stderr_tail": "" if head_ok else f"expected {args.head}",
        })
        if not head_ok:
            failures.append("REFUSED:HEAD_IDENTITY_MISMATCH")

        paths = errc_router.discover_changed_files(args.base, args.head, repo)
        routing = errc_router.route(paths)
        changed_workflows = existing_changed_workflows(routing.changed_files, repo)

        commands = [
            ("python-compile", [sys.executable, "-m", "py_compile", "scripts/ci/errc_router.py", "scripts/ci/fast_admission.py", "scripts/ci/test_errc_router.py"]),
            ("router-self-tests", [sys.executable, "scripts/ci/test_errc_router.py"]),
        ]
        if changed_workflows:
            commands.append(
                (
                    "workflow-yaml",
                    ["ruby", "-e", "require 'yaml'; ARGV.each { |p| YAML.parse_file(p) }", *changed_workflows],
                )
            )
        if (repo / "scripts/ci/test-g0-workflow-inventory.py").exists():
            commands.append(("g0-inventory", [sys.executable, "scripts/ci/test-g0-workflow-inventory.py"]))

        for check_id, command in commands:
            check = run_check(check_id, command, repo)
            checks.append(check)
            if not check["passed"]:
                failures.append(str(check["typed_failure"]))

        args.routing_report.parent.mkdir(parents=True, exist_ok=True)
        args.routing_report.write_text(
            json.dumps(routing.to_json_object(), indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        if args.github_output:
            errc_router.write_github_outputs(args.github_output, routing)
    except Exception as exc:
        failure = str(exc)
        failures.append(failure if failure.startswith(("REFUSED:", "BLOCKED:", "BUILD_BROKEN:")) else f"BUILD_BROKEN:FAST_GATE_EXCEPTION:{failure}")

    standing = "ALIVE" if not failures else "BUILD_BROKEN"
    receipt = {
        "schema": "ggen.ci.errc.receipt.v1",
        "subject": {
            "repository": args.repository,
            "base": args.base,
            "head": args.head,
            "workflow": args.workflow,
            "run_id": args.run_id,
        },
        "errc": {
            "eliminate": ["unrelated universal deep-workflow fan-out", "obsolete PR-specific docs mutation from universal PR evaluation"],
            "reduce": ["one exact-head checkout", "one dependency-light admission executor", "one setup per applicable deep lane"],
            "raise": ["exact-head identity", "typed failures", "deterministic routing", "bounded replay evidence"],
            "create": ["universal fast admission", "path-owned deep lanes", "machine-readable exact-head receipt"],
        },
        "changed_files": list(routing.changed_files),
        "routing": {lane: list(paths) for lane, paths in routing.routing.items()},
        "checks": checks,
        "failures": failures,
        "standing": standing,
        "claim_ceiling": "EXACT_HEAD_FAST_AUTHORITY_AND_ROUTING_ONLY",
        "replay": {
            "command": f"python3 scripts/ci/fast_admission.py --repository {shlex.quote(args.repository)} --base {args.base} --head {args.head} --workflow {shlex.quote(args.workflow)} --run-id local-replay --receipt /tmp/ggen-errc-receipt.json --routing-report /tmp/ggen-errc-routing.json"
        },
    }
    args.receipt.parent.mkdir(parents=True, exist_ok=True)
    args.receipt.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps({"standing": standing, "failures": failures, "routing": routing.booleans}, sort_keys=True))
    return 0 if not failures else 1


if __name__ == "__main__":
    raise SystemExit(main())
