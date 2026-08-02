#!/usr/bin/env python3
"""Construct and verify the receipted removal of the migrated ggen v26.8.1 corpus."""

from __future__ import annotations

import argparse
import filecmp
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import tomllib
from pathlib import Path
from typing import Any

SCHEMA_PLAN = "ggen.source-removal-plan/1"
SCHEMA_REPORT = "ggen.source-removal-verifier/1"
SCHEMA_RECEIPT = "ggen.source-removal-receipt/1"
SOURCE_REPOSITORY = "seanchatmangpt/ggen"
CORPUS_REPOSITORY = "seanchatmangpt/ggen-legacy"
ACTIVE_ROOTS = (
    Path("docs/v26.8.1"),
    Path("ontology/v26.8.1"),
    Path("planning/v26.8.1"),
    Path("tools/v26.8.1"),
    Path("packs/legacy-equivalence-verifier-pack"),
)
GUARD_MOVES = (
    (
        Path("tools/v26.8.1/guard_fail_open_subprocess.py"),
        Path("scripts/ci/guard_fail_open_subprocess.py"),
    ),
    (
        Path("tools/v26.8.1/guard_short_test_timeout.py"),
        Path("scripts/ci/guard_short_test_timeout.py"),
    ),
)
DESTINATION_EVIDENCE = (
    Path("migrations/ggen-v26.8.1/migration-manifest.json"),
    Path("migrations/ggen-v26.8.1/migration-receipt.json"),
    Path("migrations/ggen-v26.8.1/verifier-report.json"),
    Path("migrations/ggen-v26.8.1/equivalence-report.json"),
    Path("scripts/verify_ggen_v26_8_1_migration.py"),
)


def refuse(code: str, detail: str = "") -> "NoReturn":
    suffix = f" {detail}" if detail else ""
    raise SystemExit(f"{code}{suffix}")


def canonical_json(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True) + "\n").encode()


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    return sha256_bytes(path.read_bytes())


def read_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        refuse("JSON_ADMISSION_REFUSED", f"path={path} error={exc}")
    if not isinstance(value, dict):
        refuse("JSON_OBJECT_REQUIRED_REFUSED", f"path={path}")
    return value


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(canonical_json(value))


def git_text(*args: str, cwd: Path | None = None) -> str:
    proc = subprocess.run(
        ["git", *args],
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        refuse(
            "GIT_COMMAND_REFUSED",
            f"argv={' '.join(args)} stderr={proc.stderr.strip()}",
        )
    return proc.stdout.strip()


def run_status(argv: list[str], cwd: Path | None = None) -> dict[str, Any]:
    proc = subprocess.run(
        argv,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
        env={**os.environ, "RUSTC_WRAPPER": "", "CARGO_INCREMENTAL": "0"},
    )
    return {"argv": argv, "exit_status": proc.returncode}


def require_success(record: dict[str, Any], code: str) -> None:
    if record["exit_status"] != 0:
        refuse(code, f"argv={record['argv']} exit={record['exit_status']}")


def admit_destination(args: argparse.Namespace) -> None:
    baseline = args.source_baseline.resolve()
    corpus = args.corpus.resolve()
    if git_text("rev-parse", "HEAD", cwd=baseline) != args.source_base:
        refuse("SOURCE_BASE_COORDINATE_REFUSED")
    corpus_head = git_text("rev-parse", "HEAD", cwd=corpus)
    if corpus_head != args.corpus_ref:
        refuse(
            "CORPUS_REF_COORDINATE_REFUSED",
            f"expected={args.corpus_ref} observed={corpus_head}",
        )
    ancestry = subprocess.run(
        ["git", "merge-base", "--is-ancestor", args.corpus_checkpoint, corpus_head],
        cwd=corpus,
        check=False,
    )
    if ancestry.returncode != 0:
        refuse("CORPUS_CHECKPOINT_ANCESTRY_REFUSED")

    required = [corpus / relative for relative in DESTINATION_EVIDENCE]
    missing = [str(path) for path in required if not path.is_file()]
    if missing:
        refuse("DESTINATION_EVIDENCE_ABSENT_REFUSED", json.dumps(missing))

    manifest = read_json(required[0])
    receipt = read_json(required[1])
    verifier = read_json(required[2])
    equivalence = read_json(required[3])
    checks = {
        "source_manifest": manifest.get("source_head") == args.source_base,
        "corpus_manifest": manifest.get("corpus_head") == args.corpus_checkpoint,
        "source_receipt": receipt.get("source_commit") == args.source_base,
        "corpus_receipt": receipt.get("corpus_commit") == args.corpus_checkpoint,
        "standing_receipt": receipt.get("standing") == "PARTIAL_ALIVE",
        "standing_verifier": verifier.get("standing") == "PARTIAL_ALIVE",
        "replay_receipt": receipt.get("replay") == "REPLAY_MATCH",
        "replay_verifier": verifier.get("replay") == "REPLAY_MATCH",
        "equivalence": equivalence.get("exact_byte_identity") is True,
        "source_removal_not_ambient": receipt.get("source_removal_admitted") is False,
    }
    if not all(checks.values()):
        refuse("DESTINATION_RECEIPT_REFUSED", json.dumps(checks, sort_keys=True))

    with tempfile.TemporaryDirectory(prefix="ggen-destination-admission-") as tmp:
        report_path = Path(tmp) / "verifier-report.json"
        record = run_status(
            [
                sys.executable,
                str(required[4]),
                "--source-root",
                str(baseline),
                "--destination-root",
                str(corpus),
                "--report",
                str(report_path),
            ],
            cwd=corpus,
        )
        require_success(record, "DESTINATION_VERIFIER_EXECUTION_REFUSED")
        observed = read_json(report_path)
        if observed.get("replay") != "REPLAY_MATCH" or observed.get("failed_checks") != 0:
            refuse("DESTINATION_VERIFIER_RESULT_REFUSED")

    print(json.dumps(checks, sort_keys=True))


def remove_v26_recipes(text: str) -> str:
    lines = text.splitlines(keepends=True)
    output: list[str] = []
    skipping = False
    recipe = re.compile(r"^([A-Za-z0-9_.-]+)(?:\s+[^:]*)?:")
    assignment = re.compile(r"^[A-Za-z_][A-Za-z0-9_-]*\s*[:?+]?=")
    for line in lines:
        top = bool(line.strip()) and not line[0].isspace()
        match = recipe.match(line) if top else None
        if match and match.group(1).startswith("v26-8-1-"):
            skipping = True
            continue
        if skipping and top and (match or assignment.match(line)):
            skipping = False
        if not skipping:
            output.append(line)
    result = "".join(output)
    if re.search(r"(?m)^v26-8-1-[^:]*:", result):
        refuse("V26_RECIPE_REMAINS_REFUSED")
    return result


def assert_foundry_corpus_rooted(path: Path) -> None:
    text = path.read_text()
    required = (
        "snapshot_repository(&cli.corpus)",
        'cli.corpus.join("foundry")',
    )
    stale = (
        'default_value = "docs/v26.8.1/document-evidence-index.json"',
        "cli.source.join(&cli.evidence_ref)",
    )
    if not all(token in text for token in required):
        refuse("FOUNDRY_CORPUS_BOUNDARY_ABSENT_REFUSED")
    if any(token in text for token in stale):
        refuse("FOUNDRY_STALE_SOURCE_BOUNDARY_REFUSED")


def construct(args: argparse.Namespace) -> None:
    corpus = args.corpus.resolve()
    manifest = read_json(corpus / DESTINATION_EVIDENCE[0])

    missing_roots = [str(path) for path in ACTIVE_ROOTS if not path.exists()]
    if missing_roots:
        refuse("SOURCE_ROOT_ABSENT_REFUSED", json.dumps(missing_roots))

    for source, destination in GUARD_MOVES:
        if not source.is_file():
            refuse("GUARD_SOURCE_ABSENT_REFUSED", f"path={source}")
        if destination.exists():
            refuse("GUARD_DESTINATION_OCCUPIED_REFUSED", f"path={destination}")
        destination.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run(["git", "mv", str(source), str(destination)], check=True)

    workflow_component = next(
        (
            item
            for item in manifest.get("components", [])
            if item.get("component_id") == "GGEN-V26.8.1-SOURCE-WORKFLOW-EVIDENCE"
        ),
        None,
    )
    if not workflow_component:
        refuse("WORKFLOW_LINEAGE_ABSENT_REFUSED")
    lineage_path = corpus / str(workflow_component.get("migration_evidence", ""))
    lineage = read_json(lineage_path)
    workflow_paths: list[Path] = []
    for record in lineage.get("files", []):
        path = Path(str(record.get("source_path", "")))
        if path.parts[:2] != (".github", "workflows"):
            refuse("WORKFLOW_PATH_ESCAPE_REFUSED", f"path={path}")
        workflow_paths.append(path)
        subprocess.run(
            ["git", "rm", "-f", "--ignore-unmatch", "--", str(path)],
            check=True,
        )
    if not workflow_paths:
        refuse("WORKFLOW_LINEAGE_EMPTY_REFUSED")

    for path in ACTIVE_ROOTS:
        subprocess.run(["git", "rm", "-r", "--", str(path)], check=True)

    replacements = {str(source): str(destination) for source, destination in GUARD_MOVES}
    justfile = Path("justfile")
    just_text = justfile.read_text()
    for old, new in replacements.items():
        just_text = just_text.replace(old, new)
    justfile.write_text(remove_v26_recipes(just_text))

    governance = Path("crates/ggen-config/tests/governance_precommit_gate_count_test.rs")
    governance_text = governance.read_text()
    for old, new in replacements.items():
        governance_text = governance_text.replace(old, new)
    governance.write_text(governance_text)

    foundry = Path("tools/architecture-foundry/src/bin/admit_observation.rs")
    assert_foundry_corpus_rooted(foundry)

    config = Path("config/ggen-legacy-corpus.toml")
    config.parent.mkdir(parents=True, exist_ok=True)
    config.write_text(
        "[corpus]\n"
        f'repository = "{CORPUS_REPOSITORY}"\n'
        f'ref = "{args.corpus_ref}"\n'
        'migration = "migrations/ggen-v26.8.1/migration-receipt.json"\n'
        'composition = "project versioned corpus roots over this ggen kernel checkout"\n'
    )

    roles = Path("docs/architecture-foundry/REPOSITORY_ROLES.md")
    marker = "## Migrated v26.8.1 corpus"
    roles_text = roles.read_text()
    if marker not in roles_text:
        roles.write_text(
            roles_text.rstrip()
            + "\n\n"
            + marker
            + "\n\nThe versioned reconstruction corpus is owned by "
            + f"`{CORPUS_REPOSITORY}`; see `config/ggen-legacy-corpus.toml` for the exact "
            + "admitted coordinate. The generalized manufacturing kernel remains in this repository.\n"
        )

    plan = {
        "schema": SCHEMA_PLAN,
        "source_repository": SOURCE_REPOSITORY,
        "source_base": args.source_base,
        "corpus_repository": CORPUS_REPOSITORY,
        "corpus_ref": args.corpus_ref,
        "removed_roots": [str(path) for path in ACTIVE_ROOTS],
        "removed_workflows": [str(path) for path in sorted(workflow_paths)],
        "retained_kernel_context": ["rustfmt.toml", "tools/architecture-foundry"],
        "relocated_guards": [
            {"source": str(source), "destination": str(destination)}
            for source, destination in GUARD_MOVES
        ],
        "standing": "UNKNOWN",
    }
    write_json(args.plan, plan)


def scan_operational_references() -> list[str]:
    forbidden = [str(path) for path in ACTIVE_ROOTS] + [str(source) for source, _ in GUARD_MOVES]
    roots = [Path("justfile"), Path(".github/workflows"), Path("crates"), Path("tools/architecture-foundry")]
    findings: list[str] = []
    for root in roots:
        candidates = [root] if root.is_file() else root.rglob("*") if root.exists() else []
        for path in candidates:
            if not path.is_file() or "target" in path.parts:
                continue
            try:
                text = path.read_text()
            except (UnicodeDecodeError, OSError):
                continue
            for token in forbidden:
                if token in text:
                    findings.append(f"{path}:{token}")
    return sorted(findings)


def verify(args: argparse.Namespace) -> None:
    corpus = args.corpus.resolve()
    current_head = git_text("rev-parse", "HEAD")
    ancestry = subprocess.run(
        ["git", "merge-base", "--is-ancestor", args.subject_commit, current_head],
        check=False,
    )
    if ancestry.returncode != 0:
        refuse("REMOVAL_COMMIT_ANCESTRY_REFUSED")

    checks: dict[str, bool] = {
        "source_roots_absent": all(not path.exists() for path in ACTIVE_ROOTS),
        "rustfmt_retained": Path("rustfmt.toml").is_file(),
        "plan_present": args.plan.is_file(),
    }
    for source, destination in GUARD_MOVES:
        checks[f"guard_relocated:{destination}"] = destination.is_file()
        checks[f"guard_byte_identity:{destination}"] = destination.is_file() and filecmp.cmp(
            destination, corpus / source, shallow=False
        )

    config_path = Path("config/ggen-legacy-corpus.toml")
    try:
        config = tomllib.loads(config_path.read_text())
    except (OSError, tomllib.TOMLDecodeError):
        config = {}
    checks["corpus_config_repository"] = config.get("corpus", {}).get("repository") == CORPUS_REPOSITORY
    checks["corpus_config_ref"] = config.get("corpus", {}).get("ref") == args.corpus_ref
    assert_foundry_corpus_rooted(Path("tools/architecture-foundry/src/bin/admit_observation.rs"))

    references = scan_operational_references()
    checks["operational_source_references_absent"] = not references
    if not all(checks.values()):
        refuse("SOURCE_REMOVAL_STRUCTURAL_REFUSED", json.dumps(checks, sort_keys=True))

    commands = [
        run_status(["just", "--list"]),
        run_status(
            [
                "cargo",
                "test",
                "--manifest-path",
                "tools/architecture-foundry/Cargo.toml",
                "--all-targets",
            ]
        ),
        run_status(
            [
                "cargo",
                "test",
                "-p",
                "ggen-config",
                "--test",
                "governance_precommit_gate_count_test",
            ]
        ),
    ]
    for record in commands:
        require_success(record, "SOURCE_KERNEL_EXECUTION_REFUSED")

    with tempfile.TemporaryDirectory(prefix="ggen-v26-8-1-composed-") as tmp:
        composed = Path(tmp) / "tree"
        composed.mkdir()
        rsync = run_status(
            [
                "rsync",
                "-a",
                "--exclude=.git",
                "--exclude=.baseline-ggen",
                "--exclude=.corpus",
                "--exclude=target",
                "./",
                str(composed) + "/",
            ]
        )
        require_success(rsync, "COMPOSED_TREE_PROJECTION_REFUSED")
        commands.append(rsync)
        for path in ACTIVE_ROOTS:
            destination = composed / path
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copytree(corpus / path, destination)
        shutil.copy2(corpus / "rustfmt.toml", composed / "rustfmt.toml")

        composed_commands = [
            run_status(
                [sys.executable, "planning/v26.8.1/verify_planning.py"],
                cwd=composed,
            ),
            run_status(
                [
                    sys.executable,
                    "tools/v26.8.1/validate_shacl.py",
                    "--root",
                    ".",
                ],
                cwd=composed,
            ),
            run_status(
                [
                    "cargo",
                    "test",
                    "--manifest-path",
                    "tools/v26.8.1/Cargo.toml",
                    "--locked",
                    "--all-targets",
                ],
                cwd=composed,
            ),
        ]
        for record in composed_commands:
            require_success(record, "COMPOSED_EXECUTION_REFUSED")
        commands.extend(composed_commands)

    report = {
        "schema": SCHEMA_REPORT,
        "source_repository": SOURCE_REPOSITORY,
        "source_base": args.source_base,
        "subject_commit": args.subject_commit,
        "corpus_repository": CORPUS_REPOSITORY,
        "corpus_ref": args.corpus_ref,
        "plan_sha256": sha256_file(args.plan),
        "checks": checks,
        "commands": commands,
        "removed_roots": [str(path) for path in ACTIVE_ROOTS],
        "relocated_guards": [str(destination) for _, destination in GUARD_MOVES],
        "standing": "ALIVE",
    }
    write_json(args.report, report)


def manufacture_receipt(args: argparse.Namespace) -> None:
    report_a = args.report_a.read_bytes()
    report_b = args.report_b.read_bytes()
    if report_a != report_b:
        refuse("SOURCE_REMOVAL_REPLAY_MISMATCH_REFUSED")
    report = json.loads(report_a)
    if report.get("standing") != "ALIVE":
        refuse("SOURCE_REMOVAL_REPORT_STANDING_REFUSED")
    destination_receipt = args.corpus / DESTINATION_EVIDENCE[1]
    receipt = {
        "schema": SCHEMA_RECEIPT,
        "source_repository": SOURCE_REPOSITORY,
        "source_base": args.source_base,
        "removal_commit": args.subject_commit,
        "corpus_repository": CORPUS_REPOSITORY,
        "corpus_ref": args.corpus_ref,
        "corpus_checkpoint": args.corpus_checkpoint,
        "destination_receipt_sha256": sha256_file(destination_receipt),
        "plan_sha256": sha256_file(args.plan),
        "verification_report": report,
        "replay": "REPLAY_MATCH",
        "replay_digest": sha256_bytes(report_a),
        "standing": "ALIVE",
    }
    write_json(args.output, receipt)


def check_receipt(args: argparse.Namespace) -> None:
    receipt = read_json(args.receipt)
    checks = {
        "schema": receipt.get("schema") == SCHEMA_RECEIPT,
        "standing": receipt.get("standing") == "ALIVE",
        "replay": receipt.get("replay") == "REPLAY_MATCH",
        "source_base": receipt.get("source_base") == args.source_base,
        "corpus_ref": receipt.get("corpus_ref") == args.corpus_ref,
        "plan_digest": receipt.get("plan_sha256") == sha256_file(args.plan),
        "destination_receipt_digest": receipt.get("destination_receipt_sha256")
        == sha256_file(args.corpus / DESTINATION_EVIDENCE[1]),
    }
    removal_commit = str(receipt.get("removal_commit", ""))
    ancestry = subprocess.run(
        ["git", "merge-base", "--is-ancestor", removal_commit, "HEAD"],
        check=False,
    )
    checks["removal_commit_ancestor"] = bool(removal_commit) and ancestry.returncode == 0
    report = receipt.get("verification_report")
    checks["report_embedded"] = isinstance(report, dict) and report.get("standing") == "ALIVE"
    if isinstance(report, dict):
        checks["replay_digest"] = receipt.get("replay_digest") == sha256_bytes(canonical_json(report))
    else:
        checks["replay_digest"] = False
    if not all(checks.values()):
        refuse("SOURCE_REMOVAL_RECEIPT_REFUSED", json.dumps(checks, sort_keys=True))
    print(json.dumps(checks, sort_keys=True))


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser()
    sub = root.add_subparsers(dest="command", required=True)

    admit = sub.add_parser("admit-destination")
    admit.add_argument("--source-baseline", type=Path, required=True)
    admit.add_argument("--corpus", type=Path, required=True)
    admit.add_argument("--source-base", required=True)
    admit.add_argument("--corpus-checkpoint", required=True)
    admit.add_argument("--corpus-ref", required=True)
    admit.set_defaults(func=admit_destination)

    construct_cmd = sub.add_parser("construct")
    construct_cmd.add_argument("--corpus", type=Path, required=True)
    construct_cmd.add_argument("--source-base", required=True)
    construct_cmd.add_argument("--corpus-ref", required=True)
    construct_cmd.add_argument("--plan", type=Path, required=True)
    construct_cmd.set_defaults(func=construct)

    verify_cmd = sub.add_parser("verify")
    verify_cmd.add_argument("--corpus", type=Path, required=True)
    verify_cmd.add_argument("--source-base", required=True)
    verify_cmd.add_argument("--corpus-ref", required=True)
    verify_cmd.add_argument("--subject-commit", required=True)
    verify_cmd.add_argument("--plan", type=Path, required=True)
    verify_cmd.add_argument("--report", type=Path, required=True)
    verify_cmd.set_defaults(func=verify)

    receipt_cmd = sub.add_parser("receipt")
    receipt_cmd.add_argument("--corpus", type=Path, required=True)
    receipt_cmd.add_argument("--source-base", required=True)
    receipt_cmd.add_argument("--corpus-ref", required=True)
    receipt_cmd.add_argument("--corpus-checkpoint", required=True)
    receipt_cmd.add_argument("--subject-commit", required=True)
    receipt_cmd.add_argument("--plan", type=Path, required=True)
    receipt_cmd.add_argument("--report-a", type=Path, required=True)
    receipt_cmd.add_argument("--report-b", type=Path, required=True)
    receipt_cmd.add_argument("--output", type=Path, required=True)
    receipt_cmd.set_defaults(func=manufacture_receipt)

    check_cmd = sub.add_parser("check-receipt")
    check_cmd.add_argument("--corpus", type=Path, required=True)
    check_cmd.add_argument("--source-base", required=True)
    check_cmd.add_argument("--corpus-ref", required=True)
    check_cmd.add_argument("--plan", type=Path, required=True)
    check_cmd.add_argument("--receipt", type=Path, required=True)
    check_cmd.set_defaults(func=check_receipt)
    return root


def main() -> None:
    args = parser().parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
