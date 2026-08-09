#!/usr/bin/env python3
"""Verify root ``ggen sync`` consequences against declared output ownership.

This verifier is intentionally independent of ggen's renderer. It reads the authored
root manifest, observes the Git worktree, and refuses any unowned consequence.
Execution receipts are admitted as evidence, never mislabeled as generation outputs.

``output_file`` is a *path contract*, not always a literal path. A rule such as
``crates/{{ crate_name }}/Cargo.toml`` declares a bounded family of concrete paths.
Treating that template text as one required filename produces a false negative and
loses the ownership relation the manifest is meant to encode. Literal outputs remain
mandatory and are observed directly from the filesystem, including intentionally
Git-ignored generated consequences. Templated outputs are compiled to one-segment
path predicates; every concrete repository path may have at most one owner.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from typing import Any, Iterable

EVIDENCE_PREFIXES = (".ggen-v2/",)
TEMPLATE_EXPRESSION = re.compile(r"\{\{.*?\}\}")


@dataclass(frozen=True)
class OutputRule:
    owner: str
    output_file: str
    matcher: re.Pattern[str]
    literal: bool


def git(root: Path, *args: str) -> bytes:
    return subprocess.run(
        ["git", "-C", str(root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def _compile_output_matcher(path: str) -> tuple[re.Pattern[str], bool]:
    matches = list(TEMPLATE_EXPRESSION.finditer(path))
    if not matches:
        return re.compile(rf"^{re.escape(path)}$"), True

    pieces: list[str] = []
    cursor = 0
    for match in matches:
        pieces.append(re.escape(path[cursor : match.start()]))
        # Output substitutions are admitted as one path segment. A rendered value
        # cannot smuggle authority into a sibling/parent subtree.
        pieces.append(r"[^/]+")
        cursor = match.end()
    pieces.append(re.escape(path[cursor:]))
    return re.compile("^" + "".join(pieces) + "$"), False


def declared_output_rules(root: Path) -> list[OutputRule]:
    manifest = tomllib.loads((root / "ggen.toml").read_text(encoding="utf-8"))
    generation = manifest.get("generation", {})
    rules = generation.get("rules", []) if isinstance(generation, dict) else []
    outputs: list[OutputRule] = []
    seen_contracts: dict[str, str] = {}

    for index, rule in enumerate(rules):
        if not isinstance(rule, dict) or "output_file" not in rule:
            continue
        output = PurePosixPath(str(rule["output_file"]))
        if output.is_absolute() or ".." in output.parts:
            raise SystemExit(f"unsafe root generation output: {output}")
        path = output.as_posix()
        owner = str(rule.get("name", f"rule-{index:03d}"))
        if path in seen_contracts:
            raise SystemExit(
                f"duplicate root output owner: {path}: {seen_contracts[path]} and {owner}"
            )
        seen_contracts[path] = owner
        matcher, literal = _compile_output_matcher(path)
        outputs.append(OutputRule(owner=owner, output_file=path, matcher=matcher, literal=literal))

    if not outputs:
        raise SystemExit("root ggen.toml declares zero generation outputs")
    return outputs


def declared_outputs(root: Path) -> dict[str, str]:
    """Compatibility view of literal output contracts only."""
    return {rule.output_file: rule.owner for rule in declared_output_rules(root) if rule.literal}


def nul_paths(raw: bytes) -> set[str]:
    return {item.decode("utf-8") for item in raw.split(b"\0") if item}


def changed_paths(root: Path) -> list[str]:
    paths = set()
    paths |= nul_paths(git(root, "diff", "--name-only", "-z"))
    paths |= nul_paths(git(root, "diff", "--cached", "--name-only", "-z"))
    paths |= nul_paths(git(root, "ls-files", "--others", "--exclude-standard", "-z"))
    return sorted(paths)


def repository_paths(root: Path) -> list[str]:
    """Return tracked and non-ignored untracked paths for ambient-change admission."""
    return sorted(
        nul_paths(git(root, "ls-files", "--cached", "--others", "--exclude-standard", "-z"))
    )


def is_evidence_path(path: str) -> bool:
    return path.startswith(EVIDENCE_PREFIXES)


def owners_for_path(path: str, rules: Iterable[OutputRule]) -> list[OutputRule]:
    return [rule for rule in rules if rule.matcher.fullmatch(path)]


def concrete_owned_outputs(root: Path, rules: list[OutputRule] | None = None) -> dict[str, str]:
    """Resolve concrete consequences without losing ignored literal outputs.

    Git topology is authoritative for discovering patterned repository families. A
    literal manifest consequence has stronger identity: its exact path is already
    admitted, so existence is observed directly from the filesystem even when the
    repository intentionally ignores generated projections.
    """
    rules = rules or declared_output_rules(root)
    owned: dict[str, str] = {}
    ambiguous: dict[str, list[str]] = {}

    for rule in rules:
        if rule.literal and (root / rule.output_file).is_file():
            owned[rule.output_file] = rule.owner

    for path in repository_paths(root):
        matches = owners_for_path(path, rules)
        if len(matches) == 1:
            owned[path] = matches[0].owner
        elif len(matches) > 1:
            ambiguous[path] = sorted(rule.owner for rule in matches)

    if ambiguous:
        raise SystemExit(f"ambiguous root output owner(s): {json.dumps(ambiguous, sort_keys=True)}")
    return dict(sorted(owned.items()))


def consequence_snapshot(root: Path) -> dict[str, str]:
    """Hash every concrete path currently owned by a root generation rule."""
    rules = declared_output_rules(root)
    owned = concrete_owned_outputs(root, rules)
    result: dict[str, str] = {}
    for path in owned:
        file_path = root / path
        if file_path.is_file():
            result[path] = hashlib.sha256(file_path.read_bytes()).hexdigest()
    return result


def write_snapshot(root: Path, destination: Path) -> dict[str, str]:
    snapshot = consequence_snapshot(root)
    destination.write_text(json.dumps(snapshot, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return snapshot


def compare_snapshot(root: Path, source: Path) -> tuple[bool, dict[str, Any]]:
    expected = json.loads(source.read_text(encoding="utf-8"))
    actual = consequence_snapshot(root)
    return actual == expected, {"expected": expected, "actual": actual}


def verify(root: Path) -> dict[str, Any]:
    rules = declared_output_rules(root)
    changed = changed_paths(root)

    generated: list[str] = []
    generated_owners: dict[str, str] = {}
    ambiguous: dict[str, list[str]] = {}
    unauthorized: list[str] = []

    for path in changed:
        if is_evidence_path(path):
            continue
        matches = owners_for_path(path, rules)
        if len(matches) == 1:
            generated.append(path)
            generated_owners[path] = matches[0].owner
        elif len(matches) > 1:
            ambiguous[path] = sorted(rule.owner for rule in matches)
        else:
            unauthorized.append(path)

    literal_rules = [rule for rule in rules if rule.literal]
    pattern_rules = [rule for rule in rules if not rule.literal]
    missing = [
        rule.output_file
        for rule in literal_rules
        if not (root / rule.output_file).is_file()
    ]
    evidence = [path for path in changed if is_evidence_path(path)]

    concrete_owned = concrete_owned_outputs(root, rules)
    pattern_matches = {
        rule.output_file: sum(1 for path in concrete_owned if rule.matcher.fullmatch(path))
        for rule in pattern_rules
    }

    return {
        "schema": "ggen.root-regeneration.verification.v3",
        "revision": git(root, "rev-parse", "HEAD").decode("ascii").strip(),
        "declared_output_count": len(rules),
        "declared_literal_output_count": len(literal_rules),
        "declared_pattern_output_count": len(pattern_rules),
        "concrete_owned_output_count": len(concrete_owned),
        "pattern_concrete_match_counts": pattern_matches,
        "changed_paths": changed,
        "generated_output_paths": sorted(generated),
        "generated_output_owners": dict(sorted(generated_owners.items())),
        "receipt_evidence_paths": evidence,
        "unauthorized_paths": sorted(unauthorized),
        "ambiguous_output_owners": ambiguous,
        "missing_outputs": sorted(missing),
        "passed": not unauthorized and not ambiguous and not missing,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--report", type=Path, default=Path("self-host/observed/root-regeneration.json"))
    parser.add_argument("--write-snapshot", type=Path)
    parser.add_argument("--compare-snapshot", type=Path)
    args = parser.parse_args()
    root = args.root.resolve()

    if args.write_snapshot and args.compare_snapshot:
        parser.error("--write-snapshot and --compare-snapshot are mutually exclusive")

    if args.write_snapshot:
        destination = args.write_snapshot if args.write_snapshot.is_absolute() else root / args.write_snapshot
        destination.parent.mkdir(parents=True, exist_ok=True)
        snapshot = write_snapshot(root, destination)
        print(json.dumps({"snapshot": str(destination), "output_count": len(snapshot)}, sort_keys=True))
        return 0

    if args.compare_snapshot:
        source = args.compare_snapshot if args.compare_snapshot.is_absolute() else root / args.compare_snapshot
        passed, detail = compare_snapshot(root, source)
        if not passed:
            print(json.dumps(detail, indent=2, sort_keys=True), file=sys.stderr)
            return 1
        print(json.dumps({"replay": "byte-identical", "output_count": len(detail["actual"])}, sort_keys=True))
        return 0

    result = verify(root)
    report = args.report if args.report.is_absolute() else root / args.report
    report.parent.mkdir(parents=True, exist_ok=True)
    report.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    if not result["passed"]:
        print(json.dumps(result, indent=2, sort_keys=True), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
