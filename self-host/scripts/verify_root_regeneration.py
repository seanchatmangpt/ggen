#!/usr/bin/env python3
"""Verify that running root `ggen sync` changes only declared outputs or receipts.

This verifier is intentionally independent of ggen's renderer. It reads the authored
root manifest, observes Git worktree changes, and refuses any unowned consequence.
Execution receipts are admitted as evidence, never mislabeled as generation outputs.
"""
from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import tomllib
from pathlib import Path, PurePosixPath
from typing import Any

EVIDENCE_PREFIXES = (".ggen-v2/",)
_TEMPLATE_FIELD = re.compile(r"\{\{\s*[^{}]+?\s*\}\}")


def git(root: Path, *args: str) -> bytes:
    return subprocess.run(
        ["git", "-C", str(root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def declared_outputs(root: Path) -> dict[str, str]:
    manifest = tomllib.loads((root / "ggen.toml").read_text(encoding="utf-8"))
    generation = manifest.get("generation", {})
    rules = generation.get("rules", []) if isinstance(generation, dict) else []
    outputs: dict[str, str] = {}
    for index, rule in enumerate(rules):
        if not isinstance(rule, dict) or "output_file" not in rule:
            continue
        output = PurePosixPath(str(rule["output_file"]))
        if output.is_absolute() or ".." in output.parts:
            raise SystemExit(f"unsafe root generation output: {output}")
        path = output.as_posix()
        owner = str(rule.get("name", f"rule-{index:03d}"))
        if path in outputs:
            raise SystemExit(f"duplicate root output owner: {path}: {outputs[path]} and {owner}")
        _compile_output_pattern(path)
        outputs[path] = owner
    if not outputs:
        raise SystemExit("root ggen.toml declares zero generation outputs")
    return outputs


def _compile_output_pattern(template: str) -> re.Pattern[str]:
    """Compile a bounded output template; each Tera field owns one path segment only."""
    cursor = 0
    chunks: list[str] = ["^"]
    matches = list(_TEMPLATE_FIELD.finditer(template))
    for match in matches:
        chunks.append(re.escape(template[cursor : match.start()]))
        chunks.append(r"[^/]+")
        cursor = match.end()
    chunks.append(re.escape(template[cursor:]))
    chunks.append("$")
    if ("{{" in template or "}}" in template) and not matches:
        raise SystemExit(f"unsupported root output template: {template}")
    residual = _TEMPLATE_FIELD.sub("", template)
    if "{{" in residual or "}}" in residual:
        raise SystemExit(f"unsupported root output template: {template}")
    return re.compile("".join(chunks))


def output_owner(outputs: dict[str, str], path: str) -> str | None:
    """Return the unique rule owning a concrete output path, refusing ambiguity."""
    owners = [
        owner
        for template, owner in outputs.items()
        if _compile_output_pattern(template).fullmatch(path)
    ]
    if len(owners) > 1:
        raise SystemExit(f"ambiguous root output owner: {path}: {', '.join(sorted(owners))}")
    return owners[0] if owners else None


def _template_search_root(root: Path, template: str) -> Path:
    literal_prefix = template.split("{{", 1)[0]
    prefix = PurePosixPath(literal_prefix)
    parent = prefix if literal_prefix.endswith("/") else prefix.parent
    candidate = root / parent
    return candidate if candidate.is_dir() else root


def existing_output_matches(root: Path, template: str) -> list[str]:
    pattern = _compile_output_pattern(template)
    if not _TEMPLATE_FIELD.search(template):
        return [template] if (root / template).is_file() else []
    matches: list[str] = []
    search_root = _template_search_root(root, template)
    for candidate in search_root.rglob("*"):
        if not candidate.is_file():
            continue
        relative = candidate.relative_to(root).as_posix()
        if pattern.fullmatch(relative):
            matches.append(relative)
    return sorted(matches)


def nul_paths(raw: bytes) -> set[str]:
    return {item.decode("utf-8") for item in raw.split(b"\0") if item}


def changed_paths(root: Path) -> list[str]:
    paths = set()
    paths |= nul_paths(git(root, "diff", "--name-only", "-z"))
    paths |= nul_paths(git(root, "diff", "--cached", "--name-only", "-z"))
    paths |= nul_paths(git(root, "ls-files", "--others", "--exclude-standard", "-z"))
    return sorted(paths)


def is_evidence_path(path: str) -> bool:
    return path.startswith(EVIDENCE_PREFIXES)


def verify(root: Path) -> dict[str, Any]:
    outputs = declared_outputs(root)
    changed = changed_paths(root)
    generated_owners = {
        path: owner
        for path in changed
        if (owner := output_owner(outputs, path)) is not None
    }
    generated = sorted(generated_owners)
    evidence = [path for path in changed if is_evidence_path(path)]
    unauthorized = [
        path
        for path in changed
        if path not in generated_owners and not is_evidence_path(path)
    ]
    missing = [
        template
        for template in outputs
        if not existing_output_matches(root, template)
    ]
    return {
        "schema": "ggen.root-regeneration.verification.v1",
        "revision": git(root, "rev-parse", "HEAD").decode("ascii").strip(),
        "declared_output_count": len(outputs),
        "changed_paths": changed,
        "generated_output_paths": generated,
        "generated_output_owners": generated_owners,
        "receipt_evidence_paths": evidence,
        "unauthorized_paths": unauthorized,
        "missing_outputs": sorted(missing),
        "passed": not unauthorized and not missing,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--report", type=Path, default=Path("self-host/observed/root-regeneration.json"))
    args = parser.parse_args()
    root = args.root.resolve()
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
