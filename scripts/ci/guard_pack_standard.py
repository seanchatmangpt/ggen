#!/usr/bin/env python3
"""Admit changed ggen packs against the canonical pack-authoring contract.

The pack-authoring ontology/reference defines the minimum pack surface as:

* ``pack.toml`` with a non-empty ``[pack]`` name/version/description;
* non-empty ``ontology.ttl``; and
* ``templates/`` with at least one non-empty ``*.tmpl`` projection.

Pack-local gates are optional in the canonical reference. Chicago-TDD coverage
is enforced by the repository's separate pack-e2e coverage rail, so this guard
does not duplicate that policy.

CI should pass ``--changed-since <base-sha>``. That validates every pack whose
bytes changed between the admitted base and HEAD, while leaving unrelated
legacy debt visible but non-blocking. With no selector this command remains a
full-repository audit. ``--pack`` provides an explicit narrow audit.
"""
from __future__ import annotations

import argparse
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


@dataclass(frozen=True)
class Violation:
    code: str
    pack: str
    detail: str

    def render(self) -> str:
        return f"REFUSED:{self.code}:{self.pack}:{self.detail}"


def _is_nonempty_file(path: Path) -> bool:
    if not path.is_file():
        return False
    try:
        return bool(path.read_text(encoding="utf-8").strip())
    except UnicodeDecodeError:
        return path.stat().st_size > 0


def _manifest_violations(pack_dir: Path) -> list[str]:
    manifest = pack_dir / "pack.toml"
    if not _is_nonempty_file(manifest):
        return ["missing-or-empty=pack.toml"]
    try:
        parsed = tomllib.loads(manifest.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, tomllib.TOMLDecodeError) as error:
        return [f"invalid-pack-manifest={error}"]

    pack = parsed.get("pack")
    if not isinstance(pack, dict):
        return ["missing [pack] table"]

    problems: list[str] = []
    expected_name = pack_dir.name
    name = pack.get("name")
    if not isinstance(name, str) or not name.strip():
        problems.append("[pack].name must be non-empty")
    elif name != expected_name:
        problems.append(f"[pack].name={name!r} must match directory {expected_name!r}")

    for field in ("version", "description"):
        value = pack.get(field)
        if not isinstance(value, str) or not value.strip():
            problems.append(f"[pack].{field} must be non-empty")
    return problems


def _has_template(pack_dir: Path) -> bool:
    templates = pack_dir / "templates"
    return templates.is_dir() and any(_is_nonempty_file(path) for path in templates.rglob("*.tmpl"))


def validate_pack(pack_dir: Path) -> list[Violation]:
    pack = pack_dir.name
    violations: list[Violation] = []

    for detail in _manifest_violations(pack_dir):
        violations.append(Violation("PACK-CREATE-STANDARD-MANIFEST", pack, detail))

    if not _is_nonempty_file(pack_dir / "ontology.ttl"):
        violations.append(
            Violation(
                "PACK-CREATE-STANDARD-ONTOLOGY",
                pack,
                f"expected non-empty packs/{pack}/ontology.ttl",
            )
        )

    if not _has_template(pack_dir):
        violations.append(
            Violation(
                "PACK-CREATE-STANDARD-TEMPLATE",
                pack,
                f"expected at least one non-empty packs/{pack}/templates/*.tmpl",
            )
        )

    return violations


def _git_lines(root: Path, args: list[str]) -> list[str]:
    result = subprocess.run(
        ["git", *args],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or f"git {' '.join(args)} failed")
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def _pack_names_from_paths(paths: Iterable[str]) -> list[str]:
    names: set[str] = set()
    for raw in paths:
        parts = Path(raw).parts
        if len(parts) >= 3 and parts[0] == "packs":
            names.add(parts[1])
    return sorted(names)


def changed_pack_names(root: Path, base: str) -> list[str]:
    paths: set[str] = set()
    # Committed candidate delta. In pre-commit mode (base=HEAD), this is empty.
    paths.update(_git_lines(root, ["diff", "--name-only", "--diff-filter=ACMR", f"{base}...HEAD"]))
    # Also admit staged/unstaged and untracked pack bytes for local pre-commit use.
    paths.update(_git_lines(root, ["diff", "--name-only", "--diff-filter=ACMR", "HEAD"]))
    paths.update(_git_lines(root, ["ls-files", "--others", "--exclude-standard", "--", "packs"]))
    return _pack_names_from_paths(paths)


def discover_packs(
    root: Path,
    selected: list[str],
    *,
    deletion_is_ok: bool = False,
) -> tuple[list[Path], list[Violation]]:
    packs_root = root / "packs"
    if selected:
        pack_dirs: list[Path] = []
        selection_errors: list[Violation] = []
        for name in sorted(set(selected)):
            pack_dir = packs_root / name
            if not pack_dir.exists() and deletion_is_ok:
                continue
            if not (pack_dir / "pack.toml").is_file():
                selection_errors.append(
                    Violation(
                        "PACK-CREATE-STANDARD-SELECTION",
                        name,
                        f"missing pack manifest packs/{name}/pack.toml",
                    )
                )
            else:
                pack_dirs.append(pack_dir)
        return pack_dirs, selection_errors

    pack_dirs = sorted(path.parent for path in packs_root.glob("*/pack.toml") if path.is_file())
    if pack_dirs:
        return pack_dirs, []
    return [], [
        Violation(
            "PACK-CREATE-STANDARD-INVENTORY",
            "<repository>",
            "no packs/*/pack.toml manifests discovered",
        )
    ]


def audit(
    root: Path,
    selected: list[str] | None = None,
    *,
    changed_since: str | None = None,
) -> tuple[list[Path], list[Violation]]:
    root = root.resolve()
    names = list(selected or [])
    deletion_is_ok = False
    if changed_since is not None:
        names = changed_pack_names(root, changed_since)
        deletion_is_ok = True
        if not names:
            return [], []

    pack_dirs, violations = discover_packs(root, names, deletion_is_ok=deletion_is_ok)
    for pack_dir in pack_dirs:
        violations.extend(validate_pack(pack_dir))
    return pack_dirs, violations


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd(), help="repository root (default: cwd)")
    selector = parser.add_mutually_exclusive_group()
    selector.add_argument(
        "--pack",
        action="append",
        default=[],
        help="validate only this pack directory name; may be repeated",
    )
    selector.add_argument(
        "--changed-since",
        help="validate packs changed from this admitted git base through HEAD (plus local worktree changes)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        pack_dirs, violations = audit(args.root, args.pack, changed_since=args.changed_since)
    except RuntimeError as error:
        print(f"REFUSED:PACK-CREATE-STANDARD-GIT:<repository>:{error}", file=sys.stderr)
        return 2

    if violations:
        scope = "changed" if args.changed_since else "admitted"
        print(
            f"BUILD_BROKEN:PACK_CREATE_STANDARD:{len(violations)} violation(s) across {len(pack_dirs)} {scope} pack(s)",
            file=sys.stderr,
        )
        for violation in violations:
            print(violation.render(), file=sys.stderr)
        return 1

    names = ",".join(pack.name for pack in pack_dirs)
    scope = "changed" if args.changed_since else "pack"
    print(f"ALIVE:PACK_CREATE_STANDARD:{len(pack_dirs)} {scope}(s):{names}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
