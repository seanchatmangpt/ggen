#!/usr/bin/env python3
"""Fail closed when a ggen pack lacks the canonical product surfaces.

A pack is any direct child of ``packs/`` that contains ``pack.toml``.
Every pack must carry:

* ``examples/`` with at least one non-empty artifact other than README/.gitkeep;
* ``playground/ggen.toml`` and ``playground/ontology.ttl``; the playground
  manifest must parse as TOML and declare at least one pack dependency; and
* all four Diataxis quadrants under ``docs/``: tutorial, how-to, reference,
  and explanation. A quadrant may use the canonical filename (for example
  ``tutorial.md``) or a suffixed family (for example ``tutorial-first-run.md``).

The guard has no legacy baseline and no allowlist. A missing surface is a
hard, typed refusal. ``--pack`` can narrow validation to one or more packs so
pack-by-pack migrations can be verified independently.
"""
from __future__ import annotations

import argparse
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


DIATAXIS = {
    "tutorial": ("tutorial.md", "tutorial-*.md"),
    "how-to": ("how-to.md", "how-to-*.md"),
    "reference": ("reference.md", "reference-*.md"),
    "explanation": ("explanation.md", "explanation-*.md"),
}

IGNORED_EXAMPLE_FILES = {"readme.md", ".gitkeep"}


def _is_nonempty_file(path: Path) -> bool:
    if not path.is_file():
        return False
    try:
        return bool(path.read_text(encoding="utf-8").strip())
    except UnicodeDecodeError:
        return path.stat().st_size > 0


def _has_real_example(pack_dir: Path) -> bool:
    examples = pack_dir / "examples"
    if not examples.is_dir():
        return False
    return any(
        _is_nonempty_file(path)
        for path in examples.rglob("*")
        if path.name.lower() not in IGNORED_EXAMPLE_FILES
    )


def _playground_violations(pack_dir: Path) -> list[str]:
    playground = pack_dir / "playground"
    required = (playground / "ggen.toml", playground / "ontology.ttl")
    missing = [str(path.relative_to(pack_dir)) for path in required if not _is_nonempty_file(path)]
    if missing:
        return [f"missing-or-empty={','.join(missing)}"]

    manifest = playground / "ggen.toml"
    try:
        parsed = tomllib.loads(manifest.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, tomllib.TOMLDecodeError) as error:
        return [f"invalid-playground-manifest={manifest.relative_to(pack_dir)}:{error}"]

    packs = parsed.get("packs")
    if not isinstance(packs, dict) or not packs:
        return ["playground/ggen.toml must declare at least one [packs] dependency"]
    return []


def _has_diataxis_quadrant(docs: Path, patterns: Iterable[str]) -> bool:
    for pattern in patterns:
        if any(_is_nonempty_file(path) for path in docs.glob(pattern)):
            return True
    return False


def validate_pack(pack_dir: Path) -> list[Violation]:
    pack = pack_dir.name
    violations: list[Violation] = []

    if not _has_real_example(pack_dir):
        violations.append(
            Violation(
                "PACK-CREATE-STANDARD-EXAMPLE",
                pack,
                "expected non-empty packs/%s/examples artifact (README/.gitkeep do not count)" % pack,
            )
        )

    for detail in _playground_violations(pack_dir):
        violations.append(Violation("PACK-CREATE-STANDARD-PLAYGROUND", pack, detail))

    docs = pack_dir / "docs"
    for quadrant, patterns in DIATAXIS.items():
        if not docs.is_dir() or not _has_diataxis_quadrant(docs, patterns):
            expected = "|".join(f"docs/{pattern}" for pattern in patterns)
            violations.append(
                Violation(
                    "PACK-CREATE-STANDARD-DIATAXIS",
                    pack,
                    f"quadrant={quadrant}; expected={expected}",
                )
            )

    return violations


def discover_packs(root: Path, selected: list[str]) -> tuple[list[Path], list[Violation]]:
    packs_root = root / "packs"
    if selected:
        pack_dirs: list[Path] = []
        selection_errors: list[Violation] = []
        for name in sorted(set(selected)):
            pack_dir = packs_root / name
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


def audit(root: Path, selected: list[str] | None = None) -> tuple[list[Path], list[Violation]]:
    pack_dirs, violations = discover_packs(root.resolve(), selected or [])
    for pack_dir in pack_dirs:
        violations.extend(validate_pack(pack_dir))
    return pack_dirs, violations


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd(), help="repository root (default: cwd)")
    parser.add_argument(
        "--pack",
        action="append",
        default=[],
        help="validate only this pack directory name; may be repeated",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    pack_dirs, violations = audit(args.root, args.pack)
    if violations:
        print(
            f"BUILD_BROKEN:PACK_CREATE_STANDARD:{len(violations)} violation(s) across {len(pack_dirs)} admitted pack(s)",
            file=sys.stderr,
        )
        for violation in violations:
            print(violation.render(), file=sys.stderr)
        return 1

    names = ",".join(pack.name for pack in pack_dirs)
    print(f"ALIVE:PACK_CREATE_STANDARD:{len(pack_dirs)} pack(s):{names}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
