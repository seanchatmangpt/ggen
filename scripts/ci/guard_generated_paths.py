#!/usr/bin/env python3
"""guard_generated_paths: fail closed on any `generated` file/dir under crates/.

Closes the H09 class: the repo-wide .gitignore pattern `generated/` silently
excludes src/generated/{mod,refusals}.rs from `cargo package` tarballs, which
breaks `cargo publish` with E0583 ("file not found for module `generated`").

Rules:
  1. A `generated` path under crates/*/src/ requires its crate's [package]
     to carry an explicit `include` list whose patterns cover the path
     (fnmatch on the path relative to the crate root). Covered -> PASS row;
     uncovered -> violation (exit 2).
  2. A `generated` path anywhere else under crates/ (tests/, benches/, crate
     root, ...) is a violation (exit 2) unless listed in ALLOWED below with a
     one-line reason.

Exits: 0 clean, 2 violation(s) found.
"""

from __future__ import annotations

import fnmatch
import sys
import tomllib
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# Sanctioned non-src `generated` paths, relative to the repo root, with a
# one-line reason each. Keep this list explicit and justified.
ALLOWED: dict[str, str] = {
    "crates/ggen-cli/tests/generated/cli_proof_tests.rs":
        "test fixture consumed by ggen-cli integration tests; never compiled into the published crate",
    "crates/ggen-config/tests/generated/generated_mutation.rs":
        "mutation-court test fixture; tests/ is not packaged by cargo publish",
    "crates/ggen-lsp/generated/lsp-contract.json":
        "checked-in contract corpus read by ggen-lsp tests at runtime, not a Rust module; not under src/",
}

HAZARD = (
    "gitignored by the repo-wide `generated/` pattern; cargo publish will "
    "drop it; add an explicit [package] include entry"
)


def _include_covers(rel_path: str, patterns: list[str]) -> bool:
    """True if any include pattern matches rel_path (fnmatch semantics)."""
    return any(
        fnmatch.fnmatch(rel_path, pat)
        or fnmatch.fnmatch(rel_path, pat.rstrip("/") + "/*")
        or (pat.endswith("/**/*") and rel_path.startswith(pat[: -len("*")]))
        for pat in patterns
    )


def find_generated_paths(crates_dir: Path) -> list[Path]:
    return sorted(
        p for p in crates_dir.rglob("generated")
        if p.name == "generated" or p.parent.name == "generated"
    )


def check(root: Path) -> list[str]:
    violations: list[str] = []
    for hit in find_generated_paths(root / "crates"):
        rel = hit.relative_to(root).as_posix()
        if rel in ALLOWED or any(a.startswith(rel + "/") for a in ALLOWED):
            print(f"PASS  {rel} (allowed: {ALLOWED.get(rel, 'covered by an ALLOWED entry')})")
            continue
        parts = hit.relative_to(root / "crates").parts
        crate = parts[0]
        if len(parts) >= 3 and parts[1] == "src":
            # src/generated: require the crate's include list to cover it
            manifest = root / "crates" / crate / "Cargo.toml"
            try:
                pkg = tomllib.loads(manifest.read_text())["package"]
            except (OSError, KeyError, tomllib.TOMLDecodeError) as exc:
                violations.append(
                    f"VIOLATION {rel}: cannot read [package] from {manifest}: {exc}. {HAZARD}"
                )
                continue
            patterns = pkg.get("include")
            if patterns is None:
                violations.append(
                    f"VIOLATION {rel} (crate {crate}): no [package] include list. {HAZARD}"
                )
                continue
            covered = [
                f for f in ([str(hit)] if hit.is_file() else sorted(str(p) for p in hit.rglob("*") if p.is_file()))
                if _include_covers(Path(f).relative_to(root / "crates" / crate).as_posix(), patterns)
            ]
            expected = 0 if hit.is_file() else len([p for p in hit.rglob("*") if p.is_file()])
            if len(covered) == expected and expected > 0:
                print(f"PASS  {rel} (covered by {crate} [package] include)")
            else:
                violations.append(
                    f"VIOLATION {rel} (crate {crate}): include list {patterns} "
                    f"does not cover the generated path(s). {HAZARD}"
                )
        else:
            violations.append(
                f"VIOLATION {rel}: `generated` outside crates/*/src/ without an "
                f"ALLOWED entry. {HAZARD}"
            )
    return violations


def main() -> int:
    violations = check(REPO_ROOT)
    if violations:
        for v in violations:
            print(v, file=sys.stderr)
        print(f"\nguard-generated-paths: {len(violations)} violation(s)", file=sys.stderr)
        return 2
    print("guard-generated-paths: clean")
    return 0


if __name__ == "__main__":
    sys.exit(main())
