#!/usr/bin/env python3
"""Deterministic changed-file router for ggen's 80/20 ERRC CI.

The router is intentionally standard-library only.  It separates universal
admission from path-owned deep evidence and emits stable JSON plus exact
GitHub Actions Boolean outputs.
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from typing import Iterable, Sequence

LANES: tuple[str, ...] = (
    "ci_deep",
    "core_deep",
    "integration_deep",
    "quality_deep",
    "security_deep",
    "docs_deep",
    "lsp_deep",
    "source_removal_deep",
)

CI_EXACT = {
    ".github/workflows/ci.yml",
    ".github/workflows/quality.yml",
    ".github/actions/setup-ggen-build/action.yml",
    ".github/actions/install-cargo-tools/action.yml",
    "scripts/ci/errc_router.py",
    "scripts/ci/fast_admission.py",
    "scripts/ci/test_errc_router.py",
    "scripts/ci/analyze-g0-workflow-topology.py",
    "scripts/ci/test-g0-workflow-inventory.py",
    "scripts/ci/verify-g0-workflow-inventory.py",
    "packs/github-actions-pack/observations/g0-workflow-inventory-v26.7.31.toml",
}

ROOT_BUILD_FILES = {
    "Cargo.toml",
    "Cargo.lock",
    "rust-toolchain.toml",
    "rustfmt.toml",
    "justfile",
    "deny.toml",
}

SOURCE_REMOVAL_EXACT = {
    "scripts/ci/v26_8_1_source_removal.py",
    "evidence/ggen-v26.8.1-source-removal-plan.json",
    "evidence/ggen-v26.8.1-source-removal-receipt.json",
}


def typed_failure(reason: str, detail: str) -> RuntimeError:
    return RuntimeError(f"REFUSED:{reason}: {detail}")


def normalize_path(raw: str) -> str:
    value = raw.strip().replace("\\", "/")
    while value.startswith("./"):
        value = value[2:]
    if not value:
        raise typed_failure("INVALID_CHANGED_PATH", "empty path")
    path = PurePosixPath(value)
    if path.is_absolute() or any(part in {"", ".", ".."} for part in path.parts):
        raise typed_failure("INVALID_CHANGED_PATH", raw)
    return path.as_posix()


def normalize_paths(paths: Iterable[str]) -> list[str]:
    return sorted({normalize_path(path) for path in paths})


def _is_markdown(path: str) -> bool:
    name = PurePosixPath(path).name.lower()
    return path.startswith("docs/") or name.startswith("readme") or path.endswith((".md", ".mdx"))


def _is_rust_surface(path: str) -> bool:
    return (
        path in ROOT_BUILD_FILES
        or path.startswith(("crates/", "tests/", "benches/", "examples/"))
        or path.endswith(".rs")
    )


def _is_integration_surface(path: str) -> bool:
    return (
        path in {"Cargo.toml", "Cargo.lock", "rust-toolchain.toml", "justfile"}
        or path.startswith(("tests/", "examples/"))
        or "/tests/" in path
        or (path.startswith("crates/") and path.endswith(".rs"))
    )


def _is_security_surface(path: str) -> bool:
    return path in {"Cargo.toml", "Cargo.lock", "deny.toml"} or path.startswith(
        (".github/dependabot", "scripts/security", "security/")
    )


def classify_path(path: str) -> set[str]:
    lanes: set[str] = set()

    # Workflow definitions are CI subjects, not ambient product subjects.
    if path.startswith((".github/workflows/", ".github/actions/")) or path in CI_EXACT:
        lanes.add("ci_deep")

    if _is_markdown(path):
        lanes.add("docs_deep")

    if path.startswith("crates/ggen-lsp/") or path == ".github/workflows/ggen-lsp-runtime-crown.yml":
        lanes.update({"lsp_deep", "quality_deep"})
        return lanes

    if _is_rust_surface(path):
        lanes.update({"core_deep", "quality_deep"})
    if _is_integration_surface(path):
        lanes.add("integration_deep")
    if _is_security_surface(path):
        lanes.add("security_deep")

    if path in SOURCE_REMOVAL_EXACT or path.startswith("evidence/ggen-v26.8.1-source-removal-"):
        lanes.add("source_removal_deep")

    # CI implementation changes prove the control plane, but must not
    # accidentally activate product, migration, docs, or release evidence.
    if path in CI_EXACT:
        lanes.difference_update(
            {"core_deep", "integration_deep", "quality_deep", "security_deep", "docs_deep"}
        )
        lanes.add("ci_deep")

    return lanes


@dataclass(frozen=True)
class RoutingReport:
    changed_files: tuple[str, ...]
    routing: dict[str, tuple[str, ...]]

    @property
    def booleans(self) -> dict[str, bool]:
        return {lane: bool(self.routing[lane]) for lane in LANES}

    def to_json_object(self) -> dict[str, object]:
        return {
            "schema": "ggen.ci.errc.routing.v1",
            "changed_files": list(self.changed_files),
            "routing": {lane: list(self.routing[lane]) for lane in LANES},
            "outputs": self.booleans,
        }


def route(paths: Iterable[str]) -> RoutingReport:
    changed = normalize_paths(paths)
    routing: dict[str, list[str]] = {lane: [] for lane in LANES}
    for path in changed:
        for lane in sorted(classify_path(path)):
            routing[lane].append(path)
    return RoutingReport(
        changed_files=tuple(changed),
        routing={lane: tuple(routing[lane]) for lane in LANES},
    )


def discover_changed_files(base: str, head: str, repo: Path) -> list[str]:
    if not base or not head:
        raise typed_failure("CHANGED_FILE_DISCOVERY_FAILED", "base and head are required")
    command = ["git", "-C", str(repo), "diff", "--name-only", "--diff-filter=ACMRD", f"{base}...{head}"]
    result = subprocess.run(command, text=True, capture_output=True, check=False)
    if result.returncode != 0:
        detail = (result.stderr or result.stdout).strip()[-600:]
        raise typed_failure("CHANGED_FILE_DISCOVERY_FAILED", detail or f"exit={result.returncode}")
    return [line for line in result.stdout.splitlines() if line.strip()]


def write_github_outputs(path: Path, report: RoutingReport) -> None:
    lines = [f"{lane}={'true' if enabled else 'false'}" for lane, enabled in report.booleans.items()]
    active = [lane for lane in LANES if lane != "source_removal_deep" and report.routing[lane]]
    matrix = {"include": [{"lane": lane} for lane in (active or ["fast_only"])]}
    lines.append("deep_matrix_json=" + json.dumps(matrix, separators=(",", ":")))
    lines.append("changed_files_json=" + json.dumps(list(report.changed_files), separators=(",", ":")))
    with path.open("a", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", default="")
    parser.add_argument("--head", default="")
    parser.add_argument("--repo", type=Path, default=Path("."))
    parser.add_argument("--changed-file", action="append", default=[])
    parser.add_argument("--changed-files-json")
    parser.add_argument("--report", type=Path)
    parser.add_argument("--github-output", type=Path)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    explicit: list[str] = list(args.changed_file)
    if args.changed_files_json:
        payload = json.loads(args.changed_files_json)
        if not isinstance(payload, list) or not all(isinstance(item, str) for item in payload):
            raise typed_failure("INVALID_CHANGED_FILE_INJECTION", "expected a JSON string array")
        explicit.extend(payload)
    paths = explicit if explicit else discover_changed_files(args.base, args.head, args.repo)
    report = route(paths)
    rendered = json.dumps(report.to_json_object(), indent=2, sort_keys=True) + "\n"
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(rendered, encoding="utf-8")
    else:
        sys.stdout.write(rendered)
    output_path = args.github_output or (Path(os.environ["GITHUB_OUTPUT"]) if os.environ.get("GITHUB_OUTPUT") else None)
    if output_path:
        write_github_outputs(output_path, report)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # typed, bounded CLI failure
        print(str(exc), file=sys.stderr)
        raise SystemExit(2)
