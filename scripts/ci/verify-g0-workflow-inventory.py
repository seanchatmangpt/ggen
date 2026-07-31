#!/usr/bin/env python3
"""Verify the G0 CI/CD workflow inventory and Chesterton fences.

The verifier crosses the repository filesystem boundary, derives workflow facts
from the checked-in YAML, joins them to the admitted semantic inventory, and
writes deterministic evidence. It never changes workflow or release behavior.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
import tomllib
from collections import defaultdict
from pathlib import Path
from typing import Any

INVENTORY_REFUSAL = "CI-G0-INVENTORY-001"
OWNERSHIP_REFUSAL = "CI-G0-OWNERSHIP-001"
SEMANTIC_REFUSAL = "CI-G0-FENCE-001"

REQUIRED_FIELDS = (
    "path",
    "owner",
    "purpose",
    "evidence_output",
    "retirement_condition",
    "production_outputs",
)

TOP_KEY = re.compile(r"^([\"']?)([A-Za-z_][A-Za-z0-9_-]*)\1\s*:(.*)$")
CHILD_KEY = re.compile(r"^([\"']?)([A-Za-z_][A-Za-z0-9_.-]*)\1\s*:(.*)$")
USES_LINE = re.compile(r"^\s*-?\s*uses\s*:\s*['\"]?([^'\"#]+?)['\"]?\s*(?:#.*)?$")
RUN_LINE = re.compile(r"^(\s*)-?\s*run\s*:\s*(.*)$")

SHELL_CONTROL_PREFIXES = (
    "set ",
    "echo ",
    "printf ",
    "if ",
    "then",
    "else",
    "elif ",
    "fi",
    "for ",
    "while ",
    "do",
    "done",
    "case ",
    "esac",
    "export ",
    "cd ",
    "test ",
    "[ ",
    "[[ ",
)


def repository_root() -> Path:
    return Path(__file__).resolve().parents[2]


def strip_comment(line: str) -> str:
    out: list[str] = []
    single = False
    double = False
    for char in line:
        if char == "'" and not double:
            single = not single
        elif char == '"' and not single:
            double = not double
        elif char == "#" and not single and not double:
            break
        out.append(char)
    return "".join(out)


def indentation(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def top_level_block(lines: list[str], key: str) -> tuple[str, list[str]] | None:
    start: int | None = None
    inline = ""
    for index, raw in enumerate(lines):
        if raw and raw[0] not in (" ", "\t"):
            match = TOP_KEY.match(raw)
            if match and match.group(2) == key:
                start = index
                inline = strip_comment(match.group(3)).strip()
                break
    if start is None:
        return None

    body: list[str] = []
    for raw in lines[start + 1 :]:
        if raw.strip() and raw[0] not in (" ", "\t") and TOP_KEY.match(raw):
            break
        body.append(raw)
    return inline, body


def first_level_keys(body: list[str]) -> list[str]:
    candidate_indents = [
        indentation(raw)
        for raw in body
        if raw.strip() and not raw.lstrip().startswith("#")
    ]
    if not candidate_indents:
        return []
    child_indent = min(candidate_indents)
    keys: list[str] = []
    for raw in body:
        if indentation(raw) != child_indent:
            continue
        match = CHILD_KEY.match(strip_comment(raw[child_indent:]).strip())
        if match:
            keys.append(match.group(2))
    return keys


def parse_flow_keys(inline: str) -> list[str]:
    flattened = inline.strip().strip("[]{}")
    if not flattened:
        return []
    tokens = re.split(r"[,\s:{}]+", flattened)
    return sorted({token.strip("'\"") for token in tokens if token.strip("'\"")})


def parse_name(lines: list[str], fallback: str) -> str:
    for raw in lines:
        if raw and raw[0] not in (" ", "\t"):
            match = TOP_KEY.match(strip_comment(raw))
            if match and match.group(2) == "name":
                return match.group(3).strip().strip("'\"") or fallback
    return fallback


def parse_triggers(lines: list[str]) -> list[str]:
    block = top_level_block(lines, "on")
    if block is None:
        return []
    inline, body = block
    return parse_flow_keys(inline) if inline else sorted(set(first_level_keys(body)))


def parse_permissions(lines: list[str]) -> list[str]:
    block = top_level_block(lines, "permissions")
    if block is None:
        return ["implicit-default"]
    inline, body = block
    if inline:
        return parse_flow_keys(inline) or [inline]

    permissions: list[str] = []
    for raw in body:
        clean = strip_comment(raw).strip()
        if not clean or ":" not in clean:
            continue
        key, value = clean.split(":", 1)
        permissions.append(f"{key.strip()}:{value.strip() or 'unspecified'}")
    return sorted(set(permissions)) or ["empty"]


def parse_jobs(lines: list[str]) -> list[str]:
    block = top_level_block(lines, "jobs")
    if block is None:
        return []
    inline, body = block
    return parse_flow_keys(inline) if inline else sorted(set(first_level_keys(body)))


def normalize_command(raw: str) -> str | None:
    clean = strip_comment(raw).strip().rstrip("\\").strip()
    if not clean or clean in {"|", ">", "|-", ">-"}:
        return None
    if clean.startswith(SHELL_CONTROL_PREFIXES):
        return None
    if re.match(r"^[A-Za-z_][A-Za-z0-9_]*=", clean):
        return None
    clean = re.sub(r"\s+", " ", clean)
    return clean or None


def parse_command_families(lines: list[str]) -> list[str]:
    families: set[str] = set()
    index = 0
    while index < len(lines):
        raw = lines[index]
        uses = USES_LINE.match(raw)
        if uses:
            families.add(f"uses:{uses.group(1).strip()}")

        run = RUN_LINE.match(raw)
        if not run:
            index += 1
            continue

        run_indent = len(run.group(1))
        inline = run.group(2).strip()
        if inline and inline not in {"|", ">", "|-", ">-"}:
            command = normalize_command(inline)
            if command:
                families.add(f"run:{command}")
            index += 1
            continue

        index += 1
        while index < len(lines):
            nested = lines[index]
            if nested.strip() and indentation(nested) <= run_indent:
                break
            command = normalize_command(nested)
            if command:
                families.add(f"run:{command}")
            index += 1
    return sorted(families)


def detect_evidence_mechanisms(text: str) -> list[str]:
    mechanisms: list[str] = []
    probes = (
        ("actions/upload-artifact", "workflow-artifact"),
        ("actions/upload-pages-artifact", "pages-artifact"),
        ("actions/deploy-pages", "pages-deployment"),
        ("softprops/action-gh-release", "github-release"),
        ("git push", "git-ref-mutation"),
        ("VERIFIER_REPORT", "verifier-report"),
        ("receipt", "receipt-material"),
        ("sarif", "sarif"),
    )
    lowered = text.lower()
    for needle, label in probes:
        if needle.lower() in lowered:
            mechanisms.append(label)
    return mechanisms or ["workflow-log"]


def inspect_workflow(path: Path, repo_root: Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8")
    lines = text.splitlines()
    return {
        "path": path.relative_to(repo_root).as_posix(),
        "name": parse_name(lines, path.stem),
        "triggers": parse_triggers(lines),
        "permissions": parse_permissions(lines),
        "jobs": parse_jobs(lines),
        "command_families": parse_command_families(lines),
        "evidence_mechanisms": detect_evidence_mechanisms(text),
    }


def discover_workflows(repo_root: Path) -> list[Path]:
    workflow_dir = repo_root / ".github" / "workflows"
    return sorted(
        [*workflow_dir.glob("*.yml"), *workflow_dir.glob("*.yaml")],
        key=lambda path: path.name,
    )


def load_manifest(path: Path) -> dict[str, Any]:
    with path.open("rb") as handle:
        return tomllib.load(handle)


def validate_manifest(manifest: dict[str, Any], actual_paths: list[str]) -> list[str]:
    errors: list[str] = []
    workflows = manifest.get("workflow", [])
    expected_count = manifest.get("inventory", {}).get("expected_workflow_count")
    if expected_count != len(actual_paths):
        errors.append(
            f"{INVENTORY_REFUSAL}: expected_workflow_count={expected_count!r} "
            f"but discovered={len(actual_paths)}"
        )

    manifest_paths = [entry.get("path", "") for entry in workflows]
    missing = sorted(set(actual_paths) - set(manifest_paths))
    extra = sorted(set(manifest_paths) - set(actual_paths))
    duplicates = sorted({path for path in manifest_paths if manifest_paths.count(path) > 1})
    if missing or extra or duplicates:
        errors.append(
            f"{INVENTORY_REFUSAL}: workflow set mismatch; "
            f"missing={missing}; extra={extra}; duplicate_entries={duplicates}"
        )

    for entry in workflows:
        path = entry.get("path", "<missing-path>")
        absent = [field for field in REQUIRED_FIELDS if not entry.get(field)]
        if absent:
            errors.append(f"{SEMANTIC_REFUSAL}: {path} lacks fields {absent}")
        outputs = entry.get("production_outputs", [])
        if not isinstance(outputs, list) or not all(isinstance(item, str) and item for item in outputs):
            errors.append(f"{SEMANTIC_REFUSAL}: {path} has invalid production_outputs")

    output_owners: dict[str, dict[str, list[str]]] = defaultdict(lambda: defaultdict(list))
    for entry in workflows:
        owner = entry.get("owner", "")
        path = entry.get("path", "")
        for output in entry.get("production_outputs", []):
            output_owners[output][owner].append(path)
    for output, owners in sorted(output_owners.items()):
        if len(owners) > 1:
            errors.append(
                f"{OWNERSHIP_REFUSAL}: production output {output!r} has owners "
                f"{sorted(owners)} via {dict(owners)}"
            )
    return errors


def build_report(manifest: dict[str, Any], observed: list[dict[str, Any]]) -> dict[str, Any]:
    semantic_by_path = {entry["path"]: entry for entry in manifest["workflow"]}
    command_users: dict[str, list[str]] = defaultdict(list)
    rows: list[dict[str, Any]] = []

    for workflow in observed:
        for family in workflow["command_families"]:
            command_users[family].append(workflow["path"])
        semantic = semantic_by_path[workflow["path"]]
        rows.append({**workflow, **semantic})

    duplicates = {
        family: sorted(paths)
        for family, paths in sorted(command_users.items())
        if len(set(paths)) > 1
    }
    inventory = manifest["inventory"]
    return {
        "schema_version": inventory["schema_version"],
        "standing": "PARTIAL_ALIVE",
        "strongest_claim": (
            "The exact G0 workflow set is complete and each workflow has one semantic owner "
            "and an explicit Chesterton fence. Runtime equivalence and external standing are not claimed."
        ),
        "repository": inventory["repository"],
        "implementation_base_sha": inventory["implementation_base_sha"],
        "historical_inventory_sha": inventory["historical_inventory_sha"],
        "observed_workflow_count": len(rows),
        "branch_protection": manifest.get("branch_protection", {}),
        "queue_runtime_baseline": manifest.get("queue_runtime_baseline", {}),
        "duplicate_command_families": duplicates,
        "workflows": sorted(rows, key=lambda row: row["path"]),
        "exclusions": [
            "No workflow was added, deleted, disabled, or behaviorally changed by G0.",
            "No release or deployment actuation was changed.",
            "No branch-protection requirement is inferred from unavailable administrative state.",
            "PARTIAL_ALIVE is not release standing and cannot promote a source tree.",
        ],
    }


def render_markdown(report: dict[str, Any]) -> str:
    duplicate_count = len(report["duplicate_command_families"])
    lines = [
        "# G0 CI/CD As-Built Inventory Evidence",
        "",
        f"- Standing: `{report['standing']}`",
        f"- Repository: `{report['repository']}`",
        f"- Implementation base: `{report['implementation_base_sha']}`",
        f"- Workflows admitted: **{report['observed_workflow_count']}**",
        f"- Duplicate command families observed: **{duplicate_count}**",
        "",
        "## Workflow fences",
        "",
        "| Workflow | Owner | Triggers | Permission ceiling observed | Evidence | Retirement condition |",
        "|---|---|---|---|---|---|",
    ]
    for row in report["workflows"]:
        triggers = ", ".join(row["triggers"]) or "none-derived"
        permissions = ", ".join(row["permissions"])
        evidence = row["evidence_output"].replace("|", "\\|")
        retirement = row["retirement_condition"].replace("|", "\\|")
        lines.append(
            f"| `{row['path']}` | `{row['owner']}` | {triggers} | {permissions} | "
            f"{evidence} | {retirement} |"
        )
    lines.extend(["", "## Duplicate command map", ""])
    if report["duplicate_command_families"]:
        for family, paths in report["duplicate_command_families"].items():
            lines.append(f"- `{family}` — {', '.join(f'`{path}`' for path in paths)}")
    else:
        lines.append("- None observed.")
    lines.extend(["", "## Exclusions", ""])
    lines.extend(f"- {item}" for item in report["exclusions"])
    lines.append("")
    return "\n".join(lines)


def parse_args() -> argparse.Namespace:
    root = repository_root()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", type=Path, default=root)
    parser.add_argument(
        "--manifest",
        type=Path,
        default=root
        / "packs"
        / "github-actions-pack"
        / "observations"
        / "g0-workflow-inventory-v26.7.31.toml",
    )
    parser.add_argument("--report-dir", type=Path, default=root / "target" / "ci-g0")
    parser.add_argument("--no-write", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo_root = args.repo_root.resolve()
    manifest_path = args.manifest.resolve()
    workflows = discover_workflows(repo_root)
    actual_paths = [path.relative_to(repo_root).as_posix() for path in workflows]

    try:
        manifest = load_manifest(manifest_path)
    except (OSError, tomllib.TOMLDecodeError) as error:
        print(f"{SEMANTIC_REFUSAL}: cannot load manifest {manifest_path}: {error}", file=sys.stderr)
        return 2

    errors = validate_manifest(manifest, actual_paths)
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 2

    observed = [inspect_workflow(path, repo_root) for path in workflows]
    report = build_report(manifest, observed)
    if not args.no_write:
        args.report_dir.mkdir(parents=True, exist_ok=True)
        json_path = args.report_dir / "workflow-inventory.json"
        markdown_path = args.report_dir / "workflow-inventory.md"
        json_path.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        markdown_path.write_text(render_markdown(report), encoding="utf-8")

    summary = {
        "standing": report["standing"],
        "workflows": report["observed_workflow_count"],
        "duplicate_command_families": len(report["duplicate_command_families"]),
        "refusals": [],
    }
    print(json.dumps(summary, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
