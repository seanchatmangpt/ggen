#!/usr/bin/env python3
"""Derive G0 event, permission, and action-reference topology from workflow YAML."""

from __future__ import annotations

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path
from typing import Any

TOP_KEY = re.compile(r"^([\"']?)([A-Za-z_][A-Za-z0-9_-]*)\1\s*:(.*)$")
CHILD_KEY = re.compile(r"^([\"']?)([A-Za-z_][A-Za-z0-9_.-]*)\1\s*:(.*)$")
PERMISSIONS_LINE = re.compile(r"^(\s*)permissions\s*:\s*(.*)$")
USES_LINE = re.compile(r"^\s*-?\s*uses\s*:\s*['\"]?([^'\"#]+?)['\"]?\s*(?:#.*)?$")


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


def flow_values(inline: str) -> list[str]:
    flattened = inline.strip().strip("[]{}")
    if not flattened:
        return []
    return sorted(
        {
            token.strip("'\"")
            for token in re.split(r"[,\s:{}]+", flattened)
            if token.strip("'\"")
        }
    )


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
    indents = [
        indentation(raw)
        for raw in body
        if raw.strip() and not raw.lstrip().startswith("#")
    ]
    if not indents:
        return []
    child_indent = min(indents)
    keys: list[str] = []
    for raw in body:
        if indentation(raw) != child_indent:
            continue
        match = CHILD_KEY.match(strip_comment(raw[child_indent:]).strip())
        if match:
            keys.append(match.group(2))
    return sorted(set(keys))


def parse_triggers(lines: list[str]) -> list[str]:
    block = top_level_block(lines, "on")
    if block is None:
        return []
    inline, body = block
    return flow_values(inline) if inline else first_level_keys(body)


def parse_permissions(lines: list[str]) -> list[str]:
    permissions: set[str] = set()
    workflow_scope = False
    index = 0
    while index < len(lines):
        match = PERMISSIONS_LINE.match(strip_comment(lines[index]))
        if not match:
            index += 1
            continue
        block_indent = len(match.group(1))
        scope = "workflow" if block_indent == 0 else "job"
        workflow_scope = workflow_scope or scope == "workflow"
        inline = match.group(2).strip()
        if inline:
            for value in flow_values(inline) or [inline]:
                permissions.add(f"{scope}:{value}")
            index += 1
            continue
        index += 1
        while index < len(lines):
            nested = lines[index]
            if nested.strip() and indentation(nested) <= block_indent:
                break
            clean = strip_comment(nested).strip()
            if clean and ":" in clean:
                key, value = clean.split(":", 1)
                permissions.add(f"{scope}:{key.strip()}:{value.strip() or 'unspecified'}")
            index += 1
    if not workflow_scope:
        permissions.add("workflow:implicit-default")
    return sorted(permissions)


def action_references(lines: list[str]) -> list[str]:
    return sorted(
        {
            match.group(1).strip()
            for raw in lines
            if (match := USES_LINE.match(raw)) is not None
        }
    )


def mutable_action(reference: str) -> bool:
    if reference.startswith("./"):
        return False
    if reference.startswith("docker://"):
        return "@sha256:" not in reference
    if "@" not in reference:
        return True
    revision = reference.rsplit("@", 1)[1]
    return re.fullmatch(r"[0-9a-fA-F]{40}", revision) is None


def inspect(path: Path, root: Path) -> dict[str, Any]:
    lines = path.read_text(encoding="utf-8").splitlines()
    actions = action_references(lines)
    return {
        "path": path.relative_to(root).as_posix(),
        "triggers": parse_triggers(lines),
        "permission_ceiling": parse_permissions(lines),
        "actions": actions,
        "mutable_actions": [action for action in actions if mutable_action(action)],
    }


def report(root: Path) -> dict[str, Any]:
    workflow_dir = root / ".github" / "workflows"
    paths = sorted([*workflow_dir.glob("*.yml"), *workflow_dir.glob("*.yaml")])
    workflows = [inspect(path, root) for path in paths]
    trigger_users: dict[str, list[str]] = defaultdict(list)
    signature_users: dict[str, list[str]] = defaultdict(list)
    mutable_users: dict[str, list[str]] = defaultdict(list)
    for workflow in workflows:
        for trigger in workflow["triggers"]:
            trigger_users[trigger].append(workflow["path"])
        signature = "+".join(workflow["triggers"]) or "none-derived"
        signature_users[signature].append(workflow["path"])
        for action in workflow["mutable_actions"]:
            mutable_users[action].append(workflow["path"])
    return {
        "standing": "PARTIAL_ALIVE",
        "workflow_count": len(workflows),
        "trigger_fanout": {
            key: sorted(value) for key, value in sorted(trigger_users.items())
        },
        "duplicate_trigger_signatures": {
            key: sorted(value)
            for key, value in sorted(signature_users.items())
            if len(value) > 1
        },
        "mutable_action_references": {
            key: sorted(value) for key, value in sorted(mutable_users.items())
        },
        "workflows": workflows,
        "exclusions": [
            "Permission facts are observations, not authorization changes.",
            "Mutable action references are inventoried but not repaired in G0.",
            "No trigger or workflow behavior is changed by this analysis.",
        ],
    }


def render_markdown(data: dict[str, Any]) -> str:
    lines = [
        "# G0 Workflow Topology Evidence",
        "",
        f"- Standing: `{data['standing']}`",
        f"- Workflows: **{data['workflow_count']}**",
        "",
        "## Trigger fan-out",
        "",
    ]
    lines.extend(
        f"- `{trigger}` — {len(paths)} workflows"
        for trigger, paths in data["trigger_fanout"].items()
    )
    lines.extend(["", "## Mutable action references", ""])
    if data["mutable_action_references"]:
        for action, paths in data["mutable_action_references"].items():
            lines.append(f"- `{action}` — {', '.join(f'`{path}`' for path in paths)}")
    else:
        lines.append("- None observed.")
    lines.extend(["", "## Permission ceilings", ""])
    for workflow in data["workflows"]:
        lines.append(
            f"- `{workflow['path']}` — {', '.join(workflow['permission_ceiling'])}"
        )
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", type=Path, default=repository_root())
    parser.add_argument("--report-dir", type=Path)
    args = parser.parse_args()
    root = args.repo_root.resolve()
    out = args.report_dir or root / "target" / "ci-g0"
    data = report(root)
    out.mkdir(parents=True, exist_ok=True)
    (out / "workflow-topology.json").write_text(
        json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    (out / "workflow-topology.md").write_text(render_markdown(data), encoding="utf-8")
    print(
        json.dumps(
            {
                "standing": data["standing"],
                "workflows": data["workflow_count"],
                "trigger_fanout": len(data["trigger_fanout"]),
                "mutable_action_references": len(data["mutable_action_references"]),
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
