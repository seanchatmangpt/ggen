#!/usr/bin/env python3
"""Fail closed when a bounded CONSTRUCT-only Python subject gains ambient DO paths."""

from __future__ import annotations

import argparse
import ast
from dataclasses import dataclass
from pathlib import Path


FORBIDDEN_IMPORT_ROOTS = {
    "boto3",
    "botocore",
    "requests",
    "subprocess",
    "urllib",
}
FORBIDDEN_CALLS = {
    "os.system",
    "os.popen",
}
FORBIDDEN_ATTRIBUTE_CALLS = {
    "actuate",
}


@dataclass(frozen=True, order=True)
class Finding:
    line: int
    column: int
    rule: str
    symbol: str

    def render(self, path: Path) -> str:
        return f"{path}:{self.line}:{self.column + 1}: {self.rule}: {self.symbol}"


def dotted_name(node: ast.AST) -> str | None:
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        parent = dotted_name(node.value)
        return f"{parent}.{node.attr}" if parent else node.attr
    return None


def inspect_source(source: str) -> tuple[Finding, ...]:
    """Return executable authority violations; comments/docstrings are inert evidence."""
    tree = ast.parse(source)
    findings: set[Finding] = set()

    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                root = alias.name.split(".", 1)[0]
                if root in FORBIDDEN_IMPORT_ROOTS:
                    findings.add(
                        Finding(node.lineno, node.col_offset, "forbidden-import", alias.name)
                    )

        elif isinstance(node, ast.ImportFrom):
            module = node.module or ""
            root = module.split(".", 1)[0]
            if root in FORBIDDEN_IMPORT_ROOTS:
                findings.add(
                    Finding(node.lineno, node.col_offset, "forbidden-import", module)
                )
            if module == "os":
                for alias in node.names:
                    if alias.name in {"system", "popen"}:
                        findings.add(
                            Finding(
                                node.lineno,
                                node.col_offset,
                                "forbidden-process-import",
                                f"os.{alias.name}",
                            )
                        )

        elif isinstance(node, ast.Call):
            symbol = dotted_name(node.func)
            if symbol in FORBIDDEN_CALLS:
                findings.add(
                    Finding(node.lineno, node.col_offset, "forbidden-process-call", symbol)
                )
            if isinstance(node.func, ast.Attribute):
                if node.func.attr in FORBIDDEN_ATTRIBUTE_CALLS:
                    findings.add(
                        Finding(
                            node.lineno,
                            node.col_offset,
                            "forbidden-actuation-call",
                            symbol or node.func.attr,
                        )
                    )
                if node.func.attr == "do" and dotted_name(node.func.value) == "broker":
                    findings.add(
                        Finding(node.lineno, node.col_offset, "forbidden-broker-do", "broker.do")
                    )

    return tuple(sorted(findings))


def check_path(path: Path) -> tuple[Finding, ...]:
    return inspect_source(path.read_text(encoding="utf-8"))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("path", type=Path)
    args = parser.parse_args()

    try:
        findings = check_path(args.path)
    except SyntaxError as exc:
        print(
            f"REFUSED:SOURCE_SYNTAX_INVALID {args.path}:{exc.lineno or 0}:{exc.offset or 0}: {exc.msg}"
        )
        return 2

    if findings:
        print("REFUSED:MANUFACTURER_GAINED_DO_PATH")
        for finding in findings:
            print(finding.render(args.path))
        return 1

    print(f"NO_AMBIENT_DO path={args.path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
