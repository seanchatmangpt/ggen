#!/usr/bin/env python3
"""Verify, render, and smoke-test the Typer, FastMCP, and DSPy ggen packs."""

from __future__ import annotations

import argparse
import ast
import asyncio
import hashlib
import importlib.util
import json
import re
import subprocess
import sys
import time
import tomllib
from pathlib import Path
from types import ModuleType
from typing import Any

from jinja2 import Environment, StrictUndefined
from rdflib import Graph

PACKS = ("typer-pack", "fastmcp-pack", "dspy-pack")


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    digest.update(path.read_bytes())
    return digest.hexdigest()


def parse_frontmatter(text: str) -> tuple[str, dict[str, str], str]:
    if not text.startswith("---\n"):
        raise ValueError("template is missing opening frontmatter delimiter")
    end = text.find("\n---\n", 4)
    if end < 0:
        raise ValueError("template is missing closing frontmatter delimiter")

    target: str | None = None
    queries: dict[str, str] = {}
    in_sparql = False
    for line in text[4:end].splitlines():
        if line.startswith("to:"):
            target = line.split(":", 1)[1].strip()
        elif line.strip() == "sparql:":
            in_sparql = True
        elif in_sparql and re.match(r"^  [A-Za-z_][A-Za-z0-9_]*:", line):
            name, encoded_query = line.strip().split(":", 1)
            queries[name] = json.loads(encoded_query.strip())

    if not target or not queries:
        raise ValueError("template must declare to: and at least one SPARQL query")
    return target, queries, text[end + 5 :]


def load_module(name: str, path: Path) -> ModuleType:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load module from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def render_pack(repo_root: Path, pack_name: str, output_root: Path) -> dict[str, Any]:
    started = time.perf_counter()
    pack = repo_root / "packs" / pack_name
    manifest = tomllib.loads((pack / "pack.toml").read_text(encoding="utf-8"))
    if manifest.get("pack", {}).get("name") != pack_name:
        raise AssertionError(f"{pack_name}: manifest name does not match directory")

    graph = Graph()
    graph.parse(pack / "ontology.ttl", format="turtle")

    gate_receipts: list[dict[str, Any]] = []
    for gate in sorted((pack / "gates").glob("*.rq")):
        rows = [tuple(str(value) for value in row) for row in graph.query(gate.read_text())]
        gate_receipts.append({"gate": gate.name, "rows": rows})
        if rows:
            raise AssertionError(f"{pack_name}/{gate.name} refused ontology: {rows}")

    environment = Environment(undefined=StrictUndefined, keep_trailing_newline=True)
    environment.filters["json_encode"] = lambda value: json.dumps(
        str(value), ensure_ascii=False
    )

    outputs: list[dict[str, str]] = []
    templates = sorted((pack / "templates").glob("*.tmpl"))
    if not templates:
        raise AssertionError(f"{pack_name}: no templates")

    for template_path in templates:
        target, queries, body = parse_frontmatter(template_path.read_text(encoding="utf-8"))
        context: dict[str, list[dict[str, str]]] = {}
        for query_name, query in queries.items():
            result = graph.query(query)
            variables = [str(variable) for variable in result.vars]
            context[query_name] = [
                {variables[index]: str(value) for index, value in enumerate(row)}
                for row in result
            ]
            if not context[query_name]:
                raise AssertionError(f"{pack_name}/{template_path.name}: empty {query_name}")

        rendered = environment.from_string(body).render(**context)
        target_path = output_root / pack_name / target
        target_path.parent.mkdir(parents=True, exist_ok=True)
        target_path.write_text(rendered, encoding="utf-8")
        ast.parse(rendered, filename=str(target_path))
        outputs.append(
            {
                "template": str(template_path.relative_to(repo_root)),
                "target": target,
                "sha256": sha256(target_path),
            }
        )

    return {
        "pack": pack_name,
        "triples": len(graph),
        "gates": gate_receipts,
        "outputs": outputs,
        "elapsed_ms": round((time.perf_counter() - started) * 1000, 3),
    }


def compare_ggen_outputs(expected_root: Path, ggen_root: Path, receipts: list[dict[str, Any]]) -> None:
    for receipt in receipts:
        pack_name = receipt["pack"]
        for output in receipt["outputs"]:
            expected = expected_root / pack_name / output["target"]
            actual = ggen_root / output["target"]
            if not actual.is_file():
                raise AssertionError(f"ggen did not emit {actual}")
            if actual.read_bytes() != expected.read_bytes():
                raise AssertionError(
                    f"ggen output diverged for {pack_name}/{output['target']}: "
                    f"expected={sha256(expected)} actual={sha256(actual)}"
                )


def smoke_frameworks(generated_root: Path) -> dict[str, Any]:
    typer_path = generated_root / "src" / "typer_app.py"
    typer_run = subprocess.run(
        [sys.executable, str(typer_path), "greet", "Sean"],
        check=False,
        capture_output=True,
        text=True,
    )
    if typer_run.returncode != 0 or typer_run.stdout.strip() != "Hello, Sean!":
        raise AssertionError(
            f"Typer smoke failed: exit={typer_run.returncode} "
            f"stdout={typer_run.stdout!r} stderr={typer_run.stderr!r}"
        )

    fastmcp_module = load_module(
        "generated_fastmcp_server", generated_root / "src" / "fastmcp_server.py"
    )
    if fastmcp_module.greet("Sean") != "Hello, Sean!":
        raise AssertionError("FastMCP generated tool behavior diverged")
    registered_tool = asyncio.run(fastmcp_module.mcp.get_tool("greet"))
    if registered_tool is None or registered_tool.name != "greet":
        raise AssertionError("FastMCP did not register the generated greet tool")

    dspy_module = load_module(
        "generated_dspy_program", generated_root / "src" / "dspy_program.py"
    )
    if dspy_module.AnswerQuestion.__name__ != "AnswerQuestion":
        raise AssertionError("DSPy generated signature identity diverged")
    if type(dspy_module.program).__name__ != "Predict":
        raise AssertionError("DSPy generated module is not dspy.Predict")

    return {
        "typer": {
            "command": [sys.executable, str(typer_path), "greet", "Sean"],
            "exit": typer_run.returncode,
            "stdout": typer_run.stdout.strip(),
        },
        "fastmcp": {
            "tool": registered_tool.name,
            "direct_result": fastmcp_module.greet("Sean"),
        },
        "dspy": {
            "signature": dspy_module.AnswerQuestion.__name__,
            "module": type(dspy_module.program).__name__,
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--compare-ggen-root", type=Path)
    parser.add_argument("--framework-smoke-root", type=Path)
    args = parser.parse_args()

    repo_root = args.repo_root.resolve()
    output_root = args.output.resolve()
    output_root.mkdir(parents=True, exist_ok=True)

    receipts = [render_pack(repo_root, pack_name, output_root) for pack_name in PACKS]
    if args.compare_ggen_root:
        compare_ggen_outputs(output_root, args.compare_ggen_root.resolve(), receipts)

    framework_receipt = None
    if args.framework_smoke_root:
        framework_receipt = smoke_frameworks(args.framework_smoke_root.resolve())

    print(
        json.dumps(
            {
                "standing": "ALIVE",
                "packs": receipts,
                "ggen_byte_identity": bool(args.compare_ggen_root),
                "framework_smoke": framework_receipt,
            },
            indent=2,
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
