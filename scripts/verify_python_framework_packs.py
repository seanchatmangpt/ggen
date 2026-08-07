#!/usr/bin/env python3
"""Local verifier for ontology-driven Typer, FastMCP, and DSPy ggen packs.

This verifier never promotes structural rendering into exact-framework ALIVE.
It proves pack admission, deterministic rendering, Python syntax, negative-gate
falsifiers, an exact Typer execution when Typer is installed, and contract
execution for FastMCP/DSPy when their distributions are unavailable.
"""
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
import tempfile
import time
import tomllib
import types
from pathlib import Path
from typing import Any

from jinja2 import Environment, StrictUndefined
from rdflib import Graph, Literal, Namespace

PACKS = ("typer-pack", "fastmcp-pack", "dspy-pack")


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


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


def query_rows(graph: Graph, query: str) -> list[tuple[str, ...]]:
    return [tuple(str(value) for value in row) for row in graph.query(query)]


def load_graph(pack: Path) -> Graph:
    graph = Graph()
    graph.parse(pack / "ontology.ttl", format="turtle")
    return graph


def run_gates(pack: Path, graph: Graph) -> list[dict[str, Any]]:
    receipts: list[dict[str, Any]] = []
    for gate in sorted((pack / "gates").glob("*.rq")):
        rows = query_rows(graph, gate.read_text(encoding="utf-8"))
        receipts.append({"gate": gate.name, "rows": rows})
        if rows:
            raise AssertionError(f"{pack.name}/{gate.name} refused ontology: {rows}")
    return receipts


def render_pack(repo_root: Path, pack_name: str, output_root: Path) -> dict[str, Any]:
    started = time.perf_counter()
    pack = repo_root / "packs" / pack_name
    manifest = tomllib.loads((pack / "pack.toml").read_text(encoding="utf-8"))
    if manifest.get("pack", {}).get("name") != pack_name:
        raise AssertionError(f"{pack_name}: manifest name does not match directory")
    graph = load_graph(pack)
    gate_receipts = run_gates(pack, graph)

    environment = Environment(undefined=StrictUndefined, keep_trailing_newline=True)
    environment.filters["json_encode"] = lambda value: json.dumps(str(value), ensure_ascii=False)
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
        outputs.append({
            "template": str(template_path.relative_to(repo_root)),
            "target": target,
            "sha256": sha256(target_path),
        })
    return {
        "pack": pack_name,
        "triples": len(graph),
        "gates": gate_receipts,
        "outputs": outputs,
        "elapsed_ms": round((time.perf_counter() - started) * 1000, 3),
    }


def verify_negative_falsifiers(repo_root: Path) -> dict[str, list[tuple[str, ...]]]:
    cases: dict[str, list[tuple[str, ...]]] = {}

    t = Namespace("http://seanchatmangpt.github.io/packs/typer#")
    graph = load_graph(repo_root / "packs/typer-pack")
    graph.remove((t["greet-command"], t.argumentHelp, None))
    q = (repo_root / "packs/typer-pack/gates/010_admission.rq").read_text(encoding="utf-8")
    rows = query_rows(graph, q)
    if not rows:
        raise AssertionError("typer negative falsifier was not refused")
    cases["typer_missing_argument_help"] = rows

    f = Namespace("http://seanchatmangpt.github.io/packs/fastmcp#")
    graph = load_graph(repo_root / "packs/fastmcp-pack")
    graph.remove((f["greet-tool"], f.argument, None))
    q = (repo_root / "packs/fastmcp-pack/gates/010_admission.rq").read_text(encoding="utf-8")
    rows = query_rows(graph, q)
    if not rows:
        raise AssertionError("fastmcp negative falsifier was not refused")
    cases["fastmcp_missing_argument"] = rows

    d = Namespace("http://seanchatmangpt.github.io/packs/dspy#")
    graph = load_graph(repo_root / "packs/dspy-pack")
    graph.set((d["answer-program"], d.kind, Literal("AmbientActuator")))
    q = (repo_root / "packs/dspy-pack/gates/010_admission.rq").read_text(encoding="utf-8")
    rows = query_rows(graph, q)
    if not rows:
        raise AssertionError("dspy negative falsifier was not refused")
    cases["dspy_unadmitted_module_kind"] = rows
    return cases


def deterministic_replay(repo_root: Path) -> dict[str, str]:
    with tempfile.TemporaryDirectory() as first, tempfile.TemporaryDirectory() as second:
        first_root, second_root = Path(first), Path(second)
        a = [render_pack(repo_root, p, first_root) for p in PACKS]
        b = [render_pack(repo_root, p, second_root) for p in PACKS]
        hashes_a = {r["pack"]: r["outputs"][0]["sha256"] for r in a}
        hashes_b = {r["pack"]: r["outputs"][0]["sha256"] for r in b}
        if hashes_a != hashes_b:
            raise AssertionError(f"deterministic replay diverged: {hashes_a} != {hashes_b}")
        return hashes_a


def load_module(name: str, path: Path) -> types.ModuleType:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load module from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def smoke_typer(generated_root: Path) -> dict[str, Any]:
    path = generated_root / "typer-pack/src/typer_app.py"
    proc = subprocess.run(
        [sys.executable, str(path), "greet", "Sean"],
        capture_output=True, text=True, check=False,
    )
    if proc.returncode != 0 or proc.stdout.strip() != "Hello, Sean!":
        raise AssertionError(f"Typer smoke failed: exit={proc.returncode} stdout={proc.stdout!r} stderr={proc.stderr!r}")
    return {"exit": proc.returncode, "stdout": proc.stdout.strip()}


def smoke_fastmcp_contract(generated_root: Path) -> dict[str, Any]:
    registry: dict[str, Any] = {}
    fake = types.ModuleType("fastmcp")

    class Tool:
        def __init__(self, name: str, fn: Any): self.name, self.fn = name, fn

    class FastMCP:
        def __init__(self, name: str): self.name = name
        def tool(self, *, name: str, description: str):
            def decorate(fn: Any) -> Any:
                registry[name] = Tool(name, fn)
                return fn
            return decorate
        async def get_tool(self, name: str) -> Any: return registry.get(name)
        def run(self) -> None: raise AssertionError("contract smoke must not actuate server transport")

    fake.FastMCP = FastMCP
    prior = sys.modules.get("fastmcp")
    sys.modules["fastmcp"] = fake
    try:
        module = load_module("generated_fastmcp_contract", generated_root / "fastmcp-pack/src/fastmcp_server.py")
        tool = asyncio.run(module.mcp.get_tool("greet"))
        if tool is None or tool.name != "greet" or module.greet("Sean") != "Hello, Sean!":
            raise AssertionError("FastMCP contract smoke diverged")
        return {"server": module.mcp.name, "tool": tool.name, "result": module.greet("Sean")}
    finally:
        if prior is None: sys.modules.pop("fastmcp", None)
        else: sys.modules["fastmcp"] = prior


def smoke_dspy_contract(generated_root: Path) -> dict[str, Any]:
    fake = types.ModuleType("dspy")
    class Signature: pass
    class Field:
        def __init__(self, *, desc: str): self.desc = desc
    class Predict:
        def __init__(self, signature: type): self.signature = signature
    class ChainOfThought(Predict): pass
    fake.Signature = Signature
    fake.InputField = Field
    fake.OutputField = Field
    fake.Predict = Predict
    fake.ChainOfThought = ChainOfThought
    prior = sys.modules.get("dspy")
    sys.modules["dspy"] = fake
    try:
        module = load_module("generated_dspy_contract", generated_root / "dspy-pack/src/dspy_program.py")
        if module.AnswerQuestion.__name__ != "AnswerQuestion" or type(module.program).__name__ != "Predict":
            raise AssertionError("DSPy contract smoke diverged")
        return {"signature": module.AnswerQuestion.__name__, "module": type(module.program).__name__}
    finally:
        if prior is None: sys.modules.pop("dspy", None)
        else: sys.modules["dspy"] = prior


def exact_framework_smoke(generated_root: Path) -> tuple[dict[str, Any], list[str]]:
    results: dict[str, Any] = {}
    missing: list[str] = []
    try:
        import typer  # noqa: F401
        results["typer"] = smoke_typer(generated_root)
    except ModuleNotFoundError:
        missing.append("typer")

    try:
        import fastmcp  # noqa: F401
        module = load_module("generated_fastmcp_exact", generated_root / "fastmcp-pack/src/fastmcp_server.py")
        tool = asyncio.run(module.mcp.get_tool("greet"))
        if tool is None or tool.name != "greet" or module.greet("Sean") != "Hello, Sean!":
            raise AssertionError("FastMCP exact smoke diverged")
        results["fastmcp"] = {"tool": tool.name, "result": module.greet("Sean")}
    except ModuleNotFoundError:
        missing.append("fastmcp")

    try:
        import dspy  # noqa: F401
        module = load_module("generated_dspy_exact", generated_root / "dspy-pack/src/dspy_program.py")
        if module.AnswerQuestion.__name__ != "AnswerQuestion" or type(module.program).__name__ != "Predict":
            raise AssertionError("DSPy exact smoke diverged")
        results["dspy"] = {"signature": module.AnswerQuestion.__name__, "module": type(module.program).__name__}
    except ModuleNotFoundError:
        missing.append("dspy")
    return results, missing


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    started = time.perf_counter()
    repo_root = args.repo_root.resolve()
    output_root = args.output.resolve()
    output_root.mkdir(parents=True, exist_ok=True)

    receipts = [render_pack(repo_root, name, output_root) for name in PACKS]
    replay = deterministic_replay(repo_root)
    falsifiers = verify_negative_falsifiers(repo_root)
    contracts = {
        "fastmcp": smoke_fastmcp_contract(output_root),
        "dspy": smoke_dspy_contract(output_root),
    }
    exact, missing = exact_framework_smoke(output_root)

    exact_ggen = False
    all_exact_frameworks = not missing
    standing = "ALIVE" if exact_ggen and all_exact_frameworks else "PARTIAL_ALIVE"
    payload = {
        "standing": standing,
        "boundaries": {
            "pack_admission_render_replay": "ALIVE",
            "typer_exact_runtime": "ALIVE" if "typer" in exact else "UNKNOWN",
            "fastmcp_contract": "ALIVE",
            "fastmcp_exact_runtime": "ALIVE" if "fastmcp" in exact else "UNKNOWN",
            "dspy_contract": "ALIVE",
            "dspy_exact_runtime": "ALIVE" if "dspy" in exact else "UNKNOWN",
            "exact_ggen_cli": "UNKNOWN",
        },
        "packs": receipts,
        "replay_sha256": replay,
        "negative_falsifiers": falsifiers,
        "contract_smoke": contracts,
        "exact_framework_smoke": exact,
        "missing_exact_frameworks": missing,
        "elapsed_ms": round((time.perf_counter() - started) * 1000, 3),
    }
    print(json.dumps(payload, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
