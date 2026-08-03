#!/usr/bin/env python3
"""Independent verifier and bootstrap projector for the self-hosted LSP contract.

`ggen sync` remains the authoritative manufacturer. `--write` is a bounded
bootstrap oracle for toolchain-blocked environments; a later ggen run must
reproduce these bytes or the fixed-point claim is false.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import re
import sys
from typing import Any

try:
    from rdflib import Graph, Namespace, RDF
except ImportError as error:
    raise SystemExit(f"BLOCKED:RDFLIB_UNAVAILABLE:{error}")

LSP = Namespace("https://ggen.dev/ontology/lsp-contract#")
DCT = Namespace("http://purl.org/dc/terms/")


def literal(graph: Graph, subject: Any, predicate: Any) -> str:
    value = graph.value(subject, predicate)
    if value is None:
        raise ValueError(f"missing {predicate} on {subject}")
    return str(value)


def ordered(graph: Graph, class_name: str) -> list[Any]:
    values = list(graph.subjects(RDF.type, LSP[class_name]))
    return sorted(values, key=lambda subject: int(literal(graph, subject, LSP.order)))


def load_contract(ontology: pathlib.Path) -> dict[str, Any]:
    graph = Graph()
    graph.parse(ontology, format="turtle")
    contract = LSP.ContractV1
    methods = []
    for subject in ordered(graph, "Method"):
        capability = literal(graph, subject, LSP.capabilityKey)
        methods.append({
            "order": int(literal(graph, subject, LSP.order)),
            "method": literal(graph, subject, LSP.method),
            "rust_handler": literal(graph, subject, LSP.rustHandler),
            "legacy_handler": literal(graph, subject, LSP.legacyHandler),
            "kind": literal(graph, subject, LSP.kind),
            "family": literal(graph, subject, LSP.family),
            "capability": None if capability == "none" else capability,
            "description": literal(graph, subject, DCT.description),
            "required_by": sorted(str(value) for value in graph.objects(subject, LSP.requiredBy)),
        })
    surfaces = [{
        "order": int(literal(graph, subject, LSP.order)),
        "extension": literal(graph, subject, LSP.extension),
        "analyzer": literal(graph, subject, LSP.analyzer),
        "description": literal(graph, subject, DCT.description),
        "required_by": sorted(str(value) for value in graph.objects(subject, LSP.requiredBy)),
    } for subject in ordered(graph, "Surface")]
    diagnostics = [{
        "order": int(literal(graph, subject, LSP.order)),
        "code": literal(graph, subject, LSP.code),
        "severity": literal(graph, subject, LSP.severity),
        "description": literal(graph, subject, DCT.description),
        "owner": literal(graph, subject, LSP.owner),
    } for subject in ordered(graph, "Diagnostic")]
    invariants = [{
        "order": int(literal(graph, subject, LSP.order)),
        "code": literal(graph, subject, LSP.code),
        "description": literal(graph, subject, DCT.description),
    } for subject in ordered(graph, "Invariant")]
    representations = [{
        "order": int(literal(graph, subject, LSP.order)),
        "id": literal(graph, subject, LSP.identifier),
        "repository": literal(graph, subject, LSP.repository),
        "path": literal(graph, subject, LSP.path),
        "role": literal(graph, subject, LSP.role),
        "state": literal(graph, subject, LSP.state),
    } for subject in ordered(graph, "Representation")]
    capabilities = {
        "textDocumentSync": {"openClose": True, "change": 1, "save": {"includeText": False}},
        "completionProvider": {"triggerCharacters": [":", "@", ".", "{", "[", '"', "|"]},
        "hoverProvider": True,
        "definitionProvider": True,
        "referencesProvider": True,
        "renameProvider": {"prepareProvider": True},
        "documentSymbolProvider": True,
        "workspaceSymbolProvider": True,
        "documentFormattingProvider": True,
        "documentRangeFormattingProvider": True,
        "codeActionProvider": {"codeActionKinds": ["quickfix"], "resolveProvider": False},
        "foldingRangeProvider": True,
        "semanticTokensProvider": {
            "legend": {
                "tokenTypes": ["namespace", "class", "property", "variable", "keyword", "string", "number", "comment", "function"],
                "tokenModifiers": [],
            },
            "full": True,
        },
        "inlayHintProvider": True,
        "codeLensProvider": {"resolveProvider": False},
        "callHierarchyProvider": True,
        "typeHierarchyProvider": True,
    }
    return {
        "schema": literal(graph, contract, LSP.schema),
        "version": literal(graph, contract, LSP.contractVersion),
        "authority": literal(graph, contract, LSP.authorityPath),
        "generator": literal(graph, contract, LSP.generatorConfig),
        "legacy": {
            "repository": literal(graph, contract, LSP.legacyRepository),
            "branch": literal(graph, contract, LSP.legacyBranch),
        },
        "methods": methods,
        "surfaces": surfaces,
        "diagnostics": diagnostics,
        "invariants": invariants,
        "representations": representations,
        "capabilities": capabilities,
    }


def render_json(contract: dict[str, Any]) -> str:
    return json.dumps(contract, sort_keys=True, ensure_ascii=False, separators=(",", ":")) + "\n"


def render_rust(contract: dict[str, Any]) -> str:
    lines = [
        "//! GENERATED by ggen from `self-host/lsp-contract/ontology.ttl`.",
        "//! Edit the ontology and rerun the self-host contract projection.",
        "",
        f'pub const CONTRACT_VERSION: &str = {json.dumps(contract["version"])};',
        f'pub const CONTRACT_SCHEMA: &str = {json.dumps(contract["schema"])};',
        'pub const GGEN_SRC_004: &str = "GGEN-SRC-004";',
        "",
        "pub const REQUIRED_METHODS: &[&str] = &[",
    ]
    lines.extend(f'    {json.dumps(row["method"])},' for row in contract["methods"])
    lines.extend(["];", "", "pub const REQUIRED_SURFACES: &[&str] = &["])
    lines.extend(f'    {json.dumps(row["extension"])},' for row in contract["surfaces"])
    lines.extend(["];", "", "pub const DECLARED_DIAGNOSTICS: &[&str] = &["])
    lines.extend(f'    {json.dumps(row["code"])},' for row in contract["diagnostics"])
    lines.extend([
        "];", "", "#[must_use]", "pub fn has_method(method: &str) -> bool {",
        "    REQUIRED_METHODS.contains(&method)", "}", "", "#[cfg(test)]", "mod tests {",
        "    use super::*;", "", "    #[test]", "    fn generated_contract_is_sorted_and_unique() {",
        "        let mut methods = REQUIRED_METHODS.to_vec();", "        methods.sort_unstable();",
        "        methods.dedup();", "        assert_eq!(methods.len(), REQUIRED_METHODS.len());",
        "        assert_eq!(REQUIRED_METHODS.len(), 29);",
        "        assert_eq!(REQUIRED_SURFACES.len(), 8);",
        "        assert_eq!(DECLARED_DIAGNOSTICS.len(), 11);", "    }", "}", "",
    ])
    return "\n".join(lines)


def render_markdown(contract: dict[str, Any]) -> str:
    lines = [
        "# ggen LSP Contract", "",
        "> GENERATED by `ggen sync` from `self-host/lsp-contract/ontology.ttl`. Edit the ontology, not this projection.",
        "", f"- Contract: `{contract['schema']}`", f"- Version: `{contract['version']}`",
        "- Kernel runtime: `seanchatmangpt/ggen:crates/ggen-lsp`",
        f"- Independent receiver: `{contract['legacy']['repository']}:{contract['legacy']['branch']}`",
        "", "## Required protocol methods", "",
        "| # | Method | Rust handler | Capability | Family |", "|---:|---|---|---|---|",
    ]
    for row in contract["methods"]:
        lines.append(f"| {row['order']} | `{row['method']}` | `{row['rust_handler']}` | `{row['capability'] or 'lifecycle'}` | {row['family']} |")
    lines.extend(["", "## Required source surfaces", "", "| Extension | Analyzer | Description |", "|---|---|---|"])
    for row in contract["surfaces"]:
        lines.append(f"| `.{row['extension']}` | `{row['analyzer']}` | {row['description']} |")
    lines.extend(["", "## Diagnostic ownership", "", "| Code | Severity | Owner | Meaning |", "|---|---|---|---|"])
    for row in contract["diagnostics"]:
        lines.append(f"| `{row['code']}` | {row['severity']} | {row['owner']} | {row['description']} |")
    lines.extend(["", "## Invariants", ""])
    for row in contract["invariants"]:
        lines.append(f"{row['order']}. **`{row['code']}`** — {row['description']}")
    lines.extend(["", "## Representation graph", "", "| Representation | Repository | Path | Role |", "|---|---|---|---|"])
    for row in contract["representations"]:
        lines.append(f"| `{row['id']}` | `{row['repository']}` | `{row['path']}` | {row['role']} |")
    lines.append("")
    return "\n".join(lines)


def projections(ggen_root: pathlib.Path, contract: dict[str, Any]) -> dict[pathlib.Path, str]:
    return {
        ggen_root / "crates/ggen-lsp/generated/lsp-contract.json": render_json(contract),
        ggen_root / "crates/ggen-lsp/src/generated_contract.rs": render_rust(contract),
        ggen_root / "docs/generated/LSP_CONTRACT.md": render_markdown(contract),
    }


def check_runtime_sources(
    ggen_root: pathlib.Path,
    legacy_root: pathlib.Path | None,
    contract: dict[str, Any],
    *,
    skip_kernel: bool = False,
) -> list[str]:
    findings: list[str] = []
    if not skip_kernel:
        rust_server = (ggen_root / "crates/ggen-lsp/src/server.rs").read_text(encoding="utf-8")
        rust_state = (ggen_root / "crates/ggen-lsp/src/state.rs").read_text(encoding="utf-8")
        rust_source = (ggen_root / "crates/ggen-lsp/src/source_contract.rs").read_text(encoding="utf-8")
        for row in contract["methods"]:
            handler = row["rust_handler"]
            if handler != "framework" and not re.search(rf"\b(?:async\s+)?fn\s+{re.escape(handler)}\b", rust_server):
                findings.append(f"RUST_HANDLER_ABSENT:{row['method']}:{handler}")
        for extension in ("ttl", "nt", "nq", "rq", "sparql", "tera", "toml"):
            if f'"{extension}"' not in rust_state and f'ends_with(".{extension}")' not in rust_state:
                findings.append(f"RUST_SURFACE_UNOBSERVED:{extension}")
        if "GGEN-SRC-004" not in rust_source:
            findings.append("RUST_DIAGNOSTIC_ABSENT:GGEN-SRC-004")
    if legacy_root is not None:
        expected_json = render_json(contract)
        expected_rust = render_rust(contract)
        legacy_json = legacy_root / "authority/lsp-contract.json"
        legacy_rust = legacy_root / "src/generated_contract.rs"
        if not legacy_json.is_file() or legacy_json.read_text(encoding="utf-8") != expected_json:
            findings.append("LEGACY_CONTRACT_DRIFT")
        if not legacy_rust.is_file() or legacy_rust.read_text(encoding="utf-8") != expected_rust:
            findings.append("LEGACY_RUST_PROJECTION_DRIFT")
        backend = (legacy_root / "src/backend.rs").read_text(encoding="utf-8")
        capabilities = (legacy_root / "src/capabilities.rs").read_text(encoding="utf-8")
        analysis = (legacy_root / "src/analysis.rs").read_text(encoding="utf-8")
        generated = legacy_rust.read_text(encoding="utf-8") if legacy_rust.is_file() else ""
        for row in contract["methods"]:
            handler = row["legacy_handler"]
            if handler != "framework" and not re.search(rf"\b(?:async\s+)?fn\s+{re.escape(handler)}\b", backend):
                findings.append(f"LEGACY_HANDLER_ABSENT:{row['method']}:{handler}")
        capability_sources = capabilities + backend
        for capability in sorted({row["capability"] for row in contract["methods"] if row["capability"]}):
            snake = re.sub(r"(?<!^)(?=[A-Z])", "_", capability).lower()
            dynamic_type_hierarchy = (
                capability == "typeHierarchyProvider"
                and "textDocument/prepareTypeHierarchy" in backend
                and "register_capability" in backend
            )
            if capability not in capability_sources and snake not in capability_sources and not dynamic_type_hierarchy:
                findings.append(f"LEGACY_CAPABILITY_ABSENT:{capability}")
        for extension in (row["extension"] for row in contract["surfaces"]):
            if f'"{extension}"' not in analysis and f'"{extension}"' not in backend:
                findings.append(f"LEGACY_SURFACE_ABSENT:{extension}")
        diagnostic_sources = analysis + generated
        for row in contract["diagnostics"]:
            if row["owner"] in {"legacy", "both"} and row["code"] not in diagnostic_sources:
                findings.append(f"LEGACY_DIAGNOSTIC_ABSENT:{row['code']}")
    return findings


def sha256(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--ggen-root", type=pathlib.Path)
    parser.add_argument("--legacy-root", type=pathlib.Path)
    parser.add_argument("--write", action="store_true")
    parser.add_argument("--skip-runtime", action="store_true")
    parser.add_argument("--skip-kernel-runtime", action="store_true")
    parser.add_argument("--report", type=pathlib.Path)
    args = parser.parse_args()
    here = pathlib.Path(__file__).resolve().parent
    ggen_root = (args.ggen_root or here.parents[1]).resolve()
    ontology = ggen_root / "self-host/lsp-contract/ontology.ttl"
    contract = load_contract(ontology)
    expected = projections(ggen_root, contract)
    if args.write:
        for path, content in expected.items():
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content, encoding="utf-8")
    findings = []
    for path, content in expected.items():
        if not path.is_file():
            findings.append(f"PROJECTION_ABSENT:{path.relative_to(ggen_root)}")
        elif path.read_text(encoding="utf-8") != content:
            findings.append(f"PROJECTION_DRIFT:{path.relative_to(ggen_root)}")
    if not args.skip_runtime:
        findings.extend(check_runtime_sources(
            ggen_root,
            args.legacy_root.resolve() if args.legacy_root else None,
            contract,
            skip_kernel=args.skip_kernel_runtime,
        ))
    report = {
        "schema": "ggen.lsp.representation-sync-report/1",
        "contract_schema": contract["schema"],
        "contract_version": contract["version"],
        "ontology_sha256": hashlib.sha256(ontology.read_bytes()).hexdigest(),
        "projection_sha256": {str(path.relative_to(ggen_root)): sha256(content) for path, content in expected.items()},
        "method_count": len(contract["methods"]),
        "surface_count": len(contract["surfaces"]),
        "diagnostic_count": len(contract["diagnostics"]),
        "representation_count": len(contract["representations"]),
        "legacy_checked": args.legacy_root is not None,
        "kernel_runtime_checked": not args.skip_runtime and not args.skip_kernel_runtime,
        "findings": sorted(set(findings)),
        "standing": "ALIVE" if not findings else "BUILD_BROKEN",
        "claim_ceiling": "REPRESENTATION_SYNC_ONLY",
        "ggen_execution": "BLOCKED_TOOLCHAIN_UNAVAILABLE",
    }
    encoded = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(encoded, encoding="utf-8")
    sys.stdout.write(encoded)
    return 0 if not findings else 1


if __name__ == "__main__":
    raise SystemExit(main())
