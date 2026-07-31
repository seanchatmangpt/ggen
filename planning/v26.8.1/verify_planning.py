#!/usr/bin/env python3
"""Fail-closed structural verifier for the ggen v26.8.1 planning portfolio."""
from __future__ import annotations

import hashlib
import json
import re
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLAN = ROOT / "planning" / "v26.8.1"
OUT = ROOT / ".ggen" / "v26.8.1" / "planning-report.json"

REQUIRED = {
    "domains/ggen-v2681-core.pddl": [":derived", ":constraints", ":preferences", ":action-costs"],
    "domains/ggen-v2681-temporal.pddl": [":durative-action", ":continuous-effects", ":duration-inequalities"],
    "domains/ggen-v2681-probabilistic.ppddl": [":probabilistic-effects", "probabilistic", ":rewards"],
    "domains/ggen-v2681-hierarchical.hddl": [":hierarchy", ":method", ":task"],
    "problems/01-governance.pddl": ["1-10"],
    "problems/02-system.pddl": ["ggen-v2681-system"],
    "problems/03-engine.pddl": ["resolve", "receipt"],
    "problems/04-graph.pddl": ["oxigraph", "graphlaw"],
    "problems/05-projection.pddl": ["tera", "combinatorial-maximalism"],
    "problems/06-evidence.pddl": ["blake3", "external-witness"],
    "problems/07-products.pddl": ["marketplace", "protocols"],
    "problems/08-verification.pddl": ["property-fuzz", "negative-fixtures"],
    "problems/09-economics.pddl": ["little", "amdahl", "conway"],
    "problems/10-legacy-sunset.pddl": ["information-loss", "sunset-admitted"],
    "problems/11-recovery-under-uncertainty.ppddl": ["release-safe", "maximize"],
    "ontology-binding.ttl": ["ggen:projectsFrom", "plan:PPDDLDomain", "plan:HDDLDomain"],
    "manifest.toml": ["numbered_research_documents = 100", "require_zero_unmapped_subsystems = true"],
}

@dataclass(frozen=True)
class Finding:
    code: str
    path: str
    message: str


def normalized(text: str) -> str:
    return re.sub(r";[^\n]*", "", text).lower()


def balanced(text: str) -> bool:
    depth = 0
    in_string = False
    escaped = False
    for char in text:
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue
        if char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth < 0:
                return False
    return depth == 0 and not in_string


def git_head() -> str:
    result = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True, capture_output=True, check=False
    )
    return result.stdout.strip() if result.returncode == 0 else "UNKNOWN"


def main() -> int:
    findings: list[Finding] = []
    hashes: dict[str, str] = {}
    for relative, needles in REQUIRED.items():
        path = PLAN / relative
        if not path.is_file():
            findings.append(Finding("PDDL-MISSING", relative, "required planning surface is absent"))
            continue
        data = path.read_bytes()
        text = data.decode("utf-8")
        hashes[relative] = hashlib.sha256(data).hexdigest()
        if path.suffix in {".pddl", ".ppddl", ".hddl"} and not balanced(text):
            findings.append(Finding("PDDL-PAREN", relative, "parentheses or string delimiters are unbalanced"))
        lowered = normalized(text)
        for needle in needles:
            if needle.lower() not in lowered:
                findings.append(Finding("PDDL-FEATURE", relative, f"required feature missing: {needle}"))

    problem_files = sorted((PLAN / "problems").glob("*.pddl"))
    if len(problem_files) != 10:
        findings.append(Finding("PDDL-COVERAGE", "problems", f"expected 10 deterministic families, observed {len(problem_files)}"))

    # The 100-document numbered research corpus lives exclusively in the ten
    # canonical subsystem directories (00-governance .. 90-legacy), one
    # directory per PDDL problem family's doc_range in manifest.toml.
    # docs/v26.8.1/diagrams/ is a separate, legitimate supplementary corpus
    # (Mermaid diagram companions) that happens to reuse the same NN-topic.md
    # numbering convention (01-06) as the governance docs it illustrates; a
    # recursive **/*.md glob swept those 6 files in too, inflating the count
    # to 106 against the manifest's declared 100. Scope the glob to the
    # numbered corpus directories only, matching manifest.toml's own
    # [[problems]] doc_range coverage.
    corpus_dirs = [
        "00-governance", "10-system", "20-engine", "30-graph", "40-projection",
        "50-evidence", "60-products", "70-verification", "80-economics", "90-legacy",
    ]
    docs = sorted(
        p
        for d in corpus_dirs
        for p in (ROOT / "docs" / "v26.8.1" / d).glob("*.md")
    )
    numbered = [p for p in docs if re.match(r"\d+-", p.name)]
    if len(numbered) != 100:
        findings.append(Finding("PDDL-DOC-PARITY", "docs/v26.8.1", f"expected 100 numbered documents, observed {len(numbered)}"))

    ontology = ROOT / "ontology" / "v26.8.1" / "ontology.ttl"
    if not ontology.is_file():
        findings.append(Finding("PDDL-AUTHORITY", str(ontology.relative_to(ROOT)), "unified ontology authority is absent"))
    else:
        hashes[str(ontology.relative_to(ROOT))] = hashlib.sha256(ontology.read_bytes()).hexdigest()

    aggregate = hashlib.sha256()
    for path, digest in sorted(hashes.items()):
        aggregate.update(path.encode())
        aggregate.update(b"\0")
        aggregate.update(digest.encode())
        aggregate.update(b"\n")

    report = {
        "schema": "ggen.planning.verifier.report.v1",
        "version": "26.8.1",
        "source_head": git_head(),
        "standing": "PARTIAL_ALIVE" if not findings else "BUILD_BROKEN",
        "release_admitted": False,
        "sunset_admitted": False,
        "files": hashes,
        "aggregate_sha256": aggregate.hexdigest(),
        "findings": [asdict(f) for f in findings],
        "counts": {
            "required_surfaces": len(REQUIRED),
            "observed_surfaces": len(hashes),
            "deterministic_problem_families": len(problem_files),
            "numbered_research_documents": len(numbered),
        },
    }
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2, sort_keys=True))
    return 1 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main())
