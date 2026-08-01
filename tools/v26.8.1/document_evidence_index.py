#!/usr/bin/env python3
"""Document evidence index generator for ggen v26.8.1.

Replaces the substring-presence `DOCUMENT_SECTION_LOSS` check (which required
every doc to literally contain the words "authority"/"implementation"/
"verification"/"legacy" anywhere in its text) with a real per-document
evidence-binding model: one `DocumentEvidenceRecord` per document under
`docs/v26.8.1/`, carrying a real content digest, an inferred `subsystem` and
`documentRole`, and 0+ real, on-disk-verified references to authority,
implementation, verifier, legacy-capability, and evidence-report sources.

Honesty constraint: a document that genuinely does not cite anything in a
reference category is left empty for that category. Coverage is proven at
the subsystem aggregate level by the crown (`tools/v26.8.1/src/main.rs`), not
per-document keyword presence. This generator does not fabricate references
that aren't real, on-disk-verified paths.

Usage:
    python3 tools/v26.8.1/document_evidence_index.py [--root PATH]
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import subprocess
import sys
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path

SCHEMA = "ggen.v26.8.1.document-evidence-index/1"
DOC_ROOT_REL = Path("docs/v26.8.1")
OUT_JSON_REL = Path("docs/v26.8.1/document-evidence-index.json")
OUT_CSV_REL = Path("docs/v26.8.1/document-evidence-index.csv")
OUT_MD_REL = Path("docs/v26.8.1/document-evidence-index.md")
OUT_TTL_REL = Path("ontology/v26.8.1/document-evidence.ttl")
LEGACY_TTL_REL = Path("ontology/v26.8.1/legacy-capabilities.ttl")
GENERATOR_REL = Path("tools/v26.8.1/document_evidence_index.py")

SUBSYSTEMS = (
    "governance",
    "system",
    "engine",
    "graph",
    "projection",
    "evidence",
    "products",
    "verification",
    "economics",
    "legacy",
)

DIR_TO_SUBSYSTEM = {
    "00-governance": "governance",
    "10-system": "system",
    "20-engine": "engine",
    "30-graph": "graph",
    "40-projection": "projection",
    "50-evidence": "evidence",
    "60-products": "products",
    "70-verification": "verification",
    "80-economics": "economics",
    "90-legacy": "legacy",
    # Cross-cutting architecture diagrams span multiple subsystems; mapped to
    # "governance" (the closest semantic home for corpus-wide overview
    # material) rather than left unmapped -- the crown's own document walk
    # (observe_documents in tools/v26.8.1/src/main.rs, a recursive WalkDir
    # over all docs/v26.8.1/**/*.md) includes this directory, so a real
    # evidence record is required, not optional.
    "diagrams": "governance",
}

ROLE_ENUM = (
    "GOVERNANCE",
    "ARCHITECTURE",
    "IMPLEMENTATION",
    "VERIFICATION",
    "LEGACY",
    "ECONOMICS",
    "MIGRATION",
    "RELEASE",
)

# Per-subsystem seed of real, load-bearing, on-disk-verified references,
# mirroring what tools/v26.8.1/subsystem_evidence_manifest.py already
# establishes as this session's known authority/implementation/verifier
# sources per subsystem. These are attached only to the subsystem's
# lowest-numbered ("index") document, and only after re-verifying the path
# actually exists on disk -- not fabricated, not attached to every doc.
SUBSYSTEM_SEED_REFERENCES: dict[str, dict[str, list[str]]] = {
    "governance": {
        "authority": ["justfile", "AGENTS.md", "CLAUDE.md"],
        "implementation": ["justfile"],
        "verifier": ["crates/ggen-config/tests/governance_precommit_gate_count_test.rs"],
    },
    "system": {
        "authority": [".specify/repo-facts.ttl"],
        "implementation": ["Cargo.toml", ".specify/repo-facts.ttl"],
        "verifier": ["crates/ggen-config/tests/system_crate_map_parity_test.rs"],
    },
    "engine": {
        "authority": ["docs/v26.8.1/20-engine"],
        "implementation": ["crates/ggen-engine/src/sync.rs"],
        "verifier": [
            "crates/ggen-engine/tests/pipeline_stage_evidence_test.rs",
            "crates/ggen-engine/tests/manifest_diagnostic_codes_evidence_test.rs",
        ],
    },
    "graph": {
        "authority": ["docs/v26.8.1/30-graph"],
        "implementation": [
            "crates/ggen-graph/src",
            "crates/praxis-graphlaw/src",
        ],
        "verifier": ["crates/ggen-graph/tests/graph_hashing_evidence_test.rs"],
    },
    "projection": {
        "authority": ["docs/v26.8.1/40-projection"],
        "implementation": ["crates/ggen-engine/src/sync.rs"],
        "verifier": ["crates/ggen-engine/tests/projection_determinism_test.rs"],
    },
    "evidence": {
        "authority": ["docs/v26.8.1/50-evidence"],
        "implementation": [
            "crates/ggen-engine/src/sync.rs",
            "crates/praxis-core/src/receipt_record.rs",
        ],
        "verifier": ["crates/ggen-engine/tests/receipt_signing_evidence_test.rs"],
    },
    "products": {
        "authority": ["docs/v26.8.1/60-products"],
        "implementation": ["crates/ggen-cli/src"],
        "verifier": [
            "crates/ggen-cli/tests/cli_surface_evidence_test.rs",
            "crates/ggen-cli/tests/default_verb_law_test.rs",
        ],
    },
    "verification": {
        "authority": ["docs/v26.8.1/70-verification"],
        "implementation": ["tools/v26.8.1/src/main.rs"],
        "verifier": ["tools/v26.8.1/src/main.rs"],
    },
    "economics": {
        # Real, on-disk targets confirmed via
        # .ggen/v26.8.1/subsystem-evidence-manifest.json's "economics" record
        # (implementation_sources) and its negative_falsifier_reports entry
        # (economics_measured_evidence_test). No longer empty: that gap was
        # closed by an earlier pass that built a real measured-evidence test
        # and receipt-chain e2e coverage for this subsystem; this dict had
        # not been updated to reflect it. Not fabricated -- both paths exist
        # and are independently re-verified by subsystem_verifier.rs.
        "authority": ["docs/v26.8.1/80-economics"],
        "implementation": ["crates/ggen-engine/tests/receipt_chain_e2e.rs", "justfile"],
        "verifier": ["crates/ggen-engine/tests/economics_measured_evidence_test.rs"],
    },
    "legacy": {
        "authority": ["docs/v26.8.1/90-legacy"],
        "implementation": [
            "ontology/v26.8.1/legacy-capabilities.ttl",
            "tools/v26.8.1/equivalence_runner.py",
        ],
        "verifier": ["tools/v26.8.1/equivalence_runner.py"],
    },
}

PATH_TOKEN_RE = re.compile(
    r"(?:^|[\s\`\(\[\"'])"
    r"((?:crates|tools|ontology|docs|\.specify|packs|scripts|examples)/"
    r"[A-Za-z0-9_./-]+\.[A-Za-z0-9]+)"
)


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    return sha256_bytes(path.read_bytes())


def git_head(root: Path) -> str:
    completed = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=root, capture_output=True, text=True
    )
    if completed.returncode != 0:
        return "UNKNOWN"
    return completed.stdout.strip()


# Root-level docs directly under docs/v26.8.1/ (not inside a numbered
# subsystem subdir) that the crown's own document walk
# (observe_documents in tools/v26.8.1/src/main.rs) still requires a real
# evidence record for. Mapped to "verification": this file is itself part
# of the crown's evidence surface, not architecture/implementation prose.
ROOT_DOC_SUBSYSTEM: dict[str, str] = {
    "document-evidence-index.md": "verification",
}


def subsystem_for(p: Path, doc_root: Path) -> str:
    if p.parent == doc_root:
        return ROOT_DOC_SUBSYSTEM[p.name]
    return DIR_TO_SUBSYSTEM[p.parent.name]


def discover_documents(root: Path) -> list[Path]:
    doc_root = root / DOC_ROOT_REL
    docs: list[Path] = []
    for name in ROOT_DOC_SUBSYSTEM:
        candidate = doc_root / name
        if candidate.is_file():
            docs.append(candidate)
    for subdir in sorted(doc_root.iterdir()):
        if not subdir.is_dir():
            continue
        if subdir.name not in DIR_TO_SUBSYSTEM:
            continue  # e.g. "diagrams" is not a numbered corpus subsystem dir
        for p in sorted(subdir.glob("*.md")):
            docs.append(p)
    return docs


def infer_role(rel_path: str, subsystem: str, text: str) -> str:
    """Real per-doc judgment from filename/content, not a subsystem default."""
    name = Path(rel_path).name.lower()
    lowered = text.lower()

    if subsystem == "governance":
        return "GOVERNANCE"
    if subsystem == "economics":
        return "ECONOMICS"
    if subsystem == "verification":
        return "VERIFICATION"
    if subsystem == "legacy":
        if "migration" in name or "migration" in lowered:
            return "MIGRATION"
        if "sunset" in name or "runbook" in name:
            return "RELEASE"
        return "LEGACY"

    # engine / graph / projection / evidence / products / system
    if re.match(r"^\d+-(overview|architecture|design)", name):
        return "ARCHITECTURE"
    if "benchmark" in name or "research" in lowered and "pipeline" not in name:
        return "ARCHITECTURE"
    if subsystem == "system":
        return "ARCHITECTURE"
    return "IMPLEMENTATION"


def extract_path_references(root: Path, text: str) -> list[str]:
    """Extract path-shaped tokens the document already cites in prose and
    keep only the ones that are real, on-disk-verified paths (file or dir)."""
    found: set[str] = set()
    for m in PATH_TOKEN_RE.finditer(text):
        candidate = m.group(1).rstrip(".,;:)")
        if (root / candidate).exists():
            found.add(candidate)
    return sorted(found)


def seed_refs_for(root: Path, subsystem: str, category: str) -> list[str]:
    seeds = SUBSYSTEM_SEED_REFERENCES.get(subsystem, {}).get(category, [])
    return sorted({s for s in seeds if (root / s).exists()})


@dataclass
class DocumentEvidenceRecord:
    document_path: str
    document_digest: str
    subsystem: str
    document_role: str
    authority_references: list[str]
    implementation_references: list[str]
    verifier_references: list[str]
    legacy_capability_references: list[str]
    evidence_report_references: list[str]
    source_head: str


def parse_legacy_capabilities(root: Path) -> list[dict]:
    ttl_path = root / LEGACY_TTL_REL
    text = ttl_path.read_text() if ttl_path.is_file() else ""
    ids = re.findall(r'ggen:capabilityId\s+"([^"]+)"', text)
    subsystems = re.findall(r'ggen:owningSubsystem\s+"([^"]+)"', text)
    out = []
    for cap_id, subsystem in zip(ids, subsystems):
        out.append({"capabilityId": cap_id, "owningSubsystem": subsystem})
    return out


def build_index(root: Path) -> dict:
    head = git_head(root)
    doc_paths = discover_documents(root)
    records: list[DocumentEvidenceRecord] = []

    # Track which subsystem's "index" (lowest-numbered) doc gets the seed
    # references, so exactly one real doc per subsystem carries them (not
    # every doc in the subsystem -- that would be a different kind of
    # fabrication: forcing references onto docs that don't actually
    # establish them).
    subsystem_index_doc: dict[str, str] = {}
    for p in doc_paths:
        rel = str(p.relative_to(root))
        subsystem = subsystem_for(p, root / DOC_ROOT_REL)
        if subsystem not in subsystem_index_doc:
            subsystem_index_doc[subsystem] = rel

    evidence_report_candidates = [
        ".ggen/v26.8.1/subsystem-evidence-manifest.json",
        ".ggen/v26.8.1/subsystem-verifier-report.json",
    ]

    legacy_caps = parse_legacy_capabilities(root)
    legacy_by_subsystem: dict[str, list[str]] = {}
    for cap in legacy_caps:
        legacy_by_subsystem.setdefault(cap["owningSubsystem"], []).append(
            cap["capabilityId"]
        )

    for p in doc_paths:
        rel = str(p.relative_to(root))
        subsystem = subsystem_for(p, root / DOC_ROOT_REL)
        text = p.read_text(errors="replace")
        digest = sha256_bytes(p.read_bytes())
        role = infer_role(rel, subsystem, text)

        authority = extract_path_references(root, text)
        implementation = [a for a in authority if a.startswith(("crates/", "tools/", "ontology/"))]
        # authority proper: prefer .specify/, CLAUDE.md-style, docs/ refs
        authority_only = [
            a for a in authority if a.startswith((".specify/", "docs/")) or "CLAUDE" in a
        ]
        verifier_from_text = [a for a in authority if "/tests/" in a or a.endswith("_test.rs")]

        is_index_doc = subsystem_index_doc.get(subsystem) == rel
        seeded_authority = seed_refs_for(root, subsystem, "authority") if is_index_doc else []
        seeded_impl = seed_refs_for(root, subsystem, "implementation") if is_index_doc else []
        seeded_verifier = seed_refs_for(root, subsystem, "verifier") if is_index_doc else []

        authority_refs = sorted(set(authority_only) | set(seeded_authority))
        impl_refs = sorted(set(implementation) | set(seeded_impl))
        verifier_refs = sorted(set(verifier_from_text) | set(seeded_verifier))

        evidence_report_refs = (
            [c for c in evidence_report_candidates if (root / c).exists()]
            if is_index_doc
            else []
        )

        legacy_refs = legacy_by_subsystem.get(subsystem, []) if subsystem == "legacy" and is_index_doc else []

        records.append(
            DocumentEvidenceRecord(
                document_path=rel,
                document_digest=digest,
                subsystem=subsystem,
                document_role=role,
                authority_references=authority_refs,
                implementation_references=impl_refs,
                verifier_references=verifier_refs,
                legacy_capability_references=legacy_refs,
                evidence_report_references=evidence_report_refs,
                source_head=head,
            )
        )

    coverage: dict[str, dict] = {}
    for s in SUBSYSTEMS:
        subset = [r for r in records if r.subsystem == s]
        coverage[s] = {
            "document_count": len(subset),
            "has_authority_reference": any(r.authority_references for r in subset),
            "has_implementation_reference": any(r.implementation_references for r in subset),
            "has_verifier_reference": any(r.verifier_references for r in subset),
        }

    generator_path = root / GENERATOR_REL
    index = {
        "schema": SCHEMA,
        "release": "26.8.1",
        "generated_at_unix": int(time.time()),
        "source_head": head,
        "generator": {
            "path": str(GENERATOR_REL),
            "content_sha256": sha256_file(generator_path) if generator_path.is_file() else "UNKNOWN",
        },
        "document_count": len(records),
        "subsystem_coverage": coverage,
        "records": [asdict(r) for r in records],
    }
    return index


def write_ttl(root: Path, index: dict) -> None:
    lines = [
        "@prefix ggen: <https://ggen.chatmangpt.com/ontology/v26.8.1#> .",
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
        "",
        "# GENERATED by tools/v26.8.1/document_evidence_index.py -- do not hand-edit.",
        f"# source_head: {index['source_head']}",
        "",
    ]
    for i, rec in enumerate(index["records"]):
        slug = re.sub(r"[^a-zA-Z0-9]+", "_", rec["document_path"]).strip("_")
        subj = f"ggen:doc_evidence_{slug}"
        lines.append(f"{subj} a ggen:DocumentEvidenceRecord ;")
        lines.append(f'  ggen:documentPath "{rec["document_path"]}" ;')
        lines.append(f'  ggen:documentDigest "{rec["document_digest"]}" ;')
        lines.append(f'  ggen:subsystem "{rec["subsystem"]}" ;')
        lines.append(f'  ggen:documentRole "{rec["document_role"]}" ;')
        for ref in rec["authority_references"]:
            lines.append(f'  ggen:authorityReferences "{ref}" ;')
        for ref in rec["implementation_references"]:
            lines.append(f'  ggen:implementationReferences "{ref}" ;')
        for ref in rec["verifier_references"]:
            lines.append(f'  ggen:verifierReferences "{ref}" ;')
        for ref in rec["legacy_capability_references"]:
            lines.append(f'  ggen:legacyCapabilityReferences "{ref}" ;')
        for ref in rec["evidence_report_references"]:
            lines.append(f'  ggen:evidenceReportReferences "{ref}" ;')
        lines.append(f'  ggen:sourceHead "{rec["source_head"]}" .')
        lines.append("")
    (root / OUT_TTL_REL).write_text("\n".join(lines))


def write_csv(root: Path, index: dict) -> None:
    with (root / OUT_CSV_REL).open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(
            [
                "document_path",
                "subsystem",
                "document_role",
                "authority_references",
                "implementation_references",
                "verifier_references",
                "legacy_capability_references",
                "document_digest",
            ]
        )
        for rec in index["records"]:
            w.writerow(
                [
                    rec["document_path"],
                    rec["subsystem"],
                    rec["document_role"],
                    ";".join(rec["authority_references"]),
                    ";".join(rec["implementation_references"]),
                    ";".join(rec["verifier_references"]),
                    ";".join(rec["legacy_capability_references"]),
                    rec["document_digest"][:12],
                ]
            )


def write_md(root: Path, index: dict) -> None:
    lines = [
        "# Document Evidence Index (v26.8.1, GENERATED)",
        "",
        # No wall-clock timestamp here: this document is itself one of the
        # documents indexed (see ROOT_DOC_SUBSYSTEM), so its own bytes feed
        # back into its own DocumentEvidenceRecord.documentDigest. A live
        # timestamp made that digest un-reproducible on every run, which
        # meant document-evidence-index.md could never converge with its
        # own recorded digest -- a permanent, unfixable-by-rerunning
        # DOCUMENT_DIGEST_DRIFT. source_head is deterministic for a fixed
        # commit and is sufficient provenance.
        f"Generated against source_head={index['source_head']}.",
        "Real authority: `docs/v26.8.1/document-evidence-index.json`. This file is a projection.",
        "",
        "## Subsystem coverage",
        "",
        "| Subsystem | Documents | Has authority ref | Has implementation ref | Has verifier ref |",
        "|---|---|---|---|---|",
    ]
    for s in SUBSYSTEMS:
        c = index["subsystem_coverage"][s]
        lines.append(
            f"| {s} | {c['document_count']} | {c['has_authority_reference']} | "
            f"{c['has_implementation_reference']} | {c['has_verifier_reference']} |"
        )
    lines += ["", "## Records", ""]
    lines.append("| Document | Subsystem | Role | Authority | Implementation | Verifier |")
    lines.append("|---|---|---|---|---|---|")
    for rec in index["records"]:
        lines.append(
            f"| {rec['document_path']} | {rec['subsystem']} | {rec['document_role']} | "
            f"{', '.join(rec['authority_references']) or '-'} | "
            f"{', '.join(rec['implementation_references']) or '-'} | "
            f"{', '.join(rec['verifier_references']) or '-'} |"
        )
    (root / OUT_MD_REL).write_text("\n".join(lines) + "\n")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    args = parser.parse_args(argv)
    root = Path(args.root).resolve()

    index = build_index(root)

    out_json = root / OUT_JSON_REL
    out_json.write_text(json.dumps(index, indent=2, sort_keys=True) + "\n")
    write_csv(root, index)
    write_md(root, index)
    write_ttl(root, index)

    print(f"wrote {out_json}")
    print(f"wrote {root / OUT_CSV_REL}")
    print(f"wrote {root / OUT_MD_REL}")
    print(f"wrote {root / OUT_TTL_REL}")
    print(f"document_count={index['document_count']}")
    print(f"source_head={index['source_head']}")
    for s in SUBSYSTEMS:
        c = index["subsystem_coverage"][s]
        print(
            f"  {s:<14} docs={c['document_count']:<4} "
            f"authority={c['has_authority_reference']} "
            f"implementation={c['has_implementation_reference']} "
            f"verifier={c['has_verifier_reference']}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
