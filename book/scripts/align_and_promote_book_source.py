#!/usr/bin/env python3
"""Align every Level Five book chapter and promote the exact bytes into pack law.

This is a source migration, not a normal authoring loop.  The authoritative pack
stores each chapter as book:sourceText in
packs/level-five-book-pack/ontology.ttl; book/src is the generated product.

The migration:
1. discovers every book:Chapter already carried by the pack;
2. appends an evidence-backed capability/pack alignment section to every chapter;
3. adds the capability map as a first-class generated chapter;
4. updates each book:sourceText literal with the exact resulting Markdown bytes;
5. preserves the existing Turtle structure and listing literals.

After this script, delete book/ggen.lock and run the real ggen sync.  The sync
must reproduce book/src byte-for-byte and a second sync must be content-idempotent.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import re

REPO = Path(__file__).resolve().parents[2]
BOOK = REPO / "book"
SRC = BOOK / "src"
ONTOLOGY = REPO / "packs" / "level-five-book-pack" / "ontology.ttl"
SUMMARY = SRC / "SUMMARY.md"
CAPABILITY_MAP = SRC / "CAPABILITY_MAP.md"
CAPABILITY_LISTING = SRC / "listings" / "capability-map.txt"

BOOK_NS = "http://seanchatmangpt.github.io/packs/level-five-book#"
ALIGNMENT_HEADING = "## Repository capability alignment"

CHAPTER_RE = re.compile(
    r'(?P<prefix>book:chapter-[^\s]+\s+a\s+book:Chapter\s*;.*?'
    r'book:sourcePath\s+"(?P<path>[^"]+)"\s*;\s*'
    r'book:sourceText\s+""")'
    r'(?P<source>.*?)'
    r'(?P<suffix>"""\s*;\s*book:hasListing\s+book:[^\s]+\s*\.)',
    re.DOTALL,
)


@dataclass(frozen=True)
class Alignment:
    capabilities: tuple[str, ...]
    evidence: tuple[str, ...]
    witnesses: tuple[str, ...]
    standing: str
    gap: str


A = Alignment
ALIGNMENTS: dict[str, Alignment] = {
    "front-matter": A(
        ("ggen-config manifest admission", "ggen-engine pack projection", "mdBook rendering"),
        ("book/ggen.toml", "packs/level-five-book-pack/pack.toml", "book/scripts/check_book.py"),
        ("packs/level-five-book-pack",),
        "PACK_WITNESS",
        "Reader comprehension is not runtime standing; the self-hosted book gate must execute on the current commit.",
    ),
    "foundations": A(
        ("sync orchestration", "deterministic graph state", "bounded writes", "receipt emission"),
        ("crates/ggen-engine/src/lib.rs", "crates/ggen-engine/src/verbs/sync.rs", "crates/ggen-graph/src/lib.rs"),
        ("packs/ggen-verify-pack", "packs/star-toml-pack"),
        "PARTIAL",
        "The primitives are implemented, but no single pack witnesses every whole-system claim at Level Five.",
    ),
    "pack-model": A(
        ("pack parsing and resolution", "content hashing", "marketplace lifecycle", "admissibility-pack emission"),
        ("crates/ggen-engine/src/pack.rs", "crates/ggen-marketplace/src/packs_registry", "crates/ggen-lsp/src/pack"),
        ("packs/lsp-max-pack", "packs/star-toml-pack", "packs/ggen-verify-pack"),
        "PACK_WITNESS",
        "Passport and substitution claims remain bounded to the marketplace paths that are actually consumed and verified.",
    ),
    "pack-taxonomy": A(
        ("code-generation packs", "knowledge-hook packs", "case-study corpus packs", "marketplace package resolution"),
        ("crates/ggen-engine/src/pack.rs", "crates/praxis-graphlaw/src/hooks", "crates/ggen-marketplace/src/packs_registry"),
        ("packs/star-toml-pack", "packs/ggen-verify-pack", "packs/ma-case-study-pack"),
        "PACK_WITNESS",
        "The taxonomy is witnessed, but not every subtype has a complete Level Five consumer.",
    ),
    "maturity": A(
        ("standing classification", "consumer verification", "mutation sensitivity", "drift refusal"),
        ("docs/packs/PACK_MATURITY_MODEL.md", "book/scripts/check_level_five.py", "crates/ggen-engine/tests/composed_packs_e2e.rs"),
        ("packs/ggen-verify-pack", "packs/star-toml-pack", "packs/lsp-max-pack"),
        "PARTIAL",
        "Several cells have real witnesses; universal Level Five substitution remains a target, not a repository-wide fact.",
    ),
    "reference": A(
        ("independent reference fixtures", "reference digests", "behavioral comparison"),
        ("packs/tcps-core-pack/reference/製品版", "packs/tcps-core-pack/source-manifest.json", "book/scripts/check_level_five.py"),
        ("packs/star-toml-pack", "packs/lsp-max-pack", "packs/tcps-core-pack"),
        "PARTIAL",
        "TCPS carries a reference tree, but not every book pattern has an independent external oracle.",
    ),
    "ontology": A(
        ("RDF parsing", "stable identities", "SPARQL execution", "graph-law materialization"),
        ("crates/ggen-graph/src/lib.rs", "crates/praxis-graphlaw/src/lib.rs", "crates/ggen-engine/src/pack.rs"),
        ("packs/star-toml-pack", "packs/lsp-max-pack", "packs/praxis-core-pack"),
        "IMPLEMENTED",
        "Semantic modeling quality remains pack-specific and must be falsified by each pack's own gates and consumers.",
    ),
    "admission": A(
        ("pack SPARQL gates", "typed pre-write refusal", "reasoner-independent admission"),
        ("crates/ggen-engine/src/pack.rs", "crates/ggen-engine/tests/reasoner_independence_e2e.rs", "packs/ggen-verify-pack/gates"),
        ("packs/ggen-verify-pack", "packs/lsp-max-pack", "packs/star-toml-pack"),
        "PACK_WITNESS",
        "A chapter's specific constraint is alive only when a negative fixture reaches the expected typed refusal.",
    ),
    "sparql": A(
        ("SELECT extraction", "CONSTRUCT materialization", "deterministic row order", "union-graph isolation"),
        ("crates/ggen-graph/src/lib.rs", "crates/praxis-graphlaw/src/lib.rs", "crates/ggen-engine/tests/reasoner_independence_e2e.rs"),
        ("packs/star-toml-pack", "packs/lsp-max-pack", "packs/praxis-core-pack"),
        "PACK_WITNESS",
        "Unsupported SPARQL features and cross-pack leakage must remain typed refusals rather than silent zero-row success.",
    ),
    "tera": A(
        ("template front matter", "per-row output routing", "checksum freeze", "single-writer ownership"),
        ("crates/ggen-engine/src/pack.rs", "packs/level-five-book-pack/templates/chapter.md.tmpl", "packs/star-toml-pack/templates"),
        ("packs/level-five-book-pack", "packs/star-toml-pack", "packs/lsp-max-pack"),
        "PACK_WITNESS",
        "Template syntax alone is insufficient; a real second sync must prove output stability.",
    ),
    "rust-generation": A(
        ("Rust module generation", "manifest projection", "multi-target surfaces"),
        ("packs/star-toml-pack/templates", "packs/cargo-cicd-pack", "packs/wasm4pm-facts-pack"),
        ("packs/star-toml-pack", "packs/cargo-cicd-pack", "packs/wasm4pm-facts-pack"),
        "PARTIAL",
        "Complete C, WASM, no_std and multi-crate substitution is not demonstrated uniformly across the repository.",
    ),
    "generated-proof": A(
        ("generated proof surfaces", "independent expectations", "mutation failure", "verification facts"),
        ("examples/star-toml-verify", "examples/lsp-max-verify", "packs/ggen-verify-pack"),
        ("packs/star-toml-pack", "packs/lsp-max-pack", "packs/ggen-verify-pack"),
        "PACK_WITNESS",
        "A generated proof is not independent merely because it is emitted into a separate file.",
    ),
    "consumer": A(
        ("ggen.toml consumption", "sync", "build/test", "receipt replay"),
        ("examples/star-toml-verify", "examples/lsp-max-verify", "examples/affidavit-verify"),
        ("packs/star-toml-pack", "packs/lsp-max-pack", "packs/affidavit-pack"),
        "PACK_WITNESS",
        "Zero-knowledge standing is bounded to consumers that succeed from a clean checkout without author intervention.",
    ),
    "composition": A(
        ("union-graph composition", "non-interference", "output collision refusal", "package composition"),
        ("crates/ggen-marketplace/src/packs_registry/compose.rs", "crates/ggen-engine/tests/composed_packs_e2e.rs", "crates/ggen-engine/src/pack.rs"),
        ("packs/repo-as-found-pack", "packs/repo-load-path-pack", "packs/repo-reconciliation-pack"),
        "PARTIAL",
        "The repository has multi-pack tests, but every possible pack neighborhood is not proven safe.",
    ),
    "regeneration": A(
        ("content-hash locks", "checksum freeze", "idempotent sync", "drift refusal"),
        ("book/ggen.lock", "packs/star-toml-pack/pack.toml", "packs/lsp-max-pack/pack.toml"),
        ("packs/star-toml-pack", "packs/lsp-max-pack", "packs/level-five-book-pack"),
        "PACK_WITNESS",
        "Longitudinal repair across arbitrary local edits remains narrower than byte-identical regeneration.",
    ),
    "receipts": A(
        ("input/output digests", "receipt chains", "verification evidence", "standing ceilings"),
        ("crates/praxis-core/src/receipt_epoch.rs", "crates/ggen-engine/tests/receipt_chain_e2e.rs", "packs/ggen-verify-pack"),
        ("packs/ggen-verify-pack", "packs/level-five-book-pack"),
        "PARTIAL",
        "Several equivalence classes remain explicitly bounded; presence in a receipt is not proof of semantic equivalence.",
    ),
    "engine": A(
        ("schema dispatch", "pack gate execution", "write semantics", "typed error codes"),
        ("crates/ggen-engine/src/lib.rs", "crates/ggen-engine/src/pack.rs", "crates/ggen-engine/tests/reasoner_independence_e2e.rs"),
        ("packs/ggen-verify-pack", "packs/star-toml-pack"),
        "IMPLEMENTED",
        "Whether an engine change is necessary remains a case-specific design judgment that must be proven against multiple packs.",
    ),
    "level-five-design": A(
        ("product inventory", "ontology-to-artifact matrices", "acceptance matrices", "definition of done"),
        ("TCPS-PACK-ARD-PRD.md", "book/code/packs/canonical-level-five-pack", "book/scripts/check_level_five.py"),
        ("packs/tcps-core-pack", "packs/tcps-release-pack"),
        "PARTIAL",
        "The planning artifacts exist; current-head replay is required before crown standing.",
    ),
    "tcps-core": A(
        ("TCPS domain ontology", "reference conformance", "generated core modules"),
        ("packs/tcps-core-pack", "packs/tcps-core-pack/reference/製品版", "book/scripts/check_level_five.py"),
        ("packs/tcps-core-pack",),
        "PARTIAL",
        "Reference coverage is checked, but the complete current-head product replay remains required.",
    ),
    "tcps-generation": A(
        ("TCPS pack sync", "reference-test reuse", "idempotent core generation"),
        ("packs/tcps-core-pack", "examples/tcps-generated", "book/scripts/check_level_five.py"),
        ("packs/tcps-core-pack", "packs/ggen-verify-pack"),
        "PARTIAL",
        "Recorded prior runs do not substitute for the current commit's self-hosted verification result.",
    ),
    "tcps-product": A(
        ("multi-crate product projection", "workspace manifests", "target surfaces"),
        ("packs/tcps-core-pack", "packs/tcps-release-pack", "examples/tcps-generated"),
        ("packs/tcps-core-pack", "packs/tcps-release-pack"),
        "PARTIAL",
        "Every target family must be built and tested before complete-substitute standing.",
    ),
    "tcps-release": A(
        ("release asset generation", "SBOM/provenance paths", "delivery matrices"),
        ("packs/tcps-release-pack", "packs/cargo-cicd-pack", "examples/tcps-generated/scripts/verify.sh"),
        ("packs/tcps-release-pack", "packs/cargo-cicd-pack"),
        "PARTIAL",
        "Unavailable external SDKs or signers must remain explicit bounded outcomes, never false green.",
    ),
    "tcps-failures": A(
        ("defect-to-gate conversion", "negative fixtures", "typed refusal regression"),
        ("packs/tcps-core-pack", "packs/tcps-release-pack", "crates/ggen-engine/tests"),
        ("packs/tcps-core-pack", "packs/ggen-verify-pack"),
        "PARTIAL",
        "A defect class is closed only when sabotage reproduces the failure and the new gate catches it earlier.",
    ),
    "tcps-standing": A(
        ("inspection receipts", "reference-tree comparison", "evidence matrix closure"),
        ("book/scripts/check_level_five.py", "packs/tcps-core-pack/source-manifest.json", "packs/tcps-release-pack/source-manifest.json"),
        ("packs/tcps-core-pack", "packs/tcps-release-pack", "packs/ggen-verify-pack"),
        "PARTIAL",
        "The standing ceiling is the weakest current-head evidence cell, not the strongest historical result.",
    ),
    "practicum": A(
        ("new-domain pack design", "consumer construction", "certification bundle"),
        ("book/code/packs/canonical-level-five-pack", "book/code/examples/canonical-level-five-consumer", "book/scripts/check_level_five.py"),
        ("packs/level-five-book-pack",),
        "TARGET",
        "No repository pack can witness an arbitrary reader-selected domain before the reader builds and verifies it.",
    ),
    "certification": A(
        ("adversarial mutation", "multi-pack safety", "idempotency", "receipt-chain verification"),
        ("book/code/packs/canonical-level-five-pack", "book/code/examples/canonical-level-five-consumer", "book/scripts/check_level_five.py"),
        ("packs/ggen-verify-pack", "packs/level-five-book-pack"),
        "PARTIAL",
        "The laboratory assets exist; each laboratory must execute on the current head before graduation.",
    ),
    "appendices": A(
        ("current manifest and template reference", "failure codes", "acceptance worksheets"),
        ("book/ggen.toml", "packs/level-five-book-pack/pack.toml", "crates/ggen-engine/src/pack.rs"),
        ("packs/level-five-book-pack",),
        "PARTIAL",
        "Reference text is subordinate to the live parser, engine behavior and current pack manifests.",
    ),
}


def escape_turtle_long(value: str) -> str:
    value = value.replace("\r\n", "\n").replace("\r", "\n")
    value = value.replace("\\", "\\\\")
    value = value.replace('"""', '\\"\\"\\"')
    return value


def category_for(path: str) -> str:
    if "/" not in path:
        return "front-matter"
    category = path.split("/", 1)[0]
    return category if category in ALIGNMENTS else "appendices"


def render_alignment(path: str) -> str:
    alignment = ALIGNMENTS[category_for(path)]
    for evidence in alignment.evidence:
        if not (REPO / evidence).exists():
            raise SystemExit(f"alignment evidence does not exist: {evidence} (chapter {path})")
    capabilities = "\n".join(f"- `{item}`" for item in alignment.capabilities)
    evidence = "\n".join(f"- `{item}`" for item in alignment.evidence)
    witnesses = "\n".join(f"- `{item}`" for item in alignment.witnesses)
    return f"""{ALIGNMENT_HEADING}

This section is generated from the repository capability ledger. It distinguishes live machinery from demonstrated pack witnesses and from target-state guidance.

### Owning capabilities

{capabilities}

### Current repository evidence

{evidence}

### Pack witnesses

{witnesses}

### Bounded standing

`{alignment.standing}`

### Open gap

{alignment.gap}

### Required falsifier

Run the narrow evidence path named above, then execute the chapter's consumer or mutation test. A missing tool, skipped consumer, stale lock, changed generated byte, or unverifiable receipt lowers standing; it never counts as success.
"""


def align_chapter(path: str, text: str) -> str:
    if path == "SUMMARY.md":
        marker = "# Using the Pattern Language\n"
        link = "- [Repository Capability and Pack Map](CAPABILITY_MAP.md)\n"
        if link not in text:
            if marker not in text:
                raise SystemExit("SUMMARY.md has no Using the Pattern Language heading")
            text = text.replace(marker, marker + "\n" + link, 1)
        return text.rstrip() + "\n"

    if ALIGNMENT_HEADING in text:
        text = text.split(ALIGNMENT_HEADING, 1)[0].rstrip()
    return text.rstrip() + "\n\n" + render_alignment(path).rstrip() + "\n"


def append_capability_subject(ontology: str) -> str:
    if "book:chapter-capability-map a book:Chapter" in ontology:
        return ontology
    CAPABILITY_LISTING.parent.mkdir(parents=True, exist_ok=True)
    CAPABILITY_LISTING.write_text(
        "Generated witness for the repository capability and pack map.\n",
        encoding="utf-8",
    )
    chapter = escape_turtle_long(CAPABILITY_MAP.read_text(encoding="utf-8"))
    listing = escape_turtle_long(CAPABILITY_LISTING.read_text(encoding="utf-8"))
    block = f'''\n\n# ── repository capability map ─────────────────────────────────────\nbook:chapter-capability-map a book:Chapter ;\n    book:number 0 ;\n    book:title "Repository Capability and Pack Map" ;\n    book:slug "repository-capability-pack-map" ;\n    book:part "front-matter" ;\n    book:sourcePath "CAPABILITY_MAP.md" ;\n    book:sourceText """{chapter}""" ;\n    book:hasListing book:listing-capability-map .\n\nbook:listing-capability-map a book:Listing ;\n    book:sourcePath "listings/capability-map.txt" ;\n    book:sourceText """{listing}""" .\n'''
    return ontology.rstrip() + block


def main() -> None:
    ontology = ONTOLOGY.read_text(encoding="utf-8")
    ontology = append_capability_subject(ontology)

    matches = list(CHAPTER_RE.finditer(ontology))
    if len(matches) < 367:
        raise SystemExit(f"expected at least 367 chapter source records after map insertion; found {len(matches)}")

    paths = [match.group("path") for match in matches]
    if len(paths) != len(set(paths)):
        raise SystemExit("duplicate book:sourcePath among chapter records")

    aligned: dict[str, str] = {}
    for path in paths:
        target = SRC / path
        if not target.exists():
            raise SystemExit(f"chapter source missing from generated tree: {path}")
        text = target.read_text(encoding="utf-8")
        updated = align_chapter(path, text)
        target.write_text(updated, encoding="utf-8")
        aligned[path] = updated

    def replace(match: re.Match[str]) -> str:
        path = match.group("path")
        return match.group("prefix") + escape_turtle_long(aligned[path]) + match.group("suffix")

    updated_ontology, replacement_count = CHAPTER_RE.subn(replace, ontology)
    if replacement_count != len(matches):
        raise SystemExit(
            f"chapter source replacement mismatch: matched={len(matches)} replaced={replacement_count}"
        )

    ONTOLOGY.write_text(updated_ontology.rstrip() + "\n", encoding="utf-8")
    print(
        f"aligned and promoted {replacement_count} chapter sources; "
        f"ontology={ONTOLOGY.relative_to(REPO)}"
    )


if __name__ == "__main__":
    main()
