#!/usr/bin/env python3
"""Rewrite every Markdown chapter linked by SUMMARY.md as an Alexander-style pattern.

The transform is deterministic and source-preserving:
- SUMMARY.md supplies order, field, title, and path.
- Existing chapter sections supply chapter-specific evidence, code, laboratories, and gates.
- Category profiles bind each pattern to the live ggen crate architecture.
- The output uses one pattern grammar: context, forces, therefore, configuration,
  consequences, falsifier, connections, implementation, laboratory, acceptance.

Run from any directory:
    python3 book/scripts/rewrite_pattern_language.py
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import re
import textwrap

BOOK = Path(__file__).resolve().parents[1]
SRC = BOOK / "src"
SUMMARY = SRC / "SUMMARY.md"

LINK_RE = re.compile(r"^- \[(?P<title>.+?)\]\((?P<path>[^)]+\.md)\)\s*$")
FIELD_RE = re.compile(r"^# Field (?P<number>[IVXLCDM]+) — (?P<title>.+)$")
HEADING_RE = re.compile(r"^## (?P<name>.+?)\s*$", re.MULTILINE)


@dataclass(frozen=True)
class Entry:
    title: str
    path: str
    field: str
    field_description: str


@dataclass(frozen=True)
class Profile:
    place: str
    forces: tuple[str, ...]
    crates: tuple[str, ...]
    consequence: str
    falsifier: str


COMMON_FORCES = (
    "The source of truth must remain distinguishable from every generated projection.",
    "A successful render is weaker than an admitted, consumed, independently verified consequence.",
    "Composition creates hidden coupling unless identity, ownership, and output boundaries are explicit.",
    "Fast regeneration is useful only when unchanged inputs reproduce unchanged bytes and changed inputs expose drift.",
    "Every actuation must terminate in a durable receipt or an explicit typed refusal.",
)

PROFILES: dict[str, Profile] = {
    "front-matter": Profile(
        "the threshold of the book, before a pack writer selects a product surface",
        COMMON_FORCES,
        ("ggen-config", "ggen-engine", "ggen-graph", "ggen-marketplace", "ggen-lsp"),
        "The reader enters the pattern language with a shared vocabulary, a bounded objective, and an executable definition of completion.",
        "A reader can finish the section while still being unable to name the source law, consumer, verifier, receipt, or refusal condition.",
    ),
    "foundations": Profile(
        "the whole manufacturing system, where ontology, projection, actuation, and proof first become one process",
        COMMON_FORCES,
        ("ggen-engine", "praxis-graphlaw", "ggen-graph", "praxis-core"),
        "The pack is judged as a manufacturing system rather than as a directory of templates.",
        "The claimed artifact can be produced only by hand-editing output, bypassing admission, or omitting receipt verification.",
    ),
    "pack-model": Profile(
        "the boundary of one movable pack inside a larger graph and filesystem",
        COMMON_FORCES,
        ("ggen-config", "ggen-marketplace", "ggen-engine", "ggen-lsp"),
        "The pack becomes an identifiable, routable, hash-bound part with declared inputs, outputs, policies, and lifecycle.",
        "Two consumers cannot determine the same identity, owned outputs, required gates, and provenance from the pack itself.",
    ),
    "pack-taxonomy": Profile(
        "the design decision where a capability must be assigned the smallest lawful pack form",
        COMMON_FORCES,
        ("ggen-marketplace", "ggen-engine", "ggen-lsp"),
        "The capability receives enough machinery to carry its law and proof without importing unrelated engine responsibility.",
        "The chosen pack type either cannot reach a real consumer or contains machinery that belongs to another lifecycle boundary.",
    ),
    "maturity": Profile(
        "the promotion boundary between an attractive demonstration and a substitutable manufactured part",
        COMMON_FORCES,
        ("ggen-engine", "ggen-cheat-scanner", "chicago-tdd-tools", "praxis-core"),
        "Standing advances cell by cell from inert material to independently verified substitution.",
        "A higher maturity label survives after removing the consumer, mutation test, idempotency check, or receipt.",
    ),
    "reference": Profile(
        "the boundary between the pack and the independent reality it claims to reproduce",
        COMMON_FORCES,
        ("ggen-engine", "ggen-graph", "chicago-tdd-tools"),
        "The pack is calibrated against an oracle that can disagree with its ontology and templates.",
        "Expected values, fixtures, and generated subjects all descend from the same potentially defective transformation.",
    ),
    "ontology": Profile(
        "the semantic neighborhood from which all later queries and artifacts are projected",
        COMMON_FORCES,
        ("praxis-graphlaw", "ggen-graph", "ggen-config"),
        "Stable identities and public relations carry domain meaning independently of any one target language.",
        "A target-language rename, file move, or second pack causes semantic identity to change or collide.",
    ),
    "admission": Profile(
        "the gate between parseable RDF and graph state allowed to manufacture artifacts",
        COMMON_FORCES,
        ("praxis-graphlaw", "ggen-graph", "ggen-engine"),
        "Malformed, ambiguous, contaminating, or unsafe graph states are refused before rendering or writes begin.",
        "A negative or adversarial fixture reaches template rendering or filesystem actuation without a typed refusal.",
    ),
    "sparql": Profile(
        "the extraction boundary where admitted graph facts become deterministic construction rows",
        COMMON_FORCES,
        ("praxis-graphlaw", "ggen-graph", "ggen-engine"),
        "Queries select only owned facts, derive only declared law, and return rows in a stable order.",
        "Adding an unrelated pack changes selected rows, row order, or output bytes without changing the owning pack.",
    ),
    "tera": Profile(
        "the rendering boundary where selected facts become bytes and output paths",
        COMMON_FORCES,
        ("ggen-engine", "ggen-config"),
        "Every template has one declared ownership domain, deterministic inputs, and a write mode compatible with local work.",
        "Two writers claim one path, a missing merge marker destroys local content, or a second sync changes bytes unexpectedly.",
    ),
    "rust-generation": Profile(
        "the product surface where graph law must become complete compilable Rust rather than fragments",
        COMMON_FORCES,
        ("ggen-engine", "ggen-config", "chicago-tdd-tools"),
        "Generated crates expose coherent APIs, manifests, modules, and target-specific boundaries requiring no hidden repair.",
        "A clean consumer must invent missing modules, signatures, manifests, safety constraints, or cross-crate wiring.",
    ),
    "generated-proof": Profile(
        "the adjacent proof surface built beside each generated capability",
        COMMON_FORCES,
        ("chicago-tdd-tools", "ggen-cheat-scanner", "ggen-engine"),
        "Independent compile-time and runtime checks fail when the manufactured subject is meaningfully corrupted.",
        "The proof remains green after mutating the generated behavior, deleting an output, or replacing expected values with tautologies.",
    ),
    "consumer": Profile(
        "the place where a pack meets a clean project with no access to the author’s tacit knowledge",
        COMMON_FORCES,
        ("ggen-config", "ggen-engine", "ggen-cli-lib"),
        "A zero-knowledge consumer can resolve, sync, build, run, test, verify, and replay the pack from declared inputs alone.",
        "The consumer succeeds only with undocumented environment state, hand edits, private paths, or author intervention.",
    ),
    "composition": Profile(
        "the neighborhood formed when multiple packs share a union graph and output tree",
        COMMON_FORCES,
        ("ggen-marketplace", "ggen-engine", "ggen-graph"),
        "Packs compose through shared public vocabulary while preserving local identity, non-interference, and single-writer ownership.",
        "Introducing a second valid pack changes another pack’s rows, paths, module wiring, or receipts.",
    ),
    "regeneration": Profile(
        "the repeated lifecycle where source law, generated outputs, and local work evolve over time",
        COMMON_FORCES,
        ("ggen-engine", "ggen-graph", "praxis-core"),
        "Regeneration becomes ordinary, idempotent operation with explicit drift and repair semantics.",
        "An unchanged admitted state produces changed bytes, or a changed source silently preserves stale output.",
    ),
    "receipts": Profile(
        "the evidence chain binding admitted observations to actuated consequences",
        COMMON_FORCES,
        ("praxis-core", "ggen-engine", "ggen-graph", "ggen-marketplace"),
        "Inputs, graph state, templates, outputs, verification, and signatures form a replayable provenance chain.",
        "Tampering with a recorded input, output, transition, or signature does not invalidate verification.",
    ),
    "engine": Profile(
        "the boundary where pack law may be insufficient and shared engine semantics are considered",
        COMMON_FORCES,
        ("ggen-engine", "praxis-graphlaw", "ggen-config", "ggen-cli-lib"),
        "Engine changes remain minimal, typed, multi-pack verified, and justified by semantics no pack can lawfully carry alone.",
        "The same outcome can be achieved through pack ontology, gates, queries, templates, or consumer wiring without engine modification.",
    ),
    "level-five-design": Profile(
        "the generative sequence that turns a bounded product surface into a Level Five pack plan",
        COMMON_FORCES,
        ("ggen-config", "ggen-engine", "ggen-marketplace", "ggen-graph"),
        "Every product artifact, oracle, proof, drift rule, receipt, and release surface is named before irreversible implementation.",
        "A required artifact or acceptance consequence appears during implementation without an owner or matrix cell.",
    ),
    "tcps-core": Profile(
        "the TCPS domain, where Japanese production concepts provide the canonical semantic language",
        COMMON_FORCES,
        ("ggen-graph", "praxis-graphlaw", "genesis-types-v2", "genesis-core-v2"),
        "The case study demonstrates that rich vocabulary and typestate can survive graph projection into a complete product.",
        "A canonical TCPS concept is reduced to an English label, loses its invariant, or cannot be traced to generated behavior.",
    ),
    "tcps-generation": Profile(
        "the TCPS core-pack manufacturing line",
        COMMON_FORCES,
        ("ggen-engine", "praxis-graphlaw", "chicago-tdd-tools"),
        "The pack reproduces the reference core and its tests from RDF without making generated Rust authoritative.",
        "The original reference tests cannot run unchanged or a second sync changes the generated core.",
    ),
    "tcps-product": Profile(
        "the expansion from one generated crate to a complete multi-crate TCPS product",
        COMMON_FORCES,
        ("ggen-engine", "ggen-config", "cargo-cicd"),
        "Core, standard-library, FFI, WASM, CLI, and workspace surfaces are generated as one coherent product graph.",
        "Any target requires manual cross-crate wiring or an independently maintained manifest.",
    ),
    "tcps-release": Profile(
        "the release manufacturing system surrounding the TCPS product",
        COMMON_FORCES,
        ("cargo-cicd", "ggen-engine", "ggen-marketplace"),
        "Release workflows, packages, matrices, SBOMs, attestations, and signing paths remain projections of one admitted release model.",
        "A supported target exists in prose or code but is absent from the release inventory, generated assets, or provenance chain.",
    ),
    "tcps-failures": Profile(
        "the defect ledger where failed TCPS runs become reusable production law",
        COMMON_FORCES,
        ("ggen-engine", "ggen-cheat-scanner", "chicago-tdd-tools"),
        "Every discovered defect becomes a gate, negative fixture, ownership rule, or verifier improvement.",
        "The same defect class can recur without triggering an earlier admission or verification failure.",
    ),
    "tcps-standing": Profile(
        "the inspection boundary where TCPS claims receive bounded standing",
        COMMON_FORCES,
        ("praxis-core", "ggen-engine", "cargo-cicd"),
        "Each maturity claim names exact evidence, omissions, corrective divergences, and replay commands.",
        "The claimed standing survives after deleting or invalidating one of its named receipts.",
    ),
    "practicum": Profile(
        "the reader’s own project, after the complete language has been observed in TCPS",
        COMMON_FORCES,
        ("all pack-facing crates",),
        "The reader applies the sequence to a new bounded domain and produces a certification bundle rather than a demonstration.",
        "The final product depends on an unrecorded design decision, hidden expert action, or self-authored oracle.",
    ),
    "certification": Profile(
        "the adversarial laboratory where one claimed property is deliberately broken and repaired",
        COMMON_FORCES,
        ("ggen-engine", "ggen-graph", "ggen-cheat-scanner", "chicago-tdd-tools"),
        "The reader proves both the success path and the refusal path for a concrete Level Five property.",
        "The laboratory can pass without observing the intended failure before repair.",
    ),
    "appendices": Profile(
        "the reference shelf used while authoring and inspecting packs",
        COMMON_FORCES,
        ("ggen-config", "ggen-engine", "ggen-graph", "ggen-marketplace"),
        "Canonical layouts, schemas, examples, and worksheets remain directly usable and subordinate to live crate behavior.",
        "Copying the reference produces a pack that fails current parsing, admission, generation, or verification.",
    ),
}


def parse_entries() -> list[Entry]:
    field = "Using the Pattern Language"
    description = "Establish the reader, notation, laboratory, and final acceptance boundary."
    entries: list[Entry] = []
    pending_field_description = False
    for raw in SUMMARY.read_text(encoding="utf-8").splitlines():
        match = FIELD_RE.match(raw)
        if match:
            field = match.group("title")
            description = ""
            pending_field_description = True
            continue
        if pending_field_description and raw.strip() and not raw.startswith("#"):
            description = raw.strip()
            pending_field_description = False
        link = LINK_RE.match(raw)
        if not link:
            continue
        path = link.group("path")
        if path in {"README.md", "SUMMARY.md"}:
            continue
        entries.append(Entry(link.group("title"), path, field, description))
    return entries


def sections(text: str) -> dict[str, str]:
    matches = list(HEADING_RE.finditer(text))
    result: dict[str, str] = {}
    for index, match in enumerate(matches):
        start = match.end()
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        result[match.group("name").strip().lower()] = text[start:end].strip()
    return result


def choose(data: dict[str, str], *names: str, default: str = "") -> str:
    for name in names:
        value = data.get(name.lower())
        if value:
            return value
    return default


def title_number(title: str) -> str:
    match = re.match(r"(\d+)\.", title)
    return match.group(1) if match else "reference"


def short_name(title: str) -> str:
    return re.sub(r"^\d+\.\s*", "", title).strip()


def imperative(title: str) -> str:
    name = short_name(title)
    lower = name.lower()
    if lower.startswith(("why ", "when ", "what ", "how ")):
        return f"Make **{name}** an explicit design decision with a named owner, verifier, and refusal condition."
    if lower.startswith(("detect ", "refuse ", "generate ", "build ", "run ", "record ", "define ", "create ", "author ", "prove ", "emit ", "select ", "locate ", "inventory ", "classify ", "write ", "model ", "reuse ", "compose ", "stop ", "resolve ", "add ", "turn ", "compare ", "score ", "close ", "present ")):
        return f"**{name}.** Perform the named operation at its owning layer, and preserve the evidence needed to replay both acceptance and refusal."
    return f"Establish **{name}** as a named part of the pack contract rather than leaving it as convention or tacit knowledge."


def default_context(entry: Entry, profile: Profile) -> str:
    return textwrap.dedent(f"""
    You are working at {profile.place}. The immediate problem is **{short_name(entry.title)}**.

    The surrounding field is **{entry.field}**: {entry.field_description or 'the pattern participates in the larger ggen manufacturing language.'} The decision cannot be evaluated as isolated prose. It changes what may enter the admitted graph, what the engine may project, which path may be actuated, what a clean consumer can observe, and which receipt can later prove the consequence.
    """).strip()


def crate_alignment(profile: Profile) -> str:
    items = "\n".join(f"- `{crate}`" if crate != "all pack-facing crates" else "- all pack-facing crates selected by the product boundary" for crate in profile.crates)
    return f"The pattern is grounded in these live ownership surfaces:\n\n{items}\n\nA pack may depend on these surfaces, but it must not silently absorb their responsibilities."


def rewrite(entry: Entry) -> str:
    path = SRC / entry.path
    original = path.read_text(encoding="utf-8")
    data = sections(original)
    category = entry.path.split("/", 1)[0]
    profile = PROFILES.get(category, PROFILES["appendices"])

    context = choose(data, "production problem", "purpose", "context", default=default_context(entry, profile))
    governing = choose(data, "governing law", default="`A = μ(O*)`, with standing established only by replayable evidence of the admitted transformation.")
    construction = choose(data, "construction sequence", "procedure", "workflow", default="1. Admit the source.\n2. Apply the pattern at its owning layer.\n3. Actuate only through the declared generator path.\n4. Verify in a clean consumer.\n5. Preserve the receipt and refusal evidence.")
    implementation = choose(data, "reference implementation", "canonical form", "example", default="```text\nsource law → admission → projection → consumer consequence → receipt\n```")
    verification = choose(data, "verification procedure", "verification", default="Run the narrow verifier first, mutate the manufactured subject, confirm failure, restore the source law, then expand through integration, end-to-end, idempotency, and receipt verification.")
    failures = choose(data, "failure modes", "common failures", default="- source and projection are conflated;\n- output ownership is ambiguous;\n- the verifier shares the generator’s defect;\n- a missing tool is reported as success;\n- the receipt does not bind the observed consequence.")
    laboratory = choose(data, "laboratory", "exercise", default=f"Create the smallest valid example of **{short_name(entry.title)}**, then introduce one mutation that should violate the pattern. Preserve the failing evidence before repair.")
    gate = choose(data, "acceptance gate", "definition of done", default="- [ ] The source law is named.\n- [ ] Admission has a negative fixture.\n- [ ] A clean consumer observes the result.\n- [ ] A mutation fails independently.\n- [ ] A receipt binds the exact run.")
    case_study = choose(data, "continuous tcps case study", "case study", default="The TCPS sequence supplies the continuous product-scale example. Apply this pattern there only where it preserves the canonical vocabulary, complete generated surface, independent tests, safe regeneration, and inspection standing.")

    forces = "\n".join(f"- {force}" for force in profile.forces)
    number = title_number(entry.title)

    return f"""# {entry.title}

> **Pattern {number} · {entry.field}**
>
> **Standing rule:** this pattern is `ALIVE` only when its consequence has been observed in a real consumer and bound to replayable evidence.

## Context

{context}

## Problem

Without a stable pattern for **{short_name(entry.title)}**, locally reasonable decisions accumulate into a pack that renders but does not manufacture a substitutable product. The defect normally appears later—as graph contamination, hidden handler work, path collision, drift, false proof, or an unverifiable release claim—when repair is more expensive and evidence is weaker.

## Forces

{forces}

## Governing law

{governing}

## Therefore

{imperative(entry.title)}

Do this at the narrowest layer that owns the invariant. Keep graph-domain construction reversible until admission is complete. Route filesystem or external effects through the engine’s declared write path. Require the consumer and verifier to observe the intended consequence independently of the authoring convenience that produced it.

## Configuration

{construction}

### Crate alignment

{crate_alignment(profile)}

## Reference implementation

{implementation}

## Verification procedure

{verification}

Use the verification ladder in order:

```text
unit → integration → end-to-end → adversarial mutation → idempotency → receipt verification
```

A lower rung may establish `PARTIAL_ALIVE`; it cannot establish the crown claim of the higher rungs.

## Resulting context

{profile.consequence}

The pattern also creates obligations. The new source law must remain inspectable. Generated outputs must retain one owner. Consumers must not acquire hidden setup. Receipts must be regenerated whenever an admitted input or verifier changes. Neighboring patterns can now rely on this consequence without reintroducing the resolved ambiguity.

## Failure modes

{failures}

## Falsifier

{profile.falsifier}

Execute that falsifier deliberately. A pattern with no plausible way to fail is a slogan, not production law.

## Continuous TCPS case study

{case_study}

## Laboratory

{laboratory}

## Acceptance gate

{gate}

## Connections

This pattern receives its context from **{entry.field}** and passes a narrower, better-admitted state to the patterns that follow it in `SUMMARY.md`. When a later pattern fails, trace backward through source identity, admission, projection, ownership, consumer observation, and receipt rather than patching the generated artifact.

## Standing statement

The pattern is complete only when every checked acceptance item resolves to a concrete repository artifact or command result. Missing execution is `UNKNOWN`; a missing admitted dependency is `BLOCKED`; an unreachable verifier caused by the build is `BUILD_BROKEN`; an intentionally absent capability is `UNSUPPORTED`. None of those states may be reported as `ALIVE`.
"""


def main() -> None:
    entries = parse_entries()
    if not entries:
        raise SystemExit("SUMMARY.md produced no chapter entries")
    changed = 0
    for entry in entries:
        target = SRC / entry.path
        if not target.exists():
            raise SystemExit(f"missing SUMMARY target: {entry.path}")
        rendered = rewrite(entry)
        if target.read_text(encoding="utf-8") != rendered:
            target.write_text(rendered, encoding="utf-8")
            changed += 1
    print(f"rewrote {changed} of {len(entries)} linked chapters")


if __name__ == "__main__":
    main()
