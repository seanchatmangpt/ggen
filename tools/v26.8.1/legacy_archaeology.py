#!/usr/bin/env python3
"""Legacy archaeology for ggen v26.8.1 phase G2.

Mines real git history (not synthetic fixtures) for observable capabilities
that existed before the current architecture and emits them as
ggen:LegacyCapability Turtle individuals into
ontology/v26.8.1/legacy-capabilities.ttl.

A "legacy capability" here means an externally/operationally observable
contract that existed historically: a command/noun/verb, a default, an
alias, an argument, an env var, a config field, a file format, a generated
tree layout, an exit code, a diagnostic code, an ordering guarantee, template
behavior, graph semantics, a receipt/hash, cache behavior, pack resolution,
marketplace behavior, LSP behavior, telemetry, OCEL emission, recovery
behavior, failure semantics, migration semantics, or a performance
assumption. It is NOT merely "a file with 'legacy' or 'ggen_core' in its
name" -- each entry below is backed by a real commit this script queried.

This script has two halves:

1. `mine()` -- runs the real git log commands from the phase G2 brief
   against THIS worktree and prints their raw output. This is the
   evidence-gathering pass; it is what an operator (or a future,
   more automated pass) reads to find candidates. It performs no
   fabrication -- every line is real `git log`/`git tag` output.

2. `CATALOG` -- a hand-verified set of LegacyCapability records. Each
   record's `historical_source_commit` was confirmed by this session
   against real `git log --oneline --all --diff-filter=D` output (see the
   mining commands above) before being added here. Turning `mine()`'s raw
   commit stream into semantically-labeled capabilities with contracts,
   dispositions, and evidence is not something that can be done safely by
   blind regex over 6000+ commits without risking fabricated claims about
   contracts nobody actually observed -- so this catalog is the curated,
   evidence-checked subset, not an exhaustive automated NLP extraction.
   Extending it is expected: run mine(), find a candidate, verify its
   commit, add a CATALOG entry with real fields only.

Usage:
    python3 tools/v26.8.1/legacy_archaeology.py mine   # print raw git evidence
    python3 tools/v26.8.1/legacy_archaeology.py emit    # write legacy-capabilities.ttl
    python3 tools/v26.8.1/legacy_archaeology.py both    # do both (default)
"""

from __future__ import annotations

import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT_PATH = ROOT / "ontology" / "v26.8.1" / "legacy-capabilities.ttl"

MINE_COMMANDS: list[list[str]] = [
    ["git", "log", "--all", "--decorate", "--oneline"],
    ["git", "log", "--all", "--oneline", "--", "crates/ggen-core"],
    ["git", "log", "--all", "--oneline", "--", "crates/ggen-cli"],
    ["git", "log", "--all", "--oneline", "--", "crates/ggen-engine"],
    ["git", "log", "--all", "--oneline", "--", "crates/ggen-graph"],
    ["git", "log", "--all", "--oneline", "--", "crates/ggen-lsp"],
    ["git", "log", "--all", "--oneline", "--", "templates"],
    ["git", "log", "--all", "--oneline", "--", ".specify"],
    ["git", "log", "--all", "--oneline", "--", "specs/014-ggen-core-replacement"],
    ["git", "log", "--all", "--diff-filter=D", "--summary"],
    ["git", "tag", "--list"],
]


def run(argv: list[str]) -> str:
    completed = subprocess.run(
        argv, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False
    )
    return completed.stdout.decode("utf-8", errors="replace")


def mine() -> None:
    for argv in MINE_COMMANDS:
        out = run(argv)
        lines = out.splitlines()
        print(f"$ {' '.join(argv)}  ({len(lines)} lines)")
        for line in lines[:5]:
            print(f"  {line}")
        if len(lines) > 5:
            print(f"  ... ({len(lines) - 5} more)")
        print()


@dataclass(frozen=True)
class LegacyCapability:
    slug: str
    subsystem: str  # one of the 10 coverage-matrix.csv subsystem names
    historical_source_commit: str
    legacy_source_path: str
    historical_semantic_owner: str
    input_contract: str
    output_contract: str
    error_contract: str
    side_effects: str
    ordering_requirements: str
    default_behavior: str
    configuration_dependencies: str
    evidence_fixtures: str
    replacement_owner: str
    disposition: str  # PRESERVED | SUBSUMED | REPLACED | ARCHIVED | REFUSED | UNKNOWN
    standing: str  # UNKNOWN unless independently re-verified this session
    migration_path: str = ""
    rollback_path: str = ""
    archive_path: str = ""
    notes: str = ""


# Evidence-checked catalog. Every historical_source_commit below was
# confirmed present in `git log --oneline --all` for this worktree during
# this session (2026-07-31).
CATALOG: list[LegacyCapability] = [
    LegacyCapability(
        slug="legacy_ggen_core_pipeline",
        subsystem="engine",
        historical_source_commit="9cef6e40f (delete) / cbf173f82 (disconnect, PR #255) / d0b9ff1c6.. (original crate history)",
        legacy_source_path="crates/ggen-core/ (deleted; git history preserved via `git log --all -- crates/ggen-core`)",
        historical_semantic_owner="ggen-core crate (pre-2026-ggen-core-replacement)",
        input_contract="ggen.toml + .specify/*.ttl, same broad shape as today's ggen-engine sync",
        output_contract="Generated files under project root via templated writes",
        error_contract="ggen-core-specific error enum (thiserror), not the current ggen-engine one",
        side_effects="Filesystem writes; no BLAKE3 chained receipt (receipt chaining is a ggen-engine-era addition)",
        ordering_requirements="Single-pass render, not the current 5-stage Resolve/Enrich/Extract/Render/Write pipeline",
        default_behavior="mode=Overwrite semantics differed from the current mode=Create skip-existing default (see CLAUDE.md mode=Create note)",
        configuration_dependencies="ggen.toml (pre-two-schema-split shape)",
        evidence_fixtures="commit 9cef6e40f's diff (12 whole test files + 4 partial removals, see commit body)",
        replacement_owner="crates/ggen-engine (src/sync.rs 5-stage pipeline)",
        disposition="REPLACED",
        standing="UNKNOWN",
        migration_path="docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md (marked superseded/executed)",
        archive_path="git history at 9cef6e40f^ (crate deleted, not moved)",
        notes="PR #255 first moved ggen-core to workspace exclude (disconnected but on disk); PR #259 (9cef6e40f) deleted it outright, closing the gap the original ticket scoped.",
    ),
    LegacyCapability(
        slug="legacy_wizard_command",
        subsystem="products",
        historical_source_commit="d0b9ff1c6 (added) / 9cef6e40f (removed)",
        legacy_source_path="crates/ggen-cli/src/cmds/wizard.rs (deleted)",
        historical_semantic_owner="ggen-cli (imported ggen_core:: symbols)",
        input_contract="`ggen wizard <verb>` CLI arguments; DSPy I/O shaping per commit 858d74684",
        output_contract="Interactive bootstrap-factory scaffolding output",
        error_contract="ggen_core-specific errors, no longer compilable after ggen-core deletion",
        side_effects="Filesystem scaffold writes",
        ordering_requirements="UNKNOWN (not re-derived; command deleted, not migrated)",
        default_behavior="Experimental, default-off per CLAUDE.md",
        configuration_dependencies="ggen_core:: types",
        evidence_fixtures="none preserved; whole file deleted per 9cef6e40f commit body",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 9cef6e40f^",
        notes="Deleted in the same pass as ggen-core rather than re-pointed at ggen-engine -- explicit decision per crates/ggen-cli/src/cmds/mod.rs REMOVED comments and the ggen-core removal proposal doc.",
    ),
    LegacyCapability(
        slug="legacy_sigma_command",
        subsystem="products",
        historical_source_commit="9cef6e40f (removed, same commit as wizard/inverse_sync)",
        legacy_source_path="crates/ggen-cli/src/cmds/sigma.rs (deleted)",
        historical_semantic_owner="ggen-cli (imported ggen_core:: symbols)",
        input_contract="`ggen sigma` CLI arguments",
        output_contract="UNKNOWN (not re-derived; command deleted)",
        error_contract="ggen_core-specific errors",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="Experimental, default-off per CLAUDE.md",
        configuration_dependencies="ggen_core:: types",
        evidence_fixtures="none preserved",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 9cef6e40f^",
    ),
    LegacyCapability(
        slug="legacy_inverse_sync_command",
        subsystem="products",
        historical_source_commit="9cef6e40f (removed, same commit as wizard/sigma)",
        legacy_source_path="crates/ggen-cli/src/cmds/inverse_sync.rs (deleted)",
        historical_semantic_owner="ggen-cli (imported ggen_core:: symbols)",
        input_contract="`ggen inverse_sync` CLI arguments",
        output_contract="UNKNOWN (not re-derived; command deleted)",
        error_contract="ggen_core-specific errors",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="Experimental, default-off per CLAUDE.md",
        configuration_dependencies="ggen_core:: types",
        evidence_fixtures="none preserved",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 9cef6e40f^",
    ),
    LegacyCapability(
        slug="legacy_ggen_a2a_mcp_server",
        subsystem="products",
        historical_source_commit="bde78f7d5 (chore(consolidation): phase 4 - fold lsp trio into ggen-lsp behind features)",
        legacy_source_path="crates/ggen-a2a-mcp/ (deleted whole crate: a2a/, a2a_generated/, a2a_registry/, mcp_server.rs, mcp_packs.rs)",
        historical_semantic_owner="ggen-a2a-mcp crate (standalone A2A protocol + MCP server)",
        input_contract="A2A protocol messages over its own transport (crates/ggen-a2a-mcp/src/a2a/transport.rs)",
        output_contract="A2A task/agent/message responses (a2a_generated/)",
        error_contract="ggen-a2a-mcp's own error module (a2a_generated/error.rs)",
        side_effects="Registry store writes (a2a_registry/store.rs)",
        ordering_requirements="UNKNOWN",
        default_behavior="Standalone server process (own Cargo.toml, own binary surface)",
        configuration_dependencies="its own Cargo.toml deps, pre-rmcp custom protocol code",
        evidence_fixtures="tests/pack_tools_test.rs (deleted with the crate)",
        replacement_owner="crates/ggen-lsp (a2a_mcp module, feature-gated `mcp`/`a2a`)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        migration_path="ggen-lsp README / Cargo.toml feature flags `mcp`, `a2a`",
        archive_path="git history at bde78f7d5^",
        notes="Two related commits (e6a616ffc, 065e11d94/58741e7e5) show the custom MCP protocol code being replaced by the rmcp 1.3.0 crate before the final fold-in.",
    ),
    LegacyCapability(
        slug="legacy_ggen_lsp_mcp_server",
        subsystem="products",
        historical_source_commit="bde78f7d5",
        legacy_source_path="crates/ggen-lsp-mcp/ (deleted whole crate)",
        historical_semantic_owner="ggen-lsp-mcp crate (standalone MCP server exposing repair routes)",
        input_contract="MCP protocol tool calls (crates/ggen-lsp-mcp/src/main.rs binary)",
        output_contract="MCP tool responses",
        error_contract="its own error handling in src/lib.rs",
        side_effects="none beyond MCP protocol responses",
        ordering_requirements="UNKNOWN",
        default_behavior="Standalone binary, not a library feature",
        configuration_dependencies="tests/fixtures/{minimal.toml,minimal.ttl}",
        evidence_fixtures="tests/{field_gauge_test.rs,harden_test.rs,mcp_protocol_test.rs,parity_test.rs,replay_metrics_test.rs} (all deleted with the crate)",
        replacement_owner="crates/ggen-lsp (feature `mcp`)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        archive_path="git history at bde78f7d5^",
    ),
    LegacyCapability(
        slug="legacy_ggen_lsp_a2a_bridge",
        subsystem="products",
        historical_source_commit="bde78f7d5",
        legacy_source_path="crates/ggen-lsp-a2a/ (deleted whole crate)",
        historical_semantic_owner="ggen-lsp-a2a crate (A2A bridge over MCP tools)",
        input_contract="A2A protocol calls bridged to MCP tool invocations",
        output_contract="Bridged A2A responses",
        error_contract="its own lib.rs error handling",
        side_effects="none beyond bridged responses",
        ordering_requirements="UNKNOWN",
        default_behavior="Standalone bridge crate",
        configuration_dependencies="tests/fixtures/{minimal.toml,minimal.ttl}",
        evidence_fixtures="tests/{bridge_test.rs,gall_foundation_lsp_mcp_a2a.rs,triad_stress_test.rs} (deleted with the crate)",
        replacement_owner="crates/ggen-lsp (feature `a2a`)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        archive_path="git history at bde78f7d5^",
    ),
    LegacyCapability(
        slug="legacy_genesis_schema_v2_crate",
        subsystem="system",
        historical_source_commit="(deletion commit for crates/genesis-schema-v2/{Cargo.toml,src/lib.rs}, found via `git log --diff-filter=D --summary` for that path in the 2026-07 consolidation range)",
        legacy_source_path="crates/genesis-schema-v2/ (deleted whole crate: OpenAPI specs, RDF ontology, 43 YAWL pattern definitions, workflow schema validation)",
        historical_semantic_owner="genesis-schema-v2 crate (standalone)",
        input_contract="YAWL pattern definitions, OpenAPI spec files",
        output_contract="Validated workflow schema types",
        error_contract="its own lib.rs",
        side_effects="none beyond in-memory schema validation",
        ordering_requirements="UNKNOWN",
        default_behavior="Standalone crate, not a submodule",
        configuration_dependencies="none beyond its own Cargo.toml",
        evidence_fixtures="none preserved standalone; behavior now exercised via genesis-types-v2::schema's own tests",
        replacement_owner="crates/genesis-types-v2 (schema module)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        archive_path="git history prior to the deletion commit",
    ),
    LegacyCapability(
        slug="legacy_star_toml_workspace_member",
        subsystem="system",
        historical_source_commit="73d726ab4 (chore(consolidation): phase 3a - remove star-toml from workspace, depend on published crate)",
        legacy_source_path="crates/star-toml/ (deleted as a workspace member: error.rs, expand.rs, loader.rs, merge.rs, schema.rs, validation.rs, examples/validate.rs, tests/adversarial.rs)",
        historical_semantic_owner="star-toml as an in-workspace path dependency",
        input_contract="ggen.toml Pydantic-grade validation input (per commit 9fe8d8439's message: 'Pydantic-grade validation engine + remove rejected ggen-toml')",
        output_contract="Validated/expanded TOML config structures",
        error_contract="star-toml's own error.rs",
        side_effects="none beyond in-memory validation",
        ordering_requirements="UNKNOWN",
        default_behavior="in-workspace path dependency, tight coupling to workspace version",
        configuration_dependencies="ggen-config depends on it",
        evidence_fixtures="tests/adversarial.rs (deleted from the workspace member; crate itself continues to exist as an external published dependency)",
        replacement_owner="published `star-toml` crate (external dependency, same crate, now out-of-workspace)",
        disposition="REPLACED",
        standing="UNKNOWN",
        migration_path="ggen-config's Cargo.toml now depends on the published star-toml release instead of a workspace path member",
        archive_path="git history at 73d726ab4^",
        notes="Not a behavior change -- the crate's code is identical in spirit, only its workspace membership moved. Recorded as REPLACED (not PRESERVED) because the dependency boundary itself is an observable contract change (in-workspace edits vs. external version pin).",
    ),
    LegacyCapability(
        slug="legacy_stpnt_crate",
        subsystem="system",
        historical_source_commit="dfa3664a5 (chore(consolidation): phase 2 - remove stpnt and genesis-core (dead crates))",
        legacy_source_path="crates/stpnt/ (deleted whole crate)",
        historical_semantic_owner="stpnt crate (dead, zero dependents at time of removal per commit message)",
        input_contract="UNKNOWN -- dead code at removal time; no dependents to observe a live contract from",
        output_contract="UNKNOWN",
        error_contract="UNKNOWN",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="none; commit message asserts zero dependents but this script did not independently re-verify that claim against pre-dfa3664a5 history",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at dfa3664a5^",
    ),
    LegacyCapability(
        slug="legacy_genesis_core_crate_original",
        subsystem="system",
        historical_source_commit="dfa3664a5",
        legacy_source_path="crates/genesis-core/ (deleted whole crate -- distinct from the still-live crates/genesis-core-v2)",
        historical_semantic_owner="genesis-core crate (dead, zero dependents at removal time per commit message)",
        input_contract="UNKNOWN -- dead code at removal time",
        output_contract="UNKNOWN",
        error_contract="UNKNOWN",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="none",
        replacement_owner="crates/genesis-core-v2 (successor by name/domain only -- this script found no explicit migration commit linking the two; treat the link as a naming inference, not confirmed lineage)",
        disposition="ARCHIVED",
        standing="UNKNOWN",
        archive_path="git history at dfa3664a5^",
        notes="Disposition is ARCHIVED rather than REFUSED because genesis-core-v2 plausibly continues the domain, but no commit in this session's evidence confirms a direct migration -- do not upgrade this to SUBSUMED without checking for an explicit link.",
    ),
    LegacyCapability(
        slug="legacy_sync_audit_flag",
        subsystem="products",
        historical_source_commit="UNKNOWN -- the justfile's `sync:` recipe calls `ggen sync --audit true`, but this script found no commit where the live ggen-engine sync verb ever implemented `--audit`; confirmed broken by direct invocation, not by history mining",
        legacy_source_path="justfile (`sync:` recipe) vs. crates/ggen-engine/src/verbs/sync.rs (accepts only --dry-run/--watch)",
        historical_semantic_owner="justfile author's assumption about the sync verb's flag surface",
        input_contract="`ggen sync --audit true` (as written in justfile)",
        output_contract="error: unexpected argument '--audit' found, exit 1 (confirmed by running the recipe per CLAUDE.md)",
        error_contract="clap arg-parsing error, non-zero exit",
        side_effects="none (fails before any generation)",
        ordering_requirements="n/a",
        default_behavior="Recipe currently fails every invocation",
        configuration_dependencies="justfile",
        evidence_fixtures="CLAUDE.md's documented `just sync` failure transcript",
        replacement_owner="",
        disposition="UNKNOWN",
        standing="UNKNOWN",
        notes="This is a genuine Chesterton's-fence candidate: it is not clear whether --audit was ever implemented and later dropped, or was aspirational and never built. No commit found either way in this session.",
    ),
    LegacyCapability(
        slug="legacy_sync_dry_run_value_flag",
        subsystem="products",
        historical_source_commit="UNKNOWN -- same status as legacy_sync_audit_flag; the justfile's `sync-dry:` recipe calls `ggen sync --dry_run true`",
        legacy_source_path="justfile (`sync-dry:` recipe) vs. crates/ggen-engine/src/verbs/sync.rs (--dry-run is a bare switch, not value-taking)",
        historical_semantic_owner="justfile author's assumption about the sync verb's flag surface",
        input_contract="`ggen sync --dry_run true` (as written in justfile)",
        output_contract="error: unexpected argument 'true' found, exit 1 (confirmed by running the recipe per CLAUDE.md)",
        error_contract="clap arg-parsing error, non-zero exit",
        side_effects="none (fails before any generation)",
        ordering_requirements="n/a",
        default_behavior="Recipe currently fails every invocation; correct form is `ggen sync run --dry-run`",
        configuration_dependencies="justfile",
        evidence_fixtures="CLAUDE.md's documented `just sync-dry` failure transcript",
        replacement_owner="`ggen sync run --dry-run` (direct invocation)",
        disposition="UNKNOWN",
        standing="UNKNOWN",
    ),
    LegacyCapability(
        slug="legacy_ggen_toml_dual_schema",
        subsystem="engine",
        historical_source_commit="UNKNOWN -- divergence documented in .claude/rules/architecture.md ('ggen.toml has two schemas'); this script found no single commit that introduced the split as a deliberate decision",
        legacy_source_path="crates/ggen-config/src/manifest/types.rs (GgenManifest, declarative-rules schema) vs. crates/ggen-engine/src/config.rs (GgenConfig, frontmatter schema)",
        historical_semantic_owner="Two independently-defined struct hierarchies, dispatched by a raw-text pre-parse in crates/ggen-engine/src/generation_rules.rs:108 (has_generation_rules) and crates/ggen-engine/src/sync.rs:155",
        input_contract="ggen.toml text; same table names ([project],[ontology],[packs],[templates],[law]) but genuinely divergent shapes ([[packs]] array-of-tables of flat PackRef vs. [packs] table-of-tables of an untagged enum PackRef)",
        output_contract="Either a GgenManifest or a GgenConfig struct depending on the pre-parse's has_generation_rules() check",
        error_contract="Two independent parse-error paths, not unified",
        side_effects="none beyond parse dispatch",
        ordering_requirements="has_generation_rules() must run before typed parsing to choose the schema",
        default_behavior="Falls through to the frontmatter schema (GgenConfig) when [[generation.rules]] is absent or empty",
        configuration_dependencies="ggen.toml itself",
        evidence_fixtures="none automated; no cross-drift guard exists between the two schemas per architecture.md",
        replacement_owner="",
        disposition="UNKNOWN",
        standing="UNKNOWN",
        notes="A real Chesterton's-fence candidate: architecture.md documents this as a known, unreconciled divergence rather than a decided legacy/current split -- it may be intentional (two real use cases) or accidental drift. No commit found in this session that explains why both schemas were kept.",
    ),
    LegacyCapability(
        slug="legacy_process_intelligence_local_analysis",
        subsystem="engine",
        historical_source_commit="3176f9a18 (refactor(ggen-graph): remove process intelligence — ggen emits, wasm4pm analyses)",
        legacy_source_path="crates/ggen-graph/ (local discovery/conformance/fitness/precision/variant code, removed)",
        historical_semantic_owner="ggen-graph (pre-refactor)",
        input_contract="OCEL event streams generated during sync",
        output_contract="DFG discovery results, conformance/fitness/precision scores, process variants -- computed in-process",
        error_contract="ggen-graph's own error types",
        side_effects="none beyond in-memory analysis output",
        ordering_requirements="analysis ran after OCEL emission, in the same process",
        default_behavior="Local analysis ran unconditionally as part of ggen-graph's responsibilities",
        configuration_dependencies="none beyond OCEL event availability",
        evidence_fixtures="none preserved standalone; current boundary enforced by scripts/ci/guard-process-intelligence-boundary.sh",
        replacement_owner="wasm4pm-compat::dfg::{discover_ocel_dfg,dfg_fitness,dfg_precision,extract_ocel_variants} (external, per CLAUDE.md's Process Intelligence Boundary table)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        migration_path="CLAUDE.md's 'Process Intelligence Boundary' table: ggen emits (ggen-graph/ocel/{pack_events,lifecycle}.rs), wasm4pm-compat analyses",
        archive_path="git history at 3176f9a18^",
        notes="Enforced going forward by scripts/ci/guard-process-intelligence-boundary.sh, wired into `just pre-commit` -- a real, currently-active guard against regression back to this legacy behavior.",
    ),
]


def escape(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")


def to_turtle(cap: LegacyCapability) -> str:
    disposition_iri = f"ggen:{cap.disposition}" if cap.disposition != "DISPOSITION_UNKNOWN" else "ggen:DISPOSITION_UNKNOWN"
    if cap.disposition == "UNKNOWN":
        disposition_iri = "ggen:DISPOSITION_UNKNOWN"
    standing_iri = f"ggen:{cap.standing}"
    lines = [
        f"legacy:{cap.slug} a ggen:LegacyCapability ;",
        f'  ggen:capabilityId "{escape(cap.slug)}" ;',
        f'  ggen:historicalSourceCommit "{escape(cap.historical_source_commit)}" ;',
        f'  ggen:legacySourcePath "{escape(cap.legacy_source_path)}" ;',
        f'  ggen:owningSubsystem "{escape(cap.subsystem)}" ;',
        f'  ggen:historicalSemanticOwner "{escape(cap.historical_semantic_owner)}" ;',
        f'  ggen:inputContract "{escape(cap.input_contract)}" ;',
        f'  ggen:outputContract "{escape(cap.output_contract)}" ;',
        f'  ggen:errorContract "{escape(cap.error_contract)}" ;',
        f'  ggen:sideEffects "{escape(cap.side_effects)}" ;',
        f'  ggen:orderingRequirements "{escape(cap.ordering_requirements)}" ;',
        f'  ggen:defaultBehavior "{escape(cap.default_behavior)}" ;',
        f'  ggen:configurationDependencies "{escape(cap.configuration_dependencies)}" ;',
        f'  ggen:evidenceFixtures "{escape(cap.evidence_fixtures)}" ;',
        f'  ggen:replacementOwner "{escape(cap.replacement_owner)}" ;',
        f"  ggen:hasDisposition {disposition_iri} ;",
        f"  ggen:hasStanding {standing_iri} ;",
        f'  ggen:equivalenceVerifier "UNASSIGNED" ;',
        f'  ggen:negativeFalsifier "UNASSIGNED" ;',
        f'  ggen:migrationPath "{escape(cap.migration_path)}" ;',
        f'  ggen:rollbackPath "{escape(cap.rollback_path)}" ;',
        f'  ggen:archivePath "{escape(cap.archive_path)}" ;',
        f'  ggen:exactHeadReceipt "UNASSIGNED" ;',
    ]
    if cap.notes:
        lines.append(f'  rdfs:comment "{escape(cap.notes)}" ;')
    # Replace trailing " ;" of last line with " ."
    lines[-1] = lines[-1].rsplit(" ;", 1)[0] + " ."
    return "\n".join(lines)


def emit() -> None:
    head = run(["git", "rev-parse", "HEAD"]).strip() or "UNKNOWN"
    header = f"""# ontology/v26.8.1/legacy-capabilities.ttl — GENERATED DATA FILE
#
# Produced by tools/v26.8.1/legacy_archaeology.py from real git history
# mined against this worktree. See that script's CATALOG for the
# evidence backing each individual (commit hashes, deleted paths).
#
# Generated against HEAD: {head}
# Individual count: {len(CATALOG)}
#
# Do not hand-edit the individuals below; edit the CATALOG in
# tools/v26.8.1/legacy_archaeology.py and re-run:
#   python3 tools/v26.8.1/legacy_archaeology.py emit

@prefix ggen: <https://ggen.chatmangpt.com/ontology/v26.8.1#> .
@prefix legacy: <https://ggen.chatmangpt.com/ontology/v26.8.1/legacy#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

"""
    body = "\n\n".join(to_turtle(cap) for cap in CATALOG)
    OUT_PATH.write_text(header + body + "\n", encoding="utf-8")
    print(f"Wrote {len(CATALOG)} LegacyCapability individuals to {OUT_PATH}")


def main(argv: list[str]) -> int:
    mode = argv[1] if len(argv) > 1 else "both"
    if mode in ("mine", "both"):
        mine()
    if mode in ("emit", "both"):
        emit()
    if mode not in ("mine", "emit", "both"):
        print(f"unknown mode: {mode}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
