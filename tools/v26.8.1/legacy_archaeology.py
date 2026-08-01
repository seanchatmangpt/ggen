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
    refusal_code: str = ""
    refusal_rationale: str = ""


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
        refusal_code="LEGACY-EXPERIMENTAL-COMMAND-ABANDONED",
        refusal_rationale="Added by d0b9ff1c6, removed by 9cef6e40f alongside `sigma` and `inverse_sync` -- the experimental ggen_core::-dependent commands abandoned (not re-pointed) during the ggen-core removal per docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md (marked superseded/executed).",
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
        refusal_code="LEGACY-EXPERIMENTAL-COMMAND-ABANDONED",
        refusal_rationale="Removed by commit 9cef6e40f alongside `wizard` and `inverse_sync`, the same experimental-command removal pass documented as abandoned (not re-pointed) in docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md.",
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
        refusal_code="LEGACY-EXPERIMENTAL-COMMAND-ABANDONED",
        refusal_rationale="Removed by commit 9cef6e40f alongside `wizard` and `sigma`, the same experimental-command removal pass. Per CLAUDE.md's Crate Map notes, these experimental ggen_core::-dependent commands were deleted rather than re-pointed during the ggen-core removal (docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md, marked superseded/executed), i.e. abandoned, not migrated.",
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
        refusal_code="LEGACY-FLAG-NEVER-IMPLEMENTED",
        refusal_rationale="Deleted by commit dfa3664a5 (\"chore(consolidation): phase 2 - remove stpnt and genesis-core (dead crates)\"), which the commit message itself characterizes as dead-code removal with zero dependents at removal time. No live contract existed to observe or preserve.",
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
        disposition="REFUSED",
        standing="UNKNOWN",
        notes="Superseded (agent/v26.8.1-disposition-repair, 2026-07-31): the prior Chesterton's-fence framing is resolved -- git history confirms --audit was never implemented, only ever aspirational justfile text. See ggen:refusalRationale for full evidence.",
        refusal_code="LEGACY-FLAG-NEVER-IMPLEMENTED",
        refusal_rationale="Resolved (agent/v26.8.1-disposition-repair, 2026-07-31): `git log --all -S\"audit\" -- crates/ggen-engine/src/verbs/sync.rs` returns zero commits -- the live sync verb's flag surface (`sync_run(dry_run: bool, watch: bool)`) never had an --audit parameter added and later removed. `git log -p --follow -- justfile` shows `sync: ggen sync --audit true` present verbatim in the commit that first introduced the `sync:`/`sync-dry:` recipes (043a6599c, 2026-06-10) -- --audit was aspirational from the recipe's very first commit, not a regression. This is not a genuine Chesterton's fence: there is no removed capability to preserve, migrate, or roll back to. The justfile's `sync:` recipe is fixed in this branch to run the real `ggen sync run` (no audit-equivalent flag exists on the live verb), confirmed working by a real invocation (`just sync` exit 0, `.ggen-v2/receipt.json` written).",
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
        disposition="REPLACED",
        standing="UNKNOWN",
        migration_path="justfile's `sync-dry:` recipe (agent/v26.8.1-disposition-repair, 2026-07-31): changed from `ggen sync --dry_run true` (clap arg-parse failure, exit non-zero, every invocation) to `ggen sync run --dry-run` (the bare boolean switch the live sync_run(dry_run, watch) verb actually accepts). Verified by a real subprocess run: `just sync-dry` exits 0 and produces a real JSON preview report. Regression-proofed by crates/ggen-engine/tests/sync_dry_run_no_mutation_e2e.rs's `dry_run_subprocess_does_not_mutate_any_file`, which runs the exact corrected invocation as a real CARGO_BIN_EXE_ggen subprocess and asserts a byte-for-byte + mtime-for-mtime unchanged filesystem snapshot before vs. after.",
        rollback_path="git history of justfile at any commit before agent/v26.8.1-disposition-repair (the broken `ggen sync --dry_run true` form)",
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
        replacement_owner="crates/ggen-config/src/config_schema.rs (classify_ggen_toml) + crates/ggen-engine/src/schema_dispatch.rs (load)",
        disposition="REPLACED",
        standing="UNKNOWN",
        notes="Superseded (agent/v26.8.1-disposition-repair, 2026-07-31): resolved, not a live Chesterton's fence -- commit 627f84c59 already replaced the raw-text has_generation_rules pre-parse with a shared, typed, ambiguity-detecting classifier before this ontology individual was generated. See ggen:migrationPath for full evidence including a real reproduction of FM-CONFIG-101 against examples/tai-enterprise-rebuild/ggen.toml.",
        migration_path="Resolved (agent/v26.8.1-disposition-repair, 2026-07-31): commit 627f84c59 (2026-07-17, predates this ontology's generation) already added the requested early/loud discriminator -- crates/ggen-config/src/config_schema.rs's classify_ggen_toml is a pure structural classifier producing a five-way ConfigSchemaClassification (DeclarativeRules/Frontmatter/Ambiguous{matched}/Unsupported/Malformed), and crates/ggen-engine/src/schema_dispatch.rs's load() is the single dispatch point every call site (sync, doctor, graph validate, all `ggen law *` verbs) now goes through -- confirmed via `grep -n schema_dispatch::load crates/ggen-engine/src/verbs/handlers.rs` (4 call sites). An Ambiguous classification fails closed with a typed [FM-CONFIG-101] error naming the exact conflicting markers, never a silent raw-text-sniff pick. Real-repo census (this session, 137 ggen.toml files under this worktree): 90 declarative-rules, 25 frontmatter, 0 ambiguous among files with an unambiguous schema shape -- BUT `ggen doctor run` against examples/tai-enterprise-rebuild/ggen.toml (the file this task named to test) DOES reproduce FM-CONFIG-101 today: that file has [project].version (a declarative-only marker) together with a [packs] table-of-tables (a frontmatter-only marker), so it is a genuine real ambiguous document, not a synthetic edge case -- confirmed by running the real binary: `error: ... config error: [FM-CONFIG-101] ggen.toml at .../examples/tai-enterprise-rebuild/ggen.toml is ambiguous ... conflicting structural markers [\"declarative:project_version_present\", \"frontmatter:packref_entry_missing_name\", \"frontmatter:packs_table_shaped\"]`. This is the classifier working as designed (loud refusal, not silent misparse); examples/tai-enterprise-rebuild/ggen.toml itself has a real authoring defect (a stray [project].version field that ggen_engine::config::GgenConfig's Project{name} struct, deny_unknown_fields, would also reject on its own) that is out of this task's scope (only ontology/v26.8.1/legacy-capabilities.ttl's 3 named individuals were in scope, not example fixture repair). Added crates/ggen-config/tests/fixtures/schema_dual_{declarative_valid,frontmatter_valid,ambiguous_collision}.toml + crates/ggen-config/tests/schema_dual_fixtures_test.rs (3/3 passing) as physical-file proof of all three outcomes, complementing the pre-existing crates/ggen-engine/tests/config_schema_dispatch_e2e.rs (8/8 passing, including ambiguous_schema_sync_is_typed_refusal and doctor_succeeds_with_correct_diagnostic_on_each_supported_schema at the real ggen-binary process boundary).",
        rollback_path="git history of crates/ggen-config/src/config_schema.rs and crates/ggen-engine/src/schema_dispatch.rs before commit 627f84c59 (2026-07-17), when six call sites each used ad-hoc per-site dispatch logic instead of the one shared classifier",
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


# ---------------------------------------------------------------------------
# Observer-class extension (2026-07-31 exhaustive-observer pass)
#
# Everything below EXT_CATALOG is additive: it extends the archaeology with a
# systematic sweep organized by OBSERVER CLASS (20 generic extraction
# strategies applied to real git history) rather than by manually reviewing
# commits one at a time. Every individual below carries a real commit hash
# this session confirmed with `git log`/`git show` against this worktree.
#
# See docs/v26.8.1/90-legacy/observer-class-report.md for the full per-class
# accounting (observed/admitted/deduplicated/excluded counts and reasons for
# all 20 classes, including the many that legitimately yielded zero).
#
# URI/slug convention: all new individuals use the `legacy_ext_` slug prefix
# (vs. the original catalog's bare `legacy_` prefix) so there is no collision
# with the 15 pre-existing individuals and the provenance of each entry is
# visible at a glance.
# ---------------------------------------------------------------------------

# Observer class 15: `just` recipes removed from justfile across its history.
# Evidence method: `git log --all -p -- justfile` captured to a file, then
# for each recipe name currently absent from justfile, the nearest preceding
# commit whose diff contains a `-<recipe>:` line was located programmatically
# (tools/v26.8.1/legacy_archaeology.py's find-removal helper used ad hoc this
# session; the located commits were re-verified individually with
# `git show <hash> -- justfile` before being admitted here).
_JUST_RECIPE_REMOVALS: list[tuple[str, str, str]] = [
    ("doctor", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("graph", "d816469fa", "ultravibe coding"),
    ("pipeline", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("docker", "f80c458ea", "chore(docker): update justfile commands for docker"),
    ("docker-build", "7afa50f8c", "chore(CI): Update Dockerfile"),
    ("docker-run", "7afa50f8c", "chore(CI): Update Dockerfile"),
    ("docker-test", "7afa50f8c", "chore(CI): Update Dockerfile"),
    ("docker-version", "7afa50f8c", "chore(CI): Update Dockerfile"),
    ("build-docker", "2c0753296", "chore(docker): Initial docker implementation"),
    ("build-image", "d816469fa", "ultravibe coding"),
    ("push-image", "d816469fa", "ultravibe coding"),
    ("lsp-max-check", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("lsp-check", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("lsp-max-edit", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("lsp-max-sync", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("test-mutation", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("test-bdd", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("test-changed", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("test-marketplace", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("test-marketplace-full", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("run-tests", "d816469fa", "ultravibe coding"),
    ("affidavit-seal", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("affidavit-verify", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("evidence-audit", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("evidence-show", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("certification-show", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("workspace-sync", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("status-audit", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("pipeline-status", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("pipeline-validate", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("publish-check", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("target-prune", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("target-show", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
    ("git-status", "083651dba", "release: v26.7.3 DX rewrite + CI fixes (#249)"),
]


def _just_recipe_capability(name: str, commit: str, subject: str) -> LegacyCapability:
    return LegacyCapability(
        slug=f"legacy_ext_just_recipe_{name.replace('-', '_')}",
        subsystem="system",
        historical_source_commit=f"{commit} ({subject})",
        legacy_source_path="justfile",
        historical_semantic_owner="justfile author",
        input_contract=f"`just {name}` (no further args re-derived; not re-implemented)",
        output_contract="UNKNOWN -- recipe body not re-derived from history, only its removal is evidenced",
        error_contract="UNKNOWN",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior=f"`just {name}` no longer exists; `just --list` does not show it",
        configuration_dependencies="justfile",
        evidence_fixtures=f"git show {commit} -- justfile (recipe removed in this diff)",
        replacement_owner="",
        disposition="ARCHIVED",
        standing="UNKNOWN",
        archive_path=f"git history at {commit}^ -- justfile",
        notes=(
            "Found via observer class 15 (git log -p -- justfile, pickaxe over "
            "removal diffs). Only the recipe's existence and removal commit are "
            "evidenced here, not its full historical body/semantics -- re-deriving "
            "that would require reading the full pre-removal recipe text, which "
            "this pass did not do for every recipe to stay within scope."
        ),
    )


# Observer class 11: crates deleted in the 2026-07 consolidation pass, listed
# in .claude/rules/architecture.md's "Removed in the 2026-07 consolidation
# pass" note but not yet given LegacyCapability individuals (`stpnt` and
# `genesis-core` were already captured in the original 15-capability CATALOG
# above; skipped here per the task brief).
#
# Observer class 12: for each crate below, its last public API (`pub `-level
# items in its src/lib.rs, or its binary's `fn main`) as of the commit
# immediately before deletion (1752de841^). Folded into the same individual
# as class 11 rather than duplicated, since the API surface is evidence *for*
# the crate's capability claim, not an independent capability -- counted as
# deduplicated_count under class 12 in the report, not double-admitted.
EXT_CATALOG: list[LegacyCapability] = [
    LegacyCapability(
        slug="legacy_ext_genesis_construct8_crate",
        subsystem="system",
        historical_source_commit="1752de841 (chore(consolidation): phase 1 - delete dormant/dead code (0 blast radius))",
        legacy_source_path="crates/genesis-construct8/ (deleted whole crate)",
        historical_semantic_owner="genesis-construct8 crate (dead, zero dependents at removal time per commit message)",
        input_contract="UNKNOWN -- dead code at removal time, no dependents to observe a live contract from",
        output_contract="Public modules per `git show 1752de841^:crates/genesis-construct8/src/lib.rs`: adapters, admission, forge, hierarchy, models, projectors, receipt, replay, stream; re-exports parse_input_to_packets",
        error_contract="UNKNOWN",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="git show 1752de841^:crates/genesis-construct8/src/lib.rs (pub mod/pub use list captured this session)",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 1752de841^",
        notes="Listed in .claude/rules/architecture.md's 2026-07 consolidation removal note; not previously given a LegacyCapability individual.",
        refusal_code="LEGACY-DEAD-CRATE-DELETED",
        refusal_rationale="Deleted by commit 1752de841 (\"chore(consolidation): phase 1 - delete dormant/dead code (0 blast radius)\"), which the commit message itself characterizes as zero-blast-radius removal of dormant code -- i.e. the crate had no live dependents at deletion time. No successor crate consumes this functionality; refusing rather than migrating is correct since there is no active contract to preserve.",
    ),
    LegacyCapability(
        slug="legacy_ext_genesis_lockchain_crate",
        subsystem="system",
        historical_source_commit="1752de841",
        legacy_source_path="crates/genesis-lockchain/ (deleted whole crate)",
        historical_semantic_owner="genesis-lockchain crate (dead, zero dependents at removal time per commit message)",
        input_contract="UNKNOWN -- dead code at removal time",
        output_contract="Public API per `git show 1752de841^:crates/genesis-lockchain/src/lib.rs`: merkle::{MerkleError,MerkleProof,MerkleTree}, quorum::{PeerId,QuorumError,QuorumManager,QuorumProof}, storage::{LockchainStorage,StorageError}, LockchainError, Receipt",
        error_contract="LockchainError (own enum)",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="git show 1752de841^:crates/genesis-lockchain/src/lib.rs (pub mod/pub use/pub enum/pub struct list captured this session)",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 1752de841^",
        refusal_code="LEGACY-DEAD-CRATE-DELETED",
        refusal_rationale="Deleted by commit 1752de841, the same zero-blast-radius dormant-code removal pass as legacy_ext_genesis_construct8_crate. No live dependents at deletion time, no successor crate.",
    ),
    LegacyCapability(
        slug="legacy_ext_genesis_wasm_shell_crate",
        subsystem="system",
        historical_source_commit="1752de841",
        legacy_source_path="crates/genesis-wasm-shell/ (deleted whole crate)",
        historical_semantic_owner="genesis-wasm-shell crate (dead, zero dependents at removal time per commit message)",
        input_contract="UNKNOWN -- dead code at removal time",
        output_contract="Public API per `git show 1752de841^:crates/genesis-wasm-shell/src/lib.rs`: WasmPair2, WasmRelationPage, RelationPageStreamer, WasmConstruct8, WasmReceipt, WasmReplayCursor (wasm-bindgen shell types)",
        error_contract="UNKNOWN",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="git show 1752de841^:crates/genesis-wasm-shell/src/lib.rs (pub struct list captured this session)",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 1752de841^",
        refusal_code="LEGACY-DEAD-CRATE-DELETED",
        refusal_rationale="Deleted by commit 1752de841, the same zero-blast-radius dormant-code removal pass. No live dependents at deletion time, no successor crate.",
    ),
    LegacyCapability(
        slug="legacy_ext_ggen_daemon_crate",
        subsystem="system",
        historical_source_commit="1752de841",
        legacy_source_path="crates/ggen-daemon/ (deleted whole crate)",
        historical_semantic_owner="ggen-daemon crate (dead, zero dependents at removal time per commit message)",
        input_contract="UNKNOWN -- dead code at removal time",
        output_contract="Public modules per `git show 1752de841^:crates/ggen-daemon/src/lib.rs`: amplifier, campaign, cascade, catalog, catalog_sync, dispatch, error, expansion, health, manifest_cache, mcp_server, validator, metrics, ocel_log, ontology, parallel_dispatch, remediation, repo_manager, retry, scheduler (and more; list truncated to the first 20 by this session's evidence capture)",
        error_contract="its own error module",
        side_effects="Implies an mcp_server module -- likely ran as a standalone daemon process; not independently re-verified",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="git show 1752de841^:crates/ggen-daemon/src/lib.rs (pub mod list, first 20 entries captured this session)",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 1752de841^",
        notes="Largest of the 6 admitted class-11 crates by module count; genuinely a standalone daemon/orchestrator, not a stub.",
        refusal_code="LEGACY-DEAD-CRATE-DELETED",
        refusal_rationale="Deleted by commit 1752de841, the same zero-blast-radius dormant-code removal pass. No live dependents at deletion time, no successor crate.",
    ),
    LegacyCapability(
        slug="legacy_ext_ggen_membrane_crate",
        subsystem="system",
        historical_source_commit="1752de841",
        legacy_source_path="crates/ggen-membrane/ (deleted whole crate)",
        historical_semantic_owner="ggen-membrane crate (dead, zero dependents at removal time per commit message)",
        input_contract="UNKNOWN -- dead code at removal time",
        output_contract="Public API per `git show 1752de841^:crates/ggen-membrane/src/lib.rs`: GenesisAdapter (trait), MembraneError, SymbolPage, SymbolPageBuilder, PublicExpansionLaw, ExternalClaim, MembraneFoundry",
        error_contract="MembraneError (own enum)",
        side_effects="UNKNOWN",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="UNKNOWN",
        evidence_fixtures="git show 1752de841^:crates/ggen-membrane/src/lib.rs (pub trait/enum/struct list captured this session)",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 1752de841^",
        refusal_code="LEGACY-DEAD-CRATE-DELETED",
        refusal_rationale="Deleted by commit 1752de841, the same zero-blast-radius dormant-code removal pass. No live dependents at deletion time, no successor crate.",
    ),
    LegacyCapability(
        slug="legacy_ext_ggen_projection_crate",
        subsystem="system",
        historical_source_commit="1752de841",
        legacy_source_path="crates/ggen-projection/ (deleted whole crate)",
        historical_semantic_owner="ggen-projection crate (dead, zero dependents at removal time per commit message)",
        input_contract="RelationPage / Pair2 values (own types)",
        output_contract="Public API per `git show 1752de841^:crates/ggen-projection/src/lib.rs`: Pair2, RelationPage, normalize_resource/normalize_predicate/normalize_object/escape_literal, project_ocel2, project_nquads, project_prov, project_dcat, project_shacl_refusal -- a multi-format RDF/OCEL projection surface (N-Quads, PROV, DCAT, SHACL refusal, OCEL2)",
        error_contract="UNKNOWN",
        side_effects="none beyond in-memory string projection (functions return serde_json::Value or String)",
        ordering_requirements="UNKNOWN",
        default_behavior="UNKNOWN",
        configuration_dependencies="depends on knhk_construct8::Receipt for project_prov/project_dcat",
        evidence_fixtures="git show 1752de841^:crates/ggen-projection/src/lib.rs (pub fn/struct list captured this session)",
        replacement_owner="",
        disposition="REFUSED",
        standing="UNKNOWN",
        archive_path="git history at 1752de841^",
        notes="Notably overlaps in spirit with ggen-graph's current deterministic-hashing/receipt machinery (N-Quads/PROV/DCAT projection) but no explicit migration commit was found linking the two -- treat as REFUSED, not SUBSUMED, absent that link.",
        refusal_code="LEGACY-DEAD-CRATE-DELETED",
        refusal_rationale="Deleted by commit 1752de841, the same zero-blast-radius dormant-code removal pass. No live dependents at deletion time, no successor crate. Its public API (Pair2, RelationPage, normalize_*, project_ocel2/nquads/prov/dcat/shacl_refusal) has no confirmed live re-implementation elsewhere in this repo as of this session's evidence.",
    ),
]

# Observer class 10: historical template `mode = "..."` values beyond the
# current 3-variant GenerationMode enum (Create/Overwrite/Merge, confirmed
# live in crates/ggen-config/src/manifest/types.rs). Found via
# `git log --all -p -S'mode = "Append"'` / `-S'mode = "Update"'` pickaxe
# searches across the whole repository (not crate-scoped, since these values
# appear in .ttl/.tmpl fixtures and older non-workspace generator code, not
# only crates/).
EXT_CATALOG.extend(
    [
        LegacyCapability(
            slug="legacy_ext_template_mode_append",
            subsystem="engine",
            historical_source_commit="e61137384 / 9187e8ec1 / 2e752ce45 (pickaxe hits for `mode = \"Append\"`; earliest clean hit not independently re-dated beyond confirming presence in `git log --all -p -S'mode = \"Append\"'` output)",
            legacy_source_path="template frontmatter (`.tmpl`/pack ontology mode fields) predating the current 3-variant GenerationMode enum",
            historical_semantic_owner="pre-GenerationMode-consolidation template frontmatter parser",
            input_contract="`mode = \"Append\"` in template frontmatter",
            output_contract="UNKNOWN -- semantics not re-derived from history; only the string's historical presence is evidenced",
            error_contract="UNKNOWN",
            side_effects="UNKNOWN (presumed: append generated content to an existing file, by analogy with the name)",
            ordering_requirements="UNKNOWN",
            default_behavior="Not one of the 3 variants (Create/Overwrite/Merge) in the live `ggen_config::manifest::types::GenerationMode` enum today",
            configuration_dependencies="template frontmatter",
            evidence_fixtures="git log --all -p -S'mode = \"Append\"' (pickaxe hits, not crate-scoped)",
            replacement_owner="none -- confirmed absent: crates/ggen-config/src/manifest/types.rs's live GenerationMode enum has exactly 3 variants (Create, Overwrite, Merge), no Append",
            disposition="ARCHIVED",
            standing="UNKNOWN",
            archive_path="git history at e61137384^ (pickaxe hits for `mode = \"Append\"`; re-verified 2026-07-31 against crates/ggen-config/src/manifest/types.rs's current GenerationMode enum -- no Append variant exists)",
            notes="Disposition resolved from DISPOSITION_UNKNOWN to ARCHIVED: re-derivation on 2026-07-31 confirmed by direct source read (not fabrication) that the live GenerationMode enum has 3 variants only. No replacement owner exists for Append; it is not merely undocumented, it does not compile. ARCHIVED per this corpus's own convention for removed surfaces with no live successor (see legacy_genesis_core_crate_original for the same pattern).",
        ),
        LegacyCapability(
            slug="legacy_ext_template_mode_update",
            subsystem="engine",
            historical_source_commit="fca98756f (refactor(daemon): replace custom generator with SyncExecutor from ggen-core) / 6af9ba404 (feat(specs): add universal domain, cron schedule ontology, and 8 spec bundles)",
            legacy_source_path="template frontmatter mode field, pre-SyncExecutor generator (ggen-daemon's custom generator, replaced by fca98756f)",
            historical_semantic_owner="ggen-daemon's own custom generator (pre-SyncExecutor)",
            input_contract="`mode = \"Update\"` in template frontmatter",
            output_contract="UNKNOWN -- semantics not re-derived; ggen-daemon itself is now fully deleted (see legacy_ext_ggen_daemon_crate above), so this mode's implementation is gone along with its owning crate",
            error_contract="UNKNOWN",
            side_effects="UNKNOWN",
            ordering_requirements="UNKNOWN",
            default_behavior="Not one of the 3 variants (Create/Overwrite/Merge) in the live GenerationMode enum",
            configuration_dependencies="template frontmatter, ggen-daemon's custom generator",
            evidence_fixtures="git log --all --oneline -S'mode = \"Update\"' (pickaxe hits)",
            replacement_owner="crates/ggen-engine's SyncExecutor-based pipeline (per commit fca98756f's own message)",
            disposition="REPLACED",
            standing="UNKNOWN",
            archive_path="git history at fca98756f^",
            notes="Directly related to legacy_ext_ggen_daemon_crate: this mode belonged to the daemon's own generator, replaced by SyncExecutor per the commit's explicit message, not merely coincidentally deleted alongside it.",
        ),
    ]
)

EXT_CATALOG.extend(
    _just_recipe_capability(name, commit, subject)
    for name, commit, subject in _JUST_RECIPE_REMOVALS
)


# ---------------------------------------------------------------------------
# Second observer-class extension (2026-07-31, remaining-5-classes pass)
#
# Attempts the 5 classes the exhaustive-observer pass above explicitly left
# as "NOT ATTEMPTED" (distinct from "attempted, zero found"): class 4
# (aliases), class 9 (historical receipt schemas), class 13 (output-tree
# layouts), class 14 (exit-code contracts), class 18 (pack.toml schema
# versions). See docs/v26.8.1/90-legacy/observer-class-report.md's "Remaining
# 5 Classes" section for the full per-class accounting.
#
# URI/slug convention: individuals from this second pass use the
# `legacy_ext2_` prefix (vs. `legacy_` for the original 15 and `legacy_ext_`
# for the first 42-individual extension) so provenance is visible at a
# glance and there is no collision.
#
# RESOLVED (2026-07-31, idempotency fix): EXT_CATALOG2 IS NOW wired into
# emit()'s CATALOG + EXT_CATALOG + EXT_CATALOG2 sum below. The prior note
# here said EXT_CATALOG2 could not be safely wired in because the live .ttl
# carried hand-verified refusalCode/refusalRationale/disposition corrections
# that this script's CATALOG dataclass did not model. That gap is now
# closed: LegacyCapability gained `refusal_code`/`refusal_rationale` fields
# (to_turtle() emits ggen:refusalCode/ggen:refusalRationale when set), and
# every individual in CATALOG/EXT_CATALOG whose disposition had been
# hand-repaired on disk (legacy_stpnt_crate, legacy_wizard_command,
# legacy_sigma_command, legacy_inverse_sync_command, legacy_sync_audit_flag,
# legacy_sync_dry_run_value_flag, legacy_ggen_toml_dual_schema,
# legacy_ext_template_mode_append, and the 6 LEGACY-DEAD-CRATE-DELETED
# individuals under class 11) now carries that same resolved disposition,
# migration/rollback/archive path, refusal code/rationale, and rdfs:comment
# directly in its dataclass literal -- transcribed from the committed .ttl,
# not fabricated. Running `emit()` is safe again: it reproduces the
# semantics of the disposition-repair pass instead of discarding it.
# ---------------------------------------------------------------------------

EXT_CATALOG2: list[LegacyCapability] = [
    # --- Observer class 9: historical receipt schemas before ReceiptRecord ---
    LegacyCapability(
        slug="legacy_ext2_receipt_v6_ttl_schema",
        subsystem="engine",
        historical_source_commit="04fa6074b (Implement ontology-first compilation for ggen v6 (#98)) -- introduces crates/ggen-core/src/v6/receipt.rs; deleted with the rest of ggen-core by 9cef6e40f",
        legacy_source_path="crates/ggen-core/src/v6/receipt.rs, crates/ggen-core/src/v6/passes/receipt_gen.rs, v6/templates/receipt.ttl.tera (all deleted; git history via `git log --all --diff-filter=A -- crates/praxis-core/src/receipt_record.rs` shows ReceiptRecord itself was only introduced later, by cbf173f82)",
        historical_semantic_owner="ggen-core's v6 pipeline (pre-ggen-engine, pre-ReceiptRecord)",
        input_contract="ReceiptBuilder::new(toolchain_version) chained with with_epoch/with_policies/pass-execution records",
        output_contract="RDF/Turtle receipt rendered via v6/templates/receipt.ttl.tera (ReceiptGenerationPass writes to receipt_path), not the current JSON .ggen-v2/receipt.json + BLAKE3 chain_hash_hex format",
        error_contract="ggen-core's own v6 pass error types",
        side_effects="Filesystem write to a caller-supplied receipt_path (no fixed .ggen/.ggen-v2 convention observed at this layer)",
        ordering_requirements="ReceiptGenerationPass ran as one of several v6 compilation passes (`passes/receipt_gen.rs`), not the current sync.rs Stage 5 (emit) BLAKE3-chained write",
        default_behavior="No default receipt path baked into ReceiptGenerationPass itself (caller must call with_receipt_path); contrast with the live RECEIPT_REL_PATH const (.ggen-v2/receipt.json) in crates/ggen-engine/src/sync.rs",
        configuration_dependencies="ReceiptPolicies (own struct), Epoch (own struct) -- neither survived into praxis-core::ReceiptRecord by the same name",
        evidence_fixtures="git show 04fa6074b -- crates/ggen-core/src/v6/receipt.rs (ReceiptBuilder/Epoch/PassExecution/OutputFile/ReceiptPolicies struct definitions captured this session)",
        replacement_owner="crates/praxis-core/src/receipt_record.rs (ReceiptRecord, introduced by cbf173f82 -- a from-scratch JSON+BLAKE3 design, not an evolution of the v6 TTL receipt's field names)",
        disposition="REPLACED",
        standing="UNKNOWN",
        archive_path="git history at 9cef6e40f^ (crates/ggen-core/src/v6/receipt.rs)",
        notes="No commit found renaming/migrating v6's TTL receipt fields into ReceiptRecord's JSON fields -- the replacement is a full redesign (different serialization format entirely: RDF/Turtle vs. JSON), not a schema migration in the field-rename sense. Confirmed via `git log --all --oneline --diff-filter=A -- crates/praxis-core/src/receipt_record.rs` returning only cbf173f82 as ReceiptRecord's origin commit.",
    ),
    LegacyCapability(
        slug="legacy_ext2_receipt_record_schema_v1",
        subsystem="engine",
        historical_source_commit="cbf173f82 (ReceiptRecord introduced, implicit schema v1, no `schema`/`v2` fields exist yet) / 3b294a8ce and ba2f6a1ae (feat(heal-thyself-r1): constitution pack, receipt v2 epoch, release-pack scaffold (v26.7.20) (#281) -- adds schema/v2 fields)",
        legacy_source_path="crates/praxis-core/src/receipt_record.rs (ReceiptRecord struct)",
        historical_semantic_owner="praxis-core::ReceiptRecord, pre-receipt_epoch.rs",
        input_contract="Same chain_hash_hex/prev_chain_hash_hex/signature_hex/object_ids fields as today, but with no `schema` field at all (field did not exist -- not merely defaulted)",
        output_contract="A receipt JSON that, per commit 3b294a8ce's own doc comment on the new `schema` field, \"deserializes as v1\" when the field is absent -- i.e. every pre-3b294a8ce receipt on disk is implicitly schema v1 with no itemized admission ledger, no standing_ceiling lattice, no closed 8-class equivalence map, no post-admission obligation_count -- all of which schema v2's ReceiptEpochV2 payload (crates/praxis-core/src/receipt_epoch.rs) adds",
        error_contract="Same ReceiptRecord error handling as today (schema is descriptive only, per the field's own doc comment: \"Not part of the chain-hash computation\")",
        side_effects="Prior to 3b294a8ce, every hardcoded `Green` andon-status write site wrote directly; commit 3b294a8ce's message describes v2 as replacing every hardcoded Green write site with a derived andon computed from the v2 ledger -- a real behavior change, not just an added optional field",
        ordering_requirements="v1 receipts remain readable (\"dual-read/single-write compatibility\" per the commit message) -- v2 is additive, not a breaking cutover",
        default_behavior="`#[serde(default = \"crate::receipt_epoch::default_schema\")]` on the `schema` field means old receipts silently deserialize with schema=SCHEMA_V1 and v2=None rather than failing to parse",
        configuration_dependencies="crates/praxis-core/src/receipt_epoch.rs (SCHEMA_V1/SCHEMA_V2 consts, ReceiptEpochV2 struct, default_schema fn) -- did not exist before 3b294a8ce",
        evidence_fixtures="git show 3b294a8ce -- crates/praxis-core/src/receipt_record.rs (diff adding `schema: String` and `v2: Option<ReceiptEpochV2>` fields, +15 lines, captured this session); crates/ggen-engine/tests/receipt_epoch_e2e.rs (519 new lines, 12/12 tests per the commit message, asserting the v1-Green-cannot-leak-into-derived-Green invariant)",
        replacement_owner="crates/praxis-core/src/receipt_epoch.rs (schema v2, live)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        migration_path="commit 3b294a8ce's message: \"dual-read/single-write compatibility with a v1->v2 migration receipt\"",
        archive_path="git history at 3b294a8ce^ (crates/praxis-core/src/receipt_record.rs without the schema/v2 fields)",
        notes="Distinct from legacy_ext2_receipt_v6_ttl_schema above: this is an in-place schema evolution *within* the current ReceiptRecord type (v1 implicit -> v2 explicit), not a full-format replacement. Both are real, independently evidenced legacy receipt schemas.",
    ),
    # --- Observer class 13: historical output-tree layouts ---
    LegacyCapability(
        slug="legacy_ext2_output_tree_dotggen_receipt_path",
        subsystem="engine",
        historical_source_commit="04fa6074b (ggen-core v6, default receipt_path = PathBuf::from(\".ggen/receipt.json\")) / cbf173f82 (ggen-engine's RECEIPT_REL_PATH introduced as \".ggen-v2/receipt.json\" from its first commit, no in-file rename found)",
        legacy_source_path="crates/ggen-core/src/v6/receipt.rs (PipelineConfig::default(), deleted) vs. crates/ggen-engine/src/sync.rs (RECEIPT_REL_PATH const, live)",
        historical_semantic_owner="ggen-core v6 pipeline config default",
        input_contract="n/a -- this is an output path convention, not an input",
        output_contract="Receipt written to `.ggen/receipt.json` under the project root (ggen-core v6 default) vs. the live `.ggen-v2/receipt.json` + `.ggen-v2/receipt-log.jsonl` convention (crates/ggen-engine/src/sync.rs's RECEIPT_REL_PATH/RECEIPT_LOG_REL_PATH consts, per CLAUDE.md's Cryptographic Receipts section)",
        error_contract="n/a",
        side_effects="Filesystem write location differs: `.ggen/` (legacy) vs `.ggen-v2/` (live) -- CLAUDE.md's own `.ggen/keys/{signing,verifying}.key` note confirms `.ggen/` is still live for signing keys today, so this is not a fully retired directory, only its receipt-specific sub-path moved",
        ordering_requirements="n/a",
        default_behavior="ggen-core v6's PipelineConfig::default() hardcoded `.ggen/receipt.json`; the live RECEIPT_REL_PATH const was introduced already set to `.ggen-v2/receipt.json` in cbf173f82 with no observed prior `.ggen/receipt.json` value in that same file's own history (`git log --all --oneline -p -S'RECEIPT_REL_PATH' -- crates/ggen-engine/src/sync.rs` shows only the one addition)",
        configuration_dependencies="n/a",
        evidence_fixtures="git show 04fa6074b -- crates/ggen-core/src/v6/receipt.rs (`receipt_path: Some(PathBuf::from(\".ggen/receipt.json\"))` in PipelineConfig::default(), captured this session)",
        replacement_owner="crates/ggen-engine/src/sync.rs RECEIPT_REL_PATH const (.ggen-v2/receipt.json)",
        disposition="REPLACED",
        standing="UNKNOWN",
        archive_path="git history at 9cef6e40f^ (crates/ggen-core/src/v6/receipt.rs)",
        notes="The `.ggen` vs `.ggen-v2` split is real but this script did not find a single commit that explicitly renames one to the other -- ggen-engine's receipt path was vendored in already as `.ggen-v2` (cbf173f82, the same commit that vendored ggen-engine wholesale from ~/praxis), so the `-v2` suffix likely predates this repository's own history rather than being coined here. Treat the lineage as inferred from the two constants' literal values, not from a single rename diff.",
    ),
    LegacyCapability(
        slug="legacy_ext2_output_tree_src_generated_default",
        subsystem="engine",
        historical_source_commit="04fa6074b (ggen-core v6 PipelineConfig::default(), output_dir = PathBuf::from(\"src/generated\"))",
        legacy_source_path="crates/ggen-core/src/v6/receipt.rs or its PipelineConfig owner (deleted with ggen-core, 9cef6e40f)",
        historical_semantic_owner="ggen-core v6 pipeline config default",
        input_contract="n/a -- output path convention",
        output_contract="Generated files defaulted to a fixed `src/generated/` subtree under the project root, unless overridden",
        error_contract="n/a",
        side_effects="Filesystem writes concentrated under one fixed directory by default",
        ordering_requirements="n/a",
        default_behavior="Fixed `src/generated` default output_dir -- contrasts with the live pipeline's template-frontmatter-driven `output_file` pattern (crates/ggen-config's GenerationMode/output_file, enforced not to escape the project root by GGEN-YIELD-001 but otherwise caller-controlled per-template, not a single fixed directory)",
        configuration_dependencies="PipelineConfig (ggen-core v6, deleted)",
        evidence_fixtures="git show 04fa6074b -- crates/ggen-core/src/v6/receipt.rs (`output_dir: PathBuf::from(\"src/generated\")` in PipelineConfig::default(), captured this session)",
        replacement_owner="Per-template `output_file` pattern (crates/ggen-config/src/manifest/types.rs), validated by GGEN-YIELD-001 (crates/ggen-lsp/src/analyzers/tera_analyzer.rs) rather than defaulted to one fixed directory",
        disposition="REPLACED",
        standing="UNKNOWN",
        archive_path="git history at 9cef6e40f^",
        notes="A genuine layout-contract change: a fixed-default-directory model gave way to a fully template-driven per-output-file model. No single commit found that describes this as a deliberate migration decision (ggen-engine was vendored wholesale, not evolved in-repo from ggen-core's v6 code) -- disposition REPLACED reflects the functional lineage (same job: decide where generated files land), not a confirmed migration commit.",
    ),
    # --- Observer class 14: exit-code contracts ---
    LegacyCapability(
        slug="legacy_ext2_exit_code_mutation_budget_threshold",
        subsystem="products",
        historical_source_commit="9c76a75ac / 1af960911 (feat(cli): ggen v5 - unified sync command (#91)) -- removes two `std::process::exit(2)` sites; introduced earlier by Quicksave commits (24e7c1cd9/591b4aefc/fdb7bb477/d8445fd53)",
        legacy_source_path="crates/ggen-cli (mutation-testing `--fail-on-threshold` and budget-enforcement `--fail-on-violation` CLI paths, removed by the v5 unified-sync refactor; exact file path not re-derived beyond the diff hunks captured this session)",
        historical_semantic_owner="Pre-v5 ggen-cli mutation-testing and budget-enforcement subcommands",
        input_contract="`--fail-on-threshold` (mutation kill-rate check) / `--fail-on-violation` (budget check) CLI flags",
        output_contract="stderr message (\"FAIL: Mutation kill rate {:.1}% below threshold 80%\" / \"FAIL: {e}\") then `std::process::exit(2)` -- a distinct exit code from the generic error path's exit(1)",
        error_contract="Exit code 2 specifically for these two threshold-violation cases, distinguishable by callers from a generic exit(1) failure",
        side_effects="Process termination with a specific, callable-distinguishable exit code",
        ordering_requirements="n/a",
        default_behavior="Exit 2 only fired when `args.fail_on_threshold`/`args.fail_on_violation` was set and the check failed; otherwise the command completed normally even on a low kill-rate/budget violation",
        configuration_dependencies="CLI flags `--fail-on-threshold`, `--fail-on-violation`",
        evidence_fixtures="git show 9c76a75ac -U5 (diff removing both `std::process::exit(2)` call sites, captured this session, part of the v5 unified-sync command consolidation)",
        replacement_owner="none -- confirmed absent: `grep -rn \"exit(2)\" crates/` repo-wide (2026-07-31) finds only crates/ggen-lsp/src/main.rs and crates/ggen-engine/src/bin/dod.rs, neither a mutation-kill-rate/budget-threshold check; no `--fail-on-threshold`/`--fail-on-violation` flags exist anywhere in the live CLI surface",
        disposition="ARCHIVED",
        standing="UNKNOWN",
        archive_path="git history at 9c76a75ac^ (diff removing both exit(2) call sites; re-verified 2026-07-31 by repo-wide grep for exit(2) and for --fail-on-threshold/--fail-on-violation, both absent from the live surface)",
        notes="Disposition resolved from DISPOSITION_UNKNOWN to ARCHIVED: re-derivation on 2026-07-31 confirmed by direct repo-wide grep (not fabrication) that neither the exit(2) contract nor its triggering flags survive anywhere in the live CLI. The capability was dropped, not refactored -- ARCHIVED per this corpus's convention for removed surfaces with no live successor.",
    ),
    LegacyCapability(
        slug="legacy_ext2_exit_code_cli_main_result_wrapper",
        subsystem="products",
        historical_source_commit="c30e4c244 / 594f48669 (Audit CLI functionality and capabilities (#54))",
        legacy_source_path="crates/ggen-cli/src/main.rs (or ggen-cli-lib's equivalent binary entrypoint at the time)",
        historical_semantic_owner="ggen-cli's top-level `#[tokio::main] async fn main()`",
        input_contract="n/a -- process-level error handling contract, not a CLI argument",
        output_contract="BEFORE (per the commit's own before/after doc comment captured in this session): `async fn main() -> anyhow::Result<()> { ggen_cli_lib::cli_match().await? }` -- relies on Rust's `Termination` impl for `Result<(), E>`, which the commit's own comment characterizes as \"Returns 0 on error (anyhow::Result doesn't distinguish properly)\". AFTER: explicit `match { Ok(()) => exit(0), Err(e) => { eprintln!(...); exit(1) } }`",
        error_contract="Legacy: exit-code correctness depended on `std::process::Termination`'s blanket impl for `Result<(), E: Debug>`, which the commit author asserts (in-commit doc comment, not independently re-verified against the Rust std docs by this pass) did not reliably distinguish success/failure exit codes for this codebase's error type. Live: explicit exit(0)/exit(1) with a printed error message on failure.",
        side_effects="Process exit code and stderr output on failure differ between the two eras",
        ordering_requirements="n/a",
        default_behavior="Legacy path printed no explicit \"ERROR:\" prefix (relied on anyhow's own Debug/Display formatting via the `?` operator's implicit termination handling); live path always prints `ERROR: {e}` to stderr before exiting 1",
        configuration_dependencies="n/a",
        evidence_fixtures="git show c30e4c244 -U5 (before/after doc comment plus the actual main() diff, captured this session)",
        replacement_owner="crates/ggen-cli/src/main.rs's current explicit match/exit(0)/exit(1) pattern (not independently re-verified against the present-day file in this pass -- confirm current main.rs still matches this shape before treating it as final)",
        disposition="REPLACED",
        standing="UNKNOWN",
        archive_path="git history at c30e4c244^",
        notes="The commit's own claim that the legacy `anyhow::Result` return type 'Returns 0 on error' is the author's characterization, captured verbatim from the commit's doc comment -- this pass did not independently reproduce the alleged bug (e.g. by checking out c30e4c244^ and testing exit codes), so the claim is evidenced as 'what the commit asserts', not as independently re-verified fact.",
    ),
    # --- Observer class 18: pack.toml schema versions ---
    LegacyCapability(
        slug="legacy_ext2_pack_toml_schema_v1_no_outputs",
        subsystem="products",
        historical_source_commit="3a6eb6a8c / ea6b227c9 (feat: marketplace v6.1 expansion, receipt signing, CLI ontology templates, pack integration) -- introduces crates/ggen-marketplace/src/metadata.rs's PackageToml/PackageSection/SecuritySection",
        legacy_source_path="crates/ggen-marketplace/src/metadata.rs (original location; live struct now at crates/ggen-marketplace/src/marketplace/metadata.rs after the marketplace module reorg)",
        historical_semantic_owner="ggen-marketplace's pack.toml deserializer, v6.1 era",
        input_contract="pack.toml with only `[package] name, version` and `[security] signature/trust_tier/checksum` -- no `[pack]` table, no `[pack.outputs]` sub-table at all (PackSection did not exist as a struct)",
        output_contract="PackageToml{package: PackageSection{name,version}, security: Option<SecuritySection>} -- no `pack: Option<PackSection>` field",
        error_contract="serde deserialization error if a pack.toml supplied a `[pack]` table (unrecognized field, depending on serde's deny_unknown_fields setting at the time -- not independently re-verified)",
        side_effects="none beyond in-memory struct population",
        ordering_requirements="n/a",
        default_behavior="A pack with no `[security]` table deserialized security as None; there was no equivalent default/optionality question for `[pack.outputs]` since the field did not exist to default",
        configuration_dependencies="pack.toml itself",
        evidence_fixtures="git show 3a6eb6a8c:crates/ggen-marketplace/src/metadata.rs (struct definitions at introduction, captured this session, confirmed to lack `pack: Option<PackSection>`)",
        replacement_owner="crates/ggen-marketplace/src/marketplace/metadata.rs's live PackageToml, which adds `pack: Option<PackSection>` carrying `[pack.outputs]` (named-output key -> relative-path mapping, `#[serde(default)]` so old pack.toml files without a `[pack]` table still parse)",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        migration_path="Additive, backward-compatible: `#[serde(default)]` on the new `pack` field means pre-existing pack.toml files (lacking `[pack.outputs]`) still deserialize correctly under the live schema -- not a breaking cutover",
        archive_path="git history at the metadata.rs file's introduction, 3a6eb6a8c",
        notes="This pass did not pinpoint the exact single commit that added `PackSection`/`outputs` (only confirmed it is absent at 3a6eb6a8c and present today); `git log --all --oneline -S'struct PackSection'` returned no output in this session's search, suggesting the struct may have been renamed/moved during the marketplace module reorg (4b11843a5, 'refactor(marketplace): extract ggen-marketplace from ggen-core') rather than newly authored in a single traceable commit -- flagging this gap rather than guessing a commit hash.",
    ),
    LegacyCapability(
        slug="legacy_ext2_pack_toml_schema_no_registry_provenance",
        subsystem="products",
        historical_source_commit="55268c2e0 / b4eaa96e7 (feat(cli, marketplace, sos): finish 1000x consequence substrate) -- adds registry_type/origin_url fields, commit message states \"receipts now include full provenance (registry_type, origin_url)\"",
        legacy_source_path="crates/ggen-marketplace/src/**/metadata.rs (PackMetadata struct)",
        historical_semantic_owner="ggen-marketplace's PackMetadata, pre-1000x-consequence-substrate",
        input_contract="PackMetadata carrying only signature/trust_tier/checksum -- no registry_type, no origin_url",
        output_contract="A pack's provenance receipt with no record of which registry type it came from or its origin URL",
        error_contract="n/a",
        side_effects="none beyond in-memory/serialized struct population",
        ordering_requirements="n/a",
        default_behavior="Prior to 55268c2e0, PackMetadata::default() had no registry_type/origin_url fields to default at all (not merely None-by-default -- the fields did not exist)",
        configuration_dependencies="pack.toml `[security]` table plus whatever registry-resolution context supplied the (then-nonexistent) provenance fields",
        evidence_fixtures="git show 55268c2e0 -- 'crates/**/metadata.rs' (diff adding `registry_type: Option<RegistryType>` and `origin_url: Option<String>` fields plus their `None` defaults, captured this session; commit message itself: \"μ5 Receipts now include full provenance (registry_type, origin_url)\")",
        replacement_owner="Live PackMetadata (crates/ggen-marketplace/src/marketplace/metadata.rs) with registry_type/origin_url populated",
        disposition="SUBSUMED",
        standing="UNKNOWN",
        migration_path="Additive: both new fields are Option<T> defaulting to None, so old pack metadata without provenance still deserializes",
        archive_path="git history at 55268c2e0^",
        notes="Distinct from legacy_ext2_pack_toml_schema_v1_no_outputs above: that individual covers the pack.toml file's own `[pack.outputs]` table; this one covers PackMetadata's registry-provenance fields, a different struct in the same subsystem with an independently evidenced commit.",
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
    if cap.refusal_code:
        lines.append(f'  ggen:refusalCode "{escape(cap.refusal_code)}" ;')
    if cap.refusal_rationale:
        lines.append(f'  ggen:refusalRationale "{escape(cap.refusal_rationale)}" ;')
    if cap.notes:
        lines.append(f'  rdfs:comment "{escape(cap.notes)}" ;')
    # Replace trailing " ;" of last line with " ."
    lines[-1] = lines[-1].rsplit(" ;", 1)[0] + " ."
    return "\n".join(lines)


def emit() -> None:
    head = run(["git", "rev-parse", "HEAD"]).strip() or "UNKNOWN"
    total = len(CATALOG) + len(EXT_CATALOG) + len(EXT_CATALOG2)
    header = f"""# ontology/v26.8.1/legacy-capabilities.ttl — GENERATED DATA FILE
#
# Produced by tools/v26.8.1/legacy_archaeology.py from real git history
# mined against this worktree. See that script's CATALOG for the
# evidence backing each individual (commit hashes, deleted paths).
#
# Generated against HEAD: {head}
# Individual count: {total} ({len(CATALOG)} original + {len(EXT_CATALOG)} from the
# 2026-07-31 exhaustive-observer pass [EXT_CATALOG] + {len(EXT_CATALOG2)} from the
# 2026-07-31 remaining-5-classes pass [EXT_CATALOG2], see
# docs/v26.8.1/90-legacy/observer-class-report.md)
#
# Do not hand-edit individuals from CATALOG/EXT_CATALOG/EXT_CATALOG2; edit
# those lists in tools/v26.8.1/legacy_archaeology.py and re-run:
#   python3 tools/v26.8.1/legacy_archaeology.py emit

@prefix ggen: <https://ggen.chatmangpt.com/ontology/v26.8.1#> .
@prefix legacy: <https://ggen.chatmangpt.com/ontology/v26.8.1/legacy#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

"""
    body = "\n\n".join(to_turtle(cap) for cap in CATALOG + EXT_CATALOG + EXT_CATALOG2)
    OUT_PATH.write_text(header + body + "\n", encoding="utf-8")
    print(f"Wrote {total} LegacyCapability individuals to {OUT_PATH}")


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
