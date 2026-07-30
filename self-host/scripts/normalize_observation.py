"""Normalize the raw repository census into bounded live authority findings.

The raw model deliberately over-observes every manifest and generated marker. This
module applies the repository standing boundary: archives, templates, fixtures,
evidence, and dormant consumer manifests remain visible in the census but cannot
create live remediation obligations merely by containing generated-output text.
"""
from __future__ import annotations

import json
import re
from collections import Counter, defaultdict
from pathlib import PurePosixPath
from typing import Any

TEMPLATE_EXTENSIONS = {".tera", ".tmpl", ".template", ".j2", ".jinja"}
SOURCE_EXTENSIONS = {".rs", ".py", ".sh", ".js", ".ts", ".tsx", ".go", ".java", ".kt"}
CONFIG_EXTENSIONS = {".toml", ".yaml", ".yml", ".json", ".lock", ".ini"}
ARCHIVE_SEGMENTS = {"archive", "_archive", "archive_2025", "archive_ggen_core", "legacy_structure"}
BOOTSTRAP_PREFIXES = (
    "crates/ggen-engine/src/",
    "crates/ggen-config/src/",
    "crates/ggen-graph/src/",
    "crates/praxis-core/src/",
    "crates/praxis-graphlaw/src/",
    "crates/ggen-cli/src/",
)
REBUILT_CATEGORIES = {
    "generated-ownership",
    "generated-output",
    "output-ownership",
    "workspace-identity",
    "workspace-closure",
}


def authority_for(file: dict[str, Any]) -> str:
    path = str(file["path"])
    p = PurePosixPath(path)
    parts = set(p.parts)
    suffix = p.suffix.lower()
    if parts & ARCHIVE_SEGMENTS or any(part.startswith("archive_") for part in p.parts):
        return "Archive"
    if path.startswith("evidence/") or "/receipts/" in path or path.endswith("receipt.json"):
        return "VerificationEvidence"
    if (
        path in {"Cargo.toml", "ggen.toml", "rust-toolchain.toml", "CONSTITUTION.md"}
        or path.startswith(".specify/") and suffix in {".ttl", ".rq"}
        or re.match(r"^packs/[^/]+/(ontology\.ttl|pack\.toml|gates/.*\.rq)$", path)
        or path == "self-host/ontology.ttl"
    ):
        return "AuthoredConstitution"
    if suffix in TEMPLATE_EXTENSIONS:
        return "Template"
    if "/tests/" in path or "/fixtures/" in path or path.startswith("tests/"):
        return "TestFixture"
    if path.startswith(BOOTSTRAP_PREFIXES):
        return "BootstrapKernel" if not file["generated_marker"] else "GeneratedConsequence"
    if path.startswith(".github/workflows/") and suffix in {".yml", ".yaml"}:
        return "Workflow"
    if suffix in SOURCE_EXTENSIONS:
        return "GeneratedConsequence" if file["generated_marker"] else "ExecutableSource"
    if suffix == ".md" or path.startswith("docs/") or path.startswith("book/"):
        return "Documentation"
    if suffix in CONFIG_EXTENSIONS or p.name in {"justfile", "Makefile", "Makefile.toml"}:
        return "GeneratedConsequence" if file["generated_marker"] else "Configuration"
    return "GeneratedConsequence" if file["generated_marker"] else "Asset"


def load_bearing(path: str, authority: str) -> bool:
    if authority in {"Archive", "Template", "TestFixture", "VerificationEvidence", "Documentation", "Asset"}:
        return False
    if authority in {"AuthoredConstitution", "BootstrapKernel", "Workflow"}:
        return True
    if path in {"Cargo.toml", "ggen.toml", "justfile"}:
        return True
    return path.startswith(("crates/", "packs/", "scripts/", ".github/actions/"))


def make_finding(model: Any, **values: str) -> dict[str, Any]:
    return model.finding(
        values["category"],
        values["severity"],
        values["summary"],
        values["evidence_path"],
        values["remediation"],
        values["rationale"],
        values["must_not"],
        values["acceptance"],
        values["verification"],
    )


def normalize(observation: dict[str, Any], model: Any) -> dict[str, Any]:
    for file in observation["files"]:
        authority = authority_for(file)
        file["authority_class"] = authority
        file["load_bearing"] = load_bearing(str(file["path"]), authority)

    # Only the repository-root ggen.toml is an active self-host projection.
    # Nested examples/specifications remain catalogued and visible but do not create
    # repository build obligations until their own load path admits them.
    for claim in observation["output_claims"]:
        if claim["claim_kind"] == "active-manifest-rule" and claim["consumer"] != ".":
            claim["claim_kind"] = "catalogued-manifest-rule"

    findings = [item for item in observation["findings"] if item["category"] not in REBUILT_CATEGORIES]
    files_by_path = {item["path"]: item for item in observation["files"]}
    tracked_paths = set(files_by_path)
    root_claims = [
        item
        for item in observation["output_claims"]
        if item["claim_kind"] == "active-manifest-rule" and item["consumer"] == "."
    ]
    owned_outputs = {item["output_path"] for item in root_claims}

    output_owners: dict[str, list[str]] = defaultdict(list)
    for claim in root_claims:
        output_owners[claim["output_path"]].append(claim["owner"])
    for output, owners in sorted(output_owners.items()):
        if len(owners) <= 1:
            continue
        findings.append(
            make_finding(
                model,
                category="output-ownership",
                severity="Blocking",
                summary=f"Root self-host consumer has {len(owners)} writers for `{output}`.",
                evidence_path="ggen.toml",
                remediation="Collapse the root output to one authoritative projection owner.",
                rationale="Multiple active writers are competing actuators and make receipts non-attributable.",
                must_not="Do not rely on generation order or last-writer-wins behavior.",
                acceptance="Every root output path has exactly one active generation owner.",
                verification="python3 self-host/scripts/observe_exact_tree.py --check",
            )
        )
    for claim in root_claims:
        output = claim["output_path"]
        if output.startswith("UNSAFE::") or output in tracked_paths:
            continue
        findings.append(
            make_finding(
                model,
                category="generated-output",
                severity="High",
                summary=f"Root manifest output `{output}` is absent from the tracked tree.",
                evidence_path=claim["owner"],
                remediation="Run the owning root projection and either commit the admitted consequence or remove the stale claim.",
                rationale="A root-level declared consequence with no artifact is an open manufacturing obligation.",
                must_not="Do not create a placeholder file merely to satisfy existence checks.",
                acceptance="The root generation rule produces the artifact and second generation is byte-identical.",
                verification="ggen sync run && ggen receipt verify",
            )
        )

    for file in observation["files"]:
        if not (
            file["generated_marker"]
            and file["authority_class"] == "GeneratedConsequence"
            and file["load_bearing"]
            and file["path"] not in owned_outputs
        ):
            continue
        findings.append(
            make_finding(
                model,
                category="generated-ownership",
                severity="Blocking",
                summary=f"Live generated consequence `{file['path']}` has no root self-host projection owner.",
                evidence_path=file["path"],
                remediation="Bind the file to admitted ontology and template law or reclassify it honestly as authored bootstrap code.",
                rationale="A live generated marker without a replayable root owner is false authority.",
                must_not="Do not preserve a generated comment as a substitute for an executable projection and receipt path.",
                acceptance="A clean checkout regenerates the exact file from one named owner and verifies identical bytes.",
                verification="python3 self-host/scripts/observe_exact_tree.py --check",
            )
        )

    packages = observation["cargo_packages"]
    package_by_path = {item["path"]: item for item in packages}
    active_paths = set(observation["workspace_members"]) | ({"."} if "." in package_by_path else set())
    active_names: dict[str, list[str]] = defaultdict(list)
    for path in sorted(active_paths):
        package = package_by_path.get(path)
        if package and package["name"]:
            active_names[package["name"]].append(path)
    for name, owners in sorted(active_names.items()):
        if len(owners) <= 1:
            continue
        findings.append(
            make_finding(
                model,
                category="workspace-identity",
                severity="Blocking",
                summary=f"Active workspace package identity `{name}` has competing owners: {', '.join(owners)}.",
                evidence_path="Cargo.toml",
                remediation="Give every active workspace package one unique package identity.",
                rationale="Duplicate active identities make dependency and release projections ambiguous.",
                must_not="Do not treat archived examples or reference implementations as active workspace owners.",
                acceptance="No two active workspace packages declare the same package name.",
                verification="cargo metadata --no-deps --format-version 1",
            )
        )
    for member in sorted(observation["workspace_members"]):
        if member in package_by_path:
            continue
        findings.append(
            make_finding(
                model,
                category="workspace-closure",
                severity="Blocking",
                summary=f"Workspace member `{member}` has no tracked Cargo package manifest.",
                evidence_path="Cargo.toml",
                remediation="Repair the workspace member or restore its package manifest from admitted source.",
                rationale="A declared build member that does not exist makes workspace standing non-replayable.",
                must_not="Do not remove the member solely to force a green build without an admitted retirement decision.",
                acceptance="Every workspace member resolves to one tracked Cargo package.",
                verification="cargo metadata --no-deps --format-version 1",
            )
        )
    for package in packages:
        path = package["path"]
        parts = PurePosixPath(path).parts
        if len(parts) != 2 or parts[0] != "crates" or path in active_paths:
            continue
        findings.append(
            make_finding(
                model,
                category="workspace-closure",
                severity="High",
                summary=f"Top-level crate `{path}` is outside the active workspace.",
                evidence_path=f"{path}/Cargo.toml",
                remediation="Classify the crate as an admitted member, an explicit exclusion, or a separately verified product.",
                rationale="A top-level crate without workspace or explicit external-product standing is an ambiguous executable surface.",
                must_not="Do not silently leave live source outside every build and release boundary.",
                acceptance="The crate has exactly one explicit build and release standing with its own verifier.",
                verification="cargo metadata --no-deps --format-version 1",
            )
        )

    findings = sorted(
        {item["finding_id"]: item for item in findings}.values(),
        key=lambda item: (item["severity_order"], item["category"], item["finding_id"]),
    )
    authority_counts = Counter(item["authority_class"] for item in observation["files"])
    observation["authority_counts"] = dict(sorted(authority_counts.items()))
    observation["findings"] = findings
    observation["counts"]["generated"] = authority_counts.get("GeneratedConsequence", 0)
    observation["counts"]["findings"] = len(findings)
    observation["counts"]["blocking_findings"] = sum(item["severity"] == "Blocking" for item in findings)
    observation["counts"]["unknown_authority"] = authority_counts.get("UnknownAuthority", 0)
    observation.pop("observation_digest", None)
    canonical = json.dumps(observation, sort_keys=True, separators=(",", ":")).encode("utf-8")
    observation["observation_digest"] = model.sha256(canonical)
    return observation
