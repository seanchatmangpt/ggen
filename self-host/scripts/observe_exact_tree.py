#!/usr/bin/env python3
"""Canonical exact-tree executor for the ggen repository observer.

The model implementation lives in observe_repository.py. This executor replaces its
filesystem byte reader with Git HEAD object semantics before invoking it: regular
files and symlinks are read from the recorded tree, while gitlinks/submodules are
represented as empty content. Mutable working-tree projections therefore cannot
contaminate an exact-revision observation.
"""
from __future__ import annotations

import argparse
import importlib.util
import io
import os
from pathlib import Path
import subprocess
import tarfile

MODULE_PATH = Path(__file__).with_name("observe_repository.py")
SPEC = importlib.util.spec_from_file_location("ggen_self_observer_model", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load observer model at {MODULE_PATH}")
MODEL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODEL)
BASE_EMIT_TURTLE = MODEL.emit_turtle

_HEAD_SNAPSHOTS: dict[tuple[str, str], dict[str, bytes]] = {}


def git(root: Path, *args: str) -> bytes:
    return subprocess.run(
        ["git", "-C", str(root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def head_snapshot(root: Path) -> dict[str, bytes]:
    """Return tracked HEAD bytes without consulting mutable worktree contents."""
    revision = git(root, "rev-parse", "HEAD").decode("ascii").strip()
    key = (str(root.resolve()), revision)
    cached = _HEAD_SNAPSHOTS.get(key)
    if cached is not None:
        return cached

    archive = git(root, "archive", "--format=tar", revision)
    snapshot: dict[str, bytes] = {}
    with tarfile.open(fileobj=io.BytesIO(archive), mode="r:") as stream:
        for member in stream.getmembers():
            if member.isfile():
                handle = stream.extractfile(member)
                if handle is None:
                    raise ValueError(f"archive member unreadable: {member.name}")
                snapshot[member.name] = handle.read()
            elif member.issym():
                snapshot[member.name] = member.linkname.encode("utf-8")

    index = git(root, "ls-files", "-s", "-z")
    for record in index.split(b"\0"):
        if not record:
            continue
        metadata, path_raw = record.split(b"\t", 1)
        mode, _blob, _stage = metadata.decode("ascii").split()
        if mode == "160000":
            snapshot[path_raw.decode("utf-8")] = b""

    _HEAD_SNAPSHOTS[key] = snapshot
    return snapshot


def git_semantic_bytes(root: Path, rel: str) -> bytes:
    """Read admitted bytes from HEAD; untracked or absent paths have no standing."""
    return head_snapshot(root).get(rel, b"")


def safe_text(value: object) -> str:
    return " ".join(str(value).replace('"', "'").replace("\t", " ").splitlines()).strip()


def allowed_paths(category: str, evidence_path: str) -> list[str]:
    by_category = {
        "workspace-closure": ["Cargo.toml", "crates"],
        "workspace-identity": ["Cargo.toml", "crates"],
        "pack-contract": ["packs"],
        "output-ownership": ["ggen.toml", "packs", ".specify"],
        "generated-ownership": ["ggen.toml", "packs", ".specify", "crates"],
        "generated-output": ["ggen.toml", "packs", ".specify"],
        "repository-layout": ["docs", "book"],
        "template-authority": ["templates", "packs"],
        "script-authority": ["scripts", "docs/archive"],
        "archive-authority": ["examples"],
        "observation-authority": [".specify", "self-host", "ggen.toml"],
        "cli-authority": ["crates/ggen-cli", "schema", ".specify"],
        "contribution-governance": [".github", "self-host"],
    }
    paths = list(by_category.get(category, []))
    if evidence_path and not evidence_path.startswith(("manifest:", "pack-consumer:")):
        evidence = evidence_path.rstrip("/")
        if evidence and not evidence.startswith("/") and ".." not in Path(evidence).parts:
            paths.append(evidence)
    return sorted(dict.fromkeys(paths or ["self-host"]))


def gall_priority(severity: str) -> str:
    return {
        "Blocking": "Highest",
        "High": "High",
        "Medium": "Medium",
        "Low": "Low",
        "Information": "Lowest",
    }[severity]


def gall_work_item_lines(item: dict[str, object], order: int) -> list[str]:
    finding_id = str(item["finding_id"])
    iri = f"<urn:ggen:self:finding:{finding_id}>"
    paths = allowed_paths(str(item["category"]), str(item["evidence_path"]))
    lines = [
        f"{iri} a gall:WorkItem ;",
        f"    gall:workItemId {MODEL.turtle_literal(finding_id)} ;",
        "    gall:issueType gall:Task ;",
        f"    gall:summary {MODEL.turtle_literal(safe_text(item['summary']))} ;",
        f"    gall:objective {MODEL.turtle_literal(safe_text(item['remediation']))} ;",
        f"    gall:rationale {MODEL.turtle_literal(safe_text(item['rationale']))} ;",
        "    gall:belongsToProgram gsh:ggen ;",
        "    gall:belongsToCheckpoint gsh:checkpoint-exact-tree ;",
        f"    gall:implementationOrder {order} ;",
        f"    gall:priority gall:{gall_priority(str(item['severity']))} ;",
        f"    gall:component {MODEL.turtle_literal(safe_text(item['category']))} ;",
        '    gall:label "ggen-self-host" ;',
        '    gall:assigneeRole "ggen core implementation agent" ;',
        '    gall:reviewerRole "independent Gall verifier" ;',
        '    gall:approvalGate "green verification receipt and exact-revision replay" ;',
        "    gall:protocolState gall:Draft ;",
        '    gall:requiredContext "generated/REPOSITORY_CENSUS.md" ;',
        '    gall:requiredContext "generated/AUTHORITY_MAP.md" ;',
    ]
    for path in paths:
        lines.append(f"    gall:allowedPath {MODEL.turtle_literal(safe_text(path))} ;")
    for path in [".git", "target", "self-host/observed", "self-host/generated", ".gall", "receipts/gall"]:
        lines.append(f"    gall:forbiddenPath {MODEL.turtle_literal(path)} ;")
    verification = f"cd .. && {safe_text(item['verification_command'])}"
    lines.extend(
        [
            f"    gall:mustDo {MODEL.turtle_literal(safe_text(item['must_do']))} ;",
            f"    gall:mustNotDo {MODEL.turtle_literal(safe_text(item['must_not_do']))} ;",
            '    gall:outOfScope "Unrelated repository behavior and external platform settings" ;',
            f"    gall:acceptanceCriterion {MODEL.turtle_literal(safe_text(item['acceptance_criterion']))} ;",
            f"    gall:definitionOfDone {MODEL.turtle_literal(safe_text(item['acceptance_criterion']) + ' The next exact-tree observation no longer emits this finding.')} ;",
            f"    gall:verificationCommand {MODEL.turtle_literal(verification)} ;",
            '    gall:evidenceArtifact "observed/repository.json" ;',
            '    gall:evidenceArtifact "generated/AUTHORITY_MAP.md" ;',
            f"    gall:adversarialQuestion {MODEL.turtle_literal(safe_text('Which smallest sabotage would recreate ' + finding_id + ' and does the verifier refuse it?'))} .",
            "",
        ]
    )
    return lines


def emit_turtle(observation: dict[str, object]) -> str:
    base = BASE_EMIT_TURTLE(observation)
    findings = list(observation["findings"])
    revision = safe_text(observation["revision"])
    lines = [
        base,
        "",
        "@prefix gall: <http://seanchatmangpt.github.io/packs/gall-core#> .",
        "",
        "# Gall program generated from the same exact-tree observation.",
        "gsh:ggen a gall:GallProgram ;",
        '    gall:programId "GGEN-SELF-HOST" ;',
        f"    gall:releaseIdentity {MODEL.turtle_literal('ggen-self-host@' + revision)} ;",
        '    gall:jiraProjectKey "GGEN" ;',
        "    gall:hasCheckpoint gsh:checkpoint-exact-tree ;",
        "    gall:hasAutomationProfile gsh:automation-plan-only ;",
        "    gall:hasWorkItem gsh:work-item-census",
    ]
    for item in findings:
        lines[-1] += " ;"
        lines.append(f"    gall:hasWorkItem <urn:ggen:self:finding:{item['finding_id']}> ")
    lines[-1] = lines[-1].rstrip() + " ."
    lines.extend(
        [
            "",
            "gsh:capability-exact-tree a gall:Capability ;",
            '    gall:capabilityId "GGEN-EXACT-TREE-OBSERVATION" ;',
            '    gall:title "Exact-revision repository observation and generated retrofit work" .',
            "",
            "gsh:checkpoint-exact-tree a gall:Checkpoint, gall:RequiredCheckpoint ;",
            '    gall:checkpointId "GGEN-SELF-OBSERVE" ;',
            '    gall:title "Observe all of ggen and manufacture the retrofit program" ;',
            "    gall:archetype gall:CensusCheckpoint ;",
            "    gall:belongsToProgram gsh:ggen ;",
            "    gall:producesCapability gsh:capability-exact-tree ;",
            '    gall:runnerCommand "python3 scripts/run_checkpoint.py" ;',
            "    gall:positiveWitness gsh:obligation-positive ;",
            "    gall:negativeFalsifier gsh:obligation-negative ;",
            "    gall:receiptObligation gsh:obligation-receipt ;",
            "    gall:replayObligation gsh:obligation-replay ;",
            "    gall:hasWorkItem gsh:work-item-census",
        ]
    )
    for item in findings:
        lines[-1] += " ;"
        lines.append(f"    gall:hasWorkItem <urn:ggen:self:finding:{item['finding_id']}> ")
    lines[-1] = lines[-1].rstrip() + " ."
    lines.extend(
        [
            "",
            "gsh:obligation-positive a gall:PositiveWitness ;",
            '    gall:name "exact-tree observer witnesses" ;',
            '    gall:command "python3 scripts/run_positive_witness.py" .',
            "",
            "gsh:obligation-negative a gall:NegativeFalsifier ;",
            '    gall:name "tampered observation refusal" ;',
            '    gall:command "python3 scripts/run_negative_falsifier.py" .',
            "",
            "gsh:obligation-receipt a gall:ReceiptObligation ;",
            '    gall:name "independent observation receipt verification" ;',
            '    gall:command "python3 scripts/verify_observation.py" .',
            "",
            "gsh:obligation-replay a gall:ReplayObligation ;",
            '    gall:name "detached exact-revision observation replay" ;',
            '    gall:command "python3 self-host/scripts/run_checkpoint.py" .',
            "",
            "gsh:automation-plan-only a gall:AutomationProfile ;",
            '    gall:automationProfileId "GGEN-SELF-HOST-PLAN" ;',
            "    gall:trackerProvider gall:FileTracker ;",
            "    gall:executionMode gall:PlanOnly ;",
            "    gall:agentMode gall:HandoffOnly ;",
            "    gall:maxParallelism 1 ;",
            '    gall:branchPattern "agent/{workItemId}" ;',
            '    gall:runtimeDirectory ".gall" ;',
            '    gall:receiptDirectory "receipts/gall" .',
            "",
            "gsh:work-item-census a gall:WorkItem ;",
            '    gall:workItemId "GGEN-DOGFOOD-CENSUS" ;',
            "    gall:issueType gall:Task ;",
            '    gall:summary "Maintain exact-tree self-host observation" ;',
            '    gall:objective "Keep the repository observation, independent verifier, generated authority maps, and Gall work package reproducible at every revision" ;',
            '    gall:rationale "The retrofit cannot govern ggen unless the complete current tree is admitted before any migration or actuation" ;',
            "    gall:belongsToProgram gsh:ggen ;",
            "    gall:belongsToCheckpoint gsh:checkpoint-exact-tree ;",
            "    gall:implementationOrder 0 ;",
            "    gall:priority gall:Highest ;",
            '    gall:component "self-host-observation" ;',
            '    gall:label "ggen-self-host" ;',
            '    gall:assigneeRole "ggen core implementation agent" ;',
            '    gall:reviewerRole "independent Gall verifier" ;',
            '    gall:approvalGate "green exact-tree witness, falsifier, receipt, and replay" ;',
            "    gall:protocolState gall:Draft ;",
            '    gall:requiredContext "generated/REPOSITORY_CENSUS.md" ;',
            '    gall:requiredContext "generated/AUTHORITY_MAP.md" ;',
            '    gall:allowedPath "self-host" ;',
            '    gall:allowedPath "packs/ggen-self-host-pack" ;',
            '    gall:allowedPath ".github/workflows/ggen-self-host.yml" ;',
            '    gall:allowedPath ".github/workflows/ggen-self-host-observer.yml" ;',
            '    gall:forbiddenPath ".git" ;',
            '    gall:forbiddenPath "target" ;',
            '    gall:mustDo "Observe Git objects exactly and independently verify every path, byte digest, count, and receipt binding" ;',
            '    gall:mustNotDo "Do not follow ambient symlink targets, copy counts by hand, or assert promotion standing from the observer" ;',
            '    gall:outOfScope "Repository remediation beyond observation and planning" ;',
            '    gall:acceptanceCriterion "Positive witnesses pass, tampered observations are refused, production ggen manufactures all self-host consequences, and second manufacture is byte-identical" ;',
            '    gall:definitionOfDone "The exact revision produces one independently verified observation and one deterministic Gall planning package" ;',
            '    gall:verificationCommand "python3 scripts/run_checkpoint.py" ;',
            '    gall:evidenceArtifact "observed/repository.json" ;',
            '    gall:evidenceArtifact "observed/observation-receipt.json" ;',
            '    gall:adversarialQuestion "Can a symlink, gitlink, untracked host file, or tampered digest enter the admitted observation without refusal?" .',
            "",
        ]
    )
    for order, item in enumerate(findings, start=1):
        lines.extend(gall_work_item_lines(item, order))
    return "\n".join(lines)


MODEL.read_bytes = git_semantic_bytes
MODEL.emit_turtle = emit_turtle
observe = MODEL.observe
write_outputs = MODEL.write_outputs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, help="repository root")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    root = (args.root or Path(__file__).resolve().parents[2]).resolve()
    return write_outputs(root, observe(root), args.check)


if __name__ == "__main__":
    raise SystemExit(main())
