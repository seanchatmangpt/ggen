#!/usr/bin/env python3
"""Static ownership and constitutional checks for the ggen-first Lean 4 to Rust pipeline."""

from __future__ import annotations

import json
import re
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PACK = ROOT / "packs/ggen-lean4-rust-pipeline-pack"
CONSUMER = ROOT / "examples/ggen-first-lean4-rust"
EXPECTED_GATES = [
    "010_required.rq",
    "020_single_valued.rq",
    "030_bounded_successor.rq",
    "040_proof_boundary.rq",
    "050_fortune5_boundary.rq",
]
EXPECTED_OUTPUTS = {
    "generated/lean/lean-toolchain",
    "generated/lean/lake-manifest.json",
    "generated/lean/lakefile.lean",
    "generated/lean/Fortune5Policy.lean",
    "generated/lean/RustLib.lean",
    "generated/lean/RustMain.lean",
    "generated/lean/RustEvidence.lean",
    "generated/lean/Lean4RustPipeline.lean",
    "generated/lean/Main.lean",
    "generated/PIPELINE.md",
}
AUTHORED_CONSUMER_FILES = {"ggen.toml", "ontology.ttl"}
GGEN_ROOT_STATE_FILES = {"ggen.lock"}
FORBIDDEN_LEAN = ("sorry", "axiom", "admit", "partial_fixpoint")
FORTUNE5_SPECIMEN_FIELDS = (
    'lr:r1P99Nanos 2',
    'lr:r1TickBudget 8',
    'lr:w1P99Nanos 1000000',
    'lr:c1P99Nanos 500000000',
    'lr:r1Measurement "rdtsc"',
    'lr:w1Measurement "otel-span"',
    'lr:c1Measurement "otel-span"',
    'lr:canaryRequired true',
    'lr:stagingValidationRequired true',
    'lr:autoRollbackRequired true',
    'lr:promotionReceiptRequired true',
    'lr:regionCount 3',
    'lr:regionQuorum 2',
    'lr:crossRegionReplicationRequired true',
    'lr:receiptSynchronizationRequired true',
    'lr:failoverRequired true',
    'lr:legalHoldRequired true',
    'lr:spiffeId "spiffe://ggen.chatmangpt.com/ns/ggen/sa/lean4-rust"',
    'lr:certificateRefreshSeconds 3600',
    'lr:keyRotationSeconds 86400',
    'lr:mtlsRequired true',
    'lr:awsKmsRequired true',
    'lr:azureKeyVaultRequired true',
    'lr:hashicorpVaultRequired true',
    'lr:networkPolicyRequired true',
    'lr:firewallPolicyRequired true',
    'lr:otelRequired true',
    'lr:sloAlertRequired true',
    'lr:guardAlertRequired true',
    'lr:receiptMismatchAlertRequired true',
    'lr:performanceDegradationAlertRequired true',
)


def refuse(condition: bool, code: str) -> None:
    if condition:
        print(code, file=sys.stderr)
        raise SystemExit(1)


def main() -> int:
    pack = tomllib.loads((PACK / "pack.toml").read_text(encoding="utf-8"))
    refuse(pack["pack"]["name"] != "ggen-lean4-rust-pipeline-pack", "PACK_IDENTITY_REFUSED")
    refuse(pack["pack"]["version"] != "0.2.0", "PACK_VERSION_REFUSED")

    gates = sorted(path.name for path in (PACK / "gates").glob("*.rq"))
    refuse(gates != EXPECTED_GATES, f"GATE_SET_REFUSED:{gates}")
    for gate_name in gates:
        text = (PACK / "gates" / gate_name).read_text(encoding="utf-8")
        refuse(not text.startswith("# MESSAGE:"), f"GATE_MESSAGE_MISSING:{gate_name}")
        refuse(
            "SELECT" not in text or "?violation" not in text,
            f"GATE_RESULT_CONTRACT_REFUSED:{gate_name}",
        )

    fortune5_gate = (PACK / "gates/050_fortune5_boundary.rq").read_text(encoding="utf-8")
    for refusal in (
        "fortune5-slo-target-refused",
        "fortune5-slo-measurement-refused",
        "fortune5-promotion-policy-refused",
        "fortune5-region-policy-refused",
        "fortune5-security-policy-refused",
        "fortune5-observability-policy-refused",
    ):
        refuse(refusal not in fortune5_gate, f"FORTUNE5_REFUSAL_MISSING:{refusal}")

    outputs: dict[str, str] = {}
    templates = sorted((PACK / "templates").glob("*.tmpl"))
    refuse(any(path.name.endswith(".rs.tmpl") for path in templates), "PARALLEL_RUST_TEMPLATE_REFUSED")
    for template in templates:
        text = template.read_text(encoding="utf-8")
        match = re.search(r"(?m)^to:\s*([^\n]+)$", text)
        refuse(match is None, f"TEMPLATE_OUTPUT_MISSING:{template.name}")
        output = match.group(1).strip().strip('"')
        refuse(output in outputs, f"DUPLICATE_OUTPUT_OWNER:{output}")
        outputs[output] = template.name
    refuse(set(outputs) != EXPECTED_OUTPUTS, f"OUTPUT_SET_REFUSED:{sorted(outputs)}")

    lake_manifest_template = (PACK / "templates/lake-manifest.json.tmpl").read_text(encoding="utf-8")
    _, _, lake_manifest_body = lake_manifest_template.split("---", 2)
    lake_manifest = json.loads(lake_manifest_body)
    refuse(
        lake_manifest
        != {
            "version": "1.1.0",
            "packagesDir": ".lake/packages",
            "packages": [],
            "name": "Lean4RustPipeline",
            "lakeDir": ".lake",
        },
        "LAKE_MANIFEST_CONTRACT_REFUSED",
    )

    lakefile_template = (PACK / "templates/lakefile.lean.tmpl").read_text(encoding="utf-8")
    for required in (
        "package Lean4RustPipeline",
        "lean_lib Lean4RustPipeline",
        "lean_exe emitRust",
        "root := `Main",
    ):
        refuse(required not in lakefile_template, f"LEAN_PROJECT_TOPOLOGY_MISSING:{required}")

    proof_template = (PACK / "templates/Fortune5Policy.lean.tmpl").read_text(encoding="utf-8")
    for forbidden in FORBIDDEN_LEAN:
        refuse(
            re.search(rf"\b{re.escape(forbidden)}\b", proof_template) is not None,
            f"LEAN_TRUST_EXPANSION_REFUSED:{forbidden}",
        )
    refuse(
        re.search(r"(?m)^\s*unsafe(?:\s|$)", proof_template) is not None,
        "LEAN_TRUST_EXPANSION_REFUSED:unsafe",
    )
    for required in (
        "theorem step_le",
        "theorem step_witness",
        "theorem step_fixed_point",
        "theorem fortune5_slo_ordering",
        "theorem fortune5_quorum_majority",
        "theorem fortune5_security_limits",
        "theorem fortune5_controls_enabled",
        "theorem canonical_promotion",
        "theorem slo_violation_rolls_back",
        "theorem quorum_loss_refused",
        "theorem security_expiry_refused",
        "theorem receipt_mismatch_refused",
        "theorem failover_unready_refused",
        "theorem identity_mismatch_refused",
        "theorem kms_unready_refused",
        "theorem replication_unready_refused",
        "theorem promotion_receipt_missing_refused",
        "theorem alerts_unready_refused",
        "structure Fortune5ProofReceipt",
        "structure ProofReceipt",
        "fortune5 := fortune5ProofReceipt",
        "def promotionDecision",
    ):
        refuse(required not in proof_template, f"LEAN_PROOF_PIPELINE_SURFACE_MISSING:{required}")

    rust_lib_template = (PACK / "templates/RustLib.lean.tmpl").read_text(encoding="utf-8")
    for required in (
        "R1_TICK_BUDGET",
        "observed_p99_ticks",
        "promotion_receipt_ready",
        "cross_region_replication_ready",
        "observed_spiffe_id",
        "aws_kms_ready",
        "alerts_ready",
        "regional_receipt_digest",
    ):
        refuse(required not in rust_lib_template, f"RUST_LIBRARY_SURFACE_MISSING:{required}")

    rust_main_template = (PACK / "templates/RustMain.lean.tmpl").read_text(encoding="utf-8")
    for required in (
        "ggen.lean4-rust/execution/v2",
        "region_receipt_blake3",
        "promotion_receipt_ready",
        "observed_spiffe_id",
        "azure_key_vault_ready",
        "alerts_ready",
        "promotion_decision_name",
    ):
        refuse(required not in rust_main_template, f"RUST_EXECUTION_SURFACE_MISSING:{required}")

    rust_evidence_template = (PACK / "templates/RustEvidence.lean.tmpl").read_text(encoding="utf-8")
    for required in (
        "LEAN_PROOF_DIGEST_MISMATCH",
        "REGION_RECEIPT_DIGEST_MISMATCH",
        "rdtsc+steady-clock-batched",
        "R1_TICK_BUDGET",
        "observed_p99_ticks",
    ):
        refuse(required not in rust_evidence_template, f"RUST_EVIDENCE_SURFACE_MISSING:{required}")
    refuse(rust_evidence_template.count("unsafe {") != 2, "RUST_RDTSC_UNSAFE_SURFACE_REFUSED")
    refuse(rust_evidence_template.count("SAFETY: LFENCE/RDTSC") != 2, "RUST_RDTSC_SAFETY_COMMENT_MISSING")

    emitter_template = (PACK / "templates/Lean4RustPipeline.lean.tmpl").read_text(encoding="utf-8")
    for required in (
        "import Fortune5Policy",
        "import RustLib",
        "import RustMain",
        "import RustEvidence",
        "def emitRust (receipt : ProofReceipt)",
        "def main : IO Unit := emitRust proofReceipt",
        'IO.FS.writeFile "../rust/src/lib.rs" rustLib',
        'IO.FS.writeFile "../rust/src/bin/verify_receipt.rs" rustReceiptVerifier',
        'IO.FS.writeFile "../rust/src/bin/slo_probe.rs" rustSloProbe',
    ):
        refuse(required not in emitter_template, f"LEAN_EMITTER_SURFACE_MISSING:{required}")

    runner_template = (PACK / "templates/Main.lean.tmpl").read_text(encoding="utf-8")
    refuse("import Lean4RustPipeline" not in runner_template, "LEAN_RUNNER_IMPORT_MISSING")
    refuse("Lean4Rust.main" not in runner_template, "LEAN_RUNNER_ROUTE_MISSING")

    root_files = {path.name for path in CONSUMER.iterdir() if path.is_file()}
    unexpected_root_files = root_files - AUTHORED_CONSUMER_FILES - GGEN_ROOT_STATE_FILES
    refuse(bool(unexpected_root_files), f"CONSUMER_ROOT_SURFACE_REFUSED:{sorted(unexpected_root_files)}")
    authored_files = root_files & AUTHORED_CONSUMER_FILES
    refuse(authored_files != AUTHORED_CONSUMER_FILES, f"CONSUMER_AUTHORED_SURFACE_REFUSED:{sorted(authored_files)}")

    manifest = tomllib.loads((CONSUMER / "ggen.toml").read_text(encoding="utf-8"))
    refuse(
        "ggen-lean4-rust-pipeline-pack" not in manifest["packs"],
        "CONSUMER_PACK_BINDING_MISSING",
    )
    ontology = (CONSUMER / "ontology.ttl").read_text(encoding="utf-8")
    for required in (
        'lr:leanToolchain "leanprover/lean4:v4.30.0"',
        'lr:proofTheorem "step_le"',
        'lr:witnessTheorem "step_witness"',
        'lr:fixedPointTheorem "step_fixed_point"',
        'lr:receiptAlgorithm "blake3"',
        *FORTUNE5_SPECIMEN_FIELDS,
    ):
        refuse(required not in ontology, f"SPECIMEN_CONTRACT_MISSING:{required}")

    print("ggen-first-lean4-rust-fortune5-static-contract: GREEN")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
