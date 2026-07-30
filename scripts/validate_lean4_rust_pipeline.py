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
]
EXPECTED_OUTPUTS = {
    "generated/lean/lean-toolchain",
    "generated/lean/lake-manifest.json",
    "generated/lean/lakefile.lean",
    "generated/lean/Lean4RustPipeline.lean",
    "generated/lean/Main.lean",
    "generated/PIPELINE.md",
}
FORBIDDEN_LEAN = ("sorry", "axiom", "admit", "unsafe", "partial_fixpoint")


def refuse(condition: bool, code: str) -> None:
    if condition:
        print(code, file=sys.stderr)
        raise SystemExit(1)


def main() -> int:
    pack = tomllib.loads((PACK / "pack.toml").read_text(encoding="utf-8"))
    refuse(
        pack["pack"]["name"] != "ggen-lean4-rust-pipeline-pack",
        "PACK_IDENTITY_REFUSED",
    )

    gates = sorted(path.name for path in (PACK / "gates").glob("*.rq"))
    refuse(gates != EXPECTED_GATES, f"GATE_SET_REFUSED:{gates}")
    for gate_name in gates:
        text = (PACK / "gates" / gate_name).read_text(encoding="utf-8")
        refuse(not text.startswith("# MESSAGE:"), f"GATE_MESSAGE_MISSING:{gate_name}")
        refuse(
            "SELECT" not in text or "?violation" not in text,
            f"GATE_RESULT_CONTRACT_REFUSED:{gate_name}",
        )

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

    proof_template = (PACK / "templates/Lean4RustPipeline.lean.tmpl").read_text(encoding="utf-8")
    for forbidden in FORBIDDEN_LEAN:
        refuse(
            re.search(rf"\b{re.escape(forbidden)}\b", proof_template) is not None,
            f"LEAN_TRUST_EXPANSION_REFUSED:{forbidden}",
        )
    for required in (
        "theorem step_le",
        "theorem step_witness",
        "theorem step_fixed_point",
        "structure ProofReceipt",
        "boundProof := step_le",
        "witnessProof := step_witness",
        "fixedPointProof := step_fixed_point",
        "def emitRust (receipt : ProofReceipt)",
        "def main : IO Unit := emitRust proofReceipt",
        "IO.FS.writeFile \"../rust/src/lib.rs\" rustLib",
        "to_hex().to_string()",
        "let mut receipt = String::from",
        "println!(\\\"{receipt}\\\")",
        "lean_proof_blake3",
    ):
        refuse(required not in proof_template, f"LEAN_PROOF_PIPELINE_SURFACE_MISSING:{required}")

    runner_template = (PACK / "templates/Main.lean.tmpl").read_text(encoding="utf-8")
    refuse("import Lean4RustPipeline" not in runner_template, "LEAN_RUNNER_IMPORT_MISSING")
    refuse("Lean4Rust.main" not in runner_template, "LEAN_RUNNER_ROUTE_MISSING")

    consumer_files = sorted(path.name for path in CONSUMER.iterdir() if path.is_file())
    refuse(
        consumer_files != ["ggen.toml", "ontology.ttl"],
        f"CONSUMER_AUTHORED_SURFACE_REFUSED:{consumer_files}",
    )
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
    ):
        refuse(required not in ontology, f"SPECIMEN_CONTRACT_MISSING:{required}")

    print("ggen-first-lean4-rust-static-contract: GREEN")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
