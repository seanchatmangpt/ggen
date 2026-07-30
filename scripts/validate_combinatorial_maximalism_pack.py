#!/usr/bin/env python3
"""Static ownership and constitutional checks for the CMD pack source surface."""

from __future__ import annotations

import re
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PACK = ROOT / "packs/ggen-combinatorial-maximalism-pack"
CONSUMER = ROOT / "examples/combinatorial-maximalism"
EXPECTED_GATES = [
    "010_required.rq",
    "020_single_valued.rq",
    "030_graph_closure.rq",
    "040_candidate_totality.rq",
    "050_reversible_construction.rq",
    "060_state_authority.rq",
    "070_broker_receipt.rq",
    "080_hook_intent_only.rq",
    "090_coverage_bounds.rq",
    "100_unique_identity.rq",
    "110_proof_closure.rq",
    "120_actuation_closure.rq",
]
EXPECTED_OUTPUTS = {
    "generated/cmd-cell/Cargo.toml",
    "generated/cmd-cell/src/lib.rs",
    "generated/cmd-cell/src/main.rs",
    "generated/cmd-cell/tests/cmd_e2e.rs",
    "generated/cmd-plan.json",
    "generated/CMD_REPORT.md",
}
FORBIDDEN = ("TODO", "FIXME", "todo!", "unimplemented!", "mockall", "#[automock]")


def refuse(condition: bool, code: str) -> None:
    if condition:
        print(code, file=sys.stderr)
        raise SystemExit(1)


def main() -> int:
    pack = tomllib.loads((PACK / "pack.toml").read_text(encoding="utf-8"))
    refuse(pack["pack"]["name"] != "ggen-combinatorial-maximalism-pack", "PACK_IDENTITY_REFUSED")

    gates = sorted(path.name for path in (PACK / "gates").glob("*.rq"))
    refuse(gates != EXPECTED_GATES, f"GATE_SET_REFUSED:{gates}")
    for gate_name in gates:
        text = (PACK / "gates" / gate_name).read_text(encoding="utf-8")
        refuse(not text.startswith("# MESSAGE:"), f"GATE_MESSAGE_MISSING:{gate_name}")
        refuse("SELECT" not in text or "?violation" not in text, f"GATE_RESULT_CONTRACT_REFUSED:{gate_name}")

    outputs: dict[str, str] = {}
    for template in sorted((PACK / "templates").glob("*.tmpl")):
        text = template.read_text(encoding="utf-8")
        match = re.search(r"(?m)^to:\s*([^\n]+)$", text)
        refuse(match is None, f"TEMPLATE_OUTPUT_MISSING:{template.name}")
        output = match.group(1).strip().strip('"')
        refuse(output in outputs, f"DUPLICATE_OUTPUT_OWNER:{output}:{outputs.get(output)}:{template.name}")
        outputs[output] = template.name
        for forbidden in FORBIDDEN:
            refuse(forbidden in text, f"FORBIDDEN_TEMPLATE_SURFACE:{template.name}:{forbidden}")
        refuse("COALESCE(" in text, f"NON_PORTABLE_SPARQL_EXPRESSION_REFUSED:{template.name}")
    refuse(set(outputs) != EXPECTED_OUTPUTS, f"OUTPUT_SET_REFUSED:{sorted(outputs)}")

    cargo_template = (PACK / "templates/Cargo.toml.tmpl").read_text(encoding="utf-8")
    refuse("\n[workspace]\n" not in cargo_template, "GENERATED_WORKSPACE_ISOLATION_MISSING")

    lib_template = (PACK / "templates/lib.rs.tmpl").read_text(encoding="utf-8")
    refuse("sh_after: rustfmt" in lib_template, "UNPINNED_LIBRARY_FORMATTER_REFUSED")
    refuse(
        "resource_cost: {{ option.resource_cost }}f64," not in lib_template,
        "RDF_NUMERIC_LITERAL_TYPING_MISSING",
    )
    refuse(
        'candidate.standing == "VERIFIED"' not in lib_template
        or "Standing::Verified" not in lib_template
        or "Standing::Candidate" not in lib_template,
        "RDF_STANDING_MAPPING_MISSING",
    )
    refuse(
        "candidate.standing | pascal_case" in lib_template,
        "IMPLICIT_STANDING_CASE_CONVERSION_REFUSED",
    )
    refuse(
        "#[allow(dead_code)]\nenum CoverageMode" not in lib_template,
        "ONE_ACTIVE_COVERAGE_SPECIALIZATION_FENCE_MISSING",
    )
    for required_layout in (
        "};{% endfor %}\n\n{% for d in design %}const DESIGN",
        "};{% endfor %}\n\n#[derive(Debug)]",
        'return refuse(\n            "INSUFFICIENT_DIMENSIONS"',
        "let expected = options_by_dimension\n                .values()",
        "transaction.join(safe_relative(&envelope.receipt.output_relative_path)?)",
    ):
        refuse(required_layout not in lib_template, "RUSTFMT_STABLE_LIBRARY_LAYOUT_MISSING")

    main_template = (PACK / "templates/main.rs.tmpl").read_text(encoding="utf-8")
    refuse(
        "::{Broker, CmdError, validate_design};" not in main_template,
        "RUSTFMT_STABLE_IMPORT_ORDER_MISSING",
    )
    refuse("sh_after: rustfmt" in main_template, "UNPINNED_MAIN_FORMATTER_REFUSED")
    refuse(
        "::{Broker, CmdError, validate_design};{% endfor %}\n\nfn workspace" not in main_template,
        "RUSTFMT_STABLE_IMPORT_SPACING_MISSING",
    )

    ontology = (PACK / "ontology.ttl").read_text(encoding="utf-8")
    for term in ("cmd:DesignSpace", "cmd:Candidate", "cmd:Broker", "cmd:ActuationContract", "cmd:Receipt"):
        refuse(term not in ontology, f"CONSTITUTIONAL_TERM_MISSING:{term}")
    for public_mapping in ("rdfs:subClassOf", "rdfs:subPropertyOf", "prov:", "odrl:", "dcat:", "skos:"):
        refuse(public_mapping not in ontology, f"PUBLIC_MAPPING_MISSING:{public_mapping}")

    consumer_files = sorted(path.name for path in CONSUMER.iterdir() if path.is_file())
    refuse(consumer_files != ["ggen.toml", "ontology.ttl"], f"CONSUMER_AUTHORED_SURFACE_REFUSED:{consumer_files}")
    consumer = (CONSUMER / "ontology.ttl").read_text(encoding="utf-8")
    refuse(consumer.count("a cmd:Dimension") != 2, "SPECIMEN_DIMENSION_COUNT_REFUSED")
    refuse(consumer.count("a cmd:Candidate") != 4, "SPECIMEN_CANDIDATE_COUNT_REFUSED")
    refuse('cmd:coverageMode "exhaustive"' not in consumer, "SPECIMEN_COVERAGE_REFUSED")
    refuse('cmd:receiptAlgorithm "blake3"' not in consumer, "SPECIMEN_RECEIPT_REFUSED")
    refuse('cmd:replayMode "exact-output"' not in consumer, "SPECIMEN_REPLAY_REFUSED")
    refuse('cmd:directlyActuates' in consumer, "HOOK_ACTUATION_COLLAPSE_REFUSED")

    print("combinatorial-maximalism-pack-static-contract: GREEN")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
