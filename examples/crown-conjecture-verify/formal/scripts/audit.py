#!/usr/bin/env python3
from pathlib import Path
import re
import sys

root = Path(__file__).resolve().parents[1]
lean_files = sorted(root.rglob("*.lean"))
forbidden = [
    "so" + "rry",
    "ad" + "mit",
    "ax" + "iom",
    "op" + "aque",
    "unsafe",
    "implemented" + "_by",
    "run" + "_tac",
    "native" + "_decide",
]
findings = []
for path in lean_files:
    text = path.read_text()
    for token in forbidden:
        for match in re.finditer(
            rf"(?<![A-Za-z0-9_]){re.escape(token)}(?![A-Za-z0-9_])", text
        ):
            line = text.count("\n", 0, match.start()) + 1
            findings.append(
                f"{path.relative_to(root)}:{line}: forbidden token {token}"
            )

required = {
    "CrownFormal/Operational.lean": [
        "DiamondCertificate",
        "run_trace_eq",
        "Execution.run_eq",
        "Execution.of_run_eq",
        "transport_execution",
    ],
    "CrownFormal/Semantics.lean": [
        "SemanticBoundaryReceipt",
        "OperationalBoundaryReceipt",
        "AdmittedSemanticCrown",
        "traceSwapPreservesLawful",
    ],
    "CrownFormal/Crown.lean": [
        "quotient_crown",
        "crownLaw_iff_sound_and_reflecting",
    ],
    "CrownFormal/Falsifiers.lean": [
        "constant_map_not_crown",
        "no_false_diamond",
    ],
    "CrownFormal/Examples.lean": [
        "additiveAdmittedCrown",
        "additive_swapped_execution",
    ],
    "CrownFormal/Adequacy.lean": [
        "event_only_lawfulness_not_preserved",
        "additive_same_final_different_state_trace",
        "trajectory_observation_counterexample",
        "additive_trajectory_relation_refuses_swap",
        "CrownObservationAdmission",
        "relationEquivalence",
        "observationEq",
        "lawfulFactorization",
        "AdmittedBehavioralCrown",
        "timed_nontrivial_related",
        "timed_repaired_crown",
    ],
}
for relative, names in required.items():
    text = (root / relative).read_text()
    for name in names:
        if name not in text:
            findings.append(f"{relative}: missing required declaration {name}")

adequacy_path = root / "CrownFormal/Adequacy.lean"
adequacy = adequacy_path.read_text()

# Regression guard: the pre-refactor interface assumed the exact pairwise
# theorem it claimed to derive. The repaired interface must factor lawfulness
# pointwise and derive pairwise preservation from equal semantic coordinates.
anti_patterns = {
    r"lawfulIff\s*:\s*∀\s*\{left right\}": (
        "pairwise lawfulness equivalence reintroduced as a structure field"
    ),
    r"timestamps\s*:\s*List\s+Nat": (
        "positional timestamp lists reintroduced; timestamps must remain keyed "
        "by event identity"
    ),
}
for pattern, message in anti_patterns.items():
    match = re.search(pattern, adequacy)
    if match:
        line = adequacy.count("\n", 0, match.start()) + 1
        findings.append(f"CrownFormal/Adequacy.lean:{line}: {message}")

lawful_iff_match = re.search(
    r"theorem lawfulIff\b(?P<body>.*?)(?=\n/--|\nend CrownObservationAdmission)",
    adequacy,
    flags=re.DOTALL,
)
if lawful_iff_match is None:
    findings.append("CrownFormal/Adequacy.lean: missing derived lawfulIff theorem")
else:
    body = lawful_iff_match.group("body")
    for dependency in [
        "lawfulFactorization",
        "classify_eq_iff",
        "traceSound",
        "observationEq",
    ]:
        if dependency not in body:
            findings.append(
                "CrownFormal/Adequacy.lean: derived lawfulIff does not consume "
                f"{dependency}"
            )

if findings:
    print("\n".join(findings))
    sys.exit(1)
print(f"audit passed: {len(lean_files)} Lean files")