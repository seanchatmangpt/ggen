#!/usr/bin/env python3
"""v26.8.1 G-close-unknowns evidence for the `legacy` subsystem
(`.ggen/v26.8.1/subsystem-evidence-manifest.json`'s `legacy` record).

This is about the legacy-inventory-and-equivalence MACHINERY itself being
verified -- distinct from individual `LegacyCapability` dispositions (that
is a separate, per-capability concern). Two real, independent claims:

1. `legacy_archaeology.py`'s real output, `ontology/v26.8.1/legacy-capabilities.ttl`,
   is genuinely re-parseable RDF (rdflib) and SHACL-conforms against the
   real `ontology/v26.8.1/shapes.ttl` (pyshacl) -- a positive witness for
   the extraction half of the subsystem. This file only READS the ttl; it
   never writes to it (that file is owned by a concurrent agent).

2. `equivalence_runner.py`'s generic, data-driven `run_manifest` engine
   genuinely detects a fabricated mismatch -- a hand-built sabotage case
   manifest declaring `expected_disposition: PRESERVED` whose
   `current_adapter` deliberately prints different stdout than
   `legacy_adapter` -- and reports FAIL, not PASS. This proves the engine
   is not a rubber stamp. No mocks: `run_manifest` is called directly and
   really shells out to the two adapter commands via `subprocess` (see
   `run_adapter` inside `equivalence_runner.py`).

Positive witness: `test_legacy_capabilities_ttl_is_real_reparseable_and_shacl_conformant`.
Negative falsifier (name contains "detects", matching
`subsystem_evidence_manifest.py`'s `NEGATIVE_CONTROL_PATTERN`):
`test_equivalence_runner_detects_a_fabricated_stdout_mismatch`.

Run directly:
    python3 tools/v26.8.1/legacy_subsystem_verification_test.py
"""
from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TTL_PATH = ROOT / "ontology" / "v26.8.1" / "legacy-capabilities.ttl"
ONTOLOGY_PATH = ROOT / "ontology" / "v26.8.1" / "ontology.ttl"
SHAPES_PATH = ROOT / "ontology" / "v26.8.1" / "shapes.ttl"
EQUIVALENCE_RUNNER_PATH = ROOT / "tools" / "v26.8.1" / "equivalence_runner.py"


def _load_equivalence_runner():
    """Import equivalence_runner.py as a module without requiring
    tools/v26.8.1 to be an installed package -- real import of the real
    file, not a reimplementation."""
    spec = importlib.util.spec_from_file_location(
        "equivalence_runner_under_test", EQUIVALENCE_RUNNER_PATH
    )
    assert spec is not None and spec.loader is not None, (
        f"could not build an import spec for {EQUIVALENCE_RUNNER_PATH}"
    )
    module = importlib.util.module_from_spec(spec)
    # dataclasses needs the module registered in sys.modules to resolve
    # forward-referenced type hints during class creation.
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class LegacyCapabilitiesTtlIsRealTest(unittest.TestCase):
    """Positive witness: legacy_archaeology.py's real emitted output is
    genuinely re-parseable Turtle and SHACL-conforms."""

    def test_legacy_capabilities_ttl_is_real_reparseable_and_shacl_conformant(self):
        self.assertTrue(
            TTL_PATH.is_file(),
            f"expected legacy_archaeology.py's real emitted output at {TTL_PATH}",
        )
        self.assertTrue(ONTOLOGY_PATH.is_file(), f"expected ontology at {ONTOLOGY_PATH}")
        self.assertTrue(SHAPES_PATH.is_file(), f"expected shapes at {SHAPES_PATH}")

        import rdflib

        graph = rdflib.Graph()
        # A genuinely malformed/fabricated ttl would raise here -- this is
        # a real parse, not a regex sanity check.
        graph.parse(str(TTL_PATH), format="turtle")

        ggen_ns = "https://ggen.chatmangpt.com/ontology/v26.8.1#"
        legacy_capability_iri = rdflib.URIRef(ggen_ns + "LegacyCapability")
        capability_count = sum(
            1
            for _ in graph.subjects(
                predicate=rdflib.RDF.type, object=legacy_capability_iri
            )
        )
        self.assertGreater(
            capability_count,
            0,
            "expected at least one real ggen:LegacyCapability individual "
            f"in {TTL_PATH}, found none after a real rdflib parse",
        )

        import pyshacl

        # legacy-capabilities.ttl only instantiates ggen:Standing/etc
        # individuals (ggen:UNKNOWN, ggen:ALIVE, ...) -- their *class*
        # membership triples (`ggen:UNKNOWN a ggen:Standing`) live in the
        # imported vocabulary file, ontology.ttl (same pattern ggen.toml
        # uses to import both together as one graph). A fair SHACL
        # conformance check must load both, exactly as the live pipeline
        # would -- validating legacy-capabilities.ttl in isolation would
        # spuriously report every hasStanding triple as a class-constraint
        # violation, which is a test-harness bug, not a real ontology defect.
        data_graph = rdflib.Graph()
        data_graph.parse(str(ONTOLOGY_PATH), format="turtle")
        data_graph.parse(str(TTL_PATH), format="turtle")
        shapes_graph = rdflib.Graph()
        shapes_graph.parse(str(SHAPES_PATH), format="turtle")

        conforms, _results_graph, results_text = pyshacl.validate(
            data_graph=data_graph,
            shacl_graph=shapes_graph,
            inference="rdfs",
        )
        self.assertTrue(
            conforms,
            f"expected {TTL_PATH} (+ imported {ONTOLOGY_PATH}) to SHACL-conform "
            f"against {SHAPES_PATH}, real pyshacl validation report:\n{results_text}",
        )


class EquivalenceRunnerCatchesFabricatedMismatchTest(unittest.TestCase):
    """Negative falsifier: the generic equivalence engine genuinely fails a
    hand-built sabotage case rather than rubber-stamping PASS."""

    def test_equivalence_runner_detects_a_fabricated_stdout_mismatch(self):
        module = _load_equivalence_runner()

        sabotage_manifest = {
            "schema": "ggen.legacy-equivalence.case-manifest.v1",
            "cases": [
                {
                    "case_id": "sabotage-stdout-mismatch",
                    "title": "SABOTAGE: legacy and current adapters deliberately disagree",
                    "order": 1,
                    "legacy_adapter": "printf 'legacy-output\\n'",
                    "current_adapter": "printf 'DIFFERENT-current-output\\n'",
                    "success_inputs": [""],
                    "failure_inputs": [""],
                    "normalization_policy": "none",
                    "expected_disposition": "PRESERVED",
                    "observable_surfaces": ["stdout", "exit_code"],
                    "timeout_seconds": 5,
                    "recovery_action": "none",
                }
            ],
        }

        with tempfile.TemporaryDirectory(prefix="legacy-subsystem-verification-") as tmp:
            work_root = Path(tmp)
            report = module.run_manifest(sabotage_manifest, work_root)

        self.assertEqual(report["summary"]["total"], 1)
        self.assertEqual(
            report["summary"]["failed"],
            1,
            f"expected the fabricated stdout mismatch to be caught as a real "
            f"FAIL, got summary={report['summary']}, results={report['results']}",
        )
        self.assertEqual(report["results"][0]["case_id"], "sabotage-stdout-mismatch")
        self.assertEqual(
            report["results"][0]["status"],
            "FAIL",
            f"expected status FAIL for the deliberately mismatched adapters, "
            f"got: {report['results'][0]}",
        )

    def test_equivalence_runner_passes_a_genuinely_matching_case(self):
        """Control alongside the sabotage test above: the SAME engine, given
        adapters that genuinely agree, reports PASS -- proving the FAIL
        above is caused by the deliberate mismatch, not a broken/always-FAIL
        engine."""
        module = _load_equivalence_runner()

        matching_manifest = {
            "schema": "ggen.legacy-equivalence.case-manifest.v1",
            "cases": [
                {
                    "case_id": "control-stdout-match",
                    "title": "CONTROL: legacy and current adapters genuinely agree",
                    "order": 1,
                    "legacy_adapter": "printf 'same-output\\n'",
                    "current_adapter": "printf 'same-output\\n'",
                    "success_inputs": [""],
                    "failure_inputs": [""],
                    "normalization_policy": "none",
                    "expected_disposition": "PRESERVED",
                    "observable_surfaces": ["stdout", "exit_code"],
                    "timeout_seconds": 5,
                    "recovery_action": "none",
                }
            ],
        }

        with tempfile.TemporaryDirectory(prefix="legacy-subsystem-verification-") as tmp:
            work_root = Path(tmp)
            report = module.run_manifest(matching_manifest, work_root)

        self.assertEqual(
            report["summary"]["passed"],
            1,
            f"expected the genuinely matching adapters to PASS, got: {report['results']}",
        )


if __name__ == "__main__":
    sys.exit(unittest.main())
