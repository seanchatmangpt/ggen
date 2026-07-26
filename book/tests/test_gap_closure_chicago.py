#!/usr/bin/env python3
"""State-based Chicago TDD for the expanded book and pack boundary.

The suite uses real files, rdflib, subprocesses, and the built ggen binary. It
asserts observable postconditions rather than mocking internal collaborators.
"""
from __future__ import annotations

import hashlib
import importlib.util
import os
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TTL_VALIDATOR_PATH = ROOT / "book/scripts/validate_ttl_corpus.py"
GAP_VALIDATOR_PATH = ROOT / "book/scripts/validate_gap_closure.py"


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


validate_ttl = load_module("validate_ttl_corpus", TTL_VALIDATOR_PATH)
validate_gaps = load_module("validate_gap_closure", GAP_VALIDATOR_PATH)


def tree_digest(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted(p for p in root.rglob("*") if p.is_file()):
        if any(part in {".git", "target", ".ggen-v2", "book"} for part in path.relative_to(root).parts):
            continue
        digest.update(path.relative_to(root).as_posix().encode())
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def receipt_files(root: Path) -> list[Path]:
    return sorted(
        path
        for path in root.rglob("*")
        if path.is_file()
        and ("receipt" in path.name.lower() or ".ggen-v2" in path.parts)
    )


class TurtleCorpusStateTests(unittest.TestCase):
    def test_active_turtle_is_valid_and_archive_quarantine_is_exact(self) -> None:
        result = validate_ttl.validate(ROOT)
        self.assertTrue(result.ok, result)
        self.assertEqual(11, result.quarantined)
        self.assertGreater(result.active_valid, 0)
        self.assertGreater(result.triples, 0)
        self.assertEqual((), result.unexpected_invalid)
        self.assertEqual((), result.stale_quarantine)
        self.assertEqual((), result.illegal_quarantine)
        self.assertEqual((), result.live_references)

    def test_quarantine_is_archive_only_and_reasoned(self) -> None:
        entries = validate_ttl.load_quarantine(ROOT)
        self.assertEqual(11, len(entries))
        for path, reason in entries.items():
            self.assertTrue(path.startswith("examples/archive/"), path)
            self.assertTrue((ROOT / path).is_file(), path)
            self.assertGreaterEqual(len(reason), 20, path)


class CapabilityLedgerStateTests(unittest.TestCase):
    def test_every_declared_gap_has_bound_executable_evidence(self) -> None:
        errors, counters = validate_gaps.validate()
        self.assertEqual([], errors)
        self.assertEqual(25, counters["obligations"])
        self.assertEqual(25, counters["declared_gaps"])
        self.assertEqual(counters["evidence_records"], counters["used_evidence"])
        self.assertEqual(0, counters["errors"])

    def test_no_stale_pattern_337_claim_remains(self) -> None:
        capability_map = (ROOT / "book/src/CAPABILITY_MAP.md").read_text(encoding="utf-8")
        summary = (ROOT / "book/src/SUMMARY.md").read_text(encoding="utf-8")
        self.assertNotIn("337", capability_map)
        self.assertNotIn("337-jikoken-kensa", summary)

    def test_every_pack_row_has_real_manifest_and_witness_state(self) -> None:
        text = (ROOT / "book/src/CAPABILITY_MAP.md").read_text(encoding="utf-8")
        rows = [line for line in text.splitlines() if line.startswith("| `packs/")]
        self.assertGreater(len(rows), 0)
        manifests = list((ROOT / "examples").rglob("ggen.toml"))
        for row in rows:
            pack = row.split("`", 2)[1]
            self.assertTrue((ROOT / pack / "pack.toml").is_file(), pack)
            if "**PACK_WITNESS**" in row:
                pack_dir = Path(pack).name
                bound = any(
                    pack_dir in manifest.read_text(encoding="utf-8", errors="replace")
                    for manifest in manifests
                    if "archive" not in manifest.parts
                )
                self.assertTrue(bound, f"PACK_WITNESS without live consumer binding: {pack}")


@unittest.skipUnless(os.environ.get("GGEN_BIN"), "set GGEN_BIN to execute real consumers")
class RealConsumerStateTests(unittest.TestCase):
    def run_checked(self, cwd: Path, *args: str) -> None:
        subprocess.run([os.environ["GGEN_BIN"], *args], cwd=cwd, check=True)

    def test_discoverable_current_consumers_are_two_sync_idempotent(self) -> None:
        consumers = sorted(
            p.parent
            for p in (ROOT / "examples").glob("*/ggen.toml")
            if "archive" not in p.parts
        )
        self.assertGreaterEqual(len(consumers), 8)
        for consumer in consumers:
            self.run_checked(consumer, "sync", "run")
            self.run_checked(consumer, "receipt", "verify")
            once = tree_digest(consumer)
            receipts_once = receipt_files(consumer)

            self.run_checked(consumer, "sync", "run")
            self.run_checked(consumer, "receipt", "verify")
            twice = tree_digest(consumer)
            receipts_twice = receipt_files(consumer)

            self.assertEqual(once, twice, consumer.relative_to(ROOT))
            self.assertGreater(len(receipts_once), 0, consumer.relative_to(ROOT))
            self.assertEqual(
                [p.relative_to(consumer) for p in receipts_once],
                [p.relative_to(consumer) for p in receipts_twice],
                consumer.relative_to(ROOT),
            )


if __name__ == "__main__":
    unittest.main(verbosity=2)
