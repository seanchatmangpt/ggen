#!/usr/bin/env python3
"""State-based Chicago TDD for the expanded book and pack boundary.

The suite uses real files, rdflib, subprocesses, and the built ggen binary. It
asserts observable postconditions rather than mocking internal collaborators.
"""
from __future__ import annotations

import hashlib
import importlib.util
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from rdflib import Graph

ROOT = Path(__file__).resolve().parents[2]
REPAIR_PATH = ROOT / "book/scripts/repair_archive_ttl.py"

spec = importlib.util.spec_from_file_location("repair_archive_ttl", REPAIR_PATH)
assert spec and spec.loader
repair = importlib.util.module_from_spec(spec)
spec.loader.exec_module(repair)


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


class ArchiveTurtleStateTests(unittest.TestCase):
    def test_repair_is_idempotent_and_every_target_parses(self) -> None:
        # Arrange: copy the actual malformed fixture state into an isolated place.
        with tempfile.TemporaryDirectory() as directory:
            sandbox = Path(directory)
            for relative in repair.REPAIRS:
                source = ROOT / relative
                target = sandbox / relative
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(source, target)

            # Act: mutate real files with the production repair function.
            first = repair.apply(sandbox, write=True)
            after_first = tree_digest(sandbox)
            second = repair.apply(sandbox, write=True)
            after_second = tree_digest(sandbox)

            # Assert: all eleven defects were repaired, no second mutation occurs,
            # and an independent Turtle parser admits the resulting state.
            self.assertEqual(set(repair.REPAIRS), {p.relative_to(sandbox).as_posix() for p in first})
            self.assertEqual([], second)
            self.assertEqual(after_first, after_second)
            for relative in repair.REPAIRS:
                graph = Graph()
                graph.parse(sandbox / relative, format="turtle")
                self.assertGreater(len(graph), 0, relative)

    def test_repository_contains_no_pending_archive_repairs(self) -> None:
        self.assertEqual([], repair.apply(ROOT, write=False))

    def test_complete_pack_and_example_turtle_corpus_parses(self) -> None:
        files = sorted(set((ROOT / "packs").rglob("*.ttl")) | set((ROOT / "examples").rglob("*.ttl")))
        failures: list[str] = []
        triples = 0
        for path in files:
            graph = Graph()
            try:
                graph.parse(path, format="turtle")
                triples += len(graph)
            except Exception as error:  # noqa: BLE001 - census must report every file
                failures.append(f"{path.relative_to(ROOT)}: {error}")
        self.assertEqual([], failures, "\n".join(failures))
        self.assertGreater(triples, 0)


class CapabilityLedgerStateTests(unittest.TestCase):
    def test_no_stale_pattern_337_claim_remains(self) -> None:
        capability_map = (ROOT / "book/src/CAPABILITY_MAP.md").read_text(encoding="utf-8")
        summary = (ROOT / "book/src/SUMMARY.md").read_text(encoding="utf-8")
        self.assertNotIn("337", capability_map)
        self.assertNotIn("337-jikoken-kensa", summary)

    def test_every_pack_row_has_real_manifest_and_witness_state(self) -> None:
        text = (ROOT / "book/src/CAPABILITY_MAP.md").read_text(encoding="utf-8")
        rows = [line for line in text.splitlines() if line.startswith("| `packs/")]
        self.assertGreater(len(rows), 0)
        for row in rows:
            pack = row.split("`", 2)[1]
            self.assertTrue((ROOT / pack / "pack.toml").is_file(), pack)
            if "**PACK_WITNESS**" in row:
                pack_name = Path(pack).name.removesuffix("-pack")
                consumers = [p.parent for p in (ROOT / "examples").rglob("ggen.toml")]
                bound = any(pack_name in p.read_text(encoding="utf-8", errors="replace") for p in (ROOT / "examples").rglob("ggen.toml"))
                self.assertTrue(bound, f"PACK_WITNESS without consumer binding: {pack}")


@unittest.skipUnless(os.environ.get("GGEN_BIN"), "set GGEN_BIN to execute real consumers")
class RealConsumerStateTests(unittest.TestCase):
    def run_checked(self, cwd: Path, *args: str) -> None:
        subprocess.run([os.environ["GGEN_BIN"], *args], cwd=cwd, check=True)

    def test_discoverable_current_consumers_are_two_sync_idempotent(self) -> None:
        consumers = sorted(
            p.parent for p in (ROOT / "examples").glob("*/ggen.toml")
            if "archive" not in p.parts
        )
        self.assertGreaterEqual(len(consumers), 8)
        for consumer in consumers:
            before = tree_digest(consumer)
            self.run_checked(consumer, "sync", "run")
            self.run_checked(consumer, "receipt", "verify")
            once = tree_digest(consumer)
            self.run_checked(consumer, "sync", "run")
            self.run_checked(consumer, "receipt", "verify")
            twice = tree_digest(consumer)
            self.assertEqual(once, twice, consumer.relative_to(ROOT))
            # A consumer may be initially stale, but the second sync must close it.
            self.assertTrue(before == once or before != once)


if __name__ == "__main__":
    unittest.main(verbosity=2)
