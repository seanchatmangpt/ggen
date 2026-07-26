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
import sys
import tomllib
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TTL_VALIDATOR_PATH = ROOT / "book/scripts/validate_ttl_corpus.py"
GAP_VALIDATOR_PATH = ROOT / "book/scripts/validate_gap_closure.py"

TCPS_CONSUMER = Path("examples/tcps-generated")
PRE_SYNC_HOOKS: dict[Path, tuple[str, ...]] = {
    TCPS_CONSUMER: ("bash", "scripts/verify.sh"),
}
VOLATILE_OBSERVATION_PATHS: dict[Path, frozenset[Path]] = {
    TCPS_CONSUMER: frozenset(
        {
            Path("evidence/ontology.ttl"),
            Path("receipts/inspection-receipt.json"),
            Path("receipts/EVIDENCE_SNAPSHOT.md"),
        }
    ),
}


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


validate_ttl = load_module("validate_ttl_corpus", TTL_VALIDATOR_PATH)
validate_gaps = load_module("validate_gap_closure", GAP_VALIDATOR_PATH)


def tree_digest(root: Path, excluded: frozenset[Path] = frozenset()) -> str:
    digest = hashlib.sha256()
    for path in sorted(p for p in root.rglob("*") if p.is_file()):
        relative = path.relative_to(root)
        if relative in excluded:
            continue
        if any(part in {".git", "target", ".ggen-v2", "book"} for part in relative.parts):
            continue
        digest.update(relative.as_posix().encode())
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
    def test_active_turtle_is_valid_and_quarantine_is_exact(self) -> None:
        result = validate_ttl.validate(ROOT)
        self.assertTrue(result.ok, result)
        self.assertEqual(12, result.quarantined)
        self.assertEqual(11, result.archive_quarantined)
        self.assertEqual(1, result.negative_fixtures)
        self.assertGreater(result.active_valid, 0)
        self.assertGreater(result.triples, 0)
        self.assertEqual((), result.unexpected_invalid)
        self.assertEqual((), result.stale_quarantine)
        self.assertEqual((), result.illegal_quarantine)
        self.assertEqual((), result.live_references)

    def test_quarantine_separates_archive_state_from_executable_negative_fixture(self) -> None:
        entries = validate_ttl.load_quarantine(ROOT)
        self.assertEqual(12, len(entries))

        archive = [entry for entry in entries.values() if entry.kind == "archive"]
        negative = [entry for entry in entries.values() if entry.kind == "negative_fixture"]
        self.assertEqual(11, len(archive))
        self.assertEqual(1, len(negative))

        for entry in archive:
            self.assertTrue(entry.path.startswith("examples/archive/"), entry.path)
            self.assertTrue((ROOT / entry.path).is_file(), entry.path)
            self.assertGreaterEqual(len(entry.reason), 20, entry.path)
            self.assertEqual("", entry.evidence_path)
            self.assertEqual("", entry.evidence_marker)

        fixture = negative[0]
        self.assertEqual(
            "packs/dogfood-lifecycle-pack/fixtures/session-malformed.ttl",
            fixture.path,
        )
        self.assertTrue((ROOT / fixture.path).is_file())
        self.assertEqual(
            "book/tests/test_gap_closure_chicago.py",
            fixture.evidence_path,
        )
        self.assertEqual(
            "test_declared_malformed_turtle_fixture_is_rejected_by_real_graph_validator",
            fixture.evidence_marker,
        )


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

    def test_self_observing_tcps_evidence_pack_is_explicitly_unlocked(self) -> None:
        with (ROOT / TCPS_CONSUMER / "ggen.toml").open("rb") as stream:
            manifest = tomllib.load(stream)
        evidence = manifest["packs"]["tcps-evidence"]
        self.assertIs(
            evidence.get("lock"),
            False,
            "regenerated verification evidence must never be content-pinned",
        )


@unittest.skipUnless(os.environ.get("GGEN_BIN"), "set GGEN_BIN to execute real consumers")
class RealConsumerStateTests(unittest.TestCase):
    def run_checked(self, cwd: Path, *args: str) -> subprocess.CompletedProcess[str]:
        command = [os.environ["GGEN_BIN"], *args]
        completed = subprocess.run(
            command,
            cwd=cwd,
            text=True,
            capture_output=True,
            check=False,
        )
        if completed.returncode != 0:
            relative = cwd.relative_to(ROOT)
            self.fail(
                "consumer command refused\n"
                f"consumer={relative}\n"
                f"command={' '.join(command)}\n"
                f"exit={completed.returncode}\n"
                f"stdout:\n{completed.stdout}\n"
                f"stderr:\n{completed.stderr}"
            )
        return completed

    def run_pre_sync_hook(self, consumer: Path) -> None:
        relative = consumer.relative_to(ROOT)
        hook = PRE_SYNC_HOOKS.get(relative)
        if hook is None:
            return
        completed = subprocess.run(
            list(hook),
            cwd=consumer,
            text=True,
            capture_output=True,
            check=False,
        )
        if completed.returncode != 0:
            self.fail(
                "consumer pre-sync hook refused\n"
                f"consumer={relative}\n"
                f"command={' '.join(hook)}\n"
                f"exit={completed.returncode}\n"
                f"stdout:\n{completed.stdout}\n"
                f"stderr:\n{completed.stderr}"
            )
        print(
            f"CONSUMER_PRE_SYNC_OK consumer={relative} command={' '.join(hook)}",
            flush=True,
        )

    def test_declared_malformed_turtle_fixture_is_rejected_by_real_graph_validator(self) -> None:
        pack = ROOT / "packs/dogfood-lifecycle-pack"
        fixture = pack / "fixtures/session-malformed.ttl"
        completed = subprocess.run(
            [
                os.environ["GGEN_BIN"],
                "graph",
                "validate",
                "--files",
                str(fixture),
            ],
            cwd=pack,
            text=True,
            capture_output=True,
            check=False,
        )
        evidence = completed.stdout + completed.stderr
        self.assertNotEqual(0, completed.returncode, evidence)
        self.assertIn("session-malformed.ttl", evidence)

    def test_discoverable_current_consumers_are_two_sync_idempotent(self) -> None:
        consumers = sorted(
            p.parent
            for p in (ROOT / "examples").glob("*/ggen.toml")
            if "archive" not in p.parts
        )
        self.assertGreaterEqual(len(consumers), 8)
        for consumer in consumers:
            relative = consumer.relative_to(ROOT)
            excluded = VOLATILE_OBSERVATION_PATHS.get(relative, frozenset())
            with self.subTest(consumer=relative.as_posix()):
                print(f"CONSUMER_REPLAY_START consumer={relative}", flush=True)
                self.run_pre_sync_hook(consumer)
                self.run_checked(consumer, "sync", "run")
                self.run_checked(consumer, "receipt", "verify")
                once = tree_digest(consumer, excluded)
                receipts_once = receipt_files(consumer)

                self.run_pre_sync_hook(consumer)
                self.run_checked(consumer, "sync", "run")
                self.run_checked(consumer, "receipt", "verify")
                twice = tree_digest(consumer, excluded)
                receipts_twice = receipt_files(consumer)

                self.assertEqual(once, twice, relative)
                self.assertGreater(len(receipts_once), 0, relative)
                self.assertEqual(
                    [p.relative_to(consumer) for p in receipts_once],
                    [p.relative_to(consumer) for p in receipts_twice],
                    relative,
                )
                print(
                    "CONSUMER_REPLAY_OK "
                    f"consumer={relative} digest={twice} receipts={len(receipts_twice)} "
                    f"excluded_observations={len(excluded)}",
                    flush=True,
                )


if __name__ == "__main__":
    unittest.main(verbosity=2)
