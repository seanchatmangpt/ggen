#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path

import errc_router


class RouterTests(unittest.TestCase):
    def test_docs_only(self) -> None:
        report = errc_router.route(["docs/architecture/ci.md"])
        self.assertTrue(report.booleans["docs_deep"])
        self.assertFalse(report.booleans["core_deep"])

    def test_source_change(self) -> None:
        report = errc_router.route(["crates/ggen-engine/src/lib.rs"])
        self.assertTrue(report.booleans["core_deep"])
        self.assertTrue(report.booleans["integration_deep"])
        self.assertTrue(report.booleans["quality_deep"])

    def test_test_change(self) -> None:
        report = errc_router.route(["crates/ggen-engine/tests/receipt.rs"])
        self.assertTrue(report.booleans["integration_deep"])

    def test_workflow_only_is_not_product(self) -> None:
        report = errc_router.route([".github/workflows/random.yml"])
        self.assertTrue(report.booleans["ci_deep"])
        self.assertFalse(report.booleans["core_deep"])
        self.assertFalse(report.booleans["source_removal_deep"])

    def test_deep_lane_owned_change(self) -> None:
        report = errc_router.route(["scripts/ci/v26_8_1_source_removal.py"])
        self.assertTrue(report.booleans["source_removal_deep"])

    def test_source_removal_workflow_definition_is_ci_only(self) -> None:
        report = errc_router.route([".github/workflows/verify-v26-8-1-source-removal.yml"])
        self.assertTrue(report.booleans["ci_deep"])
        self.assertFalse(report.booleans["source_removal_deep"])

    def test_exclusion_precedence_for_ci_implementation(self) -> None:
        report = errc_router.route(["scripts/ci/errc_router.py"])
        self.assertEqual(
            [lane for lane, enabled in report.booleans.items() if enabled],
            ["ci_deep"],
        )

    def test_multi_lane_change(self) -> None:
        report = errc_router.route(["README.md", "Cargo.lock"])
        self.assertTrue(report.booleans["docs_deep"])
        self.assertTrue(report.booleans["core_deep"])
        self.assertTrue(report.booleans["security_deep"])

    def test_duplicate_paths_are_deduplicated_and_sorted(self) -> None:
        report = errc_router.route(["README.md", "./README.md", "Cargo.lock"])
        self.assertEqual(report.changed_files, ("Cargo.lock", "README.md"))

    def test_discovery_failure_is_typed(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            with self.assertRaisesRegex(RuntimeError, "REFUSED:CHANGED_FILE_DISCOVERY_FAILED"):
                errc_router.discover_changed_files("missing", "also-missing", Path(raw))

    def test_exact_github_boolean_outputs(self) -> None:
        report = errc_router.route(["docs/a.md"])
        with tempfile.TemporaryDirectory() as raw:
            output = Path(raw) / "out"
            errc_router.write_github_outputs(output, report)
            values = dict(line.split("=", 1) for line in output.read_text().splitlines())
        self.assertEqual(values["docs_deep"], "true")
        self.assertEqual(values["core_deep"], "false")
        self.assertEqual(json.loads(values["deep_matrix_json"]), {"include": [{"lane": "docs_deep"}]})
        self.assertEqual(json.loads(values["changed_files_json"]), ["docs/a.md"])

    def test_empty_routing_emits_fast_only_matrix(self) -> None:
        report = errc_router.route([])
        with tempfile.TemporaryDirectory() as raw:
            output = Path(raw) / "out"
            errc_router.write_github_outputs(output, report)
            values = dict(line.split("=", 1) for line in output.read_text().splitlines())
        self.assertEqual(json.loads(values["deep_matrix_json"]), {"include": [{"lane": "fast_only"}]})

    def test_real_two_commit_git_replay(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            repo = Path(raw)
            subprocess.run(["git", "init", "-q", str(repo)], check=True)
            subprocess.run(["git", "-C", str(repo), "config", "user.email", "ci@example.invalid"], check=True)
            subprocess.run(["git", "-C", str(repo), "config", "user.name", "CI"], check=True)
            (repo / "README.md").write_text("a\n")
            subprocess.run(["git", "-C", str(repo), "add", "README.md"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "base"], check=True)
            base = subprocess.check_output(["git", "-C", str(repo), "rev-parse", "HEAD"], text=True).strip()
            (repo / "Cargo.lock").write_text("# lock\n")
            subprocess.run(["git", "-C", str(repo), "add", "Cargo.lock"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "head"], check=True)
            head = subprocess.check_output(["git", "-C", str(repo), "rev-parse", "HEAD"], text=True).strip()
            self.assertEqual(errc_router.discover_changed_files(base, head, repo), ["Cargo.lock"])


if __name__ == "__main__":
    unittest.main(verbosity=2)
