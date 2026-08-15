#!/usr/bin/env python3
from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "verify_root_regeneration.py"
SPEC = importlib.util.spec_from_file_location("verify_root_regeneration", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class RootRegenerationTemplatedOutputTests(unittest.TestCase):
    def test_concrete_templated_output_has_declared_owner(self) -> None:
        outputs = {".specify/ci-workflows-rendered/{{ fileName }}": "github-actions-workflows"}
        self.assertEqual(
            MODULE.output_owner(outputs, ".specify/ci-workflows-rendered/ci.yml"),
            "github-actions-workflows",
        )

    def test_template_field_cannot_escape_its_path_segment(self) -> None:
        outputs = {".specify/ci-workflows-rendered/{{ fileName }}": "github-actions-workflows"}
        self.assertIsNone(
            MODULE.output_owner(outputs, ".specify/ci-workflows-rendered/nested/ci.yml")
        )

    def test_existing_concrete_output_satisfies_dynamic_declaration(self) -> None:
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            target = root / ".specify" / "ci-workflows-rendered" / "ci.yml"
            target.parent.mkdir(parents=True)
            target.write_text("name: CI\n", encoding="utf-8")
            self.assertEqual(
                MODULE.existing_output_matches(
                    root, ".specify/ci-workflows-rendered/{{ fileName }}"
                ),
                [".specify/ci-workflows-rendered/ci.yml"],
            )

    def test_unowned_sibling_remains_unowned(self) -> None:
        outputs = {".specify/ci-workflows-rendered/{{ fileName }}": "github-actions-workflows"}
        self.assertIsNone(MODULE.output_owner(outputs, ".specify/not-owned/ci.yml"))


if __name__ == "__main__":
    unittest.main()
