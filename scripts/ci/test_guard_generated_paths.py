#!/usr/bin/env python3
"""Tests for guard_generated_paths (Chicago: real fixture trees in a tempdir,
assert on exit codes and emitted paths). Run: python3 -m pytest or directly."""

from __future__ import annotations

import importlib.util
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_SCRIPT = Path(__file__).resolve().parent / "guard_generated_paths.py"
_spec = importlib.util.spec_from_file_location("guard_generated_paths", _SCRIPT)
if _spec is None or _spec.loader is None:
    raise RuntimeError(f"cannot load guard script at {_SCRIPT}")
mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(mod)


def make_fixture(with_include: bool) -> Path:
    root = Path(tempfile.mkdtemp(prefix="guard-gen-"))
    crate = root / "crates" / "mycrate"
    (crate / "src" / "generated").mkdir(parents=True)
    (crate / "src" / "generated" / "mod.rs").write_text("// gen\n")
    (crate / "Cargo.toml").write_text(
        '[package]\nname = "mycrate"\n'
        + ('include = ["Cargo.toml", "src/**/*"]\n' if with_include else "")
    )
    return root


class TestGuardGeneratedPaths(unittest.TestCase):
    def setUp(self):
        self._temps = []

    def temp_root(self, with_include: bool) -> Path:
        root = make_fixture(with_include)
        self._temps.append(root)
        return root

    def tearDown(self):
        for t in self._temps:
            shutil.rmtree(t, ignore_errors=True)

    def test_sanctioned_and_covered_passes(self):
        root = self.temp_root(with_include=True)
        self.assertEqual(mod.check(root), [])

    def test_src_generated_without_include_is_violation_naming_path(self):
        root = self.temp_root(with_include=False)
        violations = mod.check(root)
        self.assertEqual(len(violations), 1)
        self.assertIn("crates/mycrate/src/generated", violations[0])
        self.assertIn("include", violations[0])
        self.assertIn("cargo publish will drop it", violations[0])

    def test_tests_generated_is_violation(self):
        root = self.temp_root(with_include=True)
        tdir = root / "crates" / "mycrate" / "tests" / "generated"
        tdir.mkdir(parents=True)
        (tdir / "fixture.rs").write_text("// x\n")
        violations = mod.check(root)
        self.assertEqual(len(violations), 1)
        self.assertIn("tests/generated", violations[0])

    def test_removing_include_flips_covered_to_violation(self):
        root = self.temp_root(with_include=True)
        self.assertEqual(mod.check(root), [])
        (root / "crates" / "mycrate" / "Cargo.toml").write_text(
            '[package]\nname = "mycrate"\n'
        )
        violations = mod.check(root)
        self.assertEqual(len(violations), 1)
        self.assertIn("no [package] include list", violations[0])

    def test_real_tree_exits_zero(self):
        rc = mod.main()
        self.assertEqual(rc, 0, "real ggen tree must be clean: every src/generated "
                                "covered by include, others in ALLOWED")


if __name__ == "__main__":
    unittest.main(verbosity=2)
    sys.exit(0)
