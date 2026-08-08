#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import guard_pack_standard


SCRIPT = Path(__file__).with_name("guard_pack_standard.py")


def write(path: Path, content: str = "fixture\n") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def make_pack(root: Path, name: str = "demo-pack") -> Path:
    pack = root / "packs" / name
    write(pack / "pack.toml", f'[pack]\nname = "{name}"\nversion = "1.0.0"\n')
    write(pack / "examples" / "consumer.ttl", "@prefix ex: <urn:example:> .\nex:a ex:b ex:c .\n")
    write(
        pack / "playground" / "ggen.toml",
        f'[project]\nname = "{name}-playground"\nversion = "0.1.0"\n\n[packs]\n{name} = {{ path = ".." }}\n',
    )
    write(pack / "playground" / "ontology.ttl", "@prefix ex: <urn:playground:> .\nex:a ex:b ex:c .\n")
    write(pack / "docs" / "tutorial.md", "# Tutorial\n\nRun the pack.\n")
    write(pack / "docs" / "how-to-use.md", "# How to use\n\nApply the pack.\n")
    write(pack / "docs" / "reference.md", "# Reference\n\nPack contract.\n")
    write(pack / "docs" / "explanation.md", "# Explanation\n\nWhy the pack exists.\n")
    return pack


def invoke(root: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), *args],
        check=False,
        capture_output=True,
        text=True,
    )


class PackCreateStandardGuardTests(unittest.TestCase):
    def test_complete_pack_is_alive(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            make_pack(root)
            result = invoke(root)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("ALIVE:PACK_CREATE_STANDARD:1 pack(s):demo-pack", result.stdout)

    def test_missing_real_example_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            (pack / "examples" / "consumer.ttl").unlink()
            write(pack / "examples" / "README.md", "# Not an example\n")
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-EXAMPLE:demo-pack", result.stderr)

    def test_missing_playground_file_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            (pack / "playground" / "ontology.ttl").unlink()
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-PLAYGROUND:demo-pack", result.stderr)
            self.assertIn("playground/ontology.ttl", result.stderr)

    def test_playground_without_pack_dependency_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            write(pack / "playground" / "ggen.toml", '[project]\nname = "orphan"\nversion = "0.1.0"\n')
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-PLAYGROUND:demo-pack", result.stderr)
            self.assertIn("must declare at least one [packs] dependency", result.stderr)

    def test_each_diataxis_quadrant_is_load_bearing(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            (pack / "docs" / "reference.md").unlink()
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-DIATAXIS:demo-pack", result.stderr)
            self.assertIn("quadrant=reference", result.stderr)

    def test_pack_selection_supports_one_commit_per_pack_verification(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            make_pack(root, "good-pack")
            bad = make_pack(root, "bad-pack")
            (bad / "docs" / "tutorial.md").unlink()

            selected = invoke(root, "--pack", "good-pack")
            self.assertEqual(selected.returncode, 0, selected.stderr)
            self.assertIn("good-pack", selected.stdout)
            self.assertNotIn("bad-pack", selected.stdout)

            all_packs = invoke(root)
            self.assertNotEqual(all_packs.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-DIATAXIS:bad-pack", all_packs.stderr)

    def test_empty_inventory_refuses_instead_of_vacuous_green(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-INVENTORY:<repository>", result.stderr)

    def test_missing_selected_pack_is_typed(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            make_pack(root)
            _, violations = guard_pack_standard.audit(root, ["does-not-exist"])
            self.assertEqual(len(violations), 1)
            self.assertEqual(violations[0].code, "PACK-CREATE-STANDARD-SELECTION")


if __name__ == "__main__":
    unittest.main(verbosity=2)
