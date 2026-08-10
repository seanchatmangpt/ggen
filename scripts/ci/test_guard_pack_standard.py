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
    write(
        pack / "pack.toml",
        f'[pack]\nname = "{name}"\nversion = "1.0.0"\ndescription = "fixture pack"\n',
    )
    write(pack / "ontology.ttl", "@prefix ex: <urn:example:> .\nex:a ex:b ex:c .\n")
    write(
        pack / "templates" / "example.tmpl",
        '---\nto: "output/example.txt"\n---\nfixture\n',
    )
    return pack


def invoke(root: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), *args],
        check=False,
        capture_output=True,
        text=True,
    )


def git(root: Path, *args: str) -> None:
    subprocess.run(["git", *args], cwd=root, check=True, capture_output=True, text=True)


def init_repo(root: Path) -> None:
    git(root, "init")
    git(root, "config", "user.name", "ggen-ci-test")
    git(root, "config", "user.email", "ggen-ci-test@example.invalid")
    write(root / "README.md", "baseline\n")
    git(root, "add", "README.md")
    git(root, "commit", "-m", "baseline")


class PackCreateStandardGuardTests(unittest.TestCase):
    def test_complete_canonical_pack_is_alive(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            make_pack(root)
            result = invoke(root)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("ALIVE:PACK_CREATE_STANDARD:1 pack(s):demo-pack", result.stdout)

    def test_missing_ontology_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            (pack / "ontology.ttl").unlink()
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-ONTOLOGY:demo-pack", result.stderr)

    def test_missing_template_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            (pack / "templates" / "example.tmpl").unlink()
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-TEMPLATE:demo-pack", result.stderr)

    def test_manifest_identity_is_load_bearing(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            pack = make_pack(root)
            write(
                pack / "pack.toml",
                '[pack]\nname = "other-pack"\nversion = "1.0.0"\ndescription = "fixture"\n',
            )
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-MANIFEST:demo-pack", result.stderr)
            self.assertIn("must match directory", result.stderr)

    def test_pack_selection_is_independent(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            make_pack(root, "good-pack")
            bad = make_pack(root, "bad-pack")
            (bad / "ontology.ttl").unlink()

            selected = invoke(root, "--pack", "good-pack")
            self.assertEqual(selected.returncode, 0, selected.stderr)
            self.assertIn("good-pack", selected.stdout)
            self.assertNotIn("bad-pack", selected.stdout)

            all_packs = invoke(root)
            self.assertNotEqual(all_packs.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-ONTOLOGY:bad-pack", all_packs.stderr)

    def test_changed_scope_ignores_unrelated_legacy_topology(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            init_repo(root)
            write(root / "README.md", "changed but not a pack\n")
            result = invoke(root, "--changed-since", "HEAD")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("ALIVE:PACK_CREATE_STANDARD:0 changed(s):", result.stdout)

    def test_changed_scope_admits_new_pack_and_refuses_broken_new_pack(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            init_repo(root)
            pack = make_pack(root, "new-pack")

            good = invoke(root, "--changed-since", "HEAD")
            self.assertEqual(good.returncode, 0, good.stderr)
            self.assertIn("new-pack", good.stdout)

            (pack / "templates" / "example.tmpl").unlink()
            bad = invoke(root, "--changed-since", "HEAD")
            self.assertNotEqual(bad.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-TEMPLATE:new-pack", bad.stderr)

    def test_empty_inventory_refuses_in_full_audit_mode(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            result = invoke(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("REFUSED:PACK-CREATE-STANDARD-INVENTORY:<repository>", result.stderr)

    def test_missing_explicit_pack_is_typed(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            make_pack(root)
            _, violations = guard_pack_standard.audit(root, ["does-not-exist"])
            self.assertEqual(len(violations), 1)
            self.assertEqual(violations[0].code, "PACK-CREATE-STANDARD-SELECTION")


if __name__ == "__main__":
    unittest.main(verbosity=2)
