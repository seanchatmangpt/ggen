from __future__ import annotations

import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "verify_root_regeneration.py"
SPEC = importlib.util.spec_from_file_location("verify_root_regeneration", SCRIPT)
assert SPEC and SPEC.loader
verify_root_regeneration = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(verify_root_regeneration)


def write(path: Path, content: str = "fixture\n") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def git(root: Path, *args: str) -> None:
    subprocess.run(["git", *args], cwd=root, check=True, capture_output=True, text=True)


def init_repo(root: Path, manifest: str) -> None:
    git(root, "init")
    git(root, "config", "user.name", "ggen-dogfood-test")
    git(root, "config", "user.email", "ggen-dogfood-test@example.invalid")
    write(root / "ggen.toml", manifest)
    git(root, "add", "ggen.toml")
    git(root, "commit", "-m", "baseline")


MANIFEST = """
[generation]

[[generation.rules]]
name = "literal"
output_file = "generated/ONE.md"

[[generation.rules]]
name = "crate-manifest"
output_file = "crates/{{ crate_name }}/Cargo.toml"
"""


class RootRegenerationOwnershipTests(unittest.TestCase):
    def test_templated_contract_matches_concrete_path_without_becoming_literal_missing_file(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            init_repo(root, MANIFEST)
            write(root / "generated" / "ONE.md")
            write(root / "crates" / "alpha" / "Cargo.toml")

            result = verify_root_regeneration.verify(root)

            self.assertTrue(result["passed"], result)
            self.assertEqual(result["missing_outputs"], [])
            self.assertEqual(result["declared_literal_output_count"], 1)
            self.assertEqual(result["declared_pattern_output_count"], 1)
            self.assertEqual(
                result["generated_output_owners"]["crates/alpha/Cargo.toml"],
                "crate-manifest",
            )

    def test_missing_literal_output_remains_load_bearing(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            init_repo(root, MANIFEST)
            write(root / "crates" / "alpha" / "Cargo.toml")

            result = verify_root_regeneration.verify(root)

            self.assertFalse(result["passed"])
            self.assertEqual(result["missing_outputs"], ["generated/ONE.md"])

    def test_unowned_changed_path_is_refused(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            init_repo(root, MANIFEST)
            write(root / "generated" / "ONE.md")
            write(root / "notes" / "ambient.txt")

            result = verify_root_regeneration.verify(root)

            self.assertFalse(result["passed"])
            self.assertIn("notes/ambient.txt", result["unauthorized_paths"])

    def test_template_substitution_cannot_cross_path_segment(self) -> None:
        matcher, literal = verify_root_regeneration._compile_output_matcher(
            "crates/{{ crate_name }}/Cargo.toml"
        )
        self.assertFalse(literal)
        self.assertIsNotNone(matcher.fullmatch("crates/alpha/Cargo.toml"))
        self.assertIsNone(matcher.fullmatch("crates/team/alpha/Cargo.toml"))

    def test_snapshot_covers_concrete_literal_and_pattern_outputs(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            init_repo(root, MANIFEST)
            write(root / "generated" / "ONE.md", "one\n")
            write(root / "crates" / "alpha" / "Cargo.toml", "alpha\n")

            snapshot = verify_root_regeneration.consequence_snapshot(root)

            self.assertEqual(
                sorted(snapshot),
                ["crates/alpha/Cargo.toml", "generated/ONE.md"],
            )


if __name__ == "__main__":
    unittest.main(verbosity=2)
