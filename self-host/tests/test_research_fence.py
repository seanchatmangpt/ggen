from __future__ import annotations

import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "observe_self_host.py"
SPEC = importlib.util.spec_from_file_location("ggen_self_research_observer", SCRIPT)
assert SPEC and SPEC.loader
OBSERVER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(OBSERVER)


def git(root: Path, *args: str) -> None:
    subprocess.run(["git", "-C", str(root), *args], check=True, stdout=subprocess.PIPE)


class ResearchFence(unittest.TestCase):
    def test_explicit_research_artifact_is_not_told_to_add_placeholder_pack_contract(self) -> None:
        root = Path(tempfile.mkdtemp(prefix="ggen-self-research-"))
        git(root, "init")
        git(root, "config", "user.name", "ggen self-host test")
        git(root, "config", "user.email", "ggen-self-host@example.invalid")
        (root / "Cargo.toml").write_text(
            '[package]\nname="root"\nversion="0.1.0"\n\n[workspace]\nmembers=[]\n',
            encoding="utf-8",
        )
        (root / "ggen.toml").write_text(
            '[project]\nname="x"\nversion="0.1.0"\n\n[ontology]\nsource="ontology.ttl"\n\n[generation]\noutput_dir="."\n',
            encoding="utf-8",
        )
        (root / "ontology.ttl").write_text("<urn:x> <urn:p> <urn:o> .\n", encoding="utf-8")
        research = root / "packs" / "research-pack"
        research.mkdir(parents=True)
        (research / "README.md").write_text(
            "# research-pack (experimental, not wired into sync or CI)\n\n"
            "This is a research artifact, not a production capability.\n",
            encoding="utf-8",
        )
        (research / "ontology.ttl").write_text("<urn:x> <urn:p> <urn:o> .\n", encoding="utf-8")
        git(root, "add", ".")
        git(root, "commit", "-m", "research boundary")

        observation = OBSERVER.observe(root)
        self.assertFalse(
            any(
                item["category"] == "pack-contract" and item["evidence_path"] == "packs/research-pack"
                for item in observation["findings"]
            )
        )
        replacement = next(
            item
            for item in observation["findings"]
            if item["category"] == "pack-namespace" and item["evidence_path"] == "packs/research-pack"
        )
        self.assertEqual(replacement["severity"], "Medium")
        self.assertIn("Do not add a placeholder pack.toml", replacement["must_not_do"])


if __name__ == "__main__":
    unittest.main()
