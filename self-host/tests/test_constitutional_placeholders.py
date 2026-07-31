from __future__ import annotations

import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "observe_self_host.py"
SPEC = importlib.util.spec_from_file_location("ggen_self_placeholder_observer", SCRIPT)
assert SPEC and SPEC.loader
OBSERVER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(OBSERVER)


def git(root: Path, *args: str) -> None:
    subprocess.run(["git", "-C", str(root), *args], check=True, stdout=subprocess.PIPE)


class ConstitutionalPlaceholders(unittest.TestCase):
    def test_sample_repository_identity_is_not_live_observation(self) -> None:
        root = Path(tempfile.mkdtemp(prefix="ggen-self-placeholder-"))
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
        pack = root / "packs" / "as-found"
        pack.mkdir(parents=True)
        (pack / "pack.toml").write_text('[pack]\nname="as-found"\nversion="0.1.0"\n', encoding="utf-8")
        (pack / "ontology.ttl").write_text(
            '<urn:repo> <urn:url> "https://github.com/OWNER/REPOSITORY" ;\n'
            '  <urn:observedAt> "1979-01-01T00:00:00Z" .\n',
            encoding="utf-8",
        )
        git(root, "add", ".")
        git(root, "commit", "-m", "placeholder law")

        observation = OBSERVER.observe(root)
        finding = next(
            item
            for item in observation["findings"]
            if item["category"] == "constitutional-placeholder"
        )
        self.assertEqual(finding["severity"], "High")
        self.assertEqual(finding["evidence_path"], "packs/as-found/ontology.ttl")
        self.assertIn("Do not replace the sentinel", finding["must_not_do"])


if __name__ == "__main__":
    unittest.main()
