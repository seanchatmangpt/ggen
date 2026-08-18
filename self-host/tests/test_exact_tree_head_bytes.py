from __future__ import annotations

import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "observe_self_host.py"
SPEC = importlib.util.spec_from_file_location("ggen_self_observer_head_bytes", SCRIPT)
assert SPEC and SPEC.loader
OBSERVER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(OBSERVER)


def git(root: Path, *args: str) -> None:
    subprocess.run(
        ["git", "-C", str(root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


class ExactTreeHeadBytes(unittest.TestCase):
    def test_dirty_tracked_worktree_bytes_do_not_enter_exact_tree_observation(self) -> None:
        with tempfile.TemporaryDirectory(prefix="ggen-self-head-bytes-") as temporary:
            root = Path(temporary)
            git(root, "init", "-q")
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
            tracked = root / "tracked.txt"
            committed = b"committed bytes\n"
            tracked.write_bytes(committed)
            git(root, "add", ".")
            git(root, "commit", "-q", "-m", "fixture")

            tracked.write_bytes(b"dirty generated projection\n")
            observation = OBSERVER.observe(root)
            record = next(item for item in observation["files"] if item["path"] == "tracked.txt")

            self.assertEqual(record["size_bytes"], len(committed))
            self.assertEqual(record["digest"], OBSERVER.MODEL.sha256(committed))
            self.assertNotEqual(record["digest"], OBSERVER.MODEL.sha256(tracked.read_bytes()))


if __name__ == "__main__":
    unittest.main()
