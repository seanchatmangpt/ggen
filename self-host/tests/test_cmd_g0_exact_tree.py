#!/usr/bin/env python3
"""Real Git/filesystem/subprocess witnesses for CMD G0 exact-tree observation."""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
OBSERVER = SCRIPTS / "observe_cmd_g0.py"
VERIFIER = SCRIPTS / "verify_cmd_g0.py"


def run(*command: str, cwd: Path, check: bool = True) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        list(command),
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=check,
    )


def git(root: Path, *args: str) -> str:
    return run("git", "-C", str(root), *args, cwd=root).stdout.decode("utf-8").strip()


class ExactTreeContractTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="cmd-g0-real-git-")
        self.root = Path(self.temporary.name) / "subject"
        self.child = Path(self.temporary.name) / "child"
        self.root.mkdir()
        self.child.mkdir()
        for repository in (self.root, self.child):
            run("git", "init", "-q", cwd=repository)
            run("git", "config", "user.name", "CMD G0 Test", cwd=repository)
            run("git", "config", "user.email", "cmd-g0@example.invalid", cwd=repository)

        (self.child / "child.txt").write_text("child\n", encoding="utf-8")
        run("git", "add", "child.txt", cwd=self.child)
        run("git", "commit", "-q", "-m", "child", cwd=self.child)

        (self.root / "regular.txt").write_text("regular\n", encoding="utf-8")
        executable = self.root / "executable.sh"
        executable.write_text("#!/bin/sh\nprintf 'alive\\n'\n", encoding="utf-8")
        executable.chmod(0o755)
        os.symlink("regular.txt", self.root / "regular.link")
        run(
            "git",
            "-c",
            "protocol.file.allow=always",
            "submodule",
            "add",
            "-q",
            str(self.child),
            "modules/child",
            cwd=self.root,
        )
        run("git", "add", ".gitmodules", "regular.txt", "executable.sh", "regular.link", "modules/child", cwd=self.root)
        run("git", "commit", "-q", "-m", "exact tree subject", cwd=self.root)
        (self.root / "untracked.txt").write_text("not admitted\n", encoding="utf-8")
        self.revision = git(self.root, "rev-parse", "HEAD")
        self.evidence_root = self.root / "self-host" / "observed" / "cmd-g0"

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def observe(self) -> Path:
        result = run(
            sys.executable,
            str(OBSERVER),
            "--root",
            str(self.root),
            "--expected-revision",
            self.revision,
            "--evidence-root",
            str(self.evidence_root),
            cwd=self.root,
        )
        payload = json.loads(result.stdout)
        return Path(payload["evidence_dir"])

    def test_real_git_modes_symlink_gitlink_receipts_and_replay(self) -> None:
        evidence = self.observe()
        run(
            sys.executable,
            str(VERIFIER),
            "--root",
            str(self.root),
            "--expected-revision",
            self.revision,
            "--evidence-dir",
            str(evidence),
            cwd=self.root,
        )
        surfaces = json.loads((evidence / "surfaces.json").read_text(encoding="utf-8"))["surfaces"]
        by_path = {item["path"]: item for item in surfaces}
        self.assertEqual(by_path["regular.txt"]["mode"], "100644")
        self.assertEqual(by_path["executable.sh"]["mode"], "100755")
        self.assertEqual(by_path["regular.link"]["mode"], "120000")
        self.assertEqual(by_path["regular.link"]["content_semantics"], "symlink-target-bytes")
        self.assertEqual(by_path["modules/child"]["mode"], "160000")
        self.assertEqual(by_path["modules/child"]["object_type"], "commit")
        self.assertEqual(by_path["modules/child"]["content_semantics"], "gitlink-commit-identity")
        untracked = json.loads((evidence / "untracked.json").read_text(encoding="utf-8"))
        self.assertIn("untracked.txt", untracked["untracked_paths"])
        self.assertNotIn("untracked.txt", by_path)

        replay = run(
            sys.executable,
            str(OBSERVER),
            "--root",
            str(self.root),
            "--expected-revision",
            self.revision,
            "--evidence-root",
            str(self.evidence_root),
            cwd=self.root,
        )
        self.assertEqual(Path(json.loads(replay.stdout)["evidence_dir"]), evidence)

    def test_removed_tracked_path_is_refused_by_exact_set_verifier(self) -> None:
        evidence = self.observe()
        tampered = self.root / "tampered-evidence"
        shutil.copytree(evidence, tampered)
        repository_path = tampered / "repository.json"
        repository = json.loads(repository_path.read_text(encoding="utf-8"))
        repository["objects"] = repository["objects"][1:]
        repository_path.write_text(json.dumps(repository, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        result = run(
            sys.executable,
            str(VERIFIER),
            "--root",
            str(self.root),
            "--expected-revision",
            self.revision,
            "--evidence-dir",
            str(tampered),
            cwd=self.root,
            check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn(b"REFUSED: CMD-G0-EXACT-SET", result.stderr)


if __name__ == "__main__":
    unittest.main()
