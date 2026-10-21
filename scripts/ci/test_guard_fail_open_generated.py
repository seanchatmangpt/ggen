#!/usr/bin/env python3
"""RED test for the generated-surface pass of guard_fail_open_subprocess.py.

Feeds a temp dir (not a git repo -- nothing is tracked) containing the known
fail-open shape (`Command::output()` whose `!status.success()` branch only
logs) and asserts the guard flags it via --generated-roots even though it is
untracked. Also asserts a fail-closed sibling passes clean. No pytest.

    python3 scripts/ci/test_guard_fail_open_generated.py
"""
from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

GUARD = Path(__file__).resolve().parent / "guard_fail_open_subprocess.py"

FAIL_OPEN = """\
use std::process::Command;

pub fn run_verifier() -> bool {
    let output = Command::new("verifier").arg("run").output().unwrap();
    if !output.status.success() {
        eprintln!("verifier failed - using cached report");
    }
    true
}
"""

FAIL_CLOSED = """\
use std::process::Command;

pub fn run_verifier() -> anyhow::Result<()> {
    let output = Command::new("verifier").arg("run").output()?;
    if !output.status.success() {
        bail!("verifier refused: {:?}", output.status);
    }
    Ok(())
}
"""


def run(root: Path, gen_root: Path) -> subprocess.CompletedProcess:
    return subprocess.run(
        [
            sys.executable,
            str(GUARD),
            "--root",
            str(root),
            "--generated-roots",
            str(gen_root),
        ],
        capture_output=True,
        text=True,
        check=False,
    )


def main() -> int:
    with tempfile.TemporaryDirectory() as td:
        root = Path(td)
        gen = root / "examples" / "demo" / "gen"
        gen.mkdir(parents=True)
        (gen / "generated_hook.rs").write_text(FAIL_OPEN)
        (gen / "generated_safe.rs").write_text(FAIL_CLOSED)

        result = run(root, gen)
        assert result.returncode == 1, f"expected exit 1, got {result.returncode}: {result.stdout} {result.stderr}"
        assert "FAIL_OPEN_SUBPROCESS_HANDLING" in result.stdout, result.stdout
        assert "generated_hook.rs" in result.stdout, "untracked generated file must be named"
        assert "generated_safe.rs" not in result.stdout, "fail-closed sibling must not be flagged"

        # Vacuity: remove the fail-open file and the same court must go clean.
        (gen / "generated_hook.rs").unlink()
        clean = run(root, gen)
        assert clean.returncode == 0, f"expected clean exit 0 after fix, got {clean.returncode}: {clean.stdout}"

    print("test_guard_fail_open_generated: 4/4 assertions passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
