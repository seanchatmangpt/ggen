"""S-7 non-vacuity fixture: the self-host observation re-pin gate.

Court over a real scratch git repository (Chicago-style: real collaborators,
no mocks). Proves three things about `observe_self_host.py --check`:

1. it re-pins to the observation commit recorded in the committed receipt
   (passes across an unrelated HEAD shift that would flip a HEAD-bound gate);
2. a planted stale pin (bogus revision in the receipt) is refused (exit != 0);
3. byte-exact restore of the receipt returns the gate to PASS.

A gate that cannot exit non-zero is FAIL; this fixture fails if the court
cannot flip.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
GATE = SCRIPTS / "observe_self_host.py"

BOGUS_REVISION = "0" * 40


def _run_git(repo: Path, *args: str) -> None:
    subprocess.run(
        ["git", "-C", str(repo), *args],
        check=True,
        capture_output=True,
    )


def _run_gate(repo: Path) -> tuple[int, str]:
    result = subprocess.run(
        ["python3", str(GATE), "--root", str(repo), "--check"],
        capture_output=True,
        text=True,
    )
    return result.returncode, result.stderr


def _write_receipt_revision(repo: Path, revision: str) -> None:
    receipt = repo / "self-host" / "observed" / "observation-receipt.json"
    data = json.loads(receipt.read_text(encoding="utf-8"))
    data["revision"] = revision
    receipt.write_text(json.dumps(data, indent=2), encoding="utf-8")


def _init_fixture_repo(tmp_path: Path) -> tuple[Path, str]:
    repo = tmp_path / "s7-fixture"
    (repo / "self-host" / "observed").mkdir(parents=True)
    (repo / "self-host" / "ontology.ttl").write_text("ontology: fixture\n")
    (repo / "README.md").write_text("hello\n")
    _run_git(repo, "init", "-q", ".")
    _run_git(repo, "config", "user.email", "fixture@ggen.local")
    _run_git(repo, "config", "user.name", "s7-fixture")
    _run_git(repo, "add", "-A")
    _run_git(repo, "commit", "-qm", "fixture A")
    base = subprocess.run(
        ["git", "-C", str(repo), "rev-parse", "HEAD"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    # Write-mode observation pins the receipt to the base revision.
    write = subprocess.run(
        ["python3", str(GATE), "--root", str(repo)],
        capture_output=True,
        text=True,
    )
    assert write.returncode == 0, write.stderr
    receipt = repo / "self-host" / "observed" / "observation-receipt.json"
    pin = json.loads(receipt.read_text(encoding="utf-8"))["revision"]
    assert pin == base, f"receipt pin {pin} != base revision {base}"
    _run_git(repo, "add", "-A")
    _run_git(repo, "commit", "-qm", "commit observation outputs at base")
    return repo, receipt.read_bytes()


def test_repin_gate_court(tmp_path: Path) -> None:
    repo, receipt_bytes = _init_fixture_repo(tmp_path)
    receipt = repo / "self-host" / "observed" / "observation-receipt.json"

    # 1. Unrelated HEAD shift: HEAD-bound gate would flip here; re-pinned
    #    gate must PASS (exit 0).
    (repo / "README.md").write_text("hello\nshift\n")
    _run_git(repo, "add", "-A")
    _run_git(repo, "commit", "-qm", "unrelated shift")
    rc, _ = _run_gate(repo)
    assert rc == 0, "re-pinned gate flipped on HEAD shift"

    # 2. Plant a stale pin: gate must REFUSE (exit 1) with the typed message.
    _write_receipt_revision(repo, BOGUS_REVISION)
    stale_rc, stale_err = _run_gate(repo)
    assert stale_rc == 1, "gate accepted a planted stale pin (vacuous)"
    head = subprocess.run(
        ["git", "-C", str(repo), "rev-parse", "HEAD"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    assert (
        f"GGEN_SELF_HOST_DRIFT: stale observation revision {BOGUS_REVISION} != {head}"
        in stale_err
    ), f"typed drift message missing from stderr: {stale_err!r}"

    # 3. Byte-exact restore of the receipt: gate must PASS again.
    receipt.write_bytes(receipt_bytes)
    rc, _ = _run_gate(repo)
    assert rc == 0, "gate did not recover after receipt restore"
