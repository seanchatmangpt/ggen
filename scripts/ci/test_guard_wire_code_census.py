#!/usr/bin/env python3
"""RED test for guard_wire_code_census.py (lane A16).

Runs the guard against a temp fixture: a registry with codes A-ONE and
A-TWO, and a census listing A-ONE and a RENAMED A-TWO-RENAMED. The mismatch
must produce exit 2 and CENSUS_STALE on stdout. A matching fixture must
exit 0. Run with no pytest dependency:
    python3 scripts/ci/test_guard_wire_code_census.py
"""
from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

GUARD = Path(__file__).resolve().parent / "guard_wire_code_census.py"


def run_guard(registry_text: str, census_text: str, tmp: Path) -> subprocess.CompletedProcess:
    registry = tmp / "boundary-refusal.shacl.ttl"
    census = tmp / "typed_boundary_census.rs"
    registry.write_text(registry_text)
    census.write_text(census_text)
    return subprocess.run(
        [
            sys.executable,
            str(GUARD),
            "--repo",
            str(tmp),
            "--registry",
            str(registry),
            "--census",
            str(census),
        ],
        capture_output=True,
        text=True,
        check=False,
    )


def main() -> int:
    with tempfile.TemporaryDirectory() as td:
        tmp = Path(td)
        registry = (
            'br:code "E-ENVELOPE-UNDECLARED" ;\n'
            'br:code "VACUOUS_COURT" ;\n'
            'br:codePrefix "VACUOUS" ;\n'
        )
        census_renamed = (
            "// GENERATED census\npub const WIRE_CODES: &[&str] = &[\n"
            '    "E-ENVELOPE-UNDECLARED",\n    "VACUOUS_COURT_RENAMED",\n];\n'
        )
        bad = run_guard(registry, census_renamed, tmp)
        assert bad.returncode == 2, f"expected exit 2, got {bad.returncode}: {bad.stdout} {bad.stderr}"
        assert "CENSUS_STALE" in bad.stdout, f"expected CENSUS_STALE on stdout, got: {bad.stdout}"
        assert "VACUOUS_COURT_RENAMED" in bad.stdout, "renamed code must be named in output"

        census_ok = (
            "// GENERATED census\npub const WIRE_CODES: &[&str] = &[\n"
            '    "E-ENVELOPE-UNDECLARED",\n    "VACUOUS_COURT",\n];\n'
        )
        good = run_guard(registry, census_ok, tmp)
        assert good.returncode == 0, f"expected exit 0 for matching census, got {good.returncode}: {good.stdout}"

        # Vacuity: the RED branch must depend on the diff, not the runner.
        census_empty = 'pub const WIRE_CODES: &[&str] = &[];\n'
        empty = run_guard(registry, census_empty, tmp)
        assert empty.returncode == 2, "empty census against non-empty registry must be STALE"

    print("test_guard_wire_code_census: 3/3 assertions passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
