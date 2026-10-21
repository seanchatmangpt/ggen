#!/usr/bin/env python3
"""RED tests for the R05 dialect declaration court.

Plain asserts + __main__ runner (no pytest). Uses real on-disk fixture packs
(Chicago: real file I/O via tempfile, assert on real exit codes and real
output text, never call counts).
"""

import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
COURT = HERE / "dialect_declaration_court.py"


def run_court(*pack_roots):
    return subprocess.run(
        [sys.executable, str(COURT), *[str(p) for p in pack_roots]],
        capture_output=True,
        text=True,
    )


def make_pack(root: Path, name: str, pack_toml: str, files: dict):
    pack = root / name
    pack.mkdir(parents=True)
    (pack / "pack.toml").write_text(pack_toml)
    for rel, content in files.items():
        target = pack / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content)
    return pack


MINIMAL_TOML = """\
[pack]
name = "%s"
version = "0.1.0"

[capabilities]
provides = ["urn:ggen:pack:%s"]
requires = []
"""


def test_undeclared_n3_refused():
    """RED case: default dialects (*.ttl -> turtle) must refuse an .n3 file."""
    with tempfile.TemporaryDirectory() as tmp:
        pack = make_pack(
            Path(tmp),
            "undeclared-pack",
            MINIMAL_TOML % ("undeclared-pack", "undeclared-pack"),
            {
                "ontology.ttl": "<http://e/s> <http://e/p> <http://e/o> .\n",
                "rules.n3": "@prefix log: <http://www.w3.org/2000/10/swap/log#> .\n",
            },
        )
        r = run_court(pack)
        assert r.returncode == 1, f"expected exit 1, got {r.returncode}\n{r.stdout}"
        assert "rules.n3" in r.stdout, f"court must name the undeclared file:\n{r.stdout}"
        assert "UNDECLARED_DIALECT_FILE" in r.stdout
    print("PASS: undeclared .n3 -> exit 1, file named")


def test_conforming_pack_passes():
    """Default dialects cover a plain ontology.ttl pack."""
    with tempfile.TemporaryDirectory() as tmp:
        pack = make_pack(
            Path(tmp),
            "conforming-pack",
            MINIMAL_TOML % ("conforming-pack", "conforming-pack"),
            {"ontology.ttl": "<http://e/s> <http://e/p> <http://e/o> .\n"},
        )
        r = run_court(pack)
        assert r.returncode == 0, f"expected exit 0, got {r.returncode}\n{r.stdout}"
    print("PASS: conforming pack -> exit 0")


def test_declared_n3_glob_passes():
    """An explicit n3 glob admits the .n3 file."""
    toml = MINIMAL_TOML % ("n3-pack", "n3-pack") + """
[[capabilities.dialects]]
glob = "*.ttl"
dialect = "turtle"

[[capabilities.dialects]]
glob = "rules/*.n3"
dialect = "n3"
"""
    with tempfile.TemporaryDirectory() as tmp:
        pack = make_pack(
            Path(tmp),
            "n3-pack",
            toml,
            {
                "ontology.ttl": "<http://e/s> <http://e/p> <http://e/o> .\n",
                "rules/n3/rules.n3": "{} a log:Formula .\n",
            },
        )
        r = run_court(pack)
        assert r.returncode == 0, f"expected exit 0, got {r.returncode}\n{r.stdout}"
    print("PASS: declared n3 glob -> exit 0")


def test_extension_glob_conflict_refused():
    """Conflict: an .n3 file matched by a glob declaring a different dialect."""
    toml = MINIMAL_TOML % ("conflict-pack", "conflict-pack") + """
[[capabilities.dialects]]
glob = "*.ttl"
dialect = "turtle"

[[capabilities.dialects]]
glob = "*.n3"
dialect = "turtle"
"""
    with tempfile.TemporaryDirectory() as tmp:
        pack = make_pack(
            Path(tmp),
            "conflict-pack",
            toml,
            {
                "ontology.ttl": "<http://e/s> <http://e/p> <http://e/o> .\n",
                "rules.n3": "{} log:implies {} .\n",
            },
        )
        r = run_court(pack)
        assert r.returncode == 1, f"expected exit 1, got {r.returncode}\n{r.stdout}"
        assert "DIALECT_CONFLICT" in r.stdout, f"missing conflict code:\n{r.stdout}"
        assert "rules.n3" in r.stdout
    print("PASS: extension/glob dialect conflict -> exit 1")


def test_empty_dialects_list_refused():
    """FM-PACK-021 parity: explicitly empty dialects list is a loud refusal."""
    toml = (MINIMAL_TOML % ("empty-pack", "empty-pack")).replace(
        "requires = []", "requires = []\ndialects = []"
    )
    with tempfile.TemporaryDirectory() as tmp:
        pack = make_pack(
            Path(tmp),
            "empty-pack",
            toml,
            {"ontology.ttl": "<http://e/s> <http://e/p> <http://e/o> .\n"},
        )
        r = run_court(pack)
        assert r.returncode == 1, f"expected exit 1, got {r.returncode}\n{r.stdout}"
        assert "FM-PACK-021" in r.stdout
    print("PASS: empty dialects list -> exit 1 (FM-PACK-021)")


def test_anti_vacuity():
    """The court must be able to flip: an undeclared RDF file MUST change the verdict."""
    with tempfile.TemporaryDirectory() as tmp:
        pack = make_pack(
            Path(tmp),
            "vacuity-pack",
            MINIMAL_TOML % ("vacuity-pack", "vacuity-pack"),
            {"ontology.ttl": "<http://e/s> <http://e/p> <http://e/o> .\n"},
        )
        before = run_court(pack)
        assert before.returncode == 0
        (pack / "smuggle.jsonld").write_text('{"@id": "http://e/s"}\n')
        after = run_court(pack)
        assert after.returncode == 1, "shim: smuggled .jsonld must flip PASS -> FAIL"
        assert "smuggle.jsonld" in after.stdout
    print("PASS: anti-vacuity — undeclared file flips the verdict (non-vacuous court)")


def main():
    tests = [
        test_undeclared_n3_refused,
        test_conforming_pack_passes,
        test_declared_n3_glob_passes,
        test_extension_glob_conflict_refused,
        test_empty_dialects_list_refused,
        test_anti_vacuity,
    ]
    for t in tests:
        t()
    print(f"DIALECT COURT TESTS: {len(tests)}/{len(tests)} PASS")


if __name__ == "__main__":
    main()
