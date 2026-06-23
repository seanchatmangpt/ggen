#!/usr/bin/env python3
"""
validate_artifacts.py — Structural validators for the TPOT2 wasm4pm auto-config
ggen project. Stdlib only (tomllib for TOML; regex/line scanning for TTL/SPARQL/Tera).

WHAT THIS CHECKS (CONTRACT.md §§6,7,8,10 + WASM4PM-DISCOVERED-BUGS footguns):

  ggen.toml driver:
    * loads via tomllib (valid TOML)
    * has [project], [ontology], [generation]
    * [ontology] uses `imports` (BUG-001) and does NOT use `additional`
    * no `../` in any output_file (GGEN-YIELD-001)

  queries/*.rq (every SELECT):
    * has an ORDER BY clause              (BUG-006 / E0011 / E0013)
    * contains no `SELECT *`              (BUG-007 / GGEN-QUERY-002)

  templates/*.tera (every template):
    * no line starting with `---` (no YAML frontmatter — BUG-002)
    * uses `sparql_results` (the batch idiom — CONTRACT.md §7)

  coverage (CONTRACT.md §10):
    * ontology/algorithms.ttl: 60 `a pi:ProcessIntelligenceAlgorithm`
    * ontology/tpot-search-space.ttl: exactly 9 `tpot:PipelineStage`
    * at least one `compat:Breed_` reference exists
    * all 6 semconv proof filenames (ocel-load.rq, discover-dfg.rq,
      conformance-check.rq, ml-classify.rq, detect-drift.rq, predict-activity.rq)
      are referenced somewhere under the project (generated config template /
      ggen.toml / queries)

DESIGN: every check reports PASS / FAIL / MISSING with a reason. MISSING (file not
yet created by another agent) does NOT crash and does NOT count as FAIL by default
for not-yet-integrated optional files, BUT the contract-required files (the four
listed in §10 and the driver ggen.toml) DO count as FAIL when absent — they are
deliverables. Exit code is non-zero if ANY FAIL. A JSON summary is written to
verify/out/validation_report.json.

Run:  python3 verify/validate_artifacts.py [PROJECT_DIR]
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# tomllib is stdlib in Python 3.11+.
try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - environment guard
    tomllib = None  # type: ignore


_THIS_DIR = Path(__file__).resolve().parent
DEFAULT_PROJECT_DIR = _THIS_DIR.parent
OUT_DIR = _THIS_DIR / "out"

# The 6 semconv SPARQL proof file names that MUST be referenced (CONTRACT.md §6/§10).
SEMCONV_PROOFS = (
    "ocel-load.rq",
    "discover-dfg.rq",
    "conformance-check.rq",
    "ml-classify.rq",
    "detect-drift.rq",
    "predict-activity.rq",
)

# Status constants.
PASS = "PASS"
FAIL = "FAIL"
MISSING = "MISSING"


class Report:
    """Accumulates check results; tracks whether any hard FAIL occurred."""

    def __init__(self) -> None:
        self.checks: List[Dict[str, str]] = []
        self.any_fail = False

    def add(self, name: str, status: str, reason: str, *, missing_is_fail: bool = True) -> None:
        # MISSING optional files are reported but do not fail the run unless flagged.
        effective_fail = status == FAIL or (status == MISSING and missing_is_fail)
        if effective_fail:
            self.any_fail = True
        self.checks.append({"check": name, "status": status, "reason": reason})

    def to_dict(self) -> dict:
        counts = {PASS: 0, FAIL: 0, MISSING: 0}
        for c in self.checks:
            counts[c["status"]] = counts.get(c["status"], 0) + 1
        return {
            "summary": {
                "total": len(self.checks),
                "pass": counts[PASS],
                "fail": counts[FAIL],
                "missing": counts[MISSING],
                "overall": "FAIL" if self.any_fail else "PASS",
            },
            "checks": self.checks,
        }


# ---------------------------------------------------------------------------
# SPARQL helpers (line-scanning, comment-aware)
# ---------------------------------------------------------------------------

def _strip_sparql_comments(text: str) -> str:
    """Remove `#` line comments (SPARQL comments). Keeps `#` inside <...> IRIs and
    quoted strings out of scope — these queries do not use literal `#` in data, so a
    simple split at the first unquoted `#` per line is sufficient and safe here."""
    out_lines: List[str] = []
    for line in text.splitlines():
        # Find first '#' that is not inside a <...> IRI on this line.
        in_iri = False
        cut = None
        for i, ch in enumerate(line):
            if ch == "<":
                in_iri = True
            elif ch == ">":
                in_iri = False
            elif ch == "#" and not in_iri:
                cut = i
                break
        out_lines.append(line if cut is None else line[:cut])
    return "\n".join(out_lines)


def _is_select_query(code: str) -> bool:
    return re.search(r"\bSELECT\b", code, re.IGNORECASE) is not None


def _has_select_star(code: str) -> bool:
    # `SELECT *` (optionally with DISTINCT/REDUCED) — BUG-007.
    return re.search(r"\bSELECT\s+(DISTINCT\s+|REDUCED\s+)?\*", code, re.IGNORECASE) is not None


def _has_order_by(code: str) -> bool:
    return re.search(r"\bORDER\s+BY\b", code, re.IGNORECASE) is not None


# ---------------------------------------------------------------------------
# Individual check groups
# ---------------------------------------------------------------------------

def check_ggen_toml(project: Path, rep: Report) -> Optional[dict]:
    """Validate the driver ggen.toml. Returns the parsed config dict (or None)."""
    path = project / "ggen.toml"
    if not path.is_file():
        rep.add("ggen.toml exists", MISSING, f"{path} not found (Agent 4 deliverable)")
        return None
    if tomllib is None:
        rep.add("ggen.toml parses", FAIL, "tomllib unavailable (need Python 3.11+)")
        return None

    try:
        with path.open("rb") as fh:
            cfg = tomllib.load(fh)
    except Exception as exc:  # noqa: BLE001 - report any TOML error as FAIL
        rep.add("ggen.toml parses (tomllib)", FAIL, f"tomllib error: {exc}")
        return None
    rep.add("ggen.toml parses (tomllib)", PASS, "valid TOML")

    # Required top-level tables.
    for table in ("project", "ontology", "generation"):
        if table in cfg:
            rep.add(f"ggen.toml has [{table}]", PASS, "present")
        else:
            rep.add(f"ggen.toml has [{table}]", FAIL, f"missing [{table}] table")

    # [ontology] must use `imports`, never `additional` (BUG-001).
    onto = cfg.get("ontology", {})
    if isinstance(onto, dict):
        if "additional" in onto:
            rep.add(
                "ggen.toml [ontology] no `additional`", FAIL,
                "uses `additional` — silently ignored (BUG-001); must be `imports`",
            )
        else:
            rep.add("ggen.toml [ontology] no `additional`", PASS, "does not use `additional`")
        if "imports" in onto:
            rep.add("ggen.toml [ontology] uses `imports`", PASS, f"imports = {onto['imports']}")
        else:
            # imports is only needed if extra ontologies are loaded; advisory, not hard fail.
            rep.add(
                "ggen.toml [ontology] uses `imports`", MISSING,
                "no `imports` key (ok only if a single `source` ontology is intended)",
                missing_is_fail=False,
            )

    # No `../` in any output_file (GGEN-YIELD-001).
    rules = cfg.get("generation", {}).get("rules", []) if isinstance(cfg.get("generation"), dict) else []
    bad_outputs = []
    for r in rules:
        of = r.get("output_file", "") if isinstance(r, dict) else ""
        if isinstance(of, str) and ".." in of.split("/"):
            bad_outputs.append(of)
    # Also scan top-level generation.output_dir just in case.
    out_dir_val = cfg.get("generation", {}).get("output_dir", "") if isinstance(cfg.get("generation"), dict) else ""
    if isinstance(out_dir_val, str) and ".." in out_dir_val.split("/"):
        bad_outputs.append(out_dir_val)
    if bad_outputs:
        rep.add("ggen.toml output paths inside root", FAIL, f"`../` escapes root: {bad_outputs}")
    else:
        rep.add(
            "ggen.toml output paths inside root", PASS,
            f"no `../` in {len(rules)} rule output_file(s)",
        )
    return cfg


def check_queries(project: Path, rep: Report) -> None:
    qdir = project / "queries"
    rq_files = sorted(qdir.glob("*.rq")) if qdir.is_dir() else []
    if not rq_files:
        rep.add("queries/*.rq present", MISSING, f"no .rq files in {qdir} (Agent 2 deliverable)")
        return
    rep.add("queries/*.rq present", PASS, f"{len(rq_files)} query file(s)")

    for f in rq_files:
        raw = f.read_text(encoding="utf-8")
        code = _strip_sparql_comments(raw)
        name = f.name
        if not _is_select_query(code):
            # CONSTRUCT-only (inference) files live under inference/, but tolerate here.
            rep.add(f"queries/{name}", PASS, "no SELECT (CONSTRUCT/ASK) — ORDER BY/star checks N/A")
            continue
        # SELECT * check (BUG-007).
        if _has_select_star(code):
            rep.add(f"queries/{name} no `SELECT *`", FAIL, "uses `SELECT *` — disables provision checks (BUG-007)")
        else:
            rep.add(f"queries/{name} no `SELECT *`", PASS, "explicit projection")
        # ORDER BY check (BUG-006).
        if _has_order_by(code):
            rep.add(f"queries/{name} has ORDER BY", PASS, "deterministic ordering")
        else:
            rep.add(f"queries/{name} has ORDER BY", FAIL, "missing ORDER BY (BUG-006 / E0011 / E0013)")


def check_templates(project: Path, rep: Report) -> None:
    tdir = project / "templates"
    tera_files = sorted(tdir.glob("*.tera")) if tdir.is_dir() else []
    if not tera_files:
        rep.add("templates/*.tera present", MISSING, f"no .tera files in {tdir} (Agent 3 deliverable)")
        return
    rep.add("templates/*.tera present", PASS, f"{len(tera_files)} template file(s)")

    for f in tera_files:
        text = f.read_text(encoding="utf-8")
        name = f.name
        # No line starting with `---` (frontmatter renders literally — BUG-002).
        frontmatter_lines = [
            i + 1 for i, ln in enumerate(text.splitlines()) if ln.strip() == "---" or ln.startswith("---")
        ]
        if frontmatter_lines:
            rep.add(
                f"templates/{name} no frontmatter", FAIL,
                f"line(s) {frontmatter_lines} start with '---' — renders literally (BUG-002)",
            )
        else:
            rep.add(f"templates/{name} no frontmatter", PASS, "no '---' frontmatter delimiter")
        # Uses sparql_results (batch idiom — CONTRACT.md §7).
        if "sparql_results" in text:
            rep.add(f"templates/{name} uses sparql_results", PASS, "batch idiom present")
        else:
            rep.add(
                f"templates/{name} uses sparql_results", FAIL,
                "does not reference `sparql_results` (batch templates required — CONTRACT.md §7)",
            )


def _count_pattern_in_file(path: Path, pattern: str) -> int:
    if not path.is_file():
        return -1  # sentinel: missing
    text = path.read_text(encoding="utf-8")
    return len(re.findall(pattern, text))


def check_coverage(project: Path, rep: Report) -> None:
    # 60 algorithms.
    algos_ttl = project / "ontology" / "algorithms.ttl"
    n_algos = _count_pattern_in_file(algos_ttl, r"\ba\s+pi:ProcessIntelligenceAlgorithm\b")
    if n_algos < 0:
        rep.add("coverage: 60 algorithms", MISSING, f"{algos_ttl} not found (Agent 1 deliverable)")
    elif n_algos == 60:
        rep.add("coverage: 60 algorithms", PASS, "60 `a pi:ProcessIntelligenceAlgorithm`")
    else:
        rep.add("coverage: 60 algorithms", FAIL, f"found {n_algos} algorithm individuals, expected 60")

    # exactly 9 pipeline stages.
    tpot_ttl = project / "ontology" / "tpot-search-space.ttl"
    n_stages = _count_pattern_in_file(tpot_ttl, r"\ba\s+tpot:PipelineStage\b")
    if n_stages < 0:
        rep.add("coverage: 9 pipeline stages", MISSING, f"{tpot_ttl} not found (Agent 1 deliverable)")
    elif n_stages == 9:
        rep.add("coverage: 9 pipeline stages", PASS, "exactly 9 `tpot:PipelineStage`")
    else:
        rep.add("coverage: 9 pipeline stages", FAIL, f"found {n_stages} stages, expected exactly 9")

    # at least one compat:Breed_ reference as a GP meta-strategy — it must be USED,
    # not merely DECLARED in the breeds registry. So we count references OUTSIDE
    # ontology/breeds.ttl (e.g. `tpot:metaBreed compat:Breed_*` in tpot-search-space.ttl).
    # _grep_project already excludes CONTRACT.md and verify/out, so a reference cannot
    # be satisfied by the spec doc or our own output (anti-cheating: faking is harder).
    breed_hits = [
        (f, m) for (f, m) in _grep_project(project, r"compat:Breed_\w+")
        if Path(f).as_posix() != "ontology/breeds.ttl"
    ]
    if breed_hits:
        sample = sorted({m for _f, m in breed_hits})[:4]
        srcs = sorted({f for f, _m in breed_hits})[:3]
        rep.add(
            "coverage: compat:Breed_ used (not just declared)", PASS,
            f"{len(breed_hits)} use-site reference(s) in {srcs}; e.g. {sample}",
        )
    else:
        rep.add(
            "coverage: compat:Breed_ used (not just declared)", FAIL,
            "no `compat:Breed_` reference outside ontology/breeds.ttl — a breed must be "
            "USED as GP meta-strategy (e.g. tpot:metaBreed), not merely declared (CONTRACT.md §10)",
        )

    # all 6 semconv proof file names referenced somewhere under the project.
    # Evidence must come from ACTUAL generation artifacts (queries, templates, ttl,
    # ggen.toml), NOT from documentation or from this validator's own source — those
    # are excluded so the check cannot be satisfied by self-reference (anti-cheating).
    proof_skip_names = {"CONTRACT.md", "README.md", "INTEGRATION_REPORT.md",
                        "validate_artifacts.py", "reference_autoconfig.py",
                        "test_tpot2_autoconfig.py", "test_vision2030.py"}
    referenced: Dict[str, List[str]] = {p: [] for p in SEMCONV_PROOFS}
    for f in _iter_text_files(project, skip_dirs={"out", "__pycache__", ".git"}):
        if f.name in proof_skip_names:
            continue
        try:
            text = f.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            continue
        for proof in SEMCONV_PROOFS:
            if proof in text:
                referenced[proof].append(str(f.relative_to(project)))
    for proof in SEMCONV_PROOFS:
        files = referenced[proof]
        if files:
            rep.add(f"coverage: proof `{proof}` referenced", PASS, f"in {files[:3]}")
        else:
            rep.add(
                f"coverage: proof `{proof}` referenced", FAIL,
                "not referenced by any generation artifact (queries/templates/ttl/ggen.toml) "
                "— docs and verify/ are excluded as evidence (CONTRACT.md §6/§10)",
            )


# ---------------------------------------------------------------------------
# Filesystem helpers
# ---------------------------------------------------------------------------

def _iter_text_files(project: Path, skip_dirs: set) -> List[Path]:
    out: List[Path] = []
    for p in project.rglob("*"):
        if not p.is_file():
            continue
        if any(part in skip_dirs for part in p.relative_to(project).parts):
            continue
        # Only inspect plausibly-textual files.
        if p.suffix.lower() in {".rq", ".tera", ".toml", ".ttl", ".md", ".py", ".json", ".txt"}:
            out.append(p)
    return sorted(out)


def _grep_project(project: Path, pattern: str) -> List[Tuple[str, str]]:
    rx = re.compile(pattern)
    hits: List[Tuple[str, str]] = []
    for f in _iter_text_files(project, skip_dirs={"out", "__pycache__", ".git"}):
        # Skip docs and our own verify scripts so the requirement is met by actual
        # generation artifacts, not the spec doc / README / validator source.
        if f.name in {"CONTRACT.md", "README.md", "INTEGRATION_REPORT.md",
                      "validate_artifacts.py", "reference_autoconfig.py",
                      "test_tpot2_autoconfig.py", "test_vision2030.py"}:
            continue
        try:
            text = f.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            continue
        for m in rx.findall(text):
            hits.append((str(f.relative_to(project)), m))
    return hits


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def validate(project: Optional[Path] = None) -> dict:
    project = (project or DEFAULT_PROJECT_DIR).resolve()
    rep = Report()

    if not project.is_dir():
        rep.add("project dir exists", FAIL, f"{project} is not a directory")
        return rep.to_dict()

    check_ggen_toml(project, rep)
    check_queries(project, rep)
    check_templates(project, rep)
    check_coverage(project, rep)

    result = rep.to_dict()

    # Write JSON summary (observable artifact).
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    (OUT_DIR / "validation_report.json").write_text(
        json.dumps(result, indent=2) + "\n", encoding="utf-8"
    )
    return result


def _print(result: dict) -> None:
    s = result["summary"]
    print("=" * 76)
    print("STRUCTURAL VALIDATION — TPOT2 wasm4pm auto-config artifacts")
    print("=" * 76)
    for c in result["checks"]:
        mark = {"PASS": "PASS", "FAIL": "FAIL", "MISSING": "MISS"}[c["status"]]
        print(f"[{mark}] {c['check']}: {c['reason']}")
    print("-" * 76)
    print(
        f"TOTAL {s['total']} | PASS {s['pass']} | FAIL {s['fail']} | MISSING {s['missing']} "
        f"| OVERALL {s['overall']}"
    )
    print("=" * 76)


def main(argv: Optional[List[str]] = None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    project = Path(argv[0]) if argv else None
    result = validate(project)
    _print(result)
    return 0 if result["summary"]["overall"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
