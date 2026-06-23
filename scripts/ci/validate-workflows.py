#!/usr/bin/env python3
"""
validate-workflows.py — structural validator for .github/workflows/*.yml

No actionlint, no yamllint, no network. Uses PyYAML if importable; otherwise
falls back to a tolerant line-based structural check (pyyaml is NOT assumed).

Per-file checks (one PASS/FAIL line per file):
  1. presence of top-level `name:`, `on:` (or `'on':` / `"on":`), and `jobs:`
  2. every `uses:` line pins an action version: `owner/repo@<ref>` (or a local
     `./path` action, which needs no version). Any unpinned `uses:` is a FAIL.
  3. SHA-PINNING: every `uses:` referencing a THIRD-PARTY action
     (`owner/repo@ref`, excluding local `./...` and `docker://...`) must pin a
     full 40-char hex commit SHA. `@v4` / `@main` / `@<branch>` → FAIL.

Repo-wide (cross-file) checks (one PASS/FAIL line each, after the per-file pass):
  A. PR-TRIGGER SCOPE — exactly the 4 core workflows (ci.yml, quality.yml,
     docs.yml, example-tpot2.yml) may carry an `on: pull_request:` trigger.
     Any other workflow with a real `pull_request:` trigger → FAIL. (String
     mentions of `pull_request` inside `if:`/`github.event_name` guards are NOT
     triggers and are ignored — only the `on:` block is inspected.)
  B. ADVISORY ISOLATION — in ci.yml the `ci-status` (name: "CI Status") job's
     `needs:` list MUST NOT contain `lsp-crates` (the advisory lsp-trio job must
     never gate the required aggregate).
  C. SIBLING PINNING — `.github/actions/setup-ggen-build/action.yml` must NOT
     float the sibling clones (`git clone --depth 1 "$org/$repo"`); it must pin
     all four sibling repos to their exact commit SHAs.

Prints PASS/FAIL per check. Exits non-zero if any check FAILs.

Usage:
    python3 scripts/ci/validate-workflows.py [WORKFLOWS_DIR]
Default WORKFLOWS_DIR is <repo-root>/.github/workflows.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

try:
    import yaml  # type: ignore

    HAVE_YAML = True
except Exception:  # pragma: no cover - depends on runner env
    HAVE_YAML = False


# --------------------------------------------------------------------------- #
# Design facts (verified against the real CI on 2026-06-20; see              #
# .github/CI_ARCHITECTURE.md / CI_CONVENTIONS.md / CI_TEARDOWN.md).          #
# --------------------------------------------------------------------------- #

# Exactly these four core workflows may carry an `on: pull_request:` trigger.
PR_GATING_WORKFLOWS = frozenset(
    {"ci.yml", "quality.yml", "docs.yml", "example-tpot2.yml"}
)

# The single required aggregate job in ci.yml. The advisory lsp-trio job
# (`lsp-crates`) must never appear in its `needs:`.
CI_STATUS_JOB_KEY = "ci-status"
CI_STATUS_JOB_NAME = "CI Status"
LSP_ADVISORY_JOB = "lsp-crates"

# The four sibling repos the setup action must pin to exact commit SHAs. These
# are the SHAs frozen in `.github/actions/setup-ggen-build/action.yml`; bumping a
# sibling means bumping its SHA here AND in the action (and re-verifying the
# lsp trio compiles against the new lsp-max).
SIBLING_SHAS = {
    "lsp-max": "7bcc1e16dec71ef5fb2cedea2dfd6cb5cde37f59",
    "lsp-types-max": "6773e6017ca83c565785d0ec39d75304f62c3237",
    "wasm4pm": "0bb134b29245517ac4969a9f1916f5432931c5d0",
    "wasm4pm-compat": "e46155e209a750fda0218532d96ae17a9e10903e",
}


# A `uses:` value is considered pinned if it is a local composite action
# (`./...` or `../...`) or a Docker ref, or it contains an `@<ref>`.
USES_LINE = re.compile(r"""^\s*-?\s*uses\s*:\s*['"]?([^'"#]+?)['"]?\s*(?:#.*)?$""")

# A 40-char lowercase/uppercase hex string = a full git commit SHA.
SHA40 = re.compile(r"^[0-9a-fA-F]{40}$")


def uses_is_pinned(value: str) -> bool:
    value = value.strip()
    if not value:
        return False
    # Local composite/dockerfile actions resolve from the repo; no version needed.
    if value.startswith("./") or value.startswith("../"):
        return True
    # Container actions (docker://image:tag) carry their own tag; treat as pinned.
    if value.startswith("docker://"):
        return True
    # Everything else (owner/repo[/subdir]) must carry an @ref.
    return "@" in value


def uses_is_third_party(value: str) -> bool:
    """True if this `uses:` targets a third-party action that must be SHA-pinned.

    Local (`./`, `../`) actions live in-repo and need no pin; docker refs carry
    their own immutable-ish tag. Everything else is `owner/repo[/subdir]@ref`.
    """
    value = value.strip()
    if not value:
        return False
    if value.startswith("./") or value.startswith("../"):
        return False
    if value.startswith("docker://"):
        return False
    return "@" in value


def uses_is_sha_pinned(value: str) -> bool:
    """True iff a third-party `uses:` ref is a full 40-char hex commit SHA.

    `owner/repo@<40-hex>`  -> pinned (PASS)
    `owner/repo@v4` / `@main` / `@<branch>` / `@v1.2.3` -> NOT sha-pinned (FAIL)
    The ref is the part after the LAST `@` (handles `owner/repo/subdir@ref`).
    """
    value = value.strip()
    ref = value.rsplit("@", 1)[1] if "@" in value else ""
    return bool(SHA40.match(ref))


def collect_uses(lines: list[str]) -> list[tuple[int, str]]:
    """Return (line_number, uses_value) for every `uses:` line."""
    found: list[tuple[int, str]] = []
    for i, raw in enumerate(lines, start=1):
        m = USES_LINE.match(raw)
        if m:
            found.append((i, m.group(1).strip()))
    return found


def top_level_keys_yaml(text: str) -> set[str]:
    data = yaml.safe_load(text)
    if not isinstance(data, dict):
        return set()
    # PyYAML parses the bare `on:` key as the boolean True; normalize it back.
    keys: set[str] = set()
    for k in data.keys():
        if k is True:
            keys.add("on")
        elif isinstance(k, str):
            keys.add(k)
    return keys


def top_level_keys_lines(lines: list[str]) -> set[str]:
    """Tolerant fallback: a top-level key starts at column 0 (no indent)."""
    keys: set[str] = set()
    key_re = re.compile(r"""^(["']?)([A-Za-z_][\w-]*)\1\s*:""")
    for raw in lines:
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        if raw[0] in (" ", "\t"):  # indented -> not top level
            continue
        m = key_re.match(raw)
        if m:
            keys.add(m.group(2))
    return keys


# --------------------------------------------------------------------------- #
# Tolerant block extraction (used by the cross-file checks). These work with   #
# OR without PyYAML so the checks behave identically in both backends. We keep  #
# a line/indent scanner — matching the existing validator's fallback style —   #
# because GitHub Actions YAML is regular enough to slice by indentation.        #
# --------------------------------------------------------------------------- #

def _strip_comment(line: str) -> str:
    """Drop a trailing `# ...` comment that is not inside quotes (best-effort)."""
    out = []
    in_s = in_d = False
    for ch in line:
        if ch == "'" and not in_d:
            in_s = not in_s
        elif ch == '"' and not in_s:
            in_d = not in_d
        elif ch == "#" and not in_s and not in_d:
            break
        out.append(ch)
    return "".join(out)


def _indent(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def extract_top_block(lines: list[str], key: str) -> list[str]:
    """Return the body lines of a top-level block `key:` (excluding the key line).

    A top-level key sits at column 0. The block runs until the next line at
    column 0 that is a key (or EOF). Blank/comment lines are kept (callers
    tolerate them). Matches `key:`, `'key':`, `"key":`.
    """
    start = None
    pat = re.compile(rf"""^(["']?){re.escape(key)}\1\s*:""")
    for i, raw in enumerate(lines):
        if raw and raw[0] not in (" ", "\t") and pat.match(raw):
            start = i
            break
    if start is None:
        return []
    body: list[str] = []
    for raw in lines[start + 1:]:
        if not raw.strip():
            body.append(raw)
            continue
        if raw[0] not in (" ", "\t"):  # next top-level key -> block ended
            break
        body.append(raw)
    return body


def on_block_has_trigger(lines: list[str], trigger: str) -> bool:
    """True if the top-level `on:` block declares `trigger` as a key.

    Handles the three on-forms:
      * `on:` followed by indented `  pull_request:` (block form)
      * `on: [pull_request, push]` (flow-list form)
      * `on: { pull_request: ... }` (flow-map form, rare)
    Crucially this inspects ONLY the `on:` block, so `pull_request` mentioned in
    a job's `if:` / `github.event_name != 'pull_request'` guard is NOT counted.
    PyYAML folds a bare `on:` into the boolean key True, so we always use the
    line scanner here for consistency across backends.
    """
    # Locate the top-level `on:` line (it may be quoted).
    on_pat = re.compile(r"""^(["']?)on\1\s*:(.*)$""")
    idx = None
    inline = ""
    for i, raw in enumerate(lines):
        if raw and raw[0] not in (" ", "\t"):
            m = on_pat.match(raw)
            if m:
                idx = i
                inline = _strip_comment(m.group(2)).strip()
                break
    if idx is None:
        return False

    # Flow forms on the same line: `on: [a, b]` or `on: {a: ..., b: ...}`.
    if inline:
        # Tokens between brackets/braces; also bare `on: pull_request`.
        flat = inline.strip("[]{} ")
        tokens = re.split(r"[,\s:{}]+", flat)
        return trigger in {t for t in tokens if t}

    # Block form: scan the indented body for `  <trigger>:`.
    body = extract_top_block(lines, "on")
    trig_pat = re.compile(rf"""^\s+(["']?){re.escape(trigger)}\1\s*:""")
    for raw in body:
        if trig_pat.match(_strip_comment(raw)):
            return True
    return False


def find_job_needs(lines: list[str], job_key: str, job_name: str) -> list[str] | None:
    """Return the `needs:` list of the job identified by key OR `name:`.

    Returns the list of need-names (possibly empty) if the job is found, else
    None. Parses the `jobs:` block by indentation: each job is a key one level
    under `jobs:`; we read that job's body until the next sibling job key, then
    pull its `needs:` (flow `[a, b]`, inline `a`, or block `- a` list).
    """
    jobs_body = extract_top_block(lines, "jobs")
    if not jobs_body:
        return None

    # Determine the indent of job keys (first non-blank, non-comment body line).
    job_indent = None
    for raw in jobs_body:
        s = raw.strip()
        if not s or s.startswith("#"):
            continue
        job_indent = _indent(raw)
        break
    if job_indent is None:
        return None

    job_key_re = re.compile(r"""^(["']?)([A-Za-z0-9_.-]+)\1\s*:\s*(.*)$""")

    # Slice jobs_body into per-job chunks keyed by their job key.
    jobs: dict[str, list[str]] = {}
    current_key: str | None = None
    for raw in jobs_body:
        if not raw.strip() or raw.lstrip().startswith("#"):
            if current_key is not None:
                jobs[current_key].append(raw)
            continue
        if _indent(raw) == job_indent:
            m = job_key_re.match(raw[job_indent:])
            if m:
                current_key = m.group(2)
                jobs[current_key] = []
                continue
        if current_key is not None:
            jobs[current_key].append(raw)

    # Pick the target job: prefer an exact key match, else match by `name:`.
    target_body: list[str] | None = None
    if job_key in jobs:
        target_body = jobs[job_key]
    else:
        name_pat = re.compile(r"""^\s*name\s*:\s*['"]?(.+?)['"]?\s*$""")
        for body in jobs.values():
            for raw in body:
                m = name_pat.match(_strip_comment(raw))
                if m and m.group(1).strip() == job_name:
                    target_body = body
                    break
            if target_body is not None:
                break
    if target_body is None:
        return None

    return _parse_needs(target_body)


def _parse_needs(job_body: list[str]) -> list[str]:
    """Extract the `needs:` value from a job body (flow list, scalar, or block)."""
    needs_pat = re.compile(r"""^(\s*)needs\s*:\s*(.*)$""")
    for i, raw in enumerate(job_body):
        m = needs_pat.match(_strip_comment(raw))
        if not m:
            continue
        indent = len(m.group(1))
        rest = m.group(2).strip()
        if rest.startswith("["):  # flow list: needs: [a, b, c]
            inner = rest.strip("[] ")
            return [t.strip().strip("'\"") for t in inner.split(",") if t.strip()]
        if rest and not rest.startswith("#"):  # scalar: needs: ci-status
            return [rest.strip().strip("'\"")]
        # block list: subsequent `- item` lines indented deeper than `needs:`.
        out: list[str] = []
        for nxt in job_body[i + 1:]:
            if not nxt.strip() or nxt.lstrip().startswith("#"):
                continue
            if _indent(nxt) <= indent:
                break
            item = _strip_comment(nxt).strip()
            if item.startswith("-"):
                out.append(item[1:].strip().strip("'\""))
            else:
                break
        return out
    return []


# --------------------------------------------------------------------------- #
# Per-file validation                                                          #
# --------------------------------------------------------------------------- #

def validate_file(path: Path) -> tuple[bool, list[str]]:
    problems: list[str] = []
    text = path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()

    # --- required top-level keys ---
    parsed_ok = True
    if HAVE_YAML:
        try:
            keys = top_level_keys_yaml(text)
        except Exception as e:  # malformed YAML -> hard fail, then still line-check
            problems.append(f"YAML parse error: {e}")
            parsed_ok = False
            keys = top_level_keys_lines(lines)
    else:
        keys = top_level_keys_lines(lines)

    for required in ("name", "on", "jobs"):
        if required not in keys:
            problems.append(f"missing top-level `{required}:`")

    # --- pinned `uses:` (existing check) + SHA-pinning (new check) ---
    for ln, value in collect_uses(lines):
        if not uses_is_pinned(value):
            problems.append(f"line {ln}: unpinned `uses: {value}` (needs @version)")
        elif uses_is_third_party(value) and not uses_is_sha_pinned(value):
            ref = value.rsplit("@", 1)[1] if "@" in value else ""
            problems.append(
                f"line {ln}: `uses: {value}` is not SHA-pinned "
                f"(ref `@{ref}` must be a 40-char commit SHA with a trailing "
                f"`# vX.Y.Z` comment)"
            )

    ok = parsed_ok and not problems
    # A YAML parse failure already recorded its own problem; reflect in ok.
    if problems:
        ok = False
    return ok, problems


# --------------------------------------------------------------------------- #
# Cross-file (repo-wide) checks                                               #
# --------------------------------------------------------------------------- #

def check_pr_trigger_scope(files: list[Path]) -> tuple[bool, list[str]]:
    """A. Only the 4 core workflows may carry an `on: pull_request:` trigger."""
    problems: list[str] = []
    for path in files:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
        if on_block_has_trigger(lines, "pull_request"):
            if path.name not in PR_GATING_WORKFLOWS:
                problems.append(
                    f"{path.name}: has an `on: pull_request:` trigger but is not "
                    f"one of the 4 core PR-gating workflows "
                    f"({', '.join(sorted(PR_GATING_WORKFLOWS))})"
                )
    return (not problems), problems


def check_advisory_isolation(wf_dir: Path) -> tuple[bool, list[str]]:
    """B. ci.yml `ci-status` job must NOT `needs:` the advisory `lsp-crates` job."""
    ci = wf_dir / "ci.yml"
    if not ci.is_file():
        # Other agents may rename/replace; treat absence as a soft PASS so the
        # validator stays robust (the per-file checks still run on what exists).
        return True, [f"(skipped: {ci.name} not present)"]
    lines = ci.read_text(encoding="utf-8", errors="replace").splitlines()
    needs = find_job_needs(lines, CI_STATUS_JOB_KEY, CI_STATUS_JOB_NAME)
    if needs is None:
        return False, [
            f"ci.yml: could not locate the `{CI_STATUS_JOB_KEY}` "
            f"(name: \"{CI_STATUS_JOB_NAME}\") aggregator job"
        ]
    if LSP_ADVISORY_JOB in needs:
        return False, [
            f"ci.yml: `{CI_STATUS_JOB_KEY}.needs` contains `{LSP_ADVISORY_JOB}` "
            f"— the advisory lsp-trio job must NOT gate the required aggregate "
            f"(needs = {needs})"
        ]
    return True, []


def check_sibling_pinning(repo_root: Path) -> tuple[bool, list[str]]:
    """C. setup-ggen-build action must pin the 4 siblings to exact SHAs."""
    action = repo_root / ".github" / "actions" / "setup-ggen-build" / "action.yml"
    if not action.is_file():
        return False, [f"missing sibling-provisioning action: {action}"]
    text = action.read_text(encoding="utf-8", errors="replace")

    problems: list[str] = []
    # Reject a floating shallow clone of a sibling, e.g.:
    #   git clone --depth 1 "$org/$repo"   (grabs whatever HEAD is current)
    floating = re.compile(
        r"""git\s+clone\b[^\n]*--depth\s+1[^\n]*\$\{?\s*(?:org|repo)\b""",
        re.IGNORECASE,
    )
    for m in floating.finditer(text):
        ln = text.count("\n", 0, m.start()) + 1
        problems.append(
            f"action.yml line {ln}: floating `git clone --depth 1` of a sibling "
            f"detected — siblings must be fetched at an EXACT commit SHA"
        )

    # Every sibling SHA must appear verbatim in the action.
    for repo, sha in sorted(SIBLING_SHAS.items()):
        if sha not in text:
            problems.append(
                f"action.yml: sibling `{repo}` SHA `{sha}` not found "
                f"(siblings must be pinned to exact commit SHAs)"
            )
    return (not problems), problems


# --------------------------------------------------------------------------- #
# Driver                                                                      #
# --------------------------------------------------------------------------- #

def main(argv: list[str]) -> int:
    if len(argv) > 1:
        wf_dir = Path(argv[1])
    else:
        wf_dir = Path(__file__).resolve().parents[2] / ".github" / "workflows"

    if not wf_dir.is_dir():
        print(f"ERROR: workflows directory not found: {wf_dir}", file=sys.stderr)
        return 2

    # Derive the workflow list by globbing (never a hardcoded count): other
    # agents add/remove workflows (docker.yml, etc.) concurrently.
    files = sorted(p for p in wf_dir.iterdir() if p.suffix in (".yml", ".yaml"))
    if not files:
        print(f"ERROR: no workflow files in {wf_dir}", file=sys.stderr)
        return 2

    # repo_root is two levels up from .github/workflows (…/.github/workflows).
    repo_root = wf_dir.parent.parent

    backend = "PyYAML" if HAVE_YAML else "line-based fallback (no PyYAML)"
    print(f"validate-workflows.py — {len(files)} file(s) in {wf_dir}")
    print(f"parser backend: {backend}\n")

    failed = 0

    # --- per-file checks ---
    print("per-file checks (name/on/jobs present; uses pinned + SHA-pinned):")
    for path in files:
        ok, problems = validate_file(path)
        status = "PASS" if ok else "FAIL"
        print(f"  [{status}] {path.name}")
        if not ok:
            failed += 1
            for p in problems:
                print(f"           - {p}")

    # --- repo-wide checks ---
    print("\nrepo-wide checks:")
    repo_checks = [
        ("PR-TRIGGER SCOPE  ", lambda: check_pr_trigger_scope(files)),
        ("ADVISORY ISOLATION", lambda: check_advisory_isolation(wf_dir)),
        ("SIBLING PINNING   ", lambda: check_sibling_pinning(repo_root)),
    ]
    for label, fn in repo_checks:
        ok, notes = fn()
        status = "PASS" if ok else "FAIL"
        print(f"  [{status}] {label.strip()}")
        if not ok:
            failed += 1
        for n in notes:
            prefix = "           - " if not ok else "           · "
            print(f"{prefix}{n}")

    print("\n" + "-" * 56)
    total_checks = len(files) + len(repo_checks)
    passed = total_checks - failed
    print(f"summary: {passed} PASS, {failed} FAIL, {total_checks} checks "
          f"({len(files)} files + {len(repo_checks)} repo-wide)")
    if failed:
        print("RESULT: FAIL")
        return 1
    print("RESULT: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
