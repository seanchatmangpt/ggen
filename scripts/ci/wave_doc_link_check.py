#!/usr/bin/env python3
"""Wave-doc link checker (v26.10.20 lane H12).

Verifies that every repo-file reference in the wave docs resolves to a real
path on disk. Checked surfaces:
  - markdown links [text](target)  (relative targets, resolved doc-dir first,
    then repo root)
  - backtick `path/to/file.ext` tokens (absolute or repo-relative; strips
    `:line` / `::item` suffixes)
  - file: IRIs (literal path portion only)

Exit 0 = all references resolve. Exit 1 = at least one broken reference.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

GGEN = Path("/Users/sac/ggen")
GRAPHLAW = Path("/Users/sac/graphlaw")

DOCS = sorted((GGEN / "docs/jira/v26.10.20").glob("*.md")) + [
    GRAPHLAW / "docs/WASM.md",
    GRAPHLAW / "docs/DIALECTS.md",
]
# CHANGELOG entries for this wave only (per-line ranges avoid auditing history).
CHANGELOG_SECTIONS = [
    (GGEN / "CHANGELOG.md", 8, 92),
    (GRAPHLAW / "CHANGELOG.md", 10, 75),
]

# Path-like token: contains a slash, final component has an extension or the
# token ends in "/" (directory). No spaces, no globs, not a URL.
PATH_RE = re.compile(
    r"(?<![\w./-])"
    r"(?:/|~/?|\.{1,2}/)?"
    r"[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)+/?"  # at least one internal slash
)
MD_LINK_RE = re.compile(r"\[[^\]]*\]\(([^)\s]+)\)")
FILE_IRI_RE = re.compile(r"file://[^\s<>\"'`)]+")
BACKTICK_RE = re.compile(r"`([^`\n]+)`")

SUFFIX_RE = re.compile(r"(:\d+(?:-\d+|:\d+)?)$")
SKIP_EXTENSIONS = {".png", ".jpg", ".svg"}  # none expected; reserved
URL_PREFIXES = ("http://", "https://", "mailto:", "#")


def clean_token(tok: str) -> str | None:
    tok = tok.strip().strip('"\'')
    if not tok or any(tok.startswith(p) for p in URL_PREFIXES):
        return None
    if "*" in tok or "<" in tok or "{" in tok or "$" in tok:
        return None
    # build-output paths (target/...) are documented command outputs, not
    # committed-state claims — they legitimately may not exist between builds
    if tok == "target" or tok.startswith("target/") or "/target/" in tok:
        return None
    # strip trailing sentence punctuation
    tok = tok.rstrip(".,;:!?)]}")
    # strip :line / :line-line suffixes (file.rs:52, write.rs:393)
    while True:
        m = SUFFIX_RE.search(tok)
        if not m:
            break
        tok = tok[: m.start()]
    # strip ::item suffixes (test names, module paths)
    if "::" in tok:
        tok = tok.split("::")[0]
    if not tok or "/" not in tok:
        return None
    last = tok.rstrip("/").rsplit("/", 1)[-1]
    is_dir = tok.endswith("/")
    if not is_dir and "." not in last:
        return None
    if not is_dir and last.startswith("."):
        return None
    return tok


def extract_spans(text: str) -> list[str]:
    """Backtick spans, with intra-span newlines+indent normalized (wrapped
    paths in CHANGELOG entries rejoin as one token candidate)."""
    out = []
    for m in BACKTICK_RE.finditer(text):
        span = re.sub(r"\s+", " ", m.group(1))
        out.append(span)
        out.extend(span.split())  # also try individual words
    return out


def candidates(text: str) -> list[str]:
    toks: list[str] = []
    for m in MD_LINK_RE.finditer(text):
        t = m.group(1)
        if not any(t.startswith(p) for p in URL_PREFIXES) and not t.startswith("#"):
            toks.append(t)
    for span in extract_spans(text):
        t = clean_token(span)
        if t:
            toks.append(t)
    # bare (unbackticked) absolute paths, e.g. /Users/sac/graphlaw/... in prose
    for m in re.finditer(r"/Users/sac/[A-Za-z0-9_./-]+", text):
        t = clean_token(m.group(0))
        if t:
            toks.append(t)
    return toks


def resolve(tok: str, doc: Path, repo: Path) -> Path | None:
    """Return an existing Path, or None if unresolvable."""
    p = Path(tok)
    if p.is_absolute() or tok.startswith("~/"):
        exp = Path(tok.replace("~", str(Path.home()), 1)) if tok.startswith("~") else p
        return exp if exp.exists() else None
    for base in (doc.parent, repo, GRAPHLAW if repo == GGEN else GGEN):
        cand = (base / tok).resolve()
        if cand.exists():
            return cand
    return None


def main() -> int:
    checked = 0
    broken: list[str] = []
    entries: list[tuple[Path, Path, str]] = []  # (doc, repo, text)

    for doc in DOCS:
        if doc.exists():
            entries.append((doc, GGEN if doc.is_relative_to(GGEN) else GRAPHLAW, doc.read_text()))

    for path, lo, hi in CHANGELOG_SECTIONS:
        lines = path.read_text().splitlines()
        entries.append((path, GGEN if path.is_relative_to(GGEN) else GRAPHLAW, "\n".join(lines[lo - 1 : hi])))

    for doc, repo, text in entries:
        # file: IRIs (TTL docs would land here too if ever added)
        for m in FILE_IRI_RE.finditer(text):
            checked += 1
            target = m.group(0)[len("file://") :]
            # tolerate file://host/path vs file:///path
            if not Path(target).exists() and not Path("/" + target.lstrip("/")).exists():
                broken.append(f"{doc}: file IRI missing: {m.group(0)}")
        for tok in candidates(text):
            checked += 1
            if resolve(tok, doc, repo) is None:
                broken.append(f"{doc}: {tok}")

    print(f"wave_doc_link_check: {checked} references checked across "
          f"{len(entries)} doc surfaces")
    if broken:
        print(f"BROKEN ({len(broken)}):")
        for b in broken:
            print(f"  {b}")
        return 1
    print("OK: all references resolve")
    return 0


if __name__ == "__main__":
    sys.exit(main())
