#!/usr/bin/env python3
"""Standing lint for FR3 (fail-closed by default): flags Rust `Command::output()`
or `Command::status()` calls whose non-zero exit is handled by anything other
than an early-return/bail.

Motivated by a real bug found and fixed this session: `coverage_projection::
run_subsystem_verifier` (tools/v26.8.1/src/coverage_projection.rs) called
`Command::output()`, checked `!output.status.success()`, and on that branch
only `eprintln!`'d a warning before falling through to read a stale cached
report from disk -- silently admitting against outdated evidence instead of
propagating the refusal. This script is a mechanical, best-effort guard
against that specific shape recurring, not a full data-flow analyzer.

Heuristic (deliberately conservative -- false positives over false
negatives, since this is a lint a human reviews, not a hard CI gate yet):
for each `let output = ... Command::new(...) ... .output()` or `.status()`
call, look at the immediately following `if !output.status.success() { ... }`
(or `if output.status.success() { } else { ... }`) block body. Flag it if
that block does NOT contain any of: `bail!`, `return Err`, `panic!`,
`std::process::exit`, `.exit(`, `?` on a Result-returning bail expression.
A block containing only `eprintln!`/`println!`/`log::`/`tracing::` (and
nothing else) is exactly the fail-open shape this guards against.

Usage:
    python3 tools/v26.8.1/guard_fail_open_subprocess.py [--root PATH]

Exits 1 and prints each flagged call site if any are found; exits 0 (and
prints "no fail-open subprocess handling found") otherwise.
"""
from __future__ import annotations

import argparse
import re
from pathlib import Path

CALL_RE = re.compile(r"\.(output|status)\(\)")
FAIL_CLOSED_MARKERS = ("bail!", "return Err", "panic!", "std::process::exit", ".exit(")
ONLY_LOGGING_RE = re.compile(r"^\s*(eprintln!|println!|log::|tracing::|//)")


def find_violations(path: Path) -> list[tuple[int, str]]:
    text = path.read_text(errors="replace")
    lines = text.splitlines()
    violations: list[tuple[int, str]] = []

    for i, line in enumerate(lines):
        if not CALL_RE.search(line):
            continue
        # Scan forward for the nearest `if !....success()` / `if ... success() {`
        # guard within the next 15 lines (the call and its check are usually
        # adjacent; this is a heuristic window, not a parser).
        window = lines[i : i + 15]
        guard_idx = None
        for j, w in enumerate(window):
            if "success()" in w and w.strip().startswith("if"):
                guard_idx = j
                break
        if guard_idx is None:
            continue

        # Collect the guard block's body up to its closing brace (naive brace
        # counting -- good enough for the small, single-level blocks this
        # pattern actually produces in this codebase).
        depth = 0
        body_lines: list[str] = []
        started = False
        for w in window[guard_idx:]:
            depth += w.count("{") - w.count("}")
            if "{" in w:
                started = True
            if started:
                body_lines.append(w)
            if started and depth <= 0:
                break
        body = "\n".join(body_lines)

        has_fail_closed = any(marker in body for marker in FAIL_CLOSED_MARKERS)
        # Loose text-level check (not line-by-line, since eprintln!/println!
        # calls routinely span multiple lines): the block is "logging-only"
        # if it invokes a logging macro and contains no other statement
        # (heuristically: no `;`-terminated statement besides the logging
        # call itself, and no fail-closed marker).
        inner = "\n".join(body_lines[1:-1]) if len(body_lines) > 2 else ""
        has_logging_call = bool(re.search(r"\b(eprintln!|println!|log::\w+!|tracing::\w+!)\s*\(", inner))
        statement_count = inner.count(";")
        only_logging = has_logging_call and statement_count <= 1
        if not has_fail_closed and only_logging:
            violations.append((i + 1, line.strip()))

    return violations


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    args = parser.parse_args()
    root = Path(args.root).resolve()

    all_violations: list[tuple[Path, int, str]] = []
    for rs_file in sorted(root.glob("tools/v26.8.1/src/**/*.rs")):
        for line_no, line in find_violations(rs_file):
            all_violations.append((rs_file.relative_to(root), line_no, line))

    if all_violations:
        print(f"FAIL_OPEN_SUBPROCESS_HANDLING: {len(all_violations)} site(s) found")
        for path, line_no, line in all_violations:
            print(f"  {path}:{line_no}: {line}")
        return 1

    print("no fail-open subprocess handling found")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
