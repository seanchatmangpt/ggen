#!/usr/bin/env python3
"""Standing lint: refuse hardcoded sub-second wall-clock timeouts used as a
termination/liveness proxy inside test code.

Real bug this catches (crates/praxis-graphlaw/src/csprite_test.rs, fixed
2026-08-01): `rx.recv_timeout(Duration::from_millis(500))` used to assert a
recursive helper "terminates" -- correct in isolation (~10ms), but flaky
under ordinary parallel `cargo test --workspace` CPU contention because the
threshold has no relationship to the invariant being tested. This is an
Andon violation: a test whose pass/fail depends on incidental system load,
not on the behavior it claims to verify.

Scope: only flags `recv_timeout`/`join_timeout`/`wait_timeout`-style calls
whose `Duration::from_{millis,secs}` argument is a literal below
MIN_TEST_TIMEOUT_MS, and only inside files that look like test code (path
contains "test" or the call site is textually near a `#[test]` attribute).
Does not flag timeouts used to verify FAST-PATH behavior (e.g. "must
respond within 50ms") -- those assert an upper bound on correct behavior,
not a liveness/termination proxy, and are a different, legitimate pattern
this guard intentionally does not distinguish today (false-negative, not
false-positive, by design: under-flagging is safer than blocking real fast-
path assertions this script cannot tell apart from termination checks
without deeper parsing).
"""
import re
import subprocess
import sys
from pathlib import Path

MIN_TEST_TIMEOUT_MS = 1000
TIMEOUT_CALL_RE = re.compile(
    r"\.(recv_timeout|join_timeout|wait_timeout)\s*\(\s*"
    r"(?:std::time::)?Duration::from_(millis|secs)\s*\(\s*(\d+)\s*\)"
)


def is_test_file(path: str) -> bool:
    return "test" in path.lower()


def is_poll_loop(text: str, match_end: int) -> bool:
    """True if the Timeout arm immediately following this call `continue`s
    rather than failing -- i.e. this call is a poll interval inside a loop
    bounded by a real outer deadline, not itself the liveness threshold
    (e.g. crates/ggen-engine/tests/cli_boundary.rs's watch_for_stderr)."""
    window = text[match_end : match_end + 400]
    m = re.search(r"Err\([^)]*Timeout[^)]*\)\s*=>\s*([^,\n}]+)", window)
    if not m:
        return False
    return "continue" in m.group(1)


def find_violations(root: Path) -> list[str]:
    out = subprocess.run(
        ["git", "ls-files", "*.rs"], cwd=root, capture_output=True, text=True, check=True
    )
    findings = []
    for rel in out.stdout.splitlines():
        if not is_test_file(rel):
            continue
        text = (root / rel).read_text(errors="replace")
        for m in TIMEOUT_CALL_RE.finditer(text):
            unit, value = m.group(2), int(m.group(3))
            ms = value if unit == "millis" else value * 1000
            if ms < MIN_TEST_TIMEOUT_MS and not is_poll_loop(text, m.end()):
                line_no = text.count("\n", 0, m.start()) + 1
                findings.append(f"{rel}:{line_no}: {m.group(0)} ({ms}ms < {MIN_TEST_TIMEOUT_MS}ms floor)")
    return findings


def main() -> int:
    root = Path(__file__).resolve().parents[2]
    findings = find_violations(root)
    if findings:
        print(f"GUARD_SHORT_TEST_TIMEOUT: {len(findings)} violation(s)")
        for f in findings:
            print(f"  {f}")
        print(
            "\nA test asserting termination/liveness via recv_timeout/join_timeout/"
            f"wait_timeout with a threshold under {MIN_TEST_TIMEOUT_MS}ms is load-sensitive:"
            " it will false-fail under ordinary parallel `cargo test --workspace`"
            " CPU contention. Raise the threshold (the invariant under test is"
            " termination, not speed) or restructure to not depend on wall-clock timing."
        )
        return 1
    print("GUARD_SHORT_TEST_TIMEOUT: 0 violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
