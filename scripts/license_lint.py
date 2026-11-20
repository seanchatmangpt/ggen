#!/usr/bin/env python3
"""license-lint gate (FNOX-SJ-LICN-5). Fail-closed, stdlib only.

Compares the declared license of the repo under --root (default: parent of this
script's directory, the ggen checkout) with docs/license/matrix.ttl. Without --root
the other matrix rows are also checked against their evidencePath files.

Exit 0: all checks pass. Exit 1: mismatch, missing file, missing matrix row, or any
internal error. Diagnostics go to stderr; per-repo results go to stdout.

Cargo.toml `[package.metadata.deb] license-file = ["LICENSE", "4"]` is cargo-deb
packaging metadata (path, number of lines to skip), not a license declaration.
No rule is encoded for it; only the top-level `license = "..."` key is read.
"""
import argparse
import os
import re
import sys

FAILURES = []


def fail(msg):
    FAILURES.append(msg)
    print("FAIL: " + msg, file=sys.stderr)


def parse_matrix(path):
    """Minimal parser for the lic:RepoLicense blocks of matrix.ttl."""
    with open(path, encoding="utf-8") as fh:
        text = fh.read()
    rows = {}
    for block in re.split(r"\n\s*\n", text):
        m = re.search(r"^lic:([A-Za-z0-9_.-]+)\s+a\s+lic:RepoLicense", block, re.M)
        if not m:
            continue
        label = re.search(r'rdfs:label\s+"([^"]*)"', block)
        lic = re.search(r'lic:license\s+"([^"]*)"', block)
        kind = re.search(r'lic:identifierKind\s+"([^"]*)"', block)
        ev = re.search(r"lic:evidencePath\s+(.*?)(?:;\s*\n\s*dcterms:|\.\s*$)", block, re.S)
        paths = re.findall(r'"([^"]*)"', ev.group(1)) if ev else []
        name = label.group(1) if label else m.group(1)
        if not lic:
            fail("matrix row %s has no lic:license" % name)
            continue
        rows[name] = {
            "license": lic.group(1),
            "kind": kind.group(1) if kind else "",
            "evidence": paths,
        }
    return rows


def cargo_license(cargo_path):
    with open(cargo_path, encoding="utf-8") as fh:
        for line in fh:
            m = re.match(r'^license\s*=\s*"([^"]*)"\s*(#.*)?$', line)
            if m:
                return m.group(1)
    return None


def read_license_text(path):
    with open(path, encoding="utf-8", errors="replace") as fh:
        return fh.read()


def looks_mit(text):
    return "Permission is hereby granted" in text or "MIT License" in text


def check_root_repo(name, row, root):
    cargo = os.path.join(root, "Cargo.toml")
    lic_file = os.path.join(root, "LICENSE")
    if not os.path.isfile(cargo):
        fail("%s: missing Cargo.toml at %s" % (name, cargo))
        return
    declared = cargo_license(cargo)
    if declared is None:
        fail("%s: no top-level license field in %s" % (name, cargo))
        return
    if declared != row["license"]:
        fail("%s: license mismatch: Cargo.toml declares %r, matrix says %r"
             % (name, declared, row["license"]))
        return
    if not os.path.isfile(lic_file):
        fail("%s: missing LICENSE file at %s" % (name, lic_file))
        return
    if row["license"] == "MIT" and not looks_mit(read_license_text(lic_file)):
        fail("%s: LICENSE text at %s is not MIT text" % (name, lic_file))
        return
    print("OK %s: declared license %s (Cargo.toml + LICENSE)" % (name, declared))


def check_other_repo(name, row):
    ok = True
    for p in row["evidence"]:
        if not os.path.exists(p):
            fail("%s: evidencePath missing: %s" % (name, p))
            ok = False
    lic = row["license"]
    if lic == "UNDECLARED":
        for p in row["evidence"]:
            if os.path.isdir(p):
                found = [f for f in os.listdir(p) if f.upper().startswith("LICENSE")]
                if found:
                    fail("%s: matrix says UNDECLARED but root has %s" % (name, found))
                    ok = False
    elif lic == "MIT":
        for p in row["evidence"]:
            if os.path.basename(p) == "LICENSE" and os.path.isfile(p):
                if not looks_mit(read_license_text(p)):
                    fail("%s: %s is not MIT text" % (name, p))
                    ok = False
    if ok:
        print("OK %s: declared license %s (evidence present)" % (name, lic))


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--matrix", required=True)
    ap.add_argument("--root", default=None)
    ap.add_argument("--repo", default="ggen", help="matrix row the root belongs to")
    args = ap.parse_args()

    if not os.path.isfile(args.matrix):
        fail("matrix not found: %s" % args.matrix)
        return 1
    rows = parse_matrix(args.matrix)
    if not rows:
        fail("matrix has no lic:RepoLicense rows")
        return 1
    if args.repo not in rows:
        fail("no matrix row for repo %s" % args.repo)
        return 1

    explicit_root = args.root is not None
    root = args.root or os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    check_root_repo(args.repo, rows[args.repo], root)
    if not explicit_root:
        for name, row in sorted(rows.items()):
            if name != args.repo:
                check_other_repo(name, row)
    return 1 if FAILURES else 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:  # fail closed
        print("FAIL: internal error: %r" % (exc,), file=sys.stderr)
        sys.exit(1)
