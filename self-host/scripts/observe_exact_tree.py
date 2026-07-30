#!/usr/bin/env python3
"""Canonical exact-tree executor for the ggen repository observer.

The model implementation lives in observe_repository.py. This executor replaces its
filesystem byte reader with Git semantics before invoking it: symlinks are observed as
their tracked link target bytes, and gitlinks/submodule directories as empty content
(their commit identity is already represented by the exact revision and index path).
No ambient target file is followed.
"""
from __future__ import annotations

import argparse
import importlib.util
import os
from pathlib import Path

MODULE_PATH = Path(__file__).with_name("observe_repository.py")
SPEC = importlib.util.spec_from_file_location("ggen_self_observer_model", MODULE_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load observer model at {MODULE_PATH}")
MODEL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODEL)


def git_semantic_bytes(root: Path, rel: str) -> bytes:
    path = root / rel
    try:
        if path.is_symlink():
            return os.readlink(path).encode("utf-8")
        if path.is_dir():
            return b""
        return path.read_bytes()
    except OSError:
        return b""


MODEL.read_bytes = git_semantic_bytes
observe = MODEL.observe
emit_turtle = MODEL.emit_turtle
write_outputs = MODEL.write_outputs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, help="repository root")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    root = (args.root or Path(__file__).resolve().parents[2]).resolve()
    return write_outputs(root, observe(root), args.check)


if __name__ == "__main__":
    raise SystemExit(main())
