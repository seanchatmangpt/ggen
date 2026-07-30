#!/usr/bin/env python3
"""Canonical ggen self-host observer.

Pipeline:
  raw repository census
  -> Git-object byte semantics
  -> live authority and load-path normalization
  -> Gall program/checkpoint/work-item projection
  -> deterministic RDF/JSON/receipt outputs
"""
from __future__ import annotations

import argparse
import importlib.util
from pathlib import Path
from types import ModuleType


def load(name: str, path: Path) -> ModuleType:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {name} from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


SCRIPTS = Path(__file__).resolve().parent
EXACT = load("ggen_self_exact_tree", SCRIPTS / "observe_exact_tree.py")
NORMALIZER = load("ggen_self_normalizer", SCRIPTS / "normalize_observation.py")
MODEL = EXACT.MODEL
write_outputs = EXACT.write_outputs


def observe(root: Path) -> dict[str, object]:
    return NORMALIZER.normalize(EXACT.observe(root), MODEL)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, help="repository root")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    root = (args.root or Path(__file__).resolve().parents[2]).resolve()
    return write_outputs(root, observe(root), args.check)


if __name__ == "__main__":
    raise SystemExit(main())
