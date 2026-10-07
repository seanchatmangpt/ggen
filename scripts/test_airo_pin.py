#!/usr/bin/env python3
"""W695 pin test: ggen AIRo surface (docs/airo-risk-description.ttl).

Pins, all via real rdflib parse / real filesystem reads (no mocks):
  1. byte-hash + size of docs/airo-risk-description.ttl
  2. triple count: 60 bare, 618 after union with the AIRo vocabulary
  3. cited repo-relative paths exist in the tree

Runner: system python3 pytest (pytest 9.0.3 + rdflib present).
Run: python3 -m pytest scripts/test_airo_pin.py -v
"""

import hashlib
import os
import re

import rdflib
import pytest

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TTL_REL = "docs/airo-risk-description.ttl"
TTL = os.path.join(REPO, TTL_REL)

# Pins observed 2026-10-07 at ggen feat/v26.10.5-release-cut @ bc4d23909
EXPECTED_SHA256 = "7c79b80d1cddb34abe005c8eff2d0052dc6d6c10ef179bc113f1f6c0a188acd8"
EXPECTED_BYTES = 8306
EXPECTED_BARE_TRIPLES = 60
EXPECTED_UNION_TRIPLES = 618
# Vocabulary cache shared with scripts/check_airo.sh (same dependency, same location).
VOCAB_PATH = "/tmp/airo.ttl"

EXPECTED_CITED_PATHS = [
    ".github/workflows/ggen-sync-run-selftest.yml",
    ".github/workflows/ggen-sync-run.yml",
    ".github/workflows/publish-candidate.yml",
    "crates/ggen-engine/src/portable_receipt.rs",
    "justfile",
]


def test_ttl_byte_hash_and_size():
    data = open(TTL, "rb").read()
    assert len(data) == EXPECTED_BYTES
    assert hashlib.sha256(data).hexdigest() == EXPECTED_SHA256


def test_triple_count_bare_rdflib_parse():
    g = rdflib.Graph()
    g.parse(TTL, format="turtle")
    assert len(g) == EXPECTED_BARE_TRIPLES


def test_triple_count_union_with_vocabulary():
    assert os.path.exists(VOCAB_PATH), (
        "AIRo vocabulary cache missing at %s (same dependency as scripts/check_airo.sh); "
        "re-fetch before running this pin" % VOCAB_PATH
    )
    g = rdflib.Graph()
    g.parse(TTL, format="turtle")
    g.parse(VOCAB_PATH, format="turtle")
    assert len(g) == EXPECTED_UNION_TRIPLES


def test_cited_paths_grounded():
    src = open(TTL, encoding="utf-8").read()
    cited = sorted(set(re.findall(r"(?:crates|\.github|justfile)[^ )\",;]*", src)))
    assert cited == EXPECTED_CITED_PATHS
    for p in cited:
        assert os.path.exists(os.path.join(REPO, p)), "cited path missing: %s" % p
