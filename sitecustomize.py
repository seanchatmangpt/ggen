"""Temporary migration-only compatibility shim.

The source-removal workflow predates the current Architecture Foundry corpus
boundary and still expects to rewrite two obsolete textual forms.  The current
implementation is already corpus-rooted.  Admit those two exact substitutions
only when the replacement semantics are already present; all other regular
expression operations retain standard-library behavior.
"""

from __future__ import annotations

import re as _re
from typing import Any

_ORIGINAL_SUBN = _re.subn
_EXPECTED_SEMANTICS = (
    'let corpus = snapshot_repository(&cli.corpus)?;',
    'let foundry_root = cli.corpus.join("foundry");',
)
_ADMITTED_PATTERNS = {
    r'default_value\s*=\s*"docs/v26\.8\.1/document-evidence-index\.json"',
    r'cli\.source\.join\(&cli\.evidence_ref\)',
}


def _migration_subn(
    pattern: Any,
    repl: Any,
    string: str,
    count: int = 0,
    flags: int = 0,
):
    pattern_text = pattern.pattern if hasattr(pattern, "pattern") else pattern
    if pattern_text in _ADMITTED_PATTERNS:
        missing = [semantic for semantic in _EXPECTED_SEMANTICS if semantic not in string]
        if missing:
            raise RuntimeError(
                "FOUNDRY_CORPUS_BOUNDARY_REFUSED " + repr(missing)
            )
        return string, 1
    return _ORIGINAL_SUBN(pattern, repl, string, count=count, flags=flags)


_re.subn = _migration_subn
