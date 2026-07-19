#!/usr/bin/env python3
"""actuate_escalation.py -- L5-push: a SHIPPED actuation script fulfilling
Matrix 2's Actuation-closure L3 bar ("A shipped actuation script fires on
derivation and its effect is receipted").

WHAT THIS DOES, PRECISELY: loads ontology.ttl + hook.ttl + one input
Turtle file (a fixture or a real capture-script output), extracts every
`kh:Action`'s `kh:query` CONSTRUCT text VERBATIM from hook.ttl (never
re-typed -- the same discipline
`crates/praxis-graphlaw/tests/self_monitoring_real_session_actuation.rs`'s
`extract_action_construct_query` already established for this pack),
executes each CONSTRUCT against the combined graph using `rdflib`'s own
real, standards-compliant SPARQL 1.1 engine (this pack's established
second-engine cross-validation convention -- see hook.ttl's header and
README's Stage 3 for why `rdflib`/`oxigraph` and `praxis_graphlaw::
TripleStore` are treated as independent checks on the same derivation, not
duplicated effort), and writes ONE JSON receipt recording: which hooks
fired, how many rows each derived, a SHA-256 hash of the derived triples
(a real, standard hash -- NOT claimed equivalent to this repo's BLAKE3
receipt chain, which is `crates/ggen-engine`/`crates/praxis-core`
infrastructure this pack-level script does not touch), and a wall-clock
timestamp.

REFUSAL PATHS ARE RECEIPTED TOO (Matrix 2's L5 bar's own phrase, partially
addressed here): if hook.ttl or the input file fails to parse, or a
CONSTRUCT query fails to execute, this script still writes a receipt --
with `"outcome": "refused"` and the real error message -- rather than
crashing silently or leaving no trace. See "still not L5" in the pack's
final scoring for why this is disclosed as a real step toward, not the
full realization of, the L5 bar's "unattended ... with refusal paths
receipted too" (this script must still be invoked by a human/CI job; there
is no live mid-session trigger -- see README's existing "does NOT prove"
section, unchanged by this pass).

USAGE:
    python3 scripts/actuate_escalation.py --input fixtures/pattern-fires.ttl \
        --receipt-dir .smon/receipts
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
import time
from pathlib import Path

try:
    import rdflib
    from rdflib.plugins.sparql import prepareQuery
except ImportError:  # pragma: no cover
    print("ERROR: rdflib is required (pip install rdflib)", file=sys.stderr)
    sys.exit(1)

KH = rdflib.Namespace("http://seanchatmangpt.github.io/praxis/kh#")
PACK_DIR = Path(__file__).resolve().parent.parent


def extract_hook_actions(hook_ttl_path: Path) -> list[tuple[str, str]]:
    """Parse hook.ttl as RDF and return [(action_iri, construct_query_text), ...]
    for every kh:Action -- extracted VERBATIM from the kh:query literal,
    never re-typed, mirroring
    crates/praxis-graphlaw/tests/self_monitoring_real_session_actuation.rs's
    extract_action_construct_query discipline.
    """
    g = rdflib.Graph()
    g.parse(str(hook_ttl_path), format="turtle")
    actions = []
    for action in g.subjects(rdflib.RDF.type, KH.Action):
        for query_lit in g.objects(action, KH.query):
            actions.append((str(action), str(query_lit)))
    return actions


def run_actuation(input_path: Path) -> dict:
    ontology_path = PACK_DIR / "ontology.ttl"
    hook_path = PACK_DIR / "hook.ttl"

    receipt: dict = {
        "actuated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "input_file": str(input_path),
        "hook_file": str(hook_path),
        "engine": "rdflib (standards-compliant SPARQL 1.1, second-engine cross-check per this pack's Stage 3 convention)",
        "outcome": "actuated",
        "hooks": [],
    }

    try:
        data = rdflib.Graph()
        data.parse(str(ontology_path), format="turtle")
        data.parse(str(input_path), format="turtle")
    except Exception as exc:  # noqa: BLE001 -- refusal path must be receipted, not crash
        receipt["outcome"] = "refused"
        receipt["refusal_reason"] = f"failed to parse ontology/input: {exc}"
        return receipt

    try:
        actions = extract_hook_actions(hook_path)
    except Exception as exc:  # noqa: BLE001
        receipt["outcome"] = "refused"
        receipt["refusal_reason"] = f"failed to parse hook.ttl: {exc}"
        return receipt

    total_derived_triples: list[tuple] = []
    any_hard_failure = False
    for action_iri, query_text in actions:
        hook_result: dict[str, object] = {"action": action_iri}
        try:
            q = prepareQuery(query_text)
            result_graph = data.query(q).graph
            rows = list(result_graph) if result_graph is not None else []
            hook_result["fired"] = len(rows) > 0
            hook_result["derived_triple_count"] = len(rows)
            total_derived_triples.extend(
                tuple(str(t) for t in triple) for triple in rows
            )
        except Exception as exc:  # noqa: BLE001 -- one hook's failure is a refusal, not a crash
            hook_result["fired"] = False
            hook_result["error"] = str(exc)
            any_hard_failure = True
        receipt["hooks"].append(hook_result)

    if any_hard_failure:
        receipt["outcome"] = "partially_refused"

    # Deterministic hash of the derived triples (sorted so ordering doesn't
    # affect the hash) -- SHA-256, real and standard, NOT claimed equivalent
    # to this repo's BLAKE3 receipt chain (see module docstring).
    canonical = "\n".join(sorted("|".join(t) for t in total_derived_triples))
    receipt["derived_triples_sha256"] = hashlib.sha256(canonical.encode("utf-8")).hexdigest()
    receipt["total_derived_triples"] = len(total_derived_triples)
    receipt["any_hook_fired"] = any(h.get("fired") for h in receipt["hooks"])

    return receipt


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--input", required=True, type=Path, help="Turtle file to actuate against (a fixture or a real capture-script output)")
    ap.add_argument("--receipt-dir", type=Path, default=Path(".smon/receipts"))
    args = ap.parse_args()

    receipt = run_actuation(args.input)

    args.receipt_dir.mkdir(parents=True, exist_ok=True)
    stem = args.input.stem
    ts = receipt["actuated_at"].replace(":", "").replace("-", "")
    receipt_path = args.receipt_dir / f"{stem}-{ts}.json"
    receipt_path.write_text(json.dumps(receipt, indent=2) + "\n")

    print(json.dumps(receipt, indent=2))
    print(f"\nreceipt written: {receipt_path}", file=sys.stderr)

    return 0 if receipt["outcome"] == "actuated" else 1


if __name__ == "__main__":
    raise SystemExit(main())
