#!/usr/bin/env python3
"""broaden_topic_experiment.py -- a DISCLOSED, CLEARLY-LABELED counterfactual
post-processor, built to answer ADVERSARIAL CHECK (3) from this session's own
Stage 3 task brief: "show what happens with a slightly different topic-tagging
heuristic (does the derivation become more or less accurate) to disclose how
sensitive this proof-of-concept is to the classification heuristic's
quality."

THIS SCRIPT IS NOT THE PACK'S DEFAULT BEHAVIOR. transcript_to_turtle.py's own
CANONICAL_TOPIC_KEYWORDS + classify_turn() are unchanged by this file and
remain the pack's real, disclosed, default heuristic. This script reads an
ALREADY-EMITTED smon: Turtle graph (e.g. fixtures/session-real.ttl) and
applies exactly two narrow, mechanically-justified rewrites, printing a full
audit trail of every triple it changes, to demonstrate -- not silently
assume -- how sensitive hook.ttl's (unmodified) CONSTRUCT derivation is to
the input classification's granularity:

REWRITE 1 (turn-kind broadening): a Turn X where (X smon:immediatelyFollows G)
and G's smon:turnKind is smon:GroundingQuestion, but X's OWN smon:turnKind is
smon:Other, is upgraded to smon:SurveyResponse. Justification: X already
failed transcript_to_turtle.py's RUN_PATTERNS and BLOCKER_PATTERNS checks
(classify_turn()'s priority order checks those FIRST, before SURVEY_PATTERNS
and GROUNDING_PATTERNS -- see that file's classify_turn() docstring); the
turn was checked and found to be neither a real run nor a named blocker. A
reply immediately following a grounding question that is not a run and not a
blocker is, by the pack's own 4-kind taxonomy, some kind of survey/
explanation by elimination -- "Other" under the default heuristic only
because it lacked one of the specific SURVEY_PATTERNS regexes (e.g. a bare
bulleted status recap with no "real_edge"/"crown-witness"/mermaid-fence
marker). This is a real, found gap: this session's real turn-1657 ("**v26.7.13
status:** ... 13 commits this session ...") is exactly this case.

REWRITE 2 (topic-tag broadening): every Turn whose smon:turnKind is
smon:GroundingQuestion has its dcterms:subject replaced with ONE fixed
canonical literal (--canonical-topic, default "e2e-capability-grounding").
Justification: this session's OWN later meta-commentary (turn-1698, a real
turn in this transcript: "the pattern across this conversation was: you'd
ask a grounding question ('does this work end to end?', 'how far has this
evolved?'), and I would answer it accurately but abstractly") explicitly
asserts, after the fact, that repeated grounding questions about
differently-named subsystems (status / CLI-to-swarm / etc.) were restatements
of the SAME underlying question. The default per-noun keyword tagger
(CANONICAL_TOPIC_KEYWORDS) cannot see that meta-pattern; this rewrite encodes
it as a blunt, disclosed, maximally-coarse alternative, specifically to show
the OTHER end of the granularity spectrum from the default's maximally-fine
per-noun tags -- not proposed as a replacement default.

Prints a full diff-style audit: every (turn, old-subject-or-kind,
new-subject-or-kind) triple changed, so nothing here is silent.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

try:
    import rdflib
    from rdflib import Graph, Namespace, Literal, RDF
except ImportError:  # pragma: no cover
    print("ERROR: rdflib is required (pip install rdflib)", file=sys.stderr)
    sys.exit(1)

SMON = Namespace("http://seanchatmangpt.github.io/packs/self-monitoring#")
DCTERMS = Namespace("http://purl.org/dc/terms/")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--in-ttl", required=True, type=Path)
    ap.add_argument("--out-ttl", required=True, type=Path)
    ap.add_argument(
        "--canonical-topic",
        default="e2e-capability-grounding",
        help="single canonical dcterms:subject literal applied to every "
        "GroundingQuestion turn under REWRITE 2",
    )
    args = ap.parse_args()

    g = Graph()
    g.parse(str(args.in_ttl), format="turtle")
    print(f"loaded {args.in_ttl}: {len(g)} triples")

    turn_kind = {s: o for s, o in g.subject_objects(SMON.turnKind)}
    turn_subject = {s: o for s, o in g.subject_objects(DCTERMS.subject)}
    follows = {s: o for s, o in g.subject_objects(SMON.immediatelyFollows)}

    # ---- REWRITE 1: Other -> SurveyResponse when immediately following a
    # GroundingQuestion (see module docstring for the exact justification).
    rewrite1_count = 0
    for turn, prior in sorted(follows.items()):
        if turn_kind.get(prior) != SMON.GroundingQuestion:
            continue
        current = turn_kind.get(turn)
        if current == SMON.Other:
            g.remove((turn, SMON.turnKind, current))
            g.add((turn, SMON.turnKind, SMON.SurveyResponse))
            print(f"REWRITE 1: {turn} turnKind Other -> SurveyResponse (follows GroundingQuestion {prior})")
            rewrite1_count += 1

    # ---- REWRITE 2: collapse every GroundingQuestion turn's dcterms:subject
    # to one canonical literal.
    rewrite2_count = 0
    canonical = Literal(args.canonical_topic)
    for turn, kind in sorted(turn_kind.items()):
        if kind != SMON.GroundingQuestion:
            continue
        old_subject = turn_subject.get(turn)
        if old_subject is not None:
            g.remove((turn, DCTERMS.subject, old_subject))
        g.add((turn, DCTERMS.subject, canonical))
        print(f"REWRITE 2: {turn} dcterms:subject {old_subject!r} -> {canonical!r}")
        rewrite2_count += 1

    args.out_ttl.parent.mkdir(parents=True, exist_ok=True)
    g.serialize(destination=str(args.out_ttl), format="turtle")
    print(f"REWRITE 1 (Other->SurveyResponse after a GroundingQuestion): {rewrite1_count} turns changed")
    print(f"REWRITE 2 (GroundingQuestion topic collapsed to '{args.canonical_topic}'): {rewrite2_count} turns changed")
    print(f"wrote {args.out_ttl}: {len(g)} triples")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
