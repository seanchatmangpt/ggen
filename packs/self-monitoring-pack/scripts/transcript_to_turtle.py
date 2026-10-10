#!/usr/bin/env python3
"""transcript_to_turtle.py -- read a REAL Claude Code session transcript
(.jsonl, the on-disk format under ~/.claude/projects/<project>/<session-id>.jsonl)
and emit smon: Turtle facts conforming to packs/self-monitoring-pack's
ontology.ttl + shapes.ttl.

THIS IS A CAPTURE SCRIPT, NOT A CLASSIFIER (see ontology.ttl's
CLASSIFICATION-IS-INPUT FENCE). The turn-kind classification below is a
disclosed, simple, auditable pattern-matcher over turn TEXT, built for one
purpose -- turning a real transcript into smon: input facts so hook.ttl's
already-proven SPARQL-CONSTRUCT mechanism
(crates/praxis-graphlaw/tests/self_monitoring_hook_actuation.rs) can be
exercised against real conversational data instead of only hand-built
fixtures. It is NOT a general NLP classifier, is not claimed to generalize
past this repo's own conversational patterns, and will misclassify turns a
real NLU system would get right (see CLASSIFY_* pattern lists below for the
exact, complete rule set -- nothing hidden past what is printed here).

SCOPE WIDENING FROM THE LITERAL TASK WORDING (disclosed, not silent):
The task that produced this script says "extract each assistant text turn".
Taken completely literally that would extract ONLY assistant messages. But
ontology.ttl's smon:Turn class has NO actor/role property -- it is
deliberately role-agnostic (see ontology.ttl's smon:Turn rdfs:comment) --
and packs/self-monitoring-pack/fixtures/pattern-fires.ttl's own worked
example models turn-1 (the GroundingQuestion) and turn-4 (the repeat
GroundingQuestion) as the human's questions, with turn-2 (the
SurveyResponse) as Claude's reply. In THIS real transcript, grounding
questions like "does the CLI reach the swarm" are asked by the human, not
by Claude. An assistant-only extraction would therefore structurally NEVER
contain a smon:GroundingQuestion turn and could never demonstrate
hook.ttl's EscalationObligation firing on real data -- which is exactly
what this script exists to demonstrate. So this script extracts BOTH real
human-typed turns (message.content is a string AND origin.kind == "human"
-- this excludes tool-result carrier lines, task-notification injections,
and auto-injected continuation-summary/slash-command lines, all of which
also arrive with type == "user" but with a different origin shape) AND
assistant text turns (message.content blocks of type "text", explicitly
excluding "thinking" and "tool_use" blocks per the task's "not tool calls"
instruction). Pass --assistant-only to get the literal, narrower behavior.

TRANSCRIPT SHAPE (confirmed by reading the real file this session, not
assumed): each line is one JSON object. Lines with type "user" or
"assistant" carry a "message" object. A single logical LLM turn can be
split across MULTIPLE consecutive JSONL lines that share the same
message.id (thinking block, then text block, then tool_use block, each its
own line) -- confirmed by inspecting parentUuid chains and message.id
values directly in the real transcript. This script groups blocks by
message.id before extracting text, so one logical turn is not
double-counted or truncated.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

try:
    import rdflib
    from rdflib import Graph, Namespace, Literal, URIRef, RDF
    from rdflib.namespace import XSD
except ImportError:  # pragma: no cover
    print("ERROR: rdflib is required (pip install rdflib)", file=sys.stderr)
    sys.exit(1)

SMON = Namespace("http://seanchatmangpt.github.io/packs/self-monitoring#")
DFL = Namespace("http://seanchatmangpt.github.io/packs/dogfood-lifecycle#")
PROV = Namespace("http://www.w3.org/ns/prov#")
DCTERMS = Namespace("http://purl.org/dc/terms/")

# ---------------------------------------------------------------------------
# Turn-kind classification -- DISCLOSED, COMPLETE pattern list (see module
# docstring). Checked in this priority order per turn: a turn matching
# multiple categories is assigned the FIRST category below it matches,
# because a cited real run is the strongest, least-ambiguous signal (a reply
# can both run something AND summarize it in a table; the run is what
# counts), a named blocker is the next-strongest deliberate signal, then a
# survey/architecture-map shape, then a grounding question, else Other.
# ---------------------------------------------------------------------------

RUN_PATTERNS = [
    re.compile(r"test result:\s*ok", re.I),
    re.compile(r"\d+\s+passed;\s*\d+\s+failed", re.I),
    re.compile(r"->\s*exit\s+\d", re.I),
    re.compile(r"\bexit\s+code\s*0\b", re.I),
    re.compile(r"\bexit 0\b", re.I),
    re.compile(r"\$\s+(cargo|just|python3?|ggen|git|pytest|npm)\s+\S+.*\n.*\S", re.I),
    re.compile(r"shapes_conform.*true", re.I),
    re.compile(r"\brunning \d+ tests?\b", re.I),
]

BLOCKER_PATTERNS = [
    re.compile(r"\bblocked (on|by)\b", re.I),
    re.compile(r"\bblocker:", re.I),
    re.compile(r"\bcannot proceed (until|because|without)\b", re.I),
    re.compile(r"\brequires? (a |an )?(credential|external approval|manual approval|access grant)\b", re.I),
    re.compile(r"\bthe blocking hop is\b", re.I),
    re.compile(r"\bwaiting on (a |an )?(human|approval|credential)\b", re.I),
    re.compile(r"\bgenuinely underdetermined\b", re.I),
]

SURVEY_PATTERNS = [
    re.compile(r"real_edge", re.I),
    re.compile(r"partial_real_edge", re.I),
    re.compile(r"```mermaid", re.I),
    re.compile(r"crown[- ]witness", re.I),
    re.compile(r"capability[- ]fence", re.I),
    re.compile(r"\|[^\n]+\|\s*\n\s*\|[-:\s|]+\|", re.I),  # markdown table hdr+sep row
    re.compile(r"\barchitecture (survey|map|overview)\b", re.I),
    re.compile(r"\bthis project has moved through\b.{0,40}\bstages?\b", re.I),
]

GROUNDING_PATTERNS = [
    re.compile(r"does (this|it|the \w+).{0,40}\bwork\b", re.I),
    re.compile(r"how far has this (evolved|progressed|come)", re.I),
    re.compile(r"what'?s the status\b", re.I),
    re.compile(r"\bwhat is the status\b", re.I),
    re.compile(r"\bis this (actually |really )?(done|working|complete)\b", re.I),
    re.compile(r"\bdoes the cli\b", re.I),
    re.compile(r"\bend[- ]to[- ]end\?", re.I),
    re.compile(r"\be2e\?", re.I),
    re.compile(r"\bdoes it work\b", re.I),
    re.compile(r"\bcan it go from\b", re.I),
    re.compile(r"\bi want to know if it works\b", re.I),
]


def classify_turn(text: str) -> tuple[str, list[str]]:
    """Return (turnKind concept-name, list of the exact patterns that matched).

    Priority order: RunResponse > BlockerResponse > SurveyResponse >
    GroundingQuestion > Other. See module docstring for why.
    """
    for kind, patterns in (
        ("RunResponse", RUN_PATTERNS),
        ("BlockerResponse", BLOCKER_PATTERNS),
        ("SurveyResponse", SURVEY_PATTERNS),
        ("GroundingQuestion", GROUNDING_PATTERNS),
    ):
        hits = [p.pattern for p in patterns if p.search(text)]
        if hits:
            return kind, hits
    return "Other", []


# ---------------------------------------------------------------------------
# grounding_topic tagging -- DISCLOSED keyword heuristic, not general NLU.
# A fixed set of canonical project-vocabulary keywords is checked against
# the turn text (case-insensitive, word-boundary); every keyword that
# matches is included in the tag, sorted alphabetically and hyphen-joined,
# so the SAME set of real-world nouns in two different phrasings of the same
# question produces the SAME tag (e.g. "does the CLI reach the swarm" and
# "can it go from the CLI to arrazo to global swarm" both match cli+swarm+
# arazzo -> tag "arazzo-cli-swarm"). Note "arrazo" (a real misspelling that
# appears literally in this transcript) is folded into the same "arazzo" key
# via an alternation, since it is the same real-world topic, not a distinct
# one -- disclosed here, not silently normalized. If no canonical keyword
# matches, falls back to the first 3 significant (non-stopword) tokens.
# ---------------------------------------------------------------------------

CANONICAL_TOPIC_KEYWORDS: dict[str, re.Pattern] = {
    "arazzo": re.compile(r"\barr?azzo\b", re.I),
    "swarm": re.compile(r"\bswarm\b", re.I),
    "cli": re.compile(r"\bcli\b", re.I),
    "workflow": re.compile(r"\bworkflow\b", re.I),
    "atomvm": re.compile(r"\batomvm\b", re.I),
    "erlang": re.compile(r"\berlang\b", re.I),
    "crown": re.compile(r"\bcrown\b", re.I),
    "receipt": re.compile(r"\breceipts?\b", re.I),
    "hook": re.compile(r"\bhooks?\b", re.I),
    "soc2": re.compile(r"\bsoc-?2\b", re.I),
    "mfact": re.compile(r"\bmfact\b", re.I),
    "bribery": re.compile(r"\bbribery\b", re.I),
    "powl": re.compile(r"\bpowl\b", re.I),
    "pddl": re.compile(r"\bpddl\b", re.I),
    "ocel": re.compile(r"\bocel\b", re.I),
    "dogfood": re.compile(r"\bdogfood\w*\b", re.I),
    "togaf": re.compile(r"\btogaf\b", re.I),
    "progress": re.compile(r"\b(evolved|evolution|progress(ed)?)\b", re.I),
    "status": re.compile(r"\bstatus\b", re.I),
    "e2e": re.compile(r"\bend[- ]to[- ]end\b|\be2e\b", re.I),
    "cng": re.compile(r"\bcng\b", re.I),
    "swarm-agents": re.compile(r"\bagents?\b", re.I),
}

_STOPWORDS = {
    "the", "a", "an", "is", "it", "this", "that", "does", "do", "did",
    "to", "of", "and", "or", "for", "on", "in", "at", "be", "can", "will",
    "i", "you", "we", "what", "how", "so", "not", "no", "yes", "with",
    "from", "has", "have", "had", "was", "were", "are", "as", "if", "but",
    "want", "know", "go", "get", "let", "me", "my", "your", "actually",
}


def extract_topic(text: str) -> str:
    hits = sorted(key for key, pat in CANONICAL_TOPIC_KEYWORDS.items() if pat.search(text))
    if hits:
        return "-".join(hits)
    words = re.findall(r"[a-zA-Z][a-zA-Z0-9']{2,}", text.lower())
    significant = [w for w in words if w not in _STOPWORDS][:3]
    if significant:
        return "kw-" + "-".join(significant)
    return "unclassified-topic"


# ---------------------------------------------------------------------------
# Transcript parsing
# ---------------------------------------------------------------------------


@dataclass
class Turn:
    seq: int
    role: str  # "user" or "assistant"
    text: str
    uuid: str
    timestamp: str
    kind: str = ""
    evidence: list[str] = field(default_factory=list)
    topic: str = ""


def is_real_human_turn(rec: dict) -> tuple[bool, str]:
    """A genuinely human-typed chat turn: content is a plain string AND
    origin.kind == "human". This deliberately excludes (confirmed by reading
    the real transcript): tool_result carrier lines (content is a list),
    <task-notification>-wrapped lines (origin.kind == "task-notification"),
    auto-injected continuation-summary lines and slash-command lines (both
    arrive with origin == None, no promptSource) -- none of those are the
    human actually typing a question.
    """
    msg = rec.get("message", {})
    content = msg.get("content")
    origin = rec.get("origin") or {}
    if isinstance(content, str) and content.strip() and origin.get("kind") == "human":
        return True, content
    return False, ""


def iter_turns(transcript_path: Path, assistant_only: bool) -> list[Turn]:
    turns: list[Turn] = []
    # Buffer for grouping consecutive assistant blocks sharing message.id.
    pending_msgid: str | None = None
    pending_texts: list[str] = []
    pending_uuid: str = ""
    pending_ts: str = ""

    def flush_assistant():
        nonlocal pending_msgid, pending_texts, pending_uuid, pending_ts
        if pending_msgid is not None:
            text = "\n".join(t for t in pending_texts if t.strip())
            if text.strip():
                turns.append(Turn(seq=-1, role="assistant", text=text, uuid=pending_uuid, timestamp=pending_ts))
        pending_msgid = None
        pending_texts = []
        pending_uuid = ""
        pending_ts = ""

    with transcript_path.open("r", encoding="utf-8", errors="replace") as f:
        for line_no, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                rec = json.loads(line)
            except json.JSONDecodeError:
                continue
            rtype = rec.get("type")

            if rtype == "assistant":
                msg = rec.get("message", {})
                msgid = msg.get("id")
                content = msg.get("content")
                if msgid != pending_msgid:
                    flush_assistant()
                    pending_msgid = msgid
                    pending_uuid = rec.get("uuid", "")
                    pending_ts = rec.get("timestamp", "")
                if isinstance(content, list):
                    for block in content:
                        if isinstance(block, dict) and block.get("type") == "text":
                            pending_texts.append(block.get("text", ""))
                continue

            # Any non-assistant line ends the current assistant message group.
            flush_assistant()

            if rtype == "user" and not assistant_only:
                ok, text = is_real_human_turn(rec)
                if ok:
                    turns.append(
                        Turn(
                            seq=-1,
                            role="user",
                            text=text,
                            uuid=rec.get("uuid", ""),
                            timestamp=rec.get("timestamp", ""),
                        )
                    )
        flush_assistant()

    for i, t in enumerate(turns):
        t.seq = i + 1
        t.kind, t.evidence = classify_turn(t.text)
        t.topic = extract_topic(t.text)
    return turns


# ---------------------------------------------------------------------------
# Turtle emission
# ---------------------------------------------------------------------------


def build_graph(turns: list[Turn], session_id: str, base: str) -> Graph:
    g = Graph()
    g.bind("smon", SMON)
    g.bind("dfl", DFL)
    g.bind("prov", PROV)
    g.bind("dcterms", DCTERMS)

    ns = Namespace(base if base.endswith("#") else base + "#")
    session_iri = ns["session"]
    g.add((session_iri, RDF.type, DFL.Session))
    g.add((session_iri, DCTERMS.identifier, Literal(session_id)))
    g.add((session_iri, DCTERMS.description, Literal(
        "Real Claude Code session transcript, captured by "
        "packs/self-monitoring-pack/scripts/transcript_to_turtle.py."
    )))

    prior_grounding_iri: URIRef | None = None
    for t in turns:
        turn_iri = ns[f"turn-{t.seq}"]
        g.add((turn_iri, RDF.type, SMON.Turn))
        g.add((turn_iri, DCTERMS.isPartOf, session_iri))
        g.add((turn_iri, DCTERMS.subject, Literal(t.topic)))
        g.add((turn_iri, SMON.sequenceIndex, Literal(t.seq, datatype=XSD.integer)))
        g.add((turn_iri, SMON.turnKind, SMON[t.kind]))
        quote = t.text.strip().replace("\n", " ")
        if len(quote) > 220:
            quote = quote[:220] + "..."
        g.add((turn_iri, DCTERMS.description, Literal(f"[{t.role}] {quote}")))

        # immediatelyFollows -- asserted ONLY when the immediately-preceding
        # extracted turn was a GroundingQuestion (matches shapes.ttl's own
        # documented rationale: "only ... responses that directly follow a
        # grounding question need it" -- see shapes.ttl's TurnShape comment
        # and fixtures/pattern-fires.ttl's turn-2, the only turn in that
        # fixture carrying this property).
        if prior_grounding_iri is not None:
            g.add((turn_iri, SMON.immediatelyFollows, prior_grounding_iri))

        prior_grounding_iri = turn_iri if t.kind == "GroundingQuestion" else None

    return g


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--transcript", required=True, type=Path)
    ap.add_argument("--session-id", default=None)
    ap.add_argument("--out", required=True, type=Path)
    ap.add_argument("--base", default=None, help="IRI base (default derived from session id)")
    ap.add_argument("--assistant-only", action="store_true", help="Literal task scope: assistant text turns only")
    ap.add_argument("--topic-filter", default=None, help="Only print (not filter emission) turns whose topic contains this substring, for the demo excerpt")
    args = ap.parse_args()

    session_id = args.session_id or args.transcript.stem
    base = args.base or f"http://seanchatmangpt.github.io/packs/self-monitoring/sessions/{session_id}"

    turns = iter_turns(args.transcript, args.assistant_only)
    g = build_graph(turns, session_id, base)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    g.serialize(destination=str(args.out), format="turtle")

    # ---- Summary stats (real counts, printed for verification) ----
    from collections import Counter

    kind_counts = Counter(t.kind for t in turns)
    grounding_topics = sorted({t.topic for t in turns if t.kind == "GroundingQuestion"})
    topic_counts = Counter(t.topic for t in turns if t.kind == "GroundingQuestion")
    repeated_topics = sorted(t for t, c in topic_counts.items() if c >= 2)

    print(f"transcript: {args.transcript}")
    print(f"session_id: {session_id}")
    print(f"total extracted turns: {len(turns)}")
    print(f"turnKind counts: {dict(kind_counts)}")
    print(f"distinct grounding_topics (GroundingQuestion turns only): {len(grounding_topics)}")
    print(f"grounding_topics: {grounding_topics}")
    print(f"repeated grounding_topics (>=2 GroundingQuestion turns, candidate escalation pairs): {repeated_topics}")
    print(f"wrote: {args.out} ({len(g)} triples)")

    if args.topic_filter:
        print(f"\n--- turns matching topic filter '{args.topic_filter}' ---")
        for t in turns:
            if args.topic_filter in t.topic:
                q = t.text.strip().replace("\n", " ")[:160]
                print(f"turn-{t.seq} [{t.role}] kind={t.kind} topic={t.topic} :: {q}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
