# M&A case study fixture -- design note

Mirrors `crates/multifractal-workflow/fixtures/bribery-case/DESIGN.md`'s own structure: what
this case represents, which concurrent external processes it models, and why it is the "next
stronger test of LLM-assisted decomposition" `docs/releases/v26.7.14/THESIS.md` Section 33.12
names it as. Same honest-scope discipline as that file: this delivers real, independently tested
artifacts and names exactly what is NOT yet built, rather than silently assuming a fuller chain.

## What this case represents

`packs/ma-case-study-pack/` already carried a TBox-only vocabulary pass (`ontology.ttl` +
`shapes.ttl` + `COMPETENCY_QUESTIONS.md`, built the same session per that pass's own `pack.toml`
description) covering 10 M&A domain concepts against FIBO, OMG Commons, ODRL, and PROV-O. This
pass adds the instance-data half: a concrete, named acquisition -- Meridian Holdings Group, Inc.
(acquirer, fictional) proposing to acquire Corvantis Systems, Inc. (target, fictional) for USD
725,000,000 -- plus a real Knowledge Hook that derives a genuine obligation from that instance
data, and the adversarial positive/negative fixture pair Section 33.11 requires as this pack's
own `W(x)` semantic-admission-proof component.

Four fixtures, `fixtures/`:

| File | Role |
|---|---|
| `case.ttl` | The main scenario: 1 deal, all 6 named independent processes populated |
| `hook.ttl` | 1 `kh:Hook` deriving an HSR-Act-style regulatory filing obligation |
| `adversarial-positive.ttl` | G_x+: a minimal, self-contained deal that satisfies both the shapes and the hook |
| `adversarial-negative.ttl` | G_x-: a minimal, self-contained deal that fails both |

## Which concurrent external processes it models

Section 33.12 names the M&A case's decomposition-defining property directly: unlike the bribery
case's single allegation spine (one case, one contractor, one transaction, one compliance-review
process), an acquisition contains **several independent external service processes** running
concurrently, each producing unpredictable events that become deterministic inputs only after
admission (the `e_i = (eventType, payload, source, time, causalLinks)` pattern from Section 13.5).
`case.ttl` asserts real instance data across all 6 processes Section 33.12 names, not just the
4-of-6 floor this task set:

1. **Buyer diligence** -- 3 `ma:DueDiligenceItem` individuals in 3 different dispositions (IP
   assignment review: Satisfied; customer-contract consent assignments: Open; environmental
   Phase 2 survey: Waived). A real diligence workstream has items completing on their own
   schedule, not in lockstep with the other 5 processes below.
2. **Seller disclosure** -- 1 `ma:DisclosureScheduleItem` (Schedule 3.14(b)) qualifying the
   target's no-material-litigation `fibo-fnd-agr-ctr:Warranty`. Disclosure is the seller's own
   process, gated by what the seller's counsel produces, not the buyer's timeline.
3. **Financing** -- 1 `ma:FinancingCommitment` (a fictional lender's debt-commitment letter)
   `ma:subjectToCondition` 1 `ma:FinancingOutCondition` (a Material-Adverse-Change condition). A
   third-party lender's own underwriting process, external to both transacting parties.
4. **Regulatory review** -- the raw `schema:amount` fact `hook.ttl`'s
   `derive_ma_regulatory_filing_obligation` hook pattern-matches to derive whether an HSR-Act-style
   antitrust notification is owed. A federal regulator's statutory clock, external to the deal
   parties entirely.
5. **Board authority** -- 1 `ma:BoardResolution` recording Meridian's board authorizing the
   transaction, `cmns-bauth:hasAuthorizingParty` a `fibo-be-oac-exec:BoardOfDirectors` and
   `cmns-bauth:hasAuthorizedParty` the CEO. An internal corporate-governance process with its own
   meeting calendar.
6. **Shareholder action** -- 1 `ma:ShareholderVote` (82.5% quorum, `ma:Passed`) with 3
   `ma:Ballot` individuals cast by 3 fictional institutional shareholders. A collective-action
   process external to the deal negotiators, resolved on the target's own shareholder-meeting
   schedule.

Per Section 30.6's Little's Law treatment (`L = λW`, read before building this fixture per the
task instruction): if stage set `J` names these 6 processes, a stable case-flow queueing relation
`L_j = λ_j W_j` holds PER STAGE without this fixture or its hook ever needing to predict which
day the board meets, how a specific shareholder votes, or when the regulator's waiting period
actually clears. Nothing in `case.ttl` or `hook.ttl` asserts a predicted outcome for any of the 6
processes -- every fact recorded is either a raw admitted observation (the deal's aggregate
consideration, a ballot's direction, a resolution's timestamp) or, in the hook's one derived
triple, a mechanical consequence of an already-admitted raw fact (deal size crossing a threshold),
never a forecast of a still-pending external decision.

## Why this is the next stronger decomposition test

The bribery case's single spine lets one Knowledge Hook, one SHACL shape set, and (eventually)
one PDDL8 domain describe the entire case: report -> allegation -> obligations -> evidence ->
closure, all threaded through one `sc:ComplianceCase` node. An LLM (or a human) decomposing that
case only has to hold one thread in mind at a time.

An acquisition forces genuine decomposition because the 6 processes above:

- **do not share a completion order** -- board authorization can precede or follow the
  shareholder vote depending on jurisdiction and deal structure; financing can close before or
  after regulatory clearance; diligence items close continuously throughout, not at a single
  gate;
- **do not share a failure mode** -- a diligence item can be `ma:Waived` without blocking
  anything; a `ma:FinancingOutCondition` failing can kill the deal outright; a shareholder vote
  `ma:Failed` outcome is dispositive in a way a single open diligence item is not;
  cross-process severity is not uniform, and a decomposition that treats all 6 as interchangeable
  "checklist items" would be wrong;
- **are individually well-modeled by different public ontologies** -- FIBO's
  `fibo-cae-ce-act:MergerAcquisition` for the deal event, OMG Commons `cmns-bauth:` for board
  authorization, PROV-O for the shareholder vote's provenance shape, ODRL's `odrl:Duty` for
  diligence obligations -- so a correct decomposition also has to correctly route each concept to
  its own best-fit public vocabulary rather than forcing all 6 into one borrowed shape (the
  vocabulary-fit research `ontology.ttl`'s header already performed, disclosing 2 explicit false
  friends: `fibo-be-le-cb:BoardAgreement` for board authorization, and
  `fibo-fnd-agr-ctr:hasTerm` for negotiated deal terms).

This is a harder, more realistic test of whether an LLM-assisted decomposition pass can hold 6
concurrently-evolving, differently-shaped external processes in mind at once without collapsing
them into one linear story -- exactly the property Section 33.12 identifies as the reason the M&A
case is the next stronger stress test after the bribery case's single-spine allegation.

## What this pass proved live (see `crates/praxis-graphlaw/tests/ma_case_hook_actuation.rs`)

- `case.ttl`, `hook.ttl`, `adversarial-positive.ttl` all pass real, non-vacuous SHACL evaluation
  against `../shapes.ttl` (`ggen graph validate --files ... --shapes shapes.ttl`,
  `shapes_conform: true` for each, confirmed this session -- not merely Turtle-parse validation;
  see `adversarial-positive.ttl`'s own header for the one-line vocabulary restatement each fixture
  needed to pass standalone, since `ggen graph validate --files` validates each file
  independently rather than as a union graph, a real CLI behavior discovered and disclosed this
  session, not assumed).
- `adversarial-negative.ttl` genuinely FAILS the same real SHACL evaluation with exactly 2
  violations (`ma:RelationshipRecordShape`'s ownership-percentage range, `ma:MergerAcquisitionShape`'s
  missing deal-status classification) -- confirmed via the same command, non-zero exit.
- `hook.ttl`'s `derive_ma_regulatory_filing_obligation` hook fires for real over both `case.ttl`
  (USD 725,000,000) and `adversarial-positive.ttl` (USD 150,000,000), deriving exactly the 1
  `ma:hasRegulatoryFilingObligation` triple the pattern implies, via the real
  `praxis_graphlaw::TripleStore::load_hook_pack` + `.materialize()` mechanism -- and does NOT fire
  for `adversarial-negative.ttl` (USD 42,000,000, below threshold), proving the hook is a genuine
  conditional pattern match on deal size, not an unconditional actuation. 4 tests, all passing:
  `just test-bin ma_case_hook_actuation`.

## What this pass explicitly did NOT build (named, not hidden)

- **F02 observation admission** for this case (`multifractal_workflow::f02_observation_admission`).
  `case.ttl`'s own header discloses why: standing up an `AdmissionPolicy` for a 6-process case (as
  opposed to bribery-case's single-node case) is real, non-trivial design work (which predicates
  are authorized on which of the 6+ distinct subject nodes, what "the declared subject" even means
  when 6 independent processes each have their own root node) that this task's own scope carve-out
  excludes.
- **PDDL8 / POWL v2 / Arazzo / Erlang dispatch** for the M&A case. Per the task instruction and
  Section 33.12 itself ("The M&A case is PLANNED future work, not v26.7.14 implementation
  standing"), this pass's job was real vocabulary + ontology + case + hook progress, not the full
  crown chain. A future stage would need exactly the same kind of `pddl:`-vocabulary domain +
  problem-projector step bribery-case's own `DESIGN.md` names as its own Stage 2 (`sc:hasObligation`
  RDF triples -> `pddl:init` PDDL atom-literal strings), applied here to
  `ma:hasRegulatoryFilingObligation` and the other 5 processes' own derivable facts (e.g. a future
  hook deriving "board reauthorization required" when a negotiated deal term changes materially
  after signing -- named as an example in the task prompt, not built this pass).
- **A second Knowledge Hook.** The task asked for "at least one genuine obligation"; this pass
  built exactly one (`derive_ma_regulatory_filing_obligation`), leaving the board-reauthorization
  variant as a named, not-yet-built extension for a future pass, the same way bribery-case's
  `DESIGN.md` names its own unbuilt evidence-unavailable hook rather than silently assuming it
  exists.

## See Also

- `crates/multifractal-workflow/fixtures/bribery-case/DESIGN.md` -- the single-spine precedent this
  file mirrors the structure of
- `packs/ma-case-study-pack/ontology.ttl` -- the TBox vocabulary pass this instance data is built
  against, including the 2 disclosed false-friend rejections
- `packs/ma-case-study-pack/shapes.ttl` -- the SHACL shapes `fixtures/*.ttl` validate against
- `docs/releases/v26.7.14/THESIS.md` Section 33.11 (semantic admission proof obligation `W(x)`),
  Section 33.12 (the M&A decomposition case), Section 13.5 (nondeterminism becomes input), Section
  30.6 (Little's Law from first principles)
