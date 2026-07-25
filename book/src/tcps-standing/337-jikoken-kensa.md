# 337. 自らを造る機械は、自らを検査しなければならない

> **Pattern 337 · A Complete Pattern in Practice: TCPS**
>
> **Standing rule:** this pattern is `ALIVE` only when its consequence has been observed in a real consumer and bound to replayable evidence.

## Context

A generator that regenerates its own generation rules is not automatically trustworthy for having done so. The narrow problem in this chapter is **337. 自らを造る機械は、自らを検査しなければならない**: a machine whose output includes the rules that produced it must submit that output to inspection by a process it does not itself control, or standing is manufactured rather than received.

A weak implementation lets the generation that wrote a new rule also certify that rule's correctness — self-proof, disguised as diligence. The target is narrower and harder: the round that manufactures a change and the round that inspects it must be distinguishable in the receipt chain, with the inspecting round's 受領証 issued by a check the manufacturing round could not have written to pass.

## Problem

Without a stable pattern for **自らを造る機械は、自らを検査しなければならない**, locally reasonable decisions accumulate into a pack that renders but does not manufacture a substitutable product. The defect normally appears later—as graph contamination, hidden handler work, path collision, drift, false proof, or an unverifiable release claim—when repair is more expensive and evidence is weaker.

## Forces

- The source of truth must remain distinguishable from every generated projection.
- A successful render is weaker than an admitted, consumed, independently verified consequence.
- Composition creates hidden coupling unless identity, ownership, and output boundaries are explicit.
- Fast regeneration is useful only when unchanged inputs reproduce unchanged bytes and changed inputs expose drift.
- Every actuation must terminate in a durable receipt or an explicit typed refusal.

## Governing law

\\[A=\\mu(O^{\\star})\\quad\\text{with standing received, never claimed, by named evidence}\\]

Three clauses follow from the equation as applied to reflexive generation:

1. **自らを造る機械は、自らを検査しなければならない.** A machine that manufactures itself must inspect itself — but "itself" here means the lineage, not the instant. The inspecting act must be a later, separately-receipted act, not a comment the same diff appends to its own patch.
2. **標準は要求されない、受領される.** Standing (標準) is never claimed by assertion; it is received (受領) only when an independent gate, run after the fact, emits a 受領証 that the claimant could not have forged by construction.
3. **同一世代は自らの憲法を書き換えられない.** The same generation cannot rewrite its own constitution — a round that edits `.specify/*.ttl`'s governing rules may not, within that same round, also mark those rules verified. Verification is the next round's obligation, run against durable state the first round left behind.

## Therefore

Establish **自らを造る機械は、自らを検査しなければならない** as a named part of the pack contract rather than leaving it as convention or tacit knowledge.

Do this at the narrowest layer that owns the invariant. Keep graph-domain construction reversible until admission is complete. Route filesystem or external effects through the engine’s declared write path. Require the consumer and verifier to observe the intended consequence independently of the authoring convenience that produced it.

## Configuration

1. **Separate the writing round from the checking round.** The commit that changes a generation rule must not be the commit whose receipt certifies that rule as sound; the certifying receipt belongs to a subsequent, independently-runnable check.
2. **Bind the receipt to durable state, not to narration.** The 受領証 names the exact commit, graph hash, and command that produced it — never a prose claim of completion.
3. **Refuse silently-inflated standing.** A doctrine table (see `docs/GENERATIONS.md`, Appendix O) that marks every row MET without a corresponding proof object is itself a defect, not a report.

The sequence deliberately keeps the manufacturing layer (RDF ontology, SPARQL projection, Tera rendering) separate from the standing layer (receipt chain, chained BLAKE3 hash, `ggen receipt verify`). A generation ledger is honest exactly when it names more NOT MET rows than MET ones, for a young lineage.

### Crate alignment

The pattern is grounded in these live ownership surfaces:

- `praxis-core`
- `ggen-engine`
- `cargo-cicd`

A pack may depend on these surfaces, but it must not silently absorb their responsibilities.

## Reference implementation

The companion listing is stored at [`src/listings/337-jikoken-kensa.ttl`](../listings/337-jikoken-kensa.ttl). It is intentionally small enough to inspect while retaining the chapter's load-bearing boundary: it is RDF, not code, because the doctrine binds the ledger, not a runtime type.

```turtle
@prefix gen: <http://ggen.org/generations#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

gen:G1 a gen:Generation ;
    gen:manufactures "adds one book:Chapter, one gen-receipt recipe, one docs/GENERATIONS.md rule" ;
    gen:inspectedBy gen:G2 ;
    gen:selfCertified false .
```

## Verification procedure

```text
manufacture (G1 writes rules, chapter, recipe) → commit →
inspect (G2 runs `just gen-receipt`, reads .ggen-v2/receipt.json) →
compare declared doctrine rows against actual repository state →
emit 受領証 naming exactly what was found MET and NOT MET
```

The proof is non-vacuous exactly when a doctrine row can flip from MET to NOT MET by deleting the file or gate it cites — a row that stays MET no matter what you delete is not measuring anything.

Use the verification ladder in order:

```text
unit → integration → end-to-end → adversarial mutation → idempotency → receipt verification
```

A lower rung may establish `PARTIAL_ALIVE`; it cannot establish the crown claim of the higher rungs.

## Resulting context

Each maturity claim names exact evidence, omissions, corrective divergences, and replay commands.

The pattern also creates obligations. The new source law must remain inspectable. Generated outputs must retain one owner. Consumers must not acquire hidden setup. Receipts must be regenerated whenever an admitted input or verifier changes. Neighboring patterns can now rely on this consequence without reintroducing the resolved ambiguity.

## Failure modes

- **Self-certification:** the round that writes a rule also marks it verified in the same commit.
- **Narrated standing:** `docs/GENERATIONS.md` claims MET without a receipt, test, or file path a reader can check.
- **Constitutional overwrite:** a single round both edits `.specify/*.ttl`'s law and asserts the edited law is now authoritative, with no intervening inspection.

## Falsifier

The claimed standing survives after deleting or invalidating one of its named receipts.

Execute that falsifier deliberately. A pattern with no plausible way to fail is a slogan, not production law.

## Continuous TCPS case study

The TCPS sequence supplies the continuous product-scale example. Apply this pattern there only where it preserves the canonical vocabulary, complete generated surface, independent tests, safe regeneration, and inspection standing.

## Laboratory

Read `docs/GENERATIONS.md` after this round lands. Pick any row marked MET. Delete or break the file it cites. Rerun the check the row names. Confirm the row would now have to read NOT MET. If no such file exists to break, the row was narration, not standing — file that as a defect against this very chapter.

## Acceptance gate

- [ ] `.specify/generations.ttl` carries a `gen:G1` individual and a `gen:G2` individual with distinct facts.
- [ ] `docs/GENERATIONS.md` is generated (not hand-written) from `.specify/generations.ttl` by a `[[generation.rules]]` entry in root `ggen.toml`.
- [ ] `just gen-receipt` runs a real `ggen sync run` invocation and prints a real one-line confirmation, not a canned string.
- [ ] The 15-condition doctrine table names at least one row NOT MET, honestly.
- [ ] This chapter's own admission increases `book/scripts/check_book.py`'s printed count by exactly one.

## Connections

This pattern receives its context from **A Complete Pattern in Practice: TCPS** and passes a narrower, better-admitted state to the patterns that follow it in `SUMMARY.md`. When a later pattern fails, trace backward through source identity, admission, projection, ownership, consumer observation, and receipt rather than patching the generated artifact.

## Standing statement

The pattern is complete only when every checked acceptance item resolves to a concrete repository artifact or command result. Missing execution is `UNKNOWN`; a missing admitted dependency is `BLOCKED`; an unreachable verifier caused by the build is `BUILD_BROKEN`; an intentionally absent capability is `UNSUPPORTED`. None of those states may be reported as `ALIVE`.
