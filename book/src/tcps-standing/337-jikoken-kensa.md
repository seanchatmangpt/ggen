# 337. 自らを造る機械は、自らを検査しなければならない

> **Outcome:** Construct and verify the reflexive-inspection doctrine for **337. 自らを造る機械は、自らを検査しなければならない** ("a machine that manufactures itself must inspect itself").

This is chapter **337 of 336**, admitted after the original transcription pass closed; it extends Part XXIII rather than displacing any of the 336 admission-numbered chapters that precede it.

## Production problem

A generator that regenerates its own generation rules is not automatically trustworthy for having done so. The narrow problem in this chapter is **337. 自らを造る機械は、自らを検査しなければならない**: a machine whose output includes the rules that produced it must submit that output to inspection by a process it does not itself control, or standing is manufactured rather than received.

A weak implementation lets the generation that wrote a new rule also certify that rule's correctness — self-proof, disguised as diligence. The target is narrower and harder: the round that manufactures a change and the round that inspects it must be distinguishable in the receipt chain, with the inspecting round's 受領証 issued by a check the manufacturing round could not have written to pass.

## Governing law

\\[A=\\mu(O^{\\star})\\quad\\text{with standing received, never claimed, by named evidence}\\]

Three clauses follow from the equation as applied to reflexive generation:

1. **自らを造る機械は、自らを検査しなければならない.** A machine that manufactures itself must inspect itself — but "itself" here means the lineage, not the instant. The inspecting act must be a later, separately-receipted act, not a comment the same diff appends to its own patch.
2. **標準は要求されない、受領される.** Standing (標準) is never claimed by assertion; it is received (受領) only when an independent gate, run after the fact, emits a 受領証 that the claimant could not have forged by construction.
3. **同一世代は自らの憲法を書き換えられない.** The same generation cannot rewrite its own constitution — a round that edits `.specify/*.ttl`'s governing rules may not, within that same round, also mark those rules verified. Verification is the next round's obligation, run against durable state the first round left behind.

## Construction sequence

1. **Separate the writing round from the checking round.** The commit that changes a generation rule must not be the commit whose receipt certifies that rule as sound; the certifying receipt belongs to a subsequent, independently-runnable check.
2. **Bind the receipt to durable state, not to narration.** The 受領証 names the exact commit, graph hash, and command that produced it — never a prose claim of completion.
3. **Refuse silently-inflated standing.** A doctrine table (see `docs/GENERATIONS.md`, Appendix O) that marks every row MET without a corresponding proof object is itself a defect, not a report.

The sequence deliberately keeps the manufacturing layer (RDF ontology, SPARQL projection, Tera rendering) separate from the standing layer (receipt chain, chained BLAKE3 hash, `ggen receipt verify`). A generation ledger is honest exactly when it names more NOT MET rows than MET ones, for a young lineage.

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

## Failure modes

- **Self-certification:** the round that writes a rule also marks it verified in the same commit.
- **Narrated standing:** `docs/GENERATIONS.md` claims MET without a receipt, test, or file path a reader can check.
- **Constitutional overwrite:** a single round both edits `.specify/*.ttl`'s law and asserts the edited law is now authoritative, with no intervening inspection.

## Laboratory

Read `docs/GENERATIONS.md` after this round lands. Pick any row marked MET. Delete or break the file it cites. Rerun the check the row names. Confirm the row would now have to read NOT MET. If no such file exists to break, the row was narration, not standing — file that as a defect against this very chapter.

## Acceptance gate

- [ ] `.specify/generations.ttl` carries a `gen:G1` individual and a `gen:G2` individual with distinct facts.
- [ ] `docs/GENERATIONS.md` is generated (not hand-written) from `.specify/generations.ttl` by a `[[generation.rules]]` entry in root `ggen.toml`.
- [ ] `just gen-receipt` runs a real `ggen sync run` invocation and prints a real one-line confirmation, not a canned string.
- [ ] The 15-condition doctrine table names at least one row NOT MET, honestly.
- [ ] This chapter's own admission increases `book/scripts/check_book.py`'s printed count by exactly one.

## Standing statement

This chapter is complete only when its own doctrine gate is turned against the round that wrote it: `docs/GENERATIONS.md`'s G1/G2 split is not decoration if a reader can delete `.ggen-v2/receipt.json`, rerun `just gen-receipt`, and watch the affected row flip to NOT MET. Standing received, never standing claimed.
