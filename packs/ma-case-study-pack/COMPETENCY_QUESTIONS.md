# M&A Case Study Pack — Competency Questions

This document is the `Q_x` component of the `W(x) = (t_x, S_x, Q_x, G_x-, G_x+, r_x)` proof
obligation for `packs/ma-case-study-pack/ontology.ttl`: for each of the 10 domain concepts the
pack's vocabulary-fit research covered, one or more competency questions the ontology must be
able to answer, plus the exact terms (public or `ma:`) a query would join over to answer it.
This is a design document, not a report of queries actually run — no `ontology.ttl` instance
data (case-specific Acquirer/Target individuals) is committed in this pack, so no SPARQL result
set backs the "answered" claims below; each question states the terms it depends on so a future
instance-data commit can be checked against it mechanically.

## How to read an entry

Each competency question names: the concept it covers, the question itself, and the ontology
terms (`rdfs:domain`/`rdfs:range` pairs or classes) a SPARQL query would need to join to answer
it. `CLEAN_FIT` concepts answer entirely with reused public terms; `STRAINED_FIT` and
`NO_PUBLIC_TERM_FOUND` concepts depend on at least one `ma:` bridge term (see `ontology.ttl`'s
header for why each was minted).

## 1. Acquiring/target legal entity + LEI (CLEAN_FIT)

- **CQ1.1**: What is the LEI of the entity identified as `<iri>`?
  Terms: `fibo-be-le-lei:LEIRegisteredEntity`, `cmns-id:isIdentifiedBy`,
  `fibo-be-le-lei:LegalEntityIdentifier`.
- **CQ1.2**: Which entities in the graph are LEI-registered (as opposed to unregistered
  counterparties)?
  Terms: `rdf:type fibo-be-le-lei:LEIRegisteredEntity`.

## 2. Equity stake / ownership percentage (CLEAN_FIT)

- **CQ2.1**: What percentage of Target does Acquirer own after the transaction, per the
  recorded relationship record?
  Terms: `fibo-be-le-lei:RelationshipRecord`, `fibo-be-le-lei:hasOwnershipPercentage`.
- **CQ2.2**: Are there any relationship records with an ownership percentage outside `[0, 100]`
  (a data-quality check `ma:RelationshipRecordShape` enforces)?
  Terms: `fibo-be-le-lei:hasOwnershipPercentage` (`shapes.ttl`'s `sh:minInclusive`/
  `sh:maxInclusive`).

## 3. Due-diligence checklist item + disposition

- **CQ3.1**: Which due-diligence items remain open for acquisition X?
  Terms: `ma:DueDiligenceItem`, `ma:dueDiligenceDisposition = ma:Open`.
- **CQ3.2**: Which due-diligence items were waived rather than satisfied, and what does that
  imply about residual risk carried into closing?
  Terms: `ma:dueDiligenceDisposition = ma:Waived`.
- **CQ3.3**: What action and target does a given due-diligence item require verification of?
  Terms: `odrl:action`, `odrl:target` (reused directly on `ma:DueDiligenceItem`).

## 4. Representation/warranty clause + disclosed exception

- **CQ4.1**: Which representations or warranties in the agreement have at least one disclosed
  exception on file?
  Terms: `fibo-fnd-agr-ctr:Representation`, `fibo-fnd-agr-ctr:Warranty`,
  `ma:DisclosureScheduleItem`, `ma:qualifiesClause`.
- **CQ4.2**: What is the schedule-item number of the disclosure that qualifies Representation
  clause 3.14(b)?
  Terms: `ma:qualifiesClause`, `ma:scheduleItemNumber`.
- **CQ4.3**: Which representations or warranties have NO disclosed exception at all (the
  unqualified, full-strength clauses)?
  Terms: `fibo-fnd-agr-ctr:Representation`/`Warranty` MINUS the set reachable via
  `ma:qualifiesClause`.

## 5. Regulatory filing obligation + threshold

- **CQ5.1**: Does this transaction's value exceed the HSR filing threshold, and if so, what is
  the statutory waiting period before closing may occur?
  Terms: `ma:RegulatoryFilingObligation`, `ma:filingThresholdAmount`,
  `ma:statutoryWaitingPeriodDays`.
- **CQ5.2**: What statute triggers this filing obligation?
  Terms: `ma:triggeringStatute`.
- **CQ5.3**: Once submitted, what is the resulting filing document typed as?
  Terms: `cmns-rga:RegulatoryReport` (the reused public term for the artifact itself, distinct
  from the obligation to produce it).

## 6. Board authorization / resolution

- **CQ6.1**: Has the Board of Directors adopted a resolution authorizing this specific
  merger/acquisition, and when?
  Terms: `ma:BoardResolution`, `ma:authorizesTransaction`, `prov:generatedAtTime`.
- **CQ6.2**: Who is the authorizing party and who is the authorized party for a given board
  resolution?
  Terms: `cmns-bauth:hasAuthorizingParty`, `cmns-bauth:hasAuthorizedParty`.
- **CQ6.3** (negative/regression question, guards the false-friend rejection): is any
  `fibo-be-le-cb:BoardAgreement` individual ever asserted as `ma:authorizesTransaction`'s domain?
  It must not be — `ma:BoardResolution` is deliberately a disjoint class from `BoardAgreement`
  (see `ontology.ttl`'s header). A query finding a `BoardAgreement` doing resolution-shaped work
  signals the false-friend was reintroduced.

## 7. Shareholder vote + outcome

- **CQ7.1**: Did the shareholder vote on this transaction pass, fail, or get withdrawn, and
  what was the quorum?
  Terms: `ma:ShareholderVote`, `ma:voteOutcome`, `ma:quorumPercentage`.
- **CQ7.2**: How did a specific voting shareholder cast their ballot?
  Terms: `ma:Ballot`, `prov:wasAttributedTo` (-> `fibo-be-oac-cctl:VotingShareholder`),
  `ma:ballotDirection`.
- **CQ7.3**: What fraction of cast ballots were `For` vs. `Against` vs. `Abstain` for a given
  vote?
  Terms: `dcterms:isPartOf` (`Ballot` -> `ShareholderVote`), `ma:ballotDirection`, grouped count.

## 8. Negotiated deal term / counter-offer

- **CQ8.1**: What is the current price-per-share, exchange ratio, earnout, and escrow
  percentage on the table?
  Terms: `ma:DealTermLineItem`, `ma:termType`, `ma:termValue`.
- **CQ8.2**: What is the full counter-offer chain for this negotiation, from the original offer
  to the latest counter-offer?
  Terms: `ma:CounterOffer`, `ma:supersedes` (transitive walk), `fibo-fnd-agr-ctr:NonBindingTerm`.
- **CQ8.3**: Who proposed the most recent counter-offer?
  Terms: `ma:CounterOffer`, `prov:wasAttributedTo`.
- **CQ8.4** (negative/regression question): does any query mistake `fibo-fnd-agr-ctr:hasTerm`
  for a negotiated deal-term value? It must not — `hasTerm` is contract DURATION (see
  `ontology.ttl`'s false-friend disclosure), never joined against `ma:DealTermLineItem`.

## 9. Financing commitment / condition

- **CQ9.1**: Is the acquisition's financing commitment subject to a financing-out condition,
  and what is that condition?
  Terms: `ma:FinancingCommitment`, `ma:subjectToCondition`, `ma:FinancingOutCondition`.
- **CQ9.2**: What does the lender's commitment promise, in general contractual terms?
  Terms: `fibo-fnd-agr-agr:Commitment`, `dcterms:description`.

## 10. Deal closing / termination status

- **CQ10.1**: What is the current status of this merger/acquisition — signed, pending
  regulatory approval, closed, terminated, or abandoned?
  Terms: `fibo-cae-ce-act:MergerAcquisition`, `cmns-cls:isClassifiedBy`, `ma:DealStatus`.
- **CQ10.2**: When did (or will) this deal close?
  Terms: `fibo-fnd-dt-fd:hasClosingDateTime`.
- **CQ10.3**: If terminated, under which termination provision?
  Terms: `fibo-fnd-agr-ctr:TerminationProvision`, `fibo-fnd-agr-ctr:EarlyTerminationProvision`.
- **CQ10.4**: Which deals in the portfolio are currently `PendingRegulatoryApproval`, and for
  each, what is the remaining statutory waiting period (joining concepts 5 and 10)?
  Terms: `cmns-cls:isClassifiedBy = ma:PendingRegulatoryApproval`,
  `ma:RegulatoryFilingObligation`, `ma:statutoryWaitingPeriodDays`.

## Cross-concept questions

- **CQX.1**: For a given deal, list every open due-diligence item, every un-cleared disclosure
  exception, and the current deal status, as a single closing-readiness view.
  Terms: `ma:DueDiligenceItem`/`ma:dueDiligenceDisposition`, `ma:DisclosureScheduleItem`,
  `fibo-cae-ce-act:MergerAcquisition`/`cmns-cls:isClassifiedBy`/`ma:DealStatus`.
- **CQX.2**: For a given deal, has the board resolution been adopted AND has the shareholder
  vote passed (both governance gates satisfied)?
  Terms: `ma:BoardResolution`/`ma:authorizesTransaction`, `ma:ShareholderVote`/`ma:voteOutcome`.

## See Also

- `packs/ma-case-study-pack/ontology.ttl` — the vocabulary these questions are checked against
  (header discloses every public term reused and every `ma:` bridge term minted).
- `packs/ma-case-study-pack/shapes.ttl` — the SHACL constraints that make several of the above
  questions well-formed to ask (e.g. CQ3.1 depends on `ma:dueDiligenceDisposition` being
  present and closed-enumerated).
- `crates/multifractal-workflow/fixtures/bribery-case/DESIGN.md` — the closest analog in this
  repo for how a case-study vocabulary + shapes pair is later wired to instance data and a PDDL
  planning layer; this pack has not yet taken that Stage 2 step (no `case.ttl`/instance data is
  committed here).
