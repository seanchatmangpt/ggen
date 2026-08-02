# Admit Vision 2030 capabilities without fabricating progress

The Vision 2030 catalog is a design inventory. It does not prove that any capability exists. A capability counts only after its own SBB report, positive and negative evidence, verifier, result receipt, replay witness, and independent acceptance are bound into a program manifest.

## 1. Inspect the contract

```text
ggen vision2030 schema
```

The response names the twelve required domains, five horizon years, four Blue Ocean moves, four authority classes, seven mandatory evidence roles, and the minimum 1,000× target.

## 2. Select one vertical capability

Start with one catalog entry whose dependencies are already admitted. A useful first vertical is:

```text
dx-local-first-control-plane
```

The vertical must close:

```text
observation
→ architecture contract
→ ontology
→ SHACL
→ SPARQL
→ typestate
→ template
→ generated/runtime surface
→ positive witness
→ negative fixture
→ verifier
→ SBB report
→ result receipt
→ replay
→ independent acceptance
```

Do not mark a catalog entry `ALIVE` manually.

## 3. Produce an eligible SBB density report

Use the SBB capability-density surface introduced by the stacked dependency:

```text
ggen sbb inspect path/to/sbb-capability-manifest.json
ggen sbb validate path/to/sbb-capability-manifest.json
ggen sbb receipt path/to/sbb-capability-manifest.json .ggen/sbb/dx-local-first

ggen sbb replay path/to/sbb-capability-manifest.json .ggen/sbb/dx-local-first
```

The SBB report must contain:

```text
schema = ggen.sbb.capability-density-report.v1
claim_ceiling = PARTIAL_ALIVE
eligible_for_external_admission = true
commit_equivalent_units > 0
distribution_contexts > 0
delivered_capability_instances = commit_equivalent_units × distribution_contexts
```

## 4. Obtain independent acceptance

The program evaluator accepts an external-admission artifact shaped like:

```json
{
  "schema": "ggen.external-admission.v1",
  "subject": "urn:ggen:vision2030:dx:local-first-control-plane",
  "decision": "ACCEPTED",
  "issuer": "independent-release-authority",
  "report_digest": "<exact SBB report digest>"
}
```

The issuer must not equal the program identifier. The file itself is bound by BLAKE3 in the program manifest.

## 5. Bind the capability realization

```json
{
  "id": "dx-local-first-control-plane",
  "iri": "urn:ggen:vision2030:dx:local-first-control-plane",
  "domain": "dx",
  "horizon": 2026,
  "blue_ocean_move": "eliminate",
  "authority": "construct",
  "summary": "Operate the manufacturing control plane locally without mandatory cloud state.",
  "dependencies": [],
  "evidence": {
    "sbb_report": {"locator": "evidence/dx/report.json", "digest": "blake3:<64-hex>"},
    "positive": {"locator": "evidence/dx/positive.txt", "digest": "blake3:<64-hex>"},
    "negative": {"locator": "evidence/dx/negative.txt", "digest": "blake3:<64-hex>"},
    "verifier": {"locator": "evidence/dx/verifier.txt", "digest": "blake3:<64-hex>"},
    "receipt": {"locator": "evidence/dx/result.json", "digest": "blake3:<64-hex>"},
    "replay": {"locator": "evidence/dx/replay.json", "digest": "blake3:<64-hex>"},
    "external_acceptance": {"locator": "evidence/dx/acceptance.json", "digest": "blake3:<64-hex>"}
  }
}
```

Evidence locators are manifest-relative. Absolute paths, remote URIs, and parent traversal are refused.

## 6. Inspect and diagnose

```text
ggen vision2030 inspect path/to/vision-2030-program.json
ggen vision2030 validate path/to/vision-2030-program.json
ggen vision2030 roadmap path/to/vision-2030-program.json
ggen vision2030 blue-ocean path/to/vision-2030-program.json
ggen vision2030 dx path/to/vision-2030-program.json
ggen vision2030 qol path/to/vision-2030-program.json
ggen vision2030 doctor path/to/vision-2030-program.json
```

`doctor` is diagnostic only. It returns blocking findings and deterministic remediation instructions; it does not modify the repository or environment.

## 7. Authority boundaries

Healthcare capabilities may use only:

```text
observe
recommend
```

Doctor capabilities may diagnose and construct remediation plans but may not use `actuate`.

Every other capability using `actuate` must add an `execution_grant` evidence binding whose content declares:

```json
{
  "schema": "ggen.execution-grant.v1",
  "subject": "<capability IRI>",
  "broker": "<explicit broker identity>",
  "grant": "GRANTED",
  "report_digest": "<exact SBB report digest>"
}
```

The program evaluator validates the grant but does not perform the actuation.

## 8. Emit and replay the program receipt

```text
ggen vision2030 receipt path/to/vision-2030-program.json .ggen/vision2030
ggen vision2030 replay path/to/vision-2030-program.json .ggen/vision2030
```

The bounded write emits:

```text
.ggen/vision2030/
├── vision-2030-intent.json
├── vision-2030-report.json
└── vision-2030-result.json
```

The valid replay terminal is:

```text
REPLAY_MATCH
```

## 9. Achievement law

The program reports `ALIVE` only when all of the following are true:

- every declared capability is externally accepted;
- every dependency is `ALIVE`;
- every required domain has accepted coverage;
- every 2026–2030 horizon minimum is met;
- eliminate, reduce, raise, and create all have accepted coverage;
- the measured aggregate multiplier is at least 1,000×;
- violations are zero;
- the exact report and receipt chain replay.

Until then, the report remains `DESIGNED` or `PARTIAL_ALIVE`. That is the intended behavior.
