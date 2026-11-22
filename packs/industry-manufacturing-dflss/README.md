# industry-manufacturing-dflss

Manufacturing & industrial supply chain industry pack: DfLSS + OCEL 2.0 + Andon.

## Identity

- Semantic source: `ontology.ttl`
  (namespace `https://seanchatmangpt.github.io/packs/industry-manufacturing-dflss#`).
- Real vocabulary: OCEL 2.0 (`http://w3id.org/ocel/schema/`) — the event-log
  vocabulary smalt's `Smalt.Telemetry.OcelNdjson`
  (`lib/smalt/telemetry/ocel_ndjson.ex`) writes and unrdf's `@unrdf/otel` OCEL
  export (`packages/otel/src/ocel/index.mjs`) produces from OTel spans. The DfLSS
  layer (WorkOrder, AndonSignal, QualityGateOutcome, Defect, Containment,
  CorrectiveAction, SupplierLot) is minimal and pack-local.
- Grounding: smalt matrix row 5 (`docs/jira/v26.10.22/azure-industry-matrix.md`
  §Manufacturing): EH = line telemetry ingest (5673), SB = station workflow / DLQ
  (5672), Andon + OCEL lead (`lib/smalt/powl2/andon.ex`, `lib/smalt/ocel.ex`),
  andon wire `lib/smalt/powl2/andon_wire.ex`.

## Refusal laws

| Gate | Law |
|---|---|
| `gates/010_work_order_gate_outcome.rq` | work order MUST have a quality-gate outcome |
| `gates/020_red_andon_containment.rq` | red-Andon MUST trigger containment |
| `gates/030_defect_supplier_lot.rq` | defect MUST trace to a supplier lot |

Witnesses under `witnesses/pass` / `witnesses/fail` share each gate's exact stem;
the court runs via `runners/semantic_runner.py` (byte-identical projection of the
canonical `semantic-gate-witness-court-pack` template — SHACL pass + zero gate rows
for pass witnesses, exactly-one-gate-fires for fail witnesses). Shapes:
`ontology/shapes.ttl`.

## Rendered artifacts (consumer side)

- `templates/shacl-manufacturing-shapes.ttl.tmpl` — SHACL NodeShapes, one per
  refusal law, bound from `queries/refusal_laws.rq`.
- `templates/client-walkthrough.md.tmpl` — browser console → EH telemetry ingest
  per station event → SB containment workflow queue → OCEL event log sealed per
  batch, with `@unrdf/otel` as the browser-side OCEL complement.

## Evidence boundary

Marketplace admission and ggen qualification only. Grants no execution authority.
