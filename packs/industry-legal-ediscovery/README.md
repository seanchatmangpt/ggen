# industry-legal-ediscovery

Legal / e-discovery industry pack (BK-117 lane 1/6): chain-of-custody RDF
over REAL vocabularies, projected onto the smalt emulator's Azure faces.

## What it admits

`ontology.ttl` reuses **PROV-O** (`http://www.w3.org/ns/prov#`, already the
in-repo provenance vocabulary) for the custody trail — EvidenceDocument is
a `prov:Entity`, CustodyEvent a `prov:Activity`, Custodian a `prov:Agent`,
custody rides `prov:wasGeneratedBy`/`prov:wasAttributedTo` — plus a minimal
`lex:` legal evidence vocabulary (bates numbering, matter, custodian,
ethical wall) in the neutral `https://seanchatmangpt.github.io/packs/industry-legal-ediscovery#`
namespace, reusing the landed `shapes.ttl` shape/gate patterns from
`packs/sa2a-chicago-court-pack`.

## Surfaces (all cite their real landed source)

| Azure face | smalt source (BK-117 legal row) |
|---|---|
| Blob WORM (legal hold / immutability) | `lib/smalt/data_plane/blob_storage.ex:273/:305/:516`, `blob_service.ex:105-106`, `:201-222` |
| Cosmos ETag-CAS custody docs | `lib/smalt/data_plane/cosmos.ex:133` (quoted etag = sha256 of record) |
| KV PS256 seal | `lib/smalt/data_plane/kv_pss.ex` (RSA-PSS/PS256, KD-105/KD-119) |
| Confidential Ledger custody chain | `lib/smalt/egress/AttestRoutes.ex:38-58` |
| SB review workflow (5672) | `lib/smalt/messaging/amqp_broker.ex:35`, `powl2/sb_rules.ex` |

Routing shape: browser → `smaltFetch` (`/Users/sac/smalt/workspace/lib/smalt.ts`,
`assertLocalUrl` loopback enforcement) → 8443 blob/cosmos/appconfig REST +
ledger face; SB legs via MCP 9545.

## Gates (ASK true = refusal; a refused sync emits no file)

- `gates/010_evidence_bates.rq` — evidence without a bates id refuses.
- `gates/020_custodian_required.rq` — custodian-missing evidence refuses.
- `gates/030_ethical_wall.rq` — ethical-wall breach (custodian
  `lex:conflictedMatter` the evidence's matter) refuses.

Each gate carries a negative-witness pair (`witnesses/pass/*.ttl` must NOT
violate; `witnesses/fail/*.ttl` MUST violate), enforced structurally by
`gate-court.toml` (exact-stem correspondence, schema
`ggen.semantic-gate-witness-court/1`).

## Templates

- `templates/evidence_shapes.ttl.tmpl` — per-matter SHACL shapes for
  evidence documents (bates pattern, custodian, collectedIn), bound by
  SPARQL (never hardcoded per matter).
- `templates/custody_walkthrough.md.tmpl` — per-matter client-side
  walkthrough: browser console → `smaltFetch` → Blob WORM put + Cosmos
  ETag-CAS custody doc + KV PS256 seal + ledger append.

## Qualification

`qualification/consumer.ttl` supplies a synthetic fully-conforming matter
(drives both templates' SPARQL bindings when the pack is loaded in
isolation). Validate:

```bash
python3 scripts/marketplace.py validate
python3 scripts/marketplace.py check industry-legal-ediscovery
python3 scripts/check_gate_witness_courts.py --packs packs
```

## Receipt

- Pack path: `/Users/sac/ggen-marketplace/packs/industry-legal-ediscovery/`
- Scaffolded by `scripts/new_pack.py --profile projection` (first-run
  `marketplace.py check ok`), then hand-filled; no commit made.
- `gates/` refusals executed for real via rdflib ASK over the witness
  fixtures: pass fixtures → no violation; fail fixtures → violation fires.
  See `RECEIPT.md` for the executed command output.
- Known limitation: `qualify-marketplace.sh` (real-ggen two-load
  convergence) not run in this lane — ggen binary availability not
  verified here; `marketplace.py check` (which includes a qualification
  pass) is the executed gate. Standing: PARTIAL_ALIVE.
