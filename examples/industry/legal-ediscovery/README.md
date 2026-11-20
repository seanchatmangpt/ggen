# legal-ediscovery consumer example

Consumer for the `industry-legal-ediscovery` marketplace pack
(`/Users/sac/ggen-marketplace/packs/industry-legal-ediscovery/`): one
synthetic matter (`2026-Q-0001`) with fully-conforming evidence, projected
by ggen into (a) SHACL shapes for evidence documents and (b) a client-side
custody walkthrough.

## Run

```bash
cd /Users/sac/ggen/examples/industry/legal-ediscovery
ggen sync run
# → written: generated/2026-Q-0001/evidence-shapes.ttl,
#            generated/2026-Q-0001/custody-walkthrough.md
```

The manifest binds the pack ontology via `[ontology] imports` (the pack's
vocabulary) and renders the pack's two templates via
`[[generation.rules]]` whose SPARQL joins the consumer's matter/evidence
facts. Gates ride in the pack (`gates/*.rq`, ASK true = refusal).

## Client routing (BK-117 legal row)

Browser → `smaltFetch` (`/Users/sac/smalt/workspace/lib/smalt.ts`,
`assertLocalUrl` loopback enforcement) → smalt 8443:

| Step | smalt face | Cited source |
|---|---|---|
| 1. Blob WORM put under legal hold | `PUT /{container}/{blob}`; legal hold / immutability via `?comp=legalhold` / `?comp=immutabilityPolicy` | `lib/smalt/data_plane/blob_storage.ex:273/:305/:516`, `blob_service.ex:105-106`, `:201-222`, `egress_router.ex:67-68` |
| 2. Cosmos ETag-CAS custody doc | `POST /dbs/lexdb/colls/{matter}/docs`; edits need `If-Match` | `lib/smalt/data_plane/cosmos.ex:133` (quoted etag = sha256 of record) |
| 3. KV PS256 seal | `POST /keys/lex-seal/sign`, `alg=PS256` | `lib/smalt/data_plane/kv_pss.ex` (KD-105/KD-119) |
| 4. Confidential Ledger append | `POST /app/transactions`, replay via `GET /app/transactions/latest` | `lib/smalt/egress/AttestRoutes.ex:38-58` |
| 5. SB review workflow | AMQP sb 5672 via MCP 9545 | `lib/smalt/messaging/amqp_broker.ex:35`, `lib/smalt/powl2/sb_rules.ex` |

## Gate execution against this consumer graph (executed this session)

Positive (consumer ontology + pack ontology union): 010/020/030 all
`violation=False`. Negative control (batesId + hasCustodian stripped,
conflictedMatter added): 010 `violation=True`, 020 `violation=True`;
030 does not fire on THIS mutation because stripping `hasCustodian`
removes the doc→custodian join it needs — its negative coverage is the
`witnesses/fail/030_ethical_wall.ttl` fixture (violation=True), a
disjunctive-law note recorded in the pack RECEIPT.
