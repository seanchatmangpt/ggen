# GBB-001: Speedrun Talent Network

## Preserve

The external service already has two coherent doors over the same live hiring dataset: a read-only REST API and a remote MCP server. The REST surface is the canonical discovery contract. MCP adds agent-oriented descriptions plus two candidate actions. The implementation preserves that split rather than flattening MCP writes into REST or pretending that either door submits applications.

## Fence

The generic `ggen-architecture` kernel remains deterministic and IO-free. The pack generates a Speedrun specialization that manufactures `SpeedrunTransportIntent` values addressed to BRCE. A downstream broker owns HTTP execution, rate limiting, operation receipts, consequence observation, and replay.

## Calculus

```text
public OpenAPI + developer contract
→ admitted Speedrun ontology
→ canonical GBB-001 declaration
→ typed REST/MCP request
→ consent admission when required
→ BRCE transport intent
→ broker execution receipt
→ canonical URL / candidate-action consequence
→ replay
```

### REST realization

Eight operations are modeled: job search, job detail, company list/detail, collection list/detail, hiring statistics, and the OpenAPI document. Response law includes open CORS, one-to-five-minute edge caching, matching HTTP/error-envelope semantics, live facet counts, source echo, bounded pagination metadata, and addressable closed-job status. Job search uses the exact documented vocabulary and bounds: `q`, `fn`, `sen`, `emp`, `loc`, `remote`, `comp`, `portfolio`, `cohort`, `company`, `stealth`, `scope`, `sort`, `page`, and `source`.

### Machine-readable projections

The same governed transport boundary covers `/llms.txt`, `/jobs.md`, and `/jobs.rss`. These are modeled as three explicit read resources rather than hidden aliases of job search.

### MCP realization

Eight tools are read-only. `join_network` and `express_interest` are candidate actions. Both require an explicit consent evidence object. `express_interest` additionally requires the consent scope to equal the requested role identity. Candidate actions are idempotency-keyed and still execute only through BRCE.

## Exclusions

- direct network IO from generated architecture logic;
- revealing masked stealth companies;
- submitting a job application;
- candidate writes without explicit consent;
- anonymous high-volume scraping;
- re-hosting stale role descriptions instead of linking canonical URLs.

## Falsifiers

The bounded test rail refuses invalid source tags, pages above 200, missing path identifiers, missing consent, non-explicit consent, action mismatch, role-scope mismatch, and any expansion beyond the documented operation sets.

## Standing

The ontology, generated specialization, and generic kernel boundary are implementable and locally testable. Live external execution, server compatibility, broker receipts, and deterministic replay remain `UNKNOWN` until the exact-head workflow runs with network access and an admitted BRCE transport realization.
