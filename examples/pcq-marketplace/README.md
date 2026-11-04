# PCQ Atlas Marketplace

A ggen-manufactured Next.js 16 + deck.gl real-time capability marketplace. PCQ is a governed fixed-supply internal unit of account; it is not represented as fiat, legal tender, or an externally traded token.

## Boundaries

- Ontology and SPARQL gates admit applications, currency, nodes, accounts, listings, settlement policy, and stream policy.
- Next.js Route Handlers expose snapshot, SSE market stream, and settlement endpoints.
- The fixed-point PCQ ledger refuses unknown accounts, negative balances, overspending, supply drift, and invalid order quantities.
- deck.gl renders liquidity columns, market nodes, great-circle arcs, animated trips, and labels.
- Every settlement emits a SHA-256 receipt.

## Verify

```bash
ggen sync run
ggen sync run --dry-run
corepack enable
pnpm install --frozen-lockfile=false
pnpm verify
pnpm exec playwright install chromium
pnpm test:e2e
```
