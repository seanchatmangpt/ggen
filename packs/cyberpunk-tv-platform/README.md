# Cyberpunk Television Platform Pack

This pack manufactures a television-native semantic experience from public RDF.

## Canonical correspondence

```text
platform.ttl
→ SPARQL extraction
→ Tera projection
→ UNRDF semantic world
→ Mermaid structural view
→ deck.gl spatial view
→ BLAKE3 runtime receipt
```

The authored surface is `ontology/platform.ttl`. Files under `generated/` are projections and must not be edited directly.

## Manufacture

```bash
cd packs/cyberpunk-tv-platform
ggen sync
cd generated
npm install
npm run build
npm run verify
```

## Acceptance

A subject reaches `PARTIAL_ALIVE` only when:

1. `ggen sync` exits zero.
2. A second sync is byte-identical.
3. `npm run build` exits zero.
4. `npm run verify` emits `.ggen/receipts/cyberpunk-tv.json` using BLAKE3-256.
5. The generated TV runtime renders both the deck.gl world and Mermaid system view from the same RDF-derived subject.

External television packaging and physical-device execution remain outside this first exact slice and must not be inferred from a successful browser build.
