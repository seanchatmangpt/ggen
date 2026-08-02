# Cyberpunk Television Manufacturing Platform

This pack manufactures a television-native cyberpunk operating environment from public semantic authority. It is not a hand-built streaming application and it does not make a JavaScript implementation canonical.

## G1 fence

Preserve the existing Chatman ecosystem correspondence:

```text
public ontology / canon / rights / procedure
→ SPARQL + N3 + Datalog closure
→ ggen selection and projection
→ generated Rust/WASM runtime body
→ UNRDF semantic substrate
→ Mermaid/mmdio structural projection
→ deck.gl spatial-temporal projection
→ bounded television interaction
→ BLAKE3 receipt
→ replay
→ ggen-legacy equivalence and retirement decision
```

The shared object is public meaning. The deployed body is generated for an admitted device and session profile. Escrow, governance, rights, narrative state, projection selection, and watch-party procedure remain declarative; they are not recoded as a shared smart-contract implementation.

## Capability surface

The canonical graph defines and ggen projects:

1. cyberpunk canon, characters, factions, places, routes, scenes, and causal links;
2. television modes for cinema, matrix, construct, governance, market, and receipt replay;
3. deck.gl nodes, arcs, routes, labels, heat surfaces, and global-presence layers;
4. Mermaid/mmdio architecture, sequence, state, relationship, timeline, and receipt views;
5. UNRDF/Oxigraph loading, SPARQL query, and graph identity;
6. user-supplied media playback without bundling copyrighted media;
7. synchronized watch-party clocks, drift observation, host intents, and local/federated channels;
8. remote-control navigation, focus order, keyboard mapping, reduced-motion, contrast, caption, and audio-description surfaces;
9. Robert's Rules motion, second, amendment, quorum, vote, adoption, defeat, and clean adjournment;
10. two-party SPARQL `CONSTRUCT` escrow proposals with BLAKE3-anchored terms;
11. ODRL-described access, exhibition, edition, resale, royalty, and derivative rights;
12. generated Rust/WASM identity, event-chain, projection-admission, and playback-drift primitives;
13. installable/offline PWA projection for television browsers;
14. content-addressed manufacture receipts and deterministic replay;
15. explicit ggen-legacy capability disposition and equivalence evidence.

## Authored authority

```text
ontology/platform.ttl
ontology/platform-shapes.ttl
rules/escrow.n3
rules/settlement.dl
queries/*.rq
templates/*.tera
fixtures/*.ttl
```

`generated/` is projection only. Repair ontology, queries, rules, or templates rather than editing output.

## Manufactured output

```text
generated/
├── package.json
├── index.html
├── public/
│   ├── manifest.webmanifest
│   └── sw.js
├── src/
│   ├── main.js
│   ├── style.css
│   ├── world.json
│   ├── world.ttl
│   ├── capabilities.json
│   ├── projections.json
│   ├── governance.json
│   ├── rights.json
│   └── system.mmd
├── wasm/
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs
│       └── bin/receipt.rs
├── scripts/
│   └── replay.mjs
└── README.md
```

## Manufacture and execute

```bash
cd packs/cyberpunk-tv-platform
ggen sync
cp -R rules generated/rules
cd generated
npm install
npm run build
npm run verify
npm run replay
npm run dev
```

`npm run build` crosses the Rust-to-WASM boundary through `wasm-pack`. `npm run verify` executes the Rust receipt binary over the exact manufactured tree. `npm run replay` executes the receipt path twice and requires byte-identical output.

## Acceptance

The bounded browser/TV subject is `ALIVE` only after all of the following are observed against one exact head:

```text
ggen sync: exit 0
ggen sync again: NO_GENERATED_DRIFT
SHACL admission: exit 0
npm install: exit 0
npm run build: exit 0
npm run verify: valid BLAKE3 receipt
npm run replay: REPLAY_MATCH
browser execution: UNRDF + Mermaid + deck.gl + WASM loaded
remote navigation: observed
watch-party synchronization: observed across two runtime bodies
positive escrow fixture: adopted settlement
negative escrow fixture: clean refusal
independent verifier: standing assigned
```

Until those crossings are observed, implementation standing remains `UNKNOWN` or `PARTIAL_ALIVE`; authored capability density is not execution evidence.
