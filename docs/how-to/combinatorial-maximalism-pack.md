# Manufacture a Combinatorial Maximalism cell

`ggen-combinatorial-maximalism-pack` converts a bounded RDF design space into an executable construction verifier and receipted actuation broker.

## Source boundary

A consumer authors exactly two files:

```text
ggen.toml
ontology.ttl
```

The ontology declares dimensions, reversible options, candidate combinations, proof standing, authority, one broker, one actuation contract per authorized candidate, and intent-only hooks. The pack refuses the graph before rendering when any constitutional distinction collapses.

## Law-state sequence

```text
parse
→ route
→ admit or refuse
→ manufacture candidate lattice
→ verify coverage and proofs
→ authorize one candidate
→ actuate through broker
→ atomically colocate output and receipt
→ replay observed output
```

The generated Rust verifier independently recomputes exhaustive or pairwise coverage. For exhaustive mode it multiplies admitted option cardinalities and requires the unique candidate signatures to equal the full Cartesian product. This prevents `expectedCandidateCount` from becoming an unsupported self-assertion.

## Zero unreceipted actuation

The broker does not publish an output file separately from its receipt. It builds a hidden staging transaction containing both surfaces, syncs them, then atomically renames the complete directory into the visible transaction set. A surviving visible actuation therefore has a colocated BLAKE3 receipt.

## Verification ladder

Run:

```bash
bash scripts/verify-combinatorial-maximalism-pack.sh
```

The recipe performs:

```text
static ownership contract
→ real ggen build
→ valid graph gates
→ first sync and ggen receipt verification
→ Rust format, Clippy, compile, and process/filesystem integration test
→ public CLI verify, actuate, and replay
→ unauthorized-candidate refusal with no transaction growth
→ output-tamper replay refusal
→ four graph sabotage refusals
→ second sync and byte-identity comparison
```

The strongest current claim is scoped to the pack plus executable specimen. The pack does not prove that arbitrary irreversible systems can be made reversible. It proves that the construction domain remains reversible until the generated broker receives a verified and authorized candidate, and that the specimen leaves no visible output without a cryptographically derived receipt.
