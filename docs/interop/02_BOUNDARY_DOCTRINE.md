# Boundary Doctrine

## 1. Fundamental Rule
**You must not blur the boundaries between Kernel, Membrane, Body, Lifecycle, and Validator.**

## 2. Forbidden Moves
1. **Do not treat Genesis as an adapter layer.** Genesis does not parse JSON, CSV, or network protocols.
2. **Do not put outside-world dependencies inside Genesis.** Genesis is pure math (`A = μ(O)`), written in `no_std` Rust where possible.
3. **Do not treat ggen as the authority.** ggen projects data and builds parts; it does not decide consequence.
4. **Do not treat AtomVM/WASM as downstream sources.** They are the physical runtime bodies holding the Genesis kernel at the edge.
5. **Do not treat Pair2 as a compressed RDF triple.** Pair2 is a localized tuple `(left, right)` strictly bound to a predicate-fixed `RelationPage`.
6. **Do not store the predicate per tuple.** The `RelationPage` owns the predicate.
7. **Do not let an index become authority.** Indexes (HashMaps, B-Trees) are receipted, derived views. The `Construct8` packet stream is the absolute truth.
8. **Do not count mock/random matter.** Tuples without cryptographically verifiable source addresses cannot count toward real corpus claims.
9. **Do not claim implementation without evidence.** Every feature claim must point to a file, struct, or script.
10. **Do not use JSON inside Genesis.** JSON is strictly a boundary projection format owned by ggen.

## 3. The Interchangeable Part Stack

| Layer | Owner | Role |
| :--- | :--- | :--- |
| **Exterior Boundary** | External Systems | Source events, APIs, raw logs. |
| **Membrane Adapter** | ggen | Parses external boundary into `RelationPage` contexts. |
| **Custody Shell** | AtomVM / WASM | Ensures local actor event ownership and memory isolation. |
| **Consequence Kernel**| Genesis Core | Applies `O*`, processes `Construct8` packets, emits receipts. |
| **Projection Layer** | ggen | Expands receipts into OCEL, RDF, PROV for the outside world. |
| **Lifecycle Gate** | Truex | Approves or rejects the rollup of receipts into the enterprise corpus. |
| **Validation Layer** | wasm4pm / SHACL | Independently verifies projected outputs match public laws. |
