# Summary

[**The ggen Pack Language: Patterns for Manufacturing Verified Software**](README.md)

> A pattern language for pack writers. Read from the largest system boundary toward the smallest construction decision, then return through verification, receipt, and replay.
>
> Each pattern names a recurring context, the forces that make the problem difficult, a bounded configuration that resolves those forces, the consequences of applying it, and a falsifier that can prove the pattern was not realized.

# Using the Pattern Language

- [Preface: Ontology Is the Production System](front-matter/preface.md)
- [Who This Book Is For](front-matter/audience.md)
- [What a Level Five Pack Produces](front-matter/level-five-outcome.md)
- [How to Read the Book](front-matter/how-to-read.md)
- [The Continuous TCPS Case Study](front-matter/tcps-case-study.md)
- [Notation and Standing Vocabulary](front-matter/notation-and-standing.md)
- [Laboratory Requirements](front-matter/laboratory-requirements.md)
- [The Final Acceptance Test](front-matter/final-acceptance-test.md)

---

# Field I — The Whole Manufacturing System

These patterns establish the largest boundary: a pack is not a template bundle but a lawful manufacturing system spanning `ggen-config`, `ggen-engine`, graph admission, projection, bounded writes, consumers, and receipts.

- [1. From Code Generation to Manufacturing](foundations/001-manufacturing-not-codegen.md)
- [2. The Artifact Equation](foundations/002-artifact-equation.md)
- [3. Ontology as Authority](foundations/003-ontology-as-authority.md)
- [4. SPARQL as Construction Language](foundations/004-sparql-as-construction-language.md)
- [5. Tera as Rendering Surface](foundations/005-tera-as-rendering-surface.md)
- [6. SPARQL Gates as Admission](foundations/006-shacl-as-admission-gate.md)
- [7. Cargo as Verification Boundary](foundations/007-cargo-as-verification-boundary.md)
- [8. Receipts as Lineage Proof](foundations/008-receipts-as-lineage-proof.md)
- [9. The Five-Stage Manufacturing Pipeline](foundations/009-five-stage-pipeline.md)
- [11. Source Law and Generated Projection](foundations/011-source-law-generated-projection.md)
- [12. Why Generated Files Are Not Authority](foundations/012-generated-files-not-authority.md)

# Field II — The Pack as a Living Part

A pack must have identity, a declared boundary, a consumption path, owned outputs, executable gates, and replayable provenance.

- [13. Packs as Routable Capability Cells](pack-model/013-routable-capability-cells.md)
- [14. Pack Boundaries](pack-model/014-pack-boundaries.md)
- [15. Pack Identity](pack-model/015-pack-identity.md)
- [16. Pack Inputs and Outputs](pack-model/016-pack-inputs-outputs.md)
- [17. Pack Metadata](pack-model/017-pack-metadata.md)
- [18. Pack Ontology](pack-model/018-pack-ontology.md)
- [19. Pack Templates](pack-model/019-pack-templates.md)
- [20. Pack Gates](pack-model/020-pack-shapes.md)
- [21. Pack Fixtures](pack-model/021-pack-fixtures.md)
- [22. Pack Hooks](pack-model/022-pack-hooks.md)
- [23. Pack Reference Oracles](pack-model/023-pack-reference-oracles.md)
- [24. Pack Receipts](pack-model/024-pack-receipts.md)

# Field III — Choose the Right Kind of Part

Do not force every capability into the same pack form. Select the smallest pack type that carries the required law, behavior, proof, and lifecycle.

- [25. Why Pack Type Must Be Explicit](pack-taxonomy/025-explicit-pack-type.md)
- [26. Type One: Codegen Packs](pack-taxonomy/026-codegen-packs.md)
- [27. Type Two: Knowledge Hook Packs](pack-taxonomy/027-knowledge-hook-packs.md)
- [28. Type Three: Case-Study Corpus Packs](pack-taxonomy/028-case-study-packs.md)
- [29. Why Pure Ontology Packs Are Inert](pack-taxonomy/029-inert-ontology-packs.md)
- [30. Choosing a Consumption Path](pack-taxonomy/030-choosing-consumption-path.md)
- [31. Mixed Packs](pack-taxonomy/031-mixed-packs.md)
- [32. Separating Product and Release Packs](pack-taxonomy/032-product-release-split.md)
- [34. When the Pack Must Generate Behavior](pack-taxonomy/034-generate-behavior.md)
- [35. When the Pack Must Generate Proof](pack-taxonomy/035-generate-proof.md)
- [36. When the Pack Must Generate a Whole Product](pack-taxonomy/036-generate-whole-product.md)

# Field IV — Standing before Scale

A directory, valid Turtle file, or successful render is not completion. Advance only through observed maturity cells.

- [37. L0: Inert](maturity/037-l0-inert.md)
- [38. L1: Syntactically Valid](maturity/038-l1-syntactically-valid.md)
- [39. L2: Consumed](maturity/039-l2-consumed.md)
- [40. L3: Verified](maturity/040-l3-verified.md)
- [41. L4: Hardened](maturity/041-l4-hardened.md)
- [42. L5: Complete Substitute](maturity/042-l5-complete-substitute.md)
- [43. Generation Depth](maturity/043-generation-depth.md)
- [44. Handler-Gap Size](maturity/044-handler-gap-size.md)
- [46. Consumer Effort](maturity/046-consumer-effort.md)
- [48. Regeneration Lifecycle](maturity/048-regeneration-lifecycle.md)
- [49. Target-API Fidelity](maturity/049-target-api-fidelity.md)
- [50. Evidence per Maturity Cell](maturity/050-evidence-per-cell.md)
- [51. Why One Overall Level Is Insufficient](maturity/051-no-single-level.md)
- [52. The Level Five Calibration Rule](maturity/052-l5-calibration-rule.md)

# Field V — Independent Reality

A pack cannot certify itself against an oracle manufactured from the same assumptions.

- [53. The Reference Is the Customer](reference/053-reference-is-customer.md)
- [54. Independent Means Independent](reference/054-independent-means-independent.md)
- [55. Reference Source Trees](reference/055-reference-source-trees.md)
- [56. Reference Public APIs](reference/056-reference-public-apis.md)
- [57. Reference Behavior](reference/057-reference-behavior.md)
- [58. Reference Tests](reference/058-reference-tests.md)
- [59. Verbatim Test Reuse](reference/059-verbatim-test-reuse.md)
- [60. Read-Only Reference Fixtures](reference/060-read-only-reference-fixtures.md)
- [62. Reference Digests](reference/062-reference-digests.md)
- [63. Disclosed Corrective Divergence](reference/063-corrective-divergence.md)
- [64. When the Reference Is Defective](reference/064-defective-reference.md)

# Field VI — The Ontology Neighborhood

Model stable semantic identities and relations before choosing source-language syntax.

- [65. Start from Real Source](ontology/065-start-from-real-source.md)
- [66. One Semantic Identity per Domain Concept](ontology/066-one-semantic-identity.md)
- [67. Classes, Individuals, and Properties](ontology/067-classes-individuals-properties.md)
- [68. Stable IRIs](ontology/068-stable-iris.md)
- [69. Reuse Public Vocabulary](ontology/069-public-vocabulary-reuse.md)
- [70. Pack-Local Vocabulary](ontology/070-pack-local-vocabulary.md)
- [71. Modeling Modules](ontology/071-modeling-modules.md)
- [72. Modeling Types](ontology/072-modeling-types.md)
- [73. Modeling Functions](ontology/073-modeling-functions.md)
- [74. Modeling Parameters and Return Types](ontology/074-modeling-signatures.md)
- [76. Modeling Tests](ontology/076-modeling-tests.md)
- [77. Modeling Files and Output Paths](ontology/077-modeling-files.md)
- [78. Modeling Build and Release Assets](ontology/078-modeling-release-assets.md)
- [79. Modeling Historical Lineage](ontology/079-modeling-lineage.md)

# Field VII — Admission Gates

Parse first, admit second. The graph becomes production law only after explicit constraints reject malformed, ambiguous, contaminating, or unsafe states.

- [81. Syntax Is Not Admission](admission/081-syntax-not-admission.md)
- [82. Pack Gates as Production Law](admission/082-pack-shapes-production-law.md)
- [83. Required Properties](admission/083-required-properties.md)
- [84. Cardinality Boundaries](admission/084-cardinality-boundaries.md)
- [85. Datatype Boundaries](admission/085-datatype-boundaries.md)
- [86. IRI Boundaries](admission/086-iri-boundaries.md)
- [87. Output-Path Boundaries](admission/087-output-path-boundaries.md)
- [88. Template Ownership Constraints](admission/088-template-ownership-constraints.md)
- [89. Namespace Isolation Constraints](admission/089-namespace-isolation.md)
- [90. Cross-Resource Constraints](admission/090-cross-resource-constraints.md)
- [91. Positive Fixtures](admission/091-positive-fixtures.md)
- [92. Negative Fixtures](admission/092-negative-fixtures.md)
- [93. Adversarial Fixtures](admission/093-adversarial-fixtures.md)
- [94. Fail-Closed Pack Loading](admission/094-fail-closed-loading.md)

# Field VIII — Deterministic Projection

SPARQL selects and constructs admitted facts. Tera projects those facts. Neither surface may silently broaden ownership.

- [95. SELECT as a Typed Extraction Surface](sparql/095-select-extraction.md)
- [96. CONSTRUCT as Derived Law](sparql/096-construct-derived-law.md)
- [97. Query Only the Pack You Own](sparql/097-query-owned-pack.md)
- [98. Shared Classes and Union Contamination](sparql/098-shared-class-contamination.md)
- [99. IRI-Prefix Filters](sparql/099-iri-prefix-filters.md)
- [100. Ordering for Determinism](sparql/100-ordering-for-determinism.md)
- [104. Query Contracts](sparql/104-query-contracts.md)
- [105. Query Fixtures](sparql/105-query-fixtures.md)
- [106. Detecting Cross-Pack Leakage](sparql/106-detecting-cross-pack-leakage.md)
- [107. Templates Are Projection Law](tera/107-templates-projection-law.md)
- [108. Front Matter](tera/108-template-front-matter.md)
- [109. Output Ownership](tera/109-output-ownership.md)
- [110. `force` and Regeneration](tera/110-force-regeneration.md)
- [111. `skip_if` and Local Freeze](tera/111-skip-if-local-freeze.md)
- [112. Injection Modes](tera/112-injection-modes.md)
- [113. One Writer per Output](tera/113-one-writer-per-output.md)
- [115. Escaping Source Text](tera/115-escaping-source-text.md)
- [119. Formatting without Semantic Drift](tera/119-formatting-without-drift.md)
- [120. Template Review as Compiler Review](tera/120-template-review.md)

# Field IX — Complete Product Surfaces

Generate coherent modules, crates, interfaces, and manifests rather than fragments that require hidden expert repair.

- [121. Generate Modules, Not Fragments](rust-generation/121-modules-not-fragments.md)
- [122. Generate Public Types](rust-generation/122-public-types.md)
- [123. Generate Enums and Variants](rust-generation/123-enums-and-variants.md)
- [124. Generate Structs and Fields](rust-generation/124-structs-and-fields.md)
- [125. Generate Trait Boundaries](rust-generation/125-trait-boundaries.md)
- [126. Generate Functions](rust-generation/126-functions.md)
- [127. Generate Typestate](rust-generation/127-typestate.md)
- [128. Generate `no_std` Code](rust-generation/128-no-std.md)
- [129. Forbid Unsafe Code](rust-generation/129-forbid-unsafe.md)
- [130. Generate Module Aggregation](rust-generation/130-module-aggregation.md)
- [131. Generate Workspace Manifests](rust-generation/131-workspace-manifests.md)
- [132. Generate Multiple Crates](rust-generation/132-multiple-crates.md)
- [133. Generate C ABI Surfaces](rust-generation/133-c-abi.md)
- [134. Generate WebAssembly Surfaces](rust-generation/134-wasm.md)

# Field X — Proof beside Product

Every generated capability needs an independently meaningful verifier at the same scale.

- [135. Why Generated Code Needs Generated Proof](generated-proof/135-code-needs-proof.md)
- [136. Avoid Tautological Tests](generated-proof/136-avoid-tautology.md)
- [137. Independent Expected Values](generated-proof/137-independent-expected-values.md)
- [138. Chicago TDD](generated-proof/138-chicago-tdd.md)
- [139. Compile-Time Proof through Signatures](generated-proof/139-compile-time-signature-proof.md)
- [140. Runtime Behavioral Proof](generated-proof/140-runtime-behavioral-proof.md)
- [141. Verbatim Reference Tests](generated-proof/141-verbatim-reference-tests.md)
- [143. Integration Tests in Real Consumers](generated-proof/143-integration-tests-real-consumers.md)
- [145. First-Sync Green](generated-proof/145-first-sync-green.md)
- [146. Proof Failure as Jidoka](generated-proof/146-proof-failure-jidoka.md)
- [147. Mutation Tests](generated-proof/147-mutation-tests.md)
- [148. Proof Receipts](generated-proof/148-proof-receipts.md)

# Field XI — The Consumer Place

A pack achieves standing only in a real consumer that does not depend on the pack author’s tacit knowledge.

- [149. A Pack without a Consumer Is Not Finished](consumer/149-pack-needs-consumer.md)
- [150. `ggen.toml`](consumer/150-ggen-toml.md)
- [152. Source-Law Inputs](consumer/152-source-law-inputs.md)
- [153. Generated Output Mounts](consumer/153-generated-output-mounts.md)
- [154. Module Wiring](consumer/154-module-wiring.md)
- [155. Engine-Owned Aggregators](consumer/155-engine-owned-aggregators.md)
- [156. Zero-Knowledge Consumer Setup](consumer/156-zero-knowledge-consumer.md)
- [157. Build the Consumer](consumer/157-build-consumer.md)
- [158. Run the Consumer](consumer/158-run-consumer.md)
- [159. Test the Consumer](consumer/159-test-consumer.md)
- [160. Consume the Same Pack Twice](consumer/160-two-consumers.md)

# Field XII — The Pack Neighborhood

Composition creates a union graph, shared namespaces, shared output trees, and therefore new interference forces.

- [161. The Union Graph](composition/161-union-graph.md)
- [162. Namespace Safety](composition/162-namespace-safety.md)
- [163. Shared Vocabulary Reuse](composition/163-shared-vocabulary.md)
- [164. Local Individuals under Shared Classes](composition/164-local-individuals.md)
- [165. Cross-Pack Query Contamination](composition/165-query-contamination.md)
- [166. Output-Path Collisions](composition/166-output-collisions.md)
- [167. Module-Wiring Collisions](composition/167-module-wiring-collisions.md)
- [168. Single-Writer Aggregation](composition/168-single-writer-aggregation.md)
- [169. Combined Consumer Tests](composition/169-combined-consumer-tests.md)
- [170. Eleven-Pack Collision Laboratory](composition/170-eleven-pack-lab.md)
- [171. Composition Receipts](composition/171-composition-receipts.md)
- [172. Proving Safe Union](composition/172-proving-safe-union.md)

# Field XIII — Time, Change, and Repair

Regeneration is ordinary operation. A pack must preserve lawful local work, expose drift, and reproduce prior consequences.

- [173. Regeneration Is Normal Operation](regeneration/173-regeneration-normal.md)
- [174. Sync Once](regeneration/174-sync-once.md)
- [175. Sync Twice](regeneration/175-sync-twice.md)
- [176. Byte-Identical Output](regeneration/176-byte-identical.md)
- [177. Detect Nondeterministic Ordering](regeneration/177-nondeterministic-ordering.md)
- [178. Detect Template Drift](regeneration/178-template-drift.md)
- [179. Detect Ontology Drift](regeneration/179-ontology-drift.md)
- [180. Detect Reference Drift](regeneration/180-reference-drift.md)
- [181. Content Hashes](regeneration/181-content-hashes.md)
- [182. Lock Files](regeneration/182-lock-files.md)
- [183. Refuse Stale Sources](regeneration/183-refuse-stale-sources.md)
- [184. Freeze, Inject, Replace, or Refuse](regeneration/184-write-modes.md)
- [185. Longitudinal Verification](regeneration/185-longitudinal-verification.md)
- [186. Evolution without Hand Editing](regeneration/186-evolution-without-hand-editing.md)

# Field XIV — Receipts and Standing

Evidence must travel with the manufactured part and bind observation, admission, actuation, and consequence.

- [187. Every Station Emits Evidence](receipts/187-every-station-evidence.md)
- [188. Input Digests](receipts/188-input-digests.md)
- [189. Ontology Digests](receipts/189-ontology-digests.md)
- [190. Template Digests](receipts/190-template-digests.md)
- [191. Output Digests](receipts/191-output-digests.md)
- [192. Build Receipts](receipts/192-build-receipts.md)
- [193. Test Receipts](receipts/193-test-receipts.md)
- [194. Idempotency Receipts](receipts/194-idempotency-receipts.md)
- [195. Inspection Receipts](receipts/195-inspection-receipts.md)
- [197. Corrective-Divergence Ledger](receipts/197-divergence-ledger.md)
- [198. CycloneDX and SPDX](receipts/198-sbom.md)
- [199. in-toto and SLSA](receipts/199-provenance.md)
- [200. Signing and Verification](receipts/200-signing-verification.md)

# Field XV — Change the Engine Only at the Boundary

Prefer pack law. Extend engine machinery only when the required admission, ownership, or receipt semantics cannot be expressed by a pack.

- [201. Pack-Shipped Gates Must Execute](engine/201-pack-shapes-execute.md)
- [202. Fail-Closed Pack Admission](engine/202-fail-closed-pack-admission.md)
- [203. Engine-Generated Module Aggregation](engine/203-engine-module-aggregation.md)
- [204. Proof Gates in CI](engine/204-proof-gates-ci.md)
- [205. Reflexive Receipts](engine/205-reflexive-receipts.md)
- [206. Generated-Output Ownership](engine/206-generated-output-ownership.md)
- [207. Engine Error Codes](engine/207-engine-error-codes.md)
- [210. No Engine Change When Pack Law Is Enough](engine/210-no-engine-change.md)
- [211. When Engine Work Is Unavoidable](engine/211-engine-work-unavoidable.md)
- [212. Prove the Engine Change against Multiple Packs](engine/212-engine-change-multipack-proof.md)

# Field XVI — The Level Five Sequence

Apply these patterns as a generative sequence. Each decision reduces the remaining design space without closing reversible choices too early.

- [213. Write the ARD and PRD](level-five-design/213-write-ard-prd.md)
- [214. Define the Complete Product Surface](level-five-design/214-complete-product-surface.md)
- [215. Build the Ontology-to-Module Matrix](level-five-design/215-ontology-module-matrix.md)
- [216. Build the Dimension-to-Acceptance Matrix](level-five-design/216-dimension-acceptance-matrix.md)
- [217. Name Every Acceptance Artifact](level-five-design/217-name-every-artifact.md)
- [218. Define the Reference Oracle](level-five-design/218-define-reference-oracle.md)
- [219. Define the Consumer](level-five-design/219-define-consumer.md)
- [220. Define the Proof Suite](level-five-design/220-define-proof-suite.md)
- [221. Define the Drift Policy](level-five-design/221-define-drift-policy.md)
- [222. Define the Receipt Schema](level-five-design/222-define-receipt-schema.md)
- [223. Define the Release Surface](level-five-design/223-define-release-surface.md)
- [224. Define the Level Five Definition of Done](level-five-design/224-level-five-dod.md)

# Field XVII — A Complete Pattern in Practice: TCPS

The TCPS sequence demonstrates the language at product scale: canonical vocabulary, complete generation, multi-crate projection, release manufacturing, defect conversion, and standing.

- [225. Why TCPS Is the Right Level Five Case Study](tcps-core/225-why-tcps.md)
- [226. Japanese Vocabulary as Canonical Law](tcps-core/226-japanese-vocabulary.md)
- [228. Model the 24 Reference Modules](tcps-core/228-model-24-modules.md)
- [235. 標準作業](tcps-core/235-standard-work.md)
- [236. 自働化 through Typestate](tcps-core/236-jidoka-typestate.md)
- [239. 改善 and 受領証](tcps-core/239-kaizen-receipts.md)
- [241. Create `tcps-core-pack`](tcps-generation/241-create-core-pack.md)
- [242. Transcribe the Reference into RDF](tcps-generation/242-transcribe-reference.md)
- [243. Generate All 24 Modules](tcps-generation/243-generate-24-modules.md)
- [247. Run the Reference Tests Unmodified](tcps-generation/247-run-reference-tests.md)
- [251. Six-Case Conformance E2E](tcps-generation/251-six-case-e2e.md)
- [252. Sync-Twice Idempotency](tcps-generation/252-sync-twice.md)
- [253. Move from One Crate to Five](tcps-product/253-one-to-five-crates.md)
- [259. Generate the Workspace `Cargo.toml`](tcps-product/259-workspace-manifest.md)
- [264. Run the Richer Product Test Suite](tcps-product/264-product-test-suite.md)
- [265. Create `tcps-release-pack`](tcps-release/265-create-release-pack.md)
- [278. Generate SBOM, Provenance, and Signing Paths](tcps-release/278-supply-chain-assets.md)
- [279. Why Syntax Validation Was Insufficient](tcps-failures/279-syntax-insufficient.md)
- [281. Shared-Class Cross-Contamination](tcps-failures/281-shared-class-contamination.md)
- [283. Duplicate Output Ownership](tcps-failures/283-duplicate-output-ownership.md)
- [290. Turn Every Defect into a New Standard](tcps-failures/290-defect-to-standard.md)
- [291. Generate the Inspection Receipt](tcps-standing/291-inspection-receipt.md)
- [297. Compare Generated and Reference Trees](tcps-standing/297-tree-comparison.md)
- [301. Close the Level Five Evidence Matrix](tcps-standing/301-close-evidence-matrix.md)
- [302. The TCPS Level Five Acceptance Test](tcps-standing/302-tcps-l5-acceptance.md)
- [337. 自らを造る機械は、自らを検査しなければならない](tcps-standing/337-jikoken-kensa.md)

# Field XVIII — Make a New Language of Your Own

- [303. Select a Bounded Product Surface](practicum/303-select-product-surface.md)
- [304. Locate an Independent Reference](practicum/304-locate-reference.md)
- [305. Inventory Every Required Artifact](practicum/305-inventory-artifacts.md)
- [306. Classify the Pack Type](practicum/306-classify-pack.md)
- [307. Write the ARD and PRD](practicum/307-write-ard-prd.md)
- [308. Create the Ontology-to-Artifact Matrix](practicum/308-ontology-artifact-matrix.md)
- [309. Create the Acceptance Matrix](practicum/309-acceptance-matrix.md)
- [310. Author the Ontology](practicum/310-author-ontology.md)
- [311. Author the Gates](practicum/311-author-shapes.md)
- [312. Author the Projections](practicum/312-author-projections.md)
- [313. Author the Templates](practicum/313-author-templates.md)
- [314. Generate the Proof Suite](practicum/314-generate-proof-suite.md)
- [315. Build Two Independent Consumers](practicum/315-two-consumers.md)
- [316. Prove Multi-Pack Safety](practicum/316-multipack-safety.md)
- [317. Prove Idempotency and Drift Refusal](practicum/317-idempotency-drift.md)
- [318. Emit Receipts and Supply-Chain Evidence](practicum/318-receipts-supply-chain.md)
- [319. Score the Seven Dimensions](practicum/319-score-seven-dimensions.md)
- [320. Present the Level Five Certification Bundle](practicum/320-certification-bundle.md)

# Field XIX — Certification Laboratories

- [321. Detect an Inert Pack](certification/321-detect-inert-pack.md)
- [322. Add a Real Consumption Path](certification/322-add-consumption-path.md)
- [323. Stop Namespace Leakage](certification/323-stop-namespace-leakage.md)
- [324. Resolve Output Ownership](certification/324-resolve-output-ownership.md)
- [325. Generate a Complete Rust Module](certification/325-generate-rust-module.md)
- [326. Generate an Independent Proof](certification/326-generate-independent-proof.md)
- [327. Reuse a Verbatim Test](certification/327-verbatim-test.md)
- [328. Build a Zero-Knowledge Consumer](certification/328-zero-knowledge-consumer.md)
- [329. Compose Eleven Packs](certification/329-compose-eleven-packs.md)
- [330. Prove Byte-Identical Regeneration](certification/330-byte-identical-regeneration.md)
- [331. Refuse Reference Drift](certification/331-refuse-reference-drift.md)
- [332. Emit a Complete Receipt Chain](certification/332-complete-receipt-chain.md)
- [333. Generate a Multi-Crate Product](certification/333-multicrate-product.md)
- [334. Generate Release Scaffolding](certification/334-release-scaffolding.md)
- [335. Run the Final Adversarial Audit](certification/335-adversarial-audit.md)
- [336. Graduation: A Level Five Pack](certification/336-graduation.md)

---

# Pattern Reference

- [Appendix A — Canonical Pack Directory Layout](appendices/a-pack-layout.md)
- [Appendix B — Canonical `pack.toml`](appendices/b-pack-toml.md)
- [Appendix C — Canonical `ggen.toml`](appendices/c-ggen-toml.md)
- [Appendix D — Canonical Ontology Vocabulary](appendices/d-ontology-vocabulary.md)
- [Appendix E — Canonical Admission Gates](appendices/e-shacl-shapes.md)
- [Appendix F — Canonical SPARQL Queries](appendices/f-sparql-queries.md)
- [Appendix G — Canonical Tera Front Matter](appendices/g-tera-front-matter.md)
- [Appendix H — Canonical Generated-Proof Template](appendices/h-generated-proof.md)
- [Appendix I — Canonical Chicago-TDD Consumer](appendices/i-chicago-tdd-consumer.md)
- [Appendix J — Canonical Idempotency Script](appendices/j-idempotency-script.md)
- [Appendix K — Canonical Drift-Refusal Script](appendices/k-drift-refusal.md)
- [Appendix L — Canonical Inspection Receipt](appendices/l-inspection-receipt.md)
- [Appendix M — Canonical `MANIFEST.256` Workflow](appendices/m-manifest-workflow.md)
- [Appendix N — Pack Maturity Scoring Worksheet](appendices/n-maturity-worksheet.md)
- [Appendix O — Level Five Definition of Done](appendices/o-level-five-dod.md)
- [Appendix P — TCPS Ontology-to-Module Matrix](appendices/p-tcps-module-matrix.md)
- [Appendix Q — TCPS 132-File Product Inventory](appendices/q-tcps-product-inventory.md)
- [Appendix R — TCPS Corrective-Divergence Ledger](appendices/r-tcps-divergence-ledger.md)
- [Appendix S — ggen Failure-Code Reference](appendices/s-failure-codes.md)
- [Appendix T — Glossary](appendices/t-glossary.md)
- [Appendix U — Bibliography and Project Record](appendices/u-bibliography.md)
- [Appendix V — Index](appendices/v-index.md)
