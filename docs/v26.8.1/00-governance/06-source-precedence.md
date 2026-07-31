# Source precedence

When repository sources disagree, the v26.8.1 program resolves authority in this order:

1. enforced constitutional rules and CI guards;
2. active Cargo workspace and exact dependency graph;
3. admitted ontology, manifest, schema, and policy loaded by production code;
4. production implementation and generated source at the exact head;
5. executable tests, verifiers, negative fixtures, and real-boundary evidence;
6. receipts and replay evidence bound to that head;
7. generated documentation and indexes;
8. manually authored narrative;
9. historical snapshots and git history.

Historical material remains necessary for Chesterton analysis but cannot override active behavior. Conversely, active behavior cannot erase a historical capability without recording its disposition and migration consequence.
