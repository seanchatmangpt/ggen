# ggen Self-Host Pack

This pack makes the `ggen` repository a first-class consumer of its own manufacturing system.

```text
exact Git tree
→ deterministic read-only observation
→ independent Git-object verification
→ admitted RDF
→ ggen pack resolution + gates
→ repository census
→ authority map
→ load-path ledger
→ output-ownership ledger
→ Jira/agent work package
→ ggen receipt
→ second-sync byte identity
```

## Authority boundary

The observer is a deliberately small bootstrap kernel. It may read Git and tracked file bytes and may write only under `self-host/`. It cannot call cloud APIs, mutate Git, update trackers, or decide promotion standing.

`observe_repository.py` contains the repository model. `observe_exact_tree.py` is the only supported executor: it binds symlinks and gitlinks to Git semantics and refuses ambient host-file traversal. `verify_observation.py` independently reconstructs the exact Git object set and verifies every observed byte and receipt binding.

All human-facing reports and implementation tickets are pack projections. The observer emits facts; ggen admits and manufactures consequences.

## Lifecycle

```bash
cargo build -p ggen-cli-lib --bin ggen
python3 -m unittest discover -s self-host/tests -p 'test_*.py' -v
python3 self-host/scripts/observe_exact_tree.py
python3 self-host/scripts/verify_observation.py
cd self-host
../target/debug/ggen sync run
../target/debug/ggen receipt verify
../target/debug/ggen sync run
../target/debug/ggen receipt verify
```

Before observation, the seed ontology must be refused by `010_observation_complete.rq`. Planning mode may contain blocking findings and generates the complete retrofit backlog. A crown is separately asserted only after those findings are closed and clean replay is independently recorded.

## What this first checkpoint changes

It replaces handwritten repository review as the authority for mechanical facts. Cargo members, package identities, packs, templates, workflows, scripts, generated markers, output claims, and layout drift are observed from the exact revision. Existing authored architecture prose remains judgment, not fabricated observation.
