# JTBD Verb Classification Report

Generated: 2026-05-10T05:22:34Z
Binary: `/Users/sac/ggen/target/debug/ggen`

## Summary

| Class | Count |
|---|---|
| REAL    | 71 |
| PARTIAL | 6 |
| BROKEN  | 3 |
| SKIP    | 12 |
| **Total** | **92** |

## Per-Verb Classification

| Verb | Class | Exit | Evidence | Disposition (TBD) |
|---|---|---|---|---|
| `doctor.run` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.check` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.config` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.ontology` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.telemetry` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.registry` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.security` | REAL | 0 | rc=0 delta=n/a | |
| `doctor.full` | PARTIAL | 0 | rc=0 delta=n/a probe-failed | |
| `doctor.publish` | REAL | 0 | rc=0 delta=n/a | |
| `utils.doctor` | REAL | 0 | rc=0 delta=n/a | |
| `utils.env` | REAL | 0 | rc=0 delta=n/a | |
| `registry.list` | REAL | 0 | rc=0 delta=n/a | |
| `registry.search` | REAL | 0 | rc=0 delta=n/a | |
| `marketplace.list` | REAL | 0 | rc=0 delta=n/a | |
| `marketplace.search` | REAL | 0 | rc=0 delta=n/a | |
| `capability.list` | REAL | 0 | rc=0 delta=n/a | |
| `capability.graph` | REAL | 0 | rc=0 delta=n/a | |
| `capability.trust` | REAL | 0 | rc=0 delta=n/a | |
| `packs.list` | PARTIAL | 0 | rc=0 delta=no no-state-change | |
| `packs.doctor` | REAL | 0 | rc=0 delta=n/a | |
| `pack.add` | PARTIAL | 0 | rc=0 delta=no no-state-change | |
| `packs.validate` | REAL | 0 | rc=0 delta=n/a | |
| `packs.show` | REAL | 0 | rc=0 delta=n/a | |
| `packs.dependencies` | REAL | 0 | rc=0 delta=n/a | |
| `packs.compose` | REAL | 0 | rc=0 delta=n/a | |
| `packs.search` | REAL | 0 | rc=0 delta=n/a | |
| `packs.check_compatibility` | SKIP | n/a | subcommand not registered as direct verb | |
| `packs.install` | REAL | 0 | rc=0 delta=yes | |
| `packs.generate` | REAL | 0 | rc=0 delta=n/a | |
| `pack.remove` | REAL | 0 | rc=0 delta=yes | |
| `capability.enable` | REAL | 0 | rc=0 delta=yes | |
| `capability.inspect` | REAL | 0 | rc=0 delta=n/a | |
| `capability.conflicts` | REAL | 0 | rc=0 delta=n/a | |
| `capability.disable` | REAL | 0 | rc=0 delta=yes | |
| `policy.list` | REAL | 0 | rc=0 delta=n/a | |
| `policy.show` | REAL | 0 | rc=0 delta=n/a | |
| `policy.validate` | REAL | 0 | rc=0 delta=n/a | |
| `policy.check` | REAL | 0 | rc=0 delta=n/a | |
| `graph.load` | REAL | 0 | rc=0 delta=n/a | |
| `graph.query` | REAL | 0 | rc=0 delta=n/a | |
| `graph.export` | REAL | 0 | rc=0 delta=yes | |
| `graph.visualize` | REAL | 0 | rc=0 delta=n/a | |
| `ontology.validate` | REAL | 0 | rc=0 delta=n/a | |
| `ontology.generate` | REAL | 0 | rc=0 delta=yes | |
| `ontology.init` | PARTIAL | 0 | rc=0 delta=no no-state-change | |
| `template.list` | REAL | 0 | rc=0 delta=n/a | |
| `template.new` | REAL | 0 | rc=0 delta=yes | |
| `template.show` | REAL | 0 | rc=0 delta=n/a | |
| `template.get` | REAL | 0 | rc=0 delta=n/a | |
| `template.lint` | REAL | 0 | rc=0 delta=n/a | |
| `template.generate` | BROKEN | 1 | rc=1 delta=n/a | |
| `template.generate_tree` | BROKEN | 1 | rc=1 delta=n/a | |
| `template.regenerate` | BROKEN | 1 | rc=1 delta=n/a | |
| `sync.dry-run` | SKIP | n/a | needs ontology.ttl + ggen.toml manifest config | |
| `envelope.sign` | REAL | 0 | rc=0 | |
| `envelope.verify` | SKIP | n/a | needs Ed25519 verifying key derived from priv | |
| `envelope.chain_verify` | SKIP | n/a | needs verifying key + chain file | |
| `workflow.init` | REAL | 0 | rc=0 delta=yes | |
| `workflow.event` | REAL | 0 | rc=0 delta=yes | |
| `workflow.analyze` | REAL | 0 | rc=0 delta=n/a | |
| `workflow.discover` | PARTIAL | 0 | rc=0 delta=n/a probe-failed | |
| `workflow.synthesize` | REAL | 0 | rc=0 delta=n/a | |
| `workflow.report` | REAL | 0 | rc=0 delta=yes | |
| `telco.route` | REAL | 0 | rc=0 delta=n/a | |
| `telco.dial` | REAL | 0 | rc=0 delta=n/a | |
| `telco.switch` | REAL | 0 | rc=0 delta=n/a | |
| `telco.trunk` | REAL | 0 | rc=0 delta=n/a | |
| `telco.bluebox` | REAL | 0 | rc=0 delta=n/a | |
| `telco.tap` | REAL | 0 | rc=0 delta=n/a | |
| `telco.operator` | REAL | 0 | rc=0 delta=n/a | |
| `telco.phreak` | REAL | 0 | rc=0 delta=n/a | |
| `self_play.run` | PARTIAL | 0 | rc=0 delta=no no-state-change | |
| `self_play.validate` | REAL | 0 | rc=0 delta=n/a | |
| `semantic_os.compile` | REAL | 0 | rc=0 delta=n/a | |
| `semantic_os.manufacture` | REAL | 0 | rc=0 delta=n/a | |
| `semantic_os.doctor` | SKIP | n/a | needs law_id from prior compile | |
| `semantic_os.admit` | SKIP | n/a | needs prior compiled law + input file | |
| `semantic_os.replay` | SKIP | n/a | needs prior receipt file | |
| `semantic_os.runbook` | SKIP | n/a | needs law_id from prior compile | |
| `paper.templates` | REAL | 0 | rc=0 delta=n/a | |
| `paper.new` | REAL | 0 | rc=0 delta=yes | |
| `paper.validate` | REAL | 0 | rc=0 delta=n/a | |
| `paper.generate` | REAL | 0 | rc=0 delta=n/a | |
| `paper.init_bibliography` | REAL | 0 | rc=0 delta=yes | |
| `marketplace.show` | REAL | 0 | rc=0 delta=n/a | |
| `marketplace.info` | REAL | 0 | rc=0 delta=n/a | |
| `marketplace.doctor` | REAL | 0 | rc=0 delta=n/a | |
| `marketplace.install` | SKIP | n/a | duplicate of pack.add (lockfile mutation) | |
| `marketplace.sync` | SKIP | n/a | duplicate of top-level sync command | |
| `registry.info` | REAL | 0 | rc=0 delta=n/a | |
| `create.construct` | SKIP | n/a | needs OWL spec file (out of scope for smoke test) | |
| `validate.construct` | SKIP | n/a | needs prior construct module | |

## Classification Definitions

- **REAL** — exits 0, produces expected state delta, output structurally valid
- **PARTIAL** — exits 0 but: probe failed, no state change, or output contains hardcoded sentinel data
- **BROKEN** — non-zero exit, panic, or unparseable output
- **SKIP** — needs external service/auth/fixture not provisioned
