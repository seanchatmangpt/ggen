# v26.8.1 subsystem research packet

**Status:** RESEARCH BASELINE — no capability is promoted by this packet.

This packet supplies the required research structure for one bounded subsystem. The title and repository path identify the subsystem under review.

## Required implementation inventory

The final revision must enumerate every owning crate, module, generated surface, ontology, schema, manifest, command, diagnostic, file, environment variable, boundary crossing, dependency, side effect, receipt field, telemetry record, refusal, recovery path, and historical legacy behavior associated with the subsystem.

## Required authority analysis

For each discovered element, record:

1. canonical authority source;
2. active production load path;
3. generated or manually authored status;
4. semantic owner under Conway's Law;
5. inputs admitted into `O*`;
6. manufacturing operation `μ`;
7. produced artifact or evidence;
8. mutation and external-actuation boundary.

## Required execution research

Source inspection must be followed by real execution. The final document must contain or reference:

- exact source and dependency head;
- reproducible setup and command;
- real input fixture crossing the declared boundary;
- observed stdout, stderr, filesystem, state, telemetry, OCEL, or protocol consequence;
- BLAKE3 or stronger content binding where applicable;
- positive witness;
- negative fixture proving the verifier fails when the capability is broken or faked;
- replay result;
- machine-readable verifier report.

Mocks, synthetic telemetry, fabricated receipts, hardcoded success, and documentation-only contracts are forbidden.

## Zero-information-loss mapping

Every discovered legacy element receives one disposition:

- `PRESERVED` — same observable behavior and recovery contract;
- `SUBSUMED` — behavior is provided by a new shared owner with executable equivalence evidence;
- `REPLACED` — behavior changes through an explicit migration with compatibility and recovery evidence;
- `ARCHIVED` — no longer active, but a tested restoration path exists;
- `REFUSED` — intentionally unsupported with approved rationale and impact;
- `UNKNOWN` — incomplete mapping; blocks the sunset.

The mapping must include successful behavior, error behavior, exit codes, ordering, defaults, aliases, file formats, signatures, hashes, timing assumptions, and operational recovery.

## Acceptance gates

This subsystem is admitted for v26.8.1 only when:

- repository observation and the coverage matrix agree;
- semantic ownership is unique or an explicit interoperability contract exists;
- the positive witness and negative falsifier both execute;
- replay is deterministic or divergence is typed and explained;
- evidence is externally inspectable;
- all legacy capabilities are dispositioned;
- an external verifier, not the subsystem itself, assigns standing.

Until then the standing is `UNKNOWN` or `PARTIAL_ALIVE`, and the ggen-legacy sunset remains blocked.
