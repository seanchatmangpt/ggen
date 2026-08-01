# End-of-Day Cross-Repository Synthesis

## Preserve

Repositories remain separate authority domains. A pattern observed in one repository is not automatically valid in another. Each target retains its own `AGENTS.md`, generated-output ownership, architecture, dependency closure, validation ladder, base SHA, and release law.

The control plane manufactures **pull-request intents**, not branches, commits, pushes, or pull requests. Consequential GitHub writes remain outside this tool and require a target-specific implementation, validation evidence, a BLAKE3 receipt, and BRCE authorization.

## Fence

The admitted transformation is:

```text
same-day GitHub observation
→ exact repository/base/head identities
→ exact source path/blob identities
→ target capability intersection
→ deterministic transfer candidates and exclusions
→ non-actuating pull-request intent
→ target inspection and lawful implementation
→ target validation ladder
→ BLAKE3 receipt
→ BRCE GitHub actuation
→ consequence observation and replay
```

The tool does not infer equivalence from adjacency. A source artifact becomes a candidate only when one or more exported capabilities intersect the target repository's admitted capability set. The target implementation still has to prove architecture fit, dependency closure, execution, and replay.

## Input contract

`tools/eod-repo-synthesis/contracts/observation.schema.json` describes the observation carrier. Each repository records:

- exact base and observed head SHA;
- target capability admissions;
- repository constraints and validation commands;
- same-day work items grounded by GitHub URLs;
- exact source paths and Git blob SHA identities;
- capabilities exported by each source artifact.

The retained `2026-07-31` observation is grounded in immutable source objects from ggen, MFW, Ferroplan, Agile Protocol Specification, and wasm4pm. It is evidence for the bounded manufacturer, not a standing claim for those repositories.

## Output contract

The manufacturer emits one JSON intent and one Markdown projection per repository plus a manifest. Every intent carries:

- exact target base SHA;
- exact source repository, head, path, and blob identities;
- the admitted capability intersection;
- rejected source artifacts and reasons;
- repository-native constraints and validation obligations;
- `authority = intent_only`;
- `direct_actuation = false`;
- `standing = UNKNOWN`;
- an explicit `ggen-receipt/v2` requirement before GitHub actuation.

Targets without a dependency-compatible source candidate receive a deterministic `skip` disposition rather than an empty success claim.

## Typed refusals

| Code | Boundary |
|---|---|
| `EOD-SYNTH-001` | malformed observation or schema identity |
| `EOD-SYNTH-002` | duplicate repository identity |
| `EOD-SYNTH-003` | malformed commit or blob identity |
| `EOD-SYNTH-004` | duplicate source artifact identity |
| `EOD-SYNTH-005` | occupied output directory |
| `EOD-SYNTH-006` | absolute, traversing, or non-canonical repository path |
| `EOD-SYNTH-007` | direct actuation entering the observation graph |

## Verification

`verify.py` crosses real subprocess and filesystem boundaries. It proves:

1. two independent manufacturing runs are byte-identical;
2. manifest digests bind every emitted consequence;
3. a source blob identity mutation changes the complete bundle identity and propagates into downstream intents;
4. duplicate repositories, path escape, direct actuation, and occupied outputs refuse before publication;
5. candidate sources never equal their target repository;
6. all emitted standing remains `UNKNOWN` and all authority remains intent-only.

The verifier report is `PARTIAL_ALIVE` for this bounded deterministic transformation. GitHub writes, target repository builds, BLAKE3 receipt generation, external replay, and aggregate target standing remain `UNKNOWN` until their own execution evidence exists.

## Operation

```bash
python3 tools/eod-repo-synthesis/synthesize.py \
  --observation tools/eod-repo-synthesis/evidence/2026-07-31.observation.json \
  --output-dir /tmp/eod-repo-synthesis

python3 tools/eod-repo-synthesis/verify.py \
  --root . \
  --evidence-dir .ggen/eod-repo-synthesis
```

The generated intent bundle is an input to a later target-specific implementation agent. It is never sufficient evidence to open or merge a pull request by itself.
