# Zero information loss law

The sunset proof is a total disposition function over the admitted legacy capability set.

Let `F_legacy` be every externally observable or recovery-relevant legacy function. Let `D(f)` be its disposition.

Sunset requires:

`forall f in F_legacy: D(f) in {PRESERVED, SUBSUMED, REPLACED, ARCHIVED, REFUSED}`

and:

`count(D(f) = UNKNOWN) = 0`

## Discovery domains

The inventory must include commands, arguments, aliases, defaults, exit codes, diagnostics, schemas, environment variables, files, directories, network boundaries, receipts, signatures, hashes, telemetry, events, lockfiles, caches, generated/manual merge behavior, package identity, publication behavior, examples, tests, negative fixtures, operational runbooks, and recovery paths.

## Validation

Completeness is not inferred from document count. A repository observer must enumerate actual surfaces, compare them with the coverage matrix, and refuse a passing report when an observed surface is missing, duplicated under conflicting owners, or mapped without an executable verifier.
