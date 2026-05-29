# Oracle Gaps

> Explanation. The specific defect this release was convened to close.

An **Oracle Gap** is a command that works only in theory: it is advertised — in the CLI
surface, in the docs, in a feature list — but when invoked it does not do the work it
claims. The name comes from the gap between what the *oracle* (the docs, the `--help`,
the option list) promises and what the machine delivers.

An Oracle Gap is not the *absence* of a feature. Absence is honest. The Oracle Gap is the
**false advertisement**: the surface says the capability is there, and it is not.

## Oracle Gaps closed in v26.5.28

These were found by inspection-by-command during this release and closed — each is now
either real, fails loudly, or is removed from the boundary:

| Gap | Before (theory) | After (closure) |
|-----|-----------------|-----------------|
| `ggen.construct` (MCP tool) | Returned fake success without constructing anything | Returns a loud `ErrorData::internal_error` directing the caller to `ggen sync`; the work it claimed is no longer claimed |
| SPARQL formatter | `format_raw` is error-tolerant and would reformat a *broken* SPARQL tree (fail-open) | Gated on `ll-sparql-parser` parse errors — refuses to rewrite anything with parse errors |
| `serve --protocol a2a` | A theory-only option that always errored | A2A is delivered as a **bridge library** (`ggen-lsp-a2a`), not a server flag — the false option is removed |
| cpmp scan | Wrote stub TTL/SHACL (`"# CPMP Catalog TTL"`) and emitted a success receipt over no real work | Subject to the no-receipt-for-stub-work rule (closure tracked in the boundary doc) |

## The five ambiguous nouns

Five CLI nouns — `a2a`, `framework`, `sigma`, `mcp`, `wizard` — were ambiguous: present in
the surface but not provable as finished capability inside v26.5.28. Per the
**non-deletion doctrine** (code is fossil evidence; nothing is deleted), they are not
removed from the repository. They are **archived**: gated behind a default-off
`experimental` Cargo feature so they leave the default release surface while the code is
preserved. Removing a command from the boundary is a lawful way to close an Oracle Gap —
the gap is the false advertisement, and archiving ends the advertisement without
destroying the work.

> Every command either works by executable proof or is removed from the release boundary
> before rest.

## How an Oracle Gap is detected

Not by reading the code's success path — by **running the command and inspecting the
world after**. The detection recipes live in `.claude/rules/coding-agent-mistakes.md`:
diff the lockfile mtime, `jq` the receipt for a non-empty signature, delete a required
input and confirm the command now fails non-zero. A command that still reports success
after its inputs are sabotaged is an Oracle Gap.

## See also

- [Motion does not count](motion-does-not-count.md) — the Oracle Gap is motion sold as value
- [Command proof matrix](../reference/cli/command-proof-matrix.md) — every noun's proof status
- [v26.5.28 boundary](../reference/release/v26-5-28-boundary.md) — KEEP/PROVE vs ARCHIVE decisions
