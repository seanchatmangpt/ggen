# clap-noun-verb-cli — canonical ggen example

This is ggen's canonical end-to-end example: an RDF ontology (`schema/clap-noun-verb-ontology.ttl`)
describes a noun-verb CLI's commands as `cnv:Command` individuals; `ggen sync` projects them into
real, compiling `#[verb]` route wrappers (`src/clap_noun_verb_routes.rs`, generated — do not edit)
via the `clap-noun-verb-pack` ggen pack (`../../packs/clap-noun-verb-pack/`); hand-written business
logic lives behind a stable seam in `src/verbs/handlers.rs`.

Verified end-to-end this session: `ggen sync run` generates real, idempotent output; the project
compiles; all three generated commands run with real exit codes and real typed argument
validation.

## Try it

```bash
ggen sync run --dry-run   # preview
ggen sync run             # generate src/clap_noun_verb_routes.rs
cargo build
./target/debug/clap-noun-verb-cli --help
./target/debug/clap-noun-verb-cli session login --token abc123
./target/debug/clap-noun-verb-cli session verify
./target/debug/clap-noun-verb-cli user create --name Alice --email alice@example.com
```

## Pattern

- The generated file is a **thin wrapper only** — no business logic. Each `#[verb]` function's
  body is exactly one call into `crate::verbs::handlers::*`; a missing handler is a compile
  error, not a silent no-op.
- Typed, optionally-`Option<T>` arguments are projected from `cnv:args`' `field|rust_type|
  required|about` (`@@`-joined) encoding — the same wire format
  [~/clap-noun-verb](../../packs/clap-noun-verb-pack/ontology.ttl) uses for its own
  `queries/verb-signatures.rq`.
- The noun/verb are passed explicitly to `#[verb("verb", "noun")]` rather than relying on
  filename-based noun inference — required here since every command lives in one generated file,
  not one file per noun.
- Regenerating is idempotent: an unchanged ontology produces byte-identical output
  (`"unchanged: content identical"`).

## Known limitation

Only named (`--flag value`) arguments are wired, not positional args — the pack's `cnv:args`
model doesn't currently carry a positional-index property. Adding one (mirroring
`cnv:positional` in the fuller argument model this was drafted against) is a small, isolated
follow-up if positional CLI syntax is wanted.
