# ggen-cli

Command-line interface for the ggen specification-driven code generation tool. The binary is named `ggen`. This crate also exposes a library (`ggen-cli-lib`) for programmatic CLI invocation (used by the Node.js addon integration).

Commands are auto-discovered from the `cmds` module via `clap-noun-verb`. Every top-level subcommand (called a "noun") groups one or more verbs beneath it.

---

## Quick Start

### Install

Build and install from the workspace root:

```bash
cargo install --path crates/ggen-cli
```

Verify:

```bash
ggen --version
```

### Initialize a project

```bash
mkdir my-project && cd my-project
ggen init
```

`ggen init` creates:

```
my-project/
  ggen.toml               # manifest — the source of truth
  schema/domain.ttl       # seed RDF ontology (Turtle)
  templates/
    example.txt.tera      # example Tera template
  scripts/
    startup.sh            # project initialization script (executable)
  Makefile                # setup / build / clean targets
  .gitignore              # ignores .ggen/
  README.md               # project readme (created only if absent)
```

Git hooks are installed automatically unless `--skip-hooks` is passed.

### First sync

Edit `schema/domain.ttl` with your domain model, then run:

```bash
ggen sync
```

For a dry run (preview changes without writing):

```bash
ggen sync --dry-run true
```

---

## CLI Command Reference

Commands follow the pattern `ggen <noun> <verb> [flags]`. A few nouns (`sync`, `init`) register directly at the root level.

### `ggen init`

Initialize a new ggen project in the current directory (or a named path).

```
ggen init [OPTIONS]

OPTIONS:
  --path <PATH>          Project directory (default: current directory)
  --force                Overwrite existing ggen files
  --skip-hooks           Skip git hooks installation
  --name <NAME>          Project name written into ggen.toml
  --version <VERSION>    Project version written into ggen.toml
  --description <TEXT>   Project description written into ggen.toml
```

Initialization is atomic: a `FileTransaction` backs up any existing files before overwriting. On failure, all changes roll back automatically.

If a project is already initialized (any of `ggen.toml`, `Makefile`, `schema/domain.ttl`, or `scripts/startup.sh` exist), the command returns an error unless `--force` is supplied.

### `ggen sync`

Execute the full code synchronization pipeline from a `ggen.toml` manifest. This is the primary working command — it replaces the former `ggen generate`, `ggen validate`, and `ggen template` commands.

```
ggen sync [OPTIONS]

OPTIONS:
  --manifest <PATH>      Path to ggen.toml (default: ./ggen.toml)
  --output-dir <PATH>    Override output directory from manifest
  --dry-run <bool>       Preview changes without writing files
  --force                Overwrite existing files (use with --audit)
  --audit                Write a detailed audit trail to .ggen/audit/
  --rule <NAME>          Execute only the named generation rule
  --verbose              Show detailed execution logs
  --watch                Continuous file monitoring and auto-regeneration
  --validate-only        Run SHACL/SPARQL validation without generating files
  --format <FORMAT>      Output format: text (default) or json
  --timeout <MS>         Maximum execution time in milliseconds (default: 30000)
  --stage <STAGE>        Run a specific pipeline stage only: mu1|mu2|mu3|mu4|mu5 (or Unicode: mu1-mu5)
  --ontology <PATH>      Override ontology path from manifest
  --queries <DIR>        Directory of .rq SPARQL files; bypasses ggen.toml entirely
  --language <LANG>      Target language when using --queries: rust|go|typescript|python|elixir|auto
  --profile <PROFILE>    Enforcement profile: enterprise-strict, permissive, etc.
  --locked               Require exact lockfile match; hard-fails if .ggen/packs.lock is absent or mismatched
```

The pipeline (mu1-mu5):

| Stage | Name | Description |
|-------|------|-------------|
| mu1 | CONSTRUCT | Normalize the RDF ontology; expand with inference rules |
| mu2 | SELECT | Run SPARQL SELECT queries to extract bindings |
| mu3 | Tera | Render Tera templates with extracted bindings |
| mu4 | Canonicalize | Format and validate generated code |
| mu5 | Receipt | Emit a signed BLAKE3+Ed25519 receipt to `.ggen/receipts/` |

Every successful non-dry-run sync writes two receipt files:
- `.ggen/receipts/sync-<YYYYMMDD-HHMMSS>.json` — timestamped archive
- `.ggen/receipts/latest.json` — always the most recent receipt

Dry runs never write a receipt (no artifact, no receipt — contract drift prevention).

Flag precedence:
- `--validate-only` overrides `--force`
- `--dry-run` prevents all file writes; `--force` has no effect alongside it
- `--stage` limits execution to a single pipeline stage

### `ggen doctor`

Health checks for the workspace.

```
ggen doctor run     # Full check: ggen.toml, binary version, Rust toolchain
ggen doctor check   # Quick check: ggen.toml presence only
```

`doctor run` verifies:
- `ggen.toml` present in working directory
- `Cargo.toml` present (Rust workspace detection)
- `.specify/` directory present (RDF specs)
- Rust toolchain and Cargo available

### `ggen pack`

Manage installable packs (template + query bundles from the marketplace).

```
ggen pack add <PACK_NAME> [--force true]      # Install a pack
ggen pack remove <PACK_NAME>            # Uninstall a pack
ggen pack list [--verbose true] [--category true] # List available packs
ggen pack show <PACK_ID>                # Show pack details
ggen pack search <QUERY> [--limit N]    # Search packs by keyword
ggen pack doctor                        # Health check on installed packs
```

Pack names must contain only alphanumeric characters, hyphens, or underscores.

The lockfile at `.ggen/packs.lock` tracks every installed pack with its version and digest. `ggen sync --locked` hard-fails if the lockfile is absent or if any digest does not match the installed pack.

A provenance receipt is emitted after every successful `pack add`. The pack install receipt path is printed in the output.

The pack cache directory is resolved from (in order):
1. `GGEN_PACK_CACHE_DIR` environment variable
2. `$HOME/.ggen/packs/`

### `ggen receipt`

Inspect and verify cryptographic receipts emitted by sync and pack operations.

```
ggen receipt verify <RECEIPT_PATH> [--public-key <KEY_PATH>]
ggen receipt info <RECEIPT_PATH>
```

`verify` checks the Ed25519 signature. It falls back to `.ggen/keys/public.pem` if `--public-key` is not supplied. Returns `is_valid: true` only when the signature is valid; an empty or missing signature always returns `is_valid: false`.

`info` prints receipt fields (operation ID, timestamp, input and output hash counts, chain position) without verifying the signature.

### `ggen graph`

Operate on RDF graphs directly.

```
ggen graph validate <SCHEMA_FILE> [--strict]    # Validate ontology quality
ggen graph load <FILE> [--format <FMT>]         # Load RDF into the graph store
ggen graph query <SPARQL_QUERY> [--graph-file] [--format]  # Execute a SPARQL query
ggen graph export <INPUT_FILE> <OUTPUT_FILE> <FORMAT>      # Export graph to file
ggen graph visualize <INPUT_FILE> [--format]               # Visualize graph structure
```

Supported RDF formats: `turtle`, `rdfxml`, `n3`, `ntriples`. Supported export formats depend on the graph store. Visualization defaults to DOT format.

### `ggen template`

Manage Tera templates.

```
ggen template show <TEMPLATE>           # Show template metadata and variables
ggen template new <NAME> [--template-type <TYPE>]  # Create a new template
ggen template list [--directory <DIR>]  # List templates in the templates/ directory
ggen template lint <TEMPLATE>           # Lint template for errors and warnings
```

### `ggen policy`

Manage and validate enforcement profiles for installed packs.

```
ggen policy list [--verbose]            # List available policy profiles
ggen policy show <PROFILE_ID>           # Show profile policies and constraints
ggen policy validate <PROFILE>          # Validate project against a profile
ggen policy check                       # Check project against enterprise-strict (default)
```

Built-in profiles include `enterprise-strict` and `permissive`. Profiles enforce trust tiers, receipt requirements, and runtime constraints across installed packs.

### `ggen utils`

Utility commands.

```
ggen utils doctor [--all] [--format <FMT>]         # Run system diagnostics
ggen utils env [--list] [--get <KEY>] [--set <K=V>] [--system]  # Manage environment variables
```

`utils doctor` checks system health and reports pass/fail/warning per check. `utils env` lists environment variables prefixed with `GGEN_` or `RUST_`, plus `HOME` and `PATH`.

### `ggen git-hooks`

Manage git hook installation (also called automatically by `ggen init`).

Installed hooks:
- `pre-commit`: runs on commits to `main`; gates on `cargo check` and `cargo fmt --check` (target: under 10 seconds)
- `pre-push`: runs on pushes to `main`; gates on check + clippy + fmt + unit tests (target: under 90 seconds)

### Feature-gated nouns

The following nouns exist in the source tree but are off by default (gated behind the `experimental` or `lsp` Cargo features). They do not appear in `--help` or participate in command routing unless the binary is compiled with those features:

| Noun | Feature gate | Status |
|------|-------------|--------|
| `ggen lsp` | `lsp` | Opt-in; exposes LSP verbs and `serve --protocol mcp` |
| `ggen a2a` | `experimental` | Archived; code preserved, not provably complete |
| `ggen framework` | `experimental` | Archived |
| `ggen mcp` | `experimental` | Archived; prefer `ggen lsp serve --protocol mcp` |
| `ggen sigma` | `experimental` | Archived |
| `ggen wizard` | `experimental` | Archived |

---

## Configuration: ggen.toml

`ggen.toml` is the manifest for a ggen project. The full schema is defined in `crates/ggen-config/src/config_lib/schema.rs`. The only required section is `[project]`.

### Minimal manifest

```toml
[project]
name    = "my-project"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name         = "structs"
query        = { inline = "SELECT ?class WHERE { ?class a rdfs:Class }" }
template     = { file = "templates/structs.rs.tera" }
output_file  = "src/generated/structs.rs"
mode         = "Overwrite"
```

### Available sections

| Section | Required | Purpose |
|---------|----------|---------|
| `[project]` | Yes | Name, version, description, authors, license, repository |
| `[ontology]` | Yes (for sync) | `source` path, optional `imports`, `standard_only` flag |
| `[generation]` | Yes (for sync) | `output_dir`; each `[[generation.rules]]` defines one rule |
| `[inference]` | No | SPARQL CONSTRUCT inference rules applied before SELECT |
| `[sync]` | No | `enabled`, `on_change`, `validate_after`, `conflict_mode` |
| `[rdf]` | No | `base_uri`, `prefixes`, `default_format`, `cache_queries` |
| `[sparql]` | No | `timeout` (s), `max_results`, `cache_enabled` |
| `[templates]` | No | `directory`, `output_directory`, `backup_enabled`, `idempotent` |
| `[security]` | No | Path traversal / shell injection / sandbox flags |
| `[performance]` | No | `parallel_execution`, `max_workers`, `memory_limit_mb` |
| `[logging]` | No | `level` (debug/info/warn/error), `format` (json/text), `file` |
| `[telemetry]` | No | OTLP endpoint, `service_name`, `console_output` |
| `[ai]` | No | LLM provider, model, temperature, tokens, prompts, validation |
| `[mcp]` | No | MCP server config (transport, tools, discovery) |
| `[a2a]` | No | Agent-to-agent config (transport, messaging, orchestration) |
| `[features]` | No | Feature flags as `key = bool` |
| `[build]` | No | Build target, features, profile, parallel jobs |
| `[test]` | No | Test framework, parallelism, timeout, coverage |

### `[[generation.rules]]` fields

Each rule drives one pass through the mu1-mu5 pipeline:

```toml
[[generation.rules]]
name        = "structs"             # rule name; use with ggen sync --rule structs

# query: inline SPARQL or external file
query       = { inline = "SELECT ..." }
# or:
query       = { file = "queries/structs.rq" }

# template: inline Tera or external file
template    = { file = "templates/structs.rs.tera" }
# or:
template    = { inline = "..." }

output_file = "src/gen/structs.rs"  # path relative to output_dir
mode        = "Overwrite"           # or "Merge", "Skip"
```

### Telemetry configuration

Telemetry (OpenTelemetry OTLP export) is disabled by default. To enable:

```toml
[telemetry]
endpoint       = "http://localhost:4317"
service_name   = "my-ggen-project"
console_output = false
```

When the `[telemetry]` section is absent, no OTLP exporter is initialized. The CLI still emits a root `ggen.cli` span per invocation via `tracing`.

---

## Example Workflow: Ontology to Code in Five Steps

This example generates Rust struct definitions from an RDF schema.

**Step 1 — Initialize the project**

```bash
ggen init --name my-domain
cd my-domain
```

**Step 2 — Edit the ontology**

Edit `schema/domain.ttl`:

```turtle
@prefix rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:   <http://www.w3.org/2000/01/rdf-schema#> .
@prefix schema: <https://schema.org/> .

schema:Order a rdfs:Class ;
    rdfs:label "Order" ;
    rdfs:comment "A customer order." .

schema:orderNumber a rdf:Property ;
    rdfs:domain schema:Order ;
    rdfs:label  "orderNumber" .
```

**Step 3 — Write a SPARQL query and template**

`queries/structs.rq`:

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?label ?comment
WHERE {
  ?class a rdfs:Class ;
         rdfs:label ?label .
  OPTIONAL { ?class rdfs:comment ?comment . }
}
ORDER BY ?label
```

`templates/structs.rs.tera`:

```tera
{% for row in results %}
/// {{ row.comment | default(value="") }}
pub struct {{ row.label }} {}

{% endfor %}
```

**Step 4 — Configure ggen.toml**

Add to `ggen.toml` under `[generation]`:

```toml
[[generation.rules]]
name        = "structs"
query       = { file = "queries/structs.rq" }
template    = { file = "templates/structs.rs.tera" }
output_file = "src/gen/structs.rs"
mode        = "Overwrite"
```

**Step 5 — Run sync**

```bash
ggen sync --dry-run     # preview
ggen sync               # write files and emit receipt
ggen receipt verify .ggen/receipts/latest.json
```

[output depends on your ggen.toml and ontology]

---

## Error Troubleshooting

### Exit codes

| Code | Error variant | Meaning |
|------|--------------|---------|
| 0 | — | Success |
| 1 | `ValidationError` | RDF parsing, SHACL validation, or type consistency failure |
| 2 | `SparqlError` | SPARQL query syntax error or execution failure |
| 3 | `TemplateError` | Tera template rendering failure (context mismatch, syntax error) |
| 4 | `OutputInvalid` | Generated code failed post-generation validation |
| 5 | `Timeout` | Operation exceeded its time limit |
| 127 | `FileError` / `Internal` | Filesystem I/O error or unexpected internal error |

All other errors (config, network, external service, command routing) use exit code 1 and print `ERROR: <message>` to stderr.

### Common problems

**`ggen.toml not found`**
Run `ggen init` in your project root, or pass `--manifest <path>` to point at the correct file.

**`Pack 'X' not found in local registry`**
The pack TOML (`marketplace/packs/<name>.toml`) does not exist. Check the pack name and ensure the marketplace directory is present.

**`ggen sync --locked` fails with digest mismatch**
The pack was updated or removed after `ggen pack add`. Re-run `ggen pack add <name>` to refresh the lockfile, or restore the exact pack version.

**`No packs installed: .ggen/packs.lock not found`**
The lockfile does not exist. Run `ggen pack add <name>` to install at least one pack, which creates the lockfile.

**`Public key required`** (on `ggen receipt verify`)
Pass `--public-key .ggen/keys/verifying.key`, or ensure `.ggen/keys/public.pem` exists. The verifying key is written to `.ggen/keys/verifying.key` on first sync.

**Sync produces no output files**
Check that your `[[generation.rules]]` section is present and that the SPARQL query returns at least one row. Use `ggen sync --validate-only` to check SHACL without generating, and `ggen graph query <SPARQL>` to test queries interactively.

**Template variable not found** (GGEN-TPL-001 diagnostic)
A `{{ var }}` expression in a Tera template refers to a variable that the SPARQL SELECT does not produce. Check that every template variable appears in the query `SELECT` clause.

**Output path escapes project root** (GGEN-YIELD-001 diagnostic)
The `output_file` pattern resolves to a path outside the project root. Use a relative path that stays within the project directory.

---

## Performance

### SLO targets

| Metric | Target |
|--------|--------|
| `just check` (compilation check) | under 5 seconds |
| `just test-unit` (unit tests) | under 10 seconds |
| `just lint` (clippy + rustfmt) | under 60 seconds |
| `just test` (full test suite) | under 30 seconds |
| RDF processing | under 5 seconds per 1,000+ triples |
| CLI end-to-end scaffolding (`ggen init`) | under 3 seconds |
| Memory footprint during generation | under 100 MB |

### Validation gate

All four gates must pass before shipping:

```bash
just check && just lint && just test && just slo-check
```

### Environment variables affecting performance

| Variable | Effect |
|----------|--------|
| `RUST_LOG` | Set to `trace` to enable detailed span logging |
| `GGEN_PACK_CACHE_DIR` | Override pack cache location |
| `RAYON_NUM_THREADS` | Control Rayon thread pool size for parallel generation |

---

## Cryptographic Receipts

Every `ggen sync` (non-dry-run) emits a signed receipt. Receipts provide provenance: they bind all inputs (manifest, ontology, templates, queries, installed packs) and all outputs (generated files) to a single Ed25519 signature.

Receipt location: `.ggen/receipts/latest.json` and `.ggen/receipts/sync-<YYYYMMDD-HHMMSS>.json`.

Signing key: `.ggen/keys/signing.key` (hex-encoded 32-byte Ed25519 scalar). Generated on first sync; never overwritten.

Verifying key: `.ggen/keys/verifying.key`.

To verify:

```bash
ggen receipt verify .ggen/receipts/latest.json --public-key .ggen/keys/verifying.key
```

Receipts are chained: each receipt records a hash of the previous receipt, forming an append-only audit log.

---

## Repository

- Source: `crates/ggen-cli/` (binary) and `crates/ggen-cli/src/lib.rs` (library)
- Binary name: `ggen`
- Library name: `ggen_cli_lib`
- Crate version: matches workspace version (`26.7.1`)
- GitHub: https://github.com/seanchatmangpt/ggen
