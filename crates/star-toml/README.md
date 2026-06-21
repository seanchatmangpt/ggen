# star_toml

A framework for loading, layering, and **validating** any `*.toml` configuration file
in Rust — the `*` in `*.toml`.

Most config crates parse a file into a struct and stop there. `star_toml` brings the
[Pydantic](https://docs.pydantic.dev/) experience to TOML: a **validation engine** that
collects *every* error across the whole config tree at once — each with a precise
location, the offending value, and a machine-matchable code — plus a layered loader that
composes defaults, files, and environment overrides.

Dependencies: `serde`, `toml`, `thiserror`. No async runtime, no crypto, no `serde_json`.

## The headline: Pydantic-grade validation

```rust
use star_toml::{Validate, Validator};

struct Server { host: String, port: u16 }
struct App { name: String, workers: u32, server: Server }

impl Validate for Server {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("host", &self.host);
        v.check_range("port", self.port, 1..=65535);
    }
}
impl Validate for App {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("name", &self.name);
        v.check_range("workers", self.workers, 1..=1024);
        v.field("server", |v| self.server.validate(v));  // nested → server.*
    }
}
```

Feed it a broken config and you get **all** the failures, not just the first:

```text
4 validation errors for App
name
  must not be empty (got: `""`) [empty]
workers
  input must be in range 1..=1024 (got: `0`) [out_of_range]
server.host
  must not be empty (got: `""`) [empty]
server.port
  input must be in range 1..=65535 (got: `0`) [out_of_range]
```

Every error is also **programmatically matchable**:

```rust
# use star_toml::{Validate, Validator, ErrorKind};
# struct App;
# impl Validate for App { fn validate(&self, _: &mut Validator) {} }
# let app = App;
let report = app.check().unwrap_err();
for e in report.errors() {
    println!("{} failed with code {}", e.loc, e.code());   // e.g. "server.port" / "out_of_range"
    match &e.kind {
        ErrorKind::OutOfRange { lower, upper } => { /* … */ }
        ErrorKind::NotOneOf { allowed }        => { /* … */ }
        _ => {}
    }
}
```

Every error also carries a **auto-derived repair hint** and a machine-readable severity:

```rust
# use star_toml::{Validate, Validator, ErrorKind, Severity};
# struct App;
# impl Validate for App { fn validate(&self, _: &mut Validator) {} }
# let app = App;
let report = app.check().unwrap_err();

// Van der Aalst conformance score: 0.0 = total failure, 1.0 = perfect
println!("fitness: {:.0}%", report.fitness() * 100.0);

// Stable variant fingerprint — same error pattern = same ID across runs
println!("variant: {:016x}", report.variant_id());

// Object-centric grouping by top-level config section
for (section, errors) in report.by_section() {
    println!("[{section}] {} error(s)", errors.len());
    for e in errors {
        println!("  {} → {} | fix: {}", e.loc, e.code(), e.repair_hint());
    }
}
```

### Built-in checks

| Helper | Error code | Use |
|--------|-----------|-----|
| `check_non_empty(field, &str)` | `empty` | reject empty strings |
| `check_range(field, value, lo..=hi)` | `out_of_range` | numeric bounds |
| `check_one_of(field, &str, &[..])` | `not_one_of` | enumerations |
| `check_predicate(field, cond, code, msg)` | *your code* | arbitrary domain rules |
| `check_consistent(field, &[related], cond, code, msg)` | *your code* | DECLARE cross-field constraints |
| `with_severity(Severity::Warning, \|v\| …)` | — | emit non-Error severity |
| `field(name, \|v\| …)` / `index(i, \|v\| …)` | — | descend into nested structs / arrays |

### Van der Aalst innovations

| Feature | API | Description |
|---------|-----|-------------|
| **Conformance fitness** | `report.fitness() -> f64` | Alignment metric: proportion of checks that passed (0.0–1.0) |
| **Variant fingerprint** | `report.variant_id() -> u64` | FNV-1a hash of error patterns — same failures = same ID |
| **Object-centric grouping** | `report.by_section()` | Errors indexed by top-level config section |
| **Severity stratification** | `Severity::{Advisory,Warning,Error,Fatal}` | Not all failures are equal |
| **Repair hints** | `error.repair_hint()` | Auto-derived minimum fix for each error kind |
| **DECLARE constraints** | `check_consistent(…)` | Cross-field co-existence / response constraints |

## Layered loading

Sources merge left-to-right; later layers win. Tables merge key-by-key; arrays and
scalars are replaced.

```rust,no_run
use star_toml::Loader;

#[derive(serde::Deserialize)]
struct AppConfig { name: String, port: u16 }

const DEFAULTS: &str = "name = 'my-app'\nport = 8080\n";

let cfg: AppConfig = Loader::new()
    .layer_str(DEFAULTS, "built-in defaults")    // lowest priority
    .find_file("app.toml")                        // walk up from cwd
    .layer_file_if_exists("~/.config/app.toml")   // optional user overrides
    .env_prefix("APP_")                           // APP_PORT=9090 → port = 9090
    .load()?;                                     // highest priority = env
# Ok::<(), star_toml::Error>(())
```

Environment overrides map `APP_SERVER__PORT=9090` → `server.port = 9090`, coercing the
value to the right TOML scalar type (bool → int → float → string).

## Source-relative paths

`ConfigFile<T>` remembers where the config came from, so relative paths *inside* the
config resolve correctly regardless of the working directory:

```rust,no_run
use star_toml::{Loader, ConfigFile};

#[derive(serde::Deserialize)]
struct Build { template_dir: String }

let cf: ConfigFile<Build> = Loader::new().find_file("build.toml").load_file()?;
let abs = cf.resolve(&cf.config.template_dir);   // anchored at build.toml's directory
# Ok::<(), star_toml::Error>(())
```

## Write-back

```rust,no_run
use star_toml::save_file;

#[derive(serde::Serialize)]
struct Config { name: String }

save_file(&Config { name: "scaffolded".into() }, "ggen.toml")?;   // creates parent dirs
# Ok::<(), star_toml::Error>(())
```

## Loose API surface

| Function | Purpose |
|----------|---------|
| `from_str::<T>(s)` | parse a TOML string (with env expansion) |
| `load_file::<T>(path)` | load + parse a single file |
| `find_config_file(name, start)` | walk parent dirs for a file |
| `find_and_load::<T>(name, start)` | the two combined |
| `to_string(&value)` / `save_file(&value, path)` | serialize back to TOML |
| `deep_merge(&mut base, overlay)` | recursive TOML value merge |
| `expand_env_vars(s)` | `${VAR}` / `$VAR` substitution (UTF-8 safe) |

## Run the example

```bash
cargo run -p star-toml --example validate
```
