# ggen UX Developer Guide

**For developers adding new commands or improving user experience**

This guide shows you how to use the ggen UX utilities to create consistent, professional CLI output.

---

## Quick Start

### 1. Import UX Utilities

```rust
use ggen_core::codegen::ux::{
    ProgressIndicator,
    FileProgressBar,
    success_message,
    error_message,
    warning_message,
    info_message,
    print_summary,
    format_duration,
};
use ggen_core::lifecycle::dx::{
    ExecutionMode,
    ExecutionMetrics,
    Output,
};
```

### 2. Basic Usage Pattern

```rust
pub fn my_command(verbose: bool) -> Result<()> {
    // 1. Setup execution mode
    let mode = if verbose {
        ExecutionMode::verbose()
    } else {
        ExecutionMode::default()
    };
    let output = Output::new(mode.clone());

    // 2. Create metrics tracker
    let mut metrics = ExecutionMetrics::new();

    // 3. Start progress indicator
    let mut progress = ProgressIndicator::new(mode.show_progress);
    progress.start_spinner("Loading manifest...");

    // 4. Do work with feedback
    output.phase_start("Validation");
    // ... validation work ...
    metrics.record_phase("Validation".to_string(), 123);
    output.phase_complete("Validation", 123);

    // 5. Finish with summary
    progress.finish_with_message("Complete!");
    eprintln!("{}", metrics.summary(&mode));

    Ok(())
}
```

---

## Pattern Library

### Pattern 1: Long-Running Operation

**Use Case:** Loading files, network requests, SPARQL queries

```rust
fn load_ontology(path: &Path) -> Result<Ontology> {
    let mut progress = ProgressIndicator::new(true);
    progress.start_spinner(&format!("Loading ontology from {}", path.display()));

    match Ontology::load(path) {
        Ok(ont) => {
            progress.finish_with_message(&format!(
                "Loaded {} triples",
                ont.triple_count()
            ));
            Ok(ont)
        }
        Err(e) => {
            progress.finish_with_error(&format!("Failed to load: {}", e));
            Err(e)
        }
    }
}
```

**Output:**
```
⠁⠂⠄⡀⢀⠠⠐⠈  Loading ontology from schema/domain.ttl...
✓ Loaded 1,234 triples
```

---

### Pattern 2: File Generation Loop

**Use Case:** Generating multiple files, batch operations

```rust
fn generate_files(files: &[FileSpec]) -> Result<Vec<GeneratedFile>> {
    let progress = FileProgressBar::new(files.len(), true);
    let mut results = Vec::new();

    for (i, spec) in files.iter().enumerate() {
        progress.set_message(&format!("Generating {}", spec.name));

        let file = generate_file(spec)?;
        results.push(file);

        progress.inc(1);
    }

    progress.finish();
    Ok(results)
}
```

**Output:**
```
[################>-----------] 5/10 Generating Person.rs
```

---

### Pattern 3: Error Handling with Context

**Use Case:** All error scenarios

```rust
fn validate_manifest(path: &Path) -> Result<Manifest> {
    if !path.exists() {
        return Err(Error::new(&format!(
            "error[E0001]: Manifest not found\n  --> {}\n  |\n  = help: Create a ggen.toml manifest file or specify path with --manifest",
            path.display()
        )));
    }

    let content = fs::read_to_string(path).map_err(|e| {
        Error::new(&format!(
            "error[E0001]: Failed to read manifest\n  --> {}\n  |\n  = error: {}\n  = help: Check file permissions",
            path.display(),
            e
        ))
    })?;

    toml::from_str(&content).map_err(|e| {
        Error::new(&format!(
            "error[E0001]: Manifest parse error\n  --> {}\n  |\n  = error: {}\n  = help: Check ggen.toml syntax and required fields",
            path.display(),
            e
        ))
    })
}
```

**Output:**
```
error[E0001]: Manifest not found
  --> ggen.toml
  |
  = help: Create a ggen.toml manifest file or specify path with --manifest
```

---

### Pattern 4: Phase-Based Execution

**Use Case:** Multi-step pipelines with timing

```rust
fn sync_pipeline(options: &SyncOptions) -> Result<SyncResult> {
    let mode = ExecutionMode::default();
    let output = Output::new(mode.clone());
    let mut metrics = ExecutionMetrics::new();

    // Phase 1: Validation
    output.phase_start("Validation");
    let start = std::time::Instant::now();
    let manifest = validate_manifest(&options.manifest_path)?;
    let duration = start.elapsed().as_millis();
    metrics.record_phase("Validation".to_string(), duration);
    output.phase_complete("Validation", duration);

    // Phase 2: Inference
    output.phase_start("Inference");
    let start = std::time::Instant::now();
    let store = run_inference(&manifest)?;
    let duration = start.elapsed().as_millis();
    metrics.record_phase("Inference".to_string(), duration);
    output.phase_complete("Inference", duration);

    // Phase 3: Generation
    output.phase_start("Generation");
    let start = std::time::Instant::now();
    let files = generate_code(&store, &manifest)?;
    let duration = start.elapsed().as_millis();
    metrics.record_phase("Generation".to_string(), duration);
    output.phase_complete("Generation", duration);

    // Summary
    eprintln!("{}", metrics.summary(&mode));

    Ok(SyncResult {
        files_synced: files.len(),
        duration_ms: metrics.total_elapsed().as_millis() as u64,
        // ... other fields
    })
}
```

**Output:**
```
▶ Validation
✓ Validation completed in 123ms

▶ Inference
✓ Inference completed in 456ms

▶ Generation
✓ Generation completed in 789ms

═══ Execution Summary ═══
  ⏱️  Total time: 1.37s
  📊 Phase timing:
    ▸ Validation 123ms (9%)
    ▸ Inference 456ms (33%)
    ▸ Generation 789ms (58%)
═════════════════════════
```

---

### Pattern 5: Dry-Run Mode

**Use Case:** Preview operations without side effects

```rust
fn sync_with_dry_run(options: &SyncOptions) -> Result<SyncOutput> {
    if options.dry_run {
        eprintln!("{}", info_message("[DRY RUN] Previewing changes..."));
    }

    let files = collect_files_to_generate(&options)?;

    if options.dry_run {
        eprintln!("{}", info_message(&format!(
            "[DRY RUN] Would sync {} files:",
            files.len()
        )));

        for file in &files {
            eprintln!("  {} (would create)", file.path);
        }

        return Ok(SyncOutput {
            status: "success".to_string(),
            files_synced: 0,
            files: files.iter().map(|f| SyncedFile {
                path: f.path.clone(),
                size_bytes: 0,
                action: "would create".to_string(),
            }).collect(),
            // ... other fields
        });
    }

    // Actual sync logic
    write_files(&files)?;

    Ok(SyncOutput {
        status: "success".to_string(),
        files_synced: files.len(),
        // ... other fields
    })
}
```

**Output:**
```
ℹ [DRY RUN] Previewing changes...
ℹ [DRY RUN] Would sync 3 files:
  src/models.rs (would create)
  src/api.rs (would create)
  src/db.rs (would create)
```

---

### Pattern 6: Conditional Verbose Output

**Use Case:** Detailed logs only when requested

```rust
fn process_files(files: &[File], verbose: bool) -> Result<()> {
    for file in files {
        if verbose {
            eprintln!("{}", info_message(&format!(
                "Processing {}",
                file.path.display()
            )));
        }

        process_file(file)?;

        if verbose {
            eprintln!("{}", success_message(&format!(
                "Processed {} ({} bytes)",
                file.path.display(),
                file.size
            )));
        }
    }

    Ok(())
}
```

**Output (verbose=false):**
```
(no output)
```

**Output (verbose=true):**
```
ℹ Processing src/models.rs
✓ Processed src/models.rs (1234 bytes)
ℹ Processing src/api.rs
✓ Processed src/api.rs (2345 bytes)
```

---

### Pattern 7: Warning Messages

**Use Case:** Non-fatal issues, deprecations

```rust
fn check_git_status() -> Result<()> {
    if !Path::new(".git").exists() {
        eprintln!("{}", warning_message(
            "No git repository detected - changes won't be tracked"
        ));
    }

    if has_uncommitted_changes()? {
        eprintln!("{}", warning_message(
            "Uncommitted changes detected - consider committing before sync"
        ));
    }

    Ok(())
}
```

**Output:**
```
⚠ No git repository detected - changes won't be tracked
⚠ Uncommitted changes detected - consider committing before sync
```

---

### Pattern 8: Summary Tables

**Use Case:** Final output, metrics display

```rust
fn print_sync_summary(result: &SyncResult) {
    print_summary("Sync Complete", &[
        ("Status", result.status.clone()),
        ("Files synced", format!("{}", result.files_synced)),
        ("Duration", format_duration(result.duration_ms)),
        ("Inference rules", format!("{}", result.inference_rules_executed)),
        ("Generation rules", format!("{}", result.generation_rules_executed)),
    ]);

    if !result.files.is_empty() {
        eprintln!("\n{}", "Generated Files:".green().bold());
        for file in &result.files {
            eprintln!(
                "  {} {} ({})",
                match file.action.as_str() {
                    "created" => "✓".green(),
                    "updated" => "↻".yellow(),
                    _ => "·".white(),
                },
                file.path,
                format_bytes(file.size_bytes)
            );
        }
    }
}
```

**Output:**
```
Sync Complete
  Status: success
  Files synced: 5
  Duration: 1.42s
  Inference rules: 2
  Generation rules: 3

Generated Files:
  ✓ src/models.rs (1.2 KB)
  ✓ src/api.rs (2.3 KB)
  ↻ src/db.rs (3.4 KB)
```

---

## Output Modes

### Default Mode

```rust
let mode = ExecutionMode::default();
// verbose: false
// show_progress: true
// use_colors: true
```

**When to use:** Normal CLI execution

---

### Verbose Mode

```rust
let mode = ExecutionMode::verbose();
// verbose: true
// show_progress: true
// use_colors: true
```

**When to use:** User passes `--verbose` flag

---

### CI/CD Mode

```rust
let mode = ExecutionMode::ci();
// verbose: false
// show_progress: false
// use_colors: false
```

**When to use:**
- Running in CI/CD pipeline
- Output piped to file
- `NO_COLOR` environment variable set

---

### Dry-Run Mode

```rust
let mode = ExecutionMode::dry_run();
// verbose: true (always!)
// dry_run: true
// show_progress: true
// use_colors: true
```

**When to use:** User passes `--dry-run` flag

---

## Error Code Guidelines

### Error Code Assignment

| Range | Category | Example |
|-------|----------|---------|
| E0001-E0999 | Manifest/Validation | E0001: Manifest not found |
| E1000-E1999 | Ontology/RDF | E1001: RDF parse error |
| E2000-E2999 | SPARQL | E2001: Query syntax error |
| E3000-E3999 | Templates | E3001: Template not found |
| E4000-E4999 | Code Generation | E4001: Output validation failed |
| E5000-E5999 | Runtime | E5001: Timeout exceeded |

### Error Message Format

```
error[EXXXX]: <short description>
  --> <file path>
  |
  = error: <detailed error>
  |
<line number> | <source code line>
  |              <caret indicator>
<error details>
  = help: <actionable suggestion>
```

### Example Error Messages

#### Good Error Message ✅
```
error[E0001]: Manifest not found
  --> ggen.toml
  |
  = help: Create a ggen.toml manifest file or specify path with --manifest
```

**Why good:**
- Error code present
- Clear description
- Actionable help
- File location shown

#### Bad Error Message ❌
```
Error: file not found
```

**Why bad:**
- No error code
- Vague description
- No help text
- No context

---

## JSON Output

### When to Return JSON

1. **Command completes successfully** → Always return structured output
2. **Command fails** → Return error in JSON (if `--format json`)
3. **Progress updates** → Text only (not JSON)

### JSON Structure Template

```rust
#[derive(Debug, Clone, Serialize)]
pub struct CommandOutput {
    /// Status: "success" or "error"
    pub status: String,

    /// Primary metric (files synced, templates generated, etc.)
    pub primary_metric: usize,

    /// Duration in milliseconds
    pub duration_ms: u64,

    /// Detailed results
    pub details: Vec<DetailItem>,

    /// Error message (if status = "error")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// Warning message (if present)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warning: Option<String>,

    /// Next steps
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub next_steps: Vec<String>,
}
```

### JSON Serialization

```rust
// Serialize and print
let output = CommandOutput { /* ... */ };
let json = serde_json::to_string_pretty(&output)?;
println!("{}", json);
```

---

## Testing UX Features

### Unit Test Template

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_progress_indicator() {
        let mut progress = ProgressIndicator::new(true);
        progress.start_spinner("Testing");
        progress.finish_with_message("Done");
        // No panic = success
    }

    #[test]
    fn test_message_formatting() {
        let msg = success_message("Test");
        assert!(msg.contains("Test"));
        assert!(msg.contains("✓"));
    }

    #[test]
    fn test_duration_formatting() {
        assert_eq!(format_duration(500), "500ms");
        assert_eq!(format_duration(1500), "1.50s");
        assert_eq!(format_duration(65000), "1m 5s");
    }
}
```

### Visual Testing

```bash
# Capture reference output
cargo run --bin ggen -- sync --dry-run > tests/fixtures/sync-dry-run.txt

# Visual inspection
cat tests/fixtures/sync-dry-run.txt

# Regression test
diff <(cargo run --bin ggen -- sync --dry-run) tests/fixtures/sync-dry-run.txt
```

---

## Common Mistakes

### ❌ Mistake 1: Using println! Instead of Message Formatters

**Bad:**
```rust
println!("✓ Success");
println!("✗ Error");
```

**Good:**
```rust
eprintln!("{}", success_message("Success"));
eprintln!("{}", error_message("Error"));
```

**Why:** Consistency, color support, CI/CD compatibility

---

### ❌ Mistake 2: Not Respecting Execution Mode

**Bad:**
```rust
fn process(verbose: bool) {
    println!("Processing...");  // Always prints
}
```

**Good:**
```rust
fn process(mode: &ExecutionMode) {
    if mode.verbose {
        eprintln!("{}", info_message("Processing..."));
    }
}
```

**Why:** Respect user preferences, avoid noise in CI/CD

---

### ❌ Mistake 3: Forgetting Progress Cleanup

**Bad:**
```rust
let mut progress = ProgressIndicator::new(true);
progress.start_spinner("Loading");
// ... work ...
// Forgot to finish!
return result;
```

**Good:**
```rust
let mut progress = ProgressIndicator::new(true);
progress.start_spinner("Loading");

let result = match do_work() {
    Ok(r) => {
        progress.finish_with_message("Complete");
        Ok(r)
    }
    Err(e) => {
        progress.finish_with_error(&format!("Failed: {}", e));
        Err(e)
    }
};

result
```

**Why:** Spinners must be cleaned up to avoid terminal corruption

---

### ❌ Mistake 4: Generic Error Messages

**Bad:**
```rust
return Err(Error::new("Validation failed"));
```

**Good:**
```rust
return Err(Error::new(&format!(
    "error[E0001]: Manifest validation failed\n  --> {}\n  |\n  = error: Missing required field 'project.name'\n  = help: Add [project] section with name field",
    path.display()
)));
```

**Why:** Users need actionable context

---

### ❌ Mistake 5: Not Tracking Metrics

**Bad:**
```rust
fn sync() -> Result<()> {
    validate()?;
    infer()?;
    generate()?;
    Ok(())
}
```

**Good:**
```rust
fn sync() -> Result<SyncResult> {
    let mut metrics = ExecutionMetrics::new();

    let start = Instant::now();
    validate()?;
    metrics.record_phase("Validation".to_string(), start.elapsed().as_millis());

    let start = Instant::now();
    infer()?;
    metrics.record_phase("Inference".to_string(), start.elapsed().as_millis());

    Ok(SyncResult {
        duration_ms: metrics.total_elapsed().as_millis() as u64,
        // ...
    })
}
```

**Why:** Users want to know performance characteristics

---

## Checklist for New Commands

When adding a new command, ensure:

- [ ] Uses `ExecutionMode` for output control
- [ ] Tracks execution metrics (`ExecutionMetrics`)
- [ ] Shows progress for long operations (`ProgressIndicator`)
- [ ] Uses message formatters (success/error/warning/info)
- [ ] Returns structured JSON output
- [ ] Has error codes (EXXXX format)
- [ ] Provides actionable help messages
- [ ] Respects `--verbose` flag
- [ ] Supports `--dry-run` mode
- [ ] Cleans up progress indicators properly
- [ ] Has comprehensive help documentation
- [ ] Includes examples in help text
- [ ] Prints execution summary
- [ ] Has unit tests for UX features

---

## Reference

### Color Palette

```rust
use colored::Colorize;

// Success
"✓".green()

// Error
"✗".red()

// Warning
"⚠".yellow()

// Info
"ℹ".blue()

// Headers
"Section".cyan().bold()

// Workspace
"📦 Package".magenta()

// Phase
"▶ Phase".cyan().bold()

// Dimmed
"details".dimmed()
```

### Unicode Symbols

| Symbol | Unicode | Usage |
|--------|---------|-------|
| ✓ | U+2713 | Success |
| ✗ | U+2717 | Error |
| ⚠ | U+26A0 | Warning |
| ℹ | U+2139 | Info |
| ▶ | U+25B6 | Phase start |
| ⏱️ | U+23F1 | Time/duration |
| 📊 | U+1F4CA | Statistics |
| 📈 | U+1F4C8 | Metrics |
| ⚡ | U+26A1 | Performance/cache |
| 📦 | U+1F4E6 | Package |
| ↻ | U+21BB | Updated |

---

## Additional Resources

### Internal Documentation
- `/home/user/ggen/crates/ggen-core/src/codegen/ux.rs` - UX utilities
- `/home/user/ggen/crates/ggen-core/src/lifecycle/dx.rs` - DX utilities
- `/home/user/ggen/crates/ggen-cli/src/error.rs` - Error handling

### External Documentation
- [indicatif docs](https://docs.rs/indicatif)
- [colored docs](https://docs.rs/colored)
- [clap docs](https://docs.rs/clap)

### Examples
- `ggen-cli/src/cmds/init.rs` - InitOutput structure
- `ggen-cli/src/cmds/sync.rs` - SyncOutput structure
- `ggen-core/src/codegen/executor.rs` - Phase-based execution

---

**Document Version:** 1.0
**Last Updated:** 2026-01-18
**Maintainer:** ggen-core team
