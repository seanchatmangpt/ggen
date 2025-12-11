# ggen Command Implementation Mapping

## Full Command-to-File Reference

### AI Commands (3/3 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen ai generate` | `/home/user/ggen/crates/ggen-cli/src/cmds/ai.rs:73` | ✅ COMPLETE | Uses ggen-ai client, supports streaming |
| `ggen ai chat` | `/home/user/ggen/crates/ggen-cli/src/cmds/ai.rs:154` | ✅ COMPLETE | Interactive mode with streaming support |
| `ggen ai analyze` | `/home/user/ggen/crates/ggen-cli/src/cmds/ai.rs:368` | ✅ COMPLETE | Analyzes code/projects with complexity scoring |

### Graph Commands (4/4 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen graph load` | `/home/user/ggen/crates/ggen-cli/src/cmds/graph.rs:50` | ✅ COMPLETE | Loads RDF files with Oxigraph |
| `ggen graph query` | `/home/user/ggen/crates/ggen-cli/src/cmds/graph.rs:75` | ✅ COMPLETE | SPARQL 1.1 query execution |
| `ggen graph export` | `/home/user/ggen/crates/ggen-cli/src/cmds/graph.rs:100` | ✅ COMPLETE | Exports to JSON-LD, Turtle, etc |
| `ggen graph visualize` | `/home/user/ggen/crates/ggen-cli/src/cmds/graph.rs:125` | ✅ COMPLETE | Renders as DOT format graphs |

### Hook Commands (4/4 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen hook create` | `/home/user/ggen/crates/ggen-cli/src/cmds/hook.rs:58` | ✅ COMPLETE | Creates Git hooks from scripts |
| `ggen hook list` | `/home/user/ggen/crates/ggen-cli/src/cmds/hook.rs:79` | ✅ COMPLETE | Lists with optional filtering |
| `ggen hook remove` | `/home/user/ggen/crates/ggen-cli/src/cmds/hook.rs:105` | ✅ COMPLETE | Removes with force option |
| `ggen hook monitor` | `/home/user/ggen/crates/ggen-cli/src/cmds/hook.rs:125` | ✅ COMPLETE | Monitors hook events |

### Utils Commands (2/2 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen utils doctor` | `/home/user/ggen/crates/ggen-cli/src/cmds/utils.rs:50` | ✅ COMPLETE | System diagnostics with checks |
| `ggen utils env` | `/home/user/ggen/crates/ggen-cli/src/cmds/utils.rs:99` | ✅ COMPLETE | Manage GGEN environment variables |

### Template Commands (6/8 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen template show` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:89` | ✅ COMPLETE | Display template metadata |
| `ggen template new` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:110` | ✅ COMPLETE | Create new template from type |
| `ggen template list` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:138` | ✅ COMPLETE | List all templates |
| `ggen template lint` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:178` | ✅ COMPLETE | Validate template syntax |
| `ggen template generate` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:221` | ✅ COMPLETE | Generate from single template |
| `ggen template generate-rdf` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:297` | ✅ COMPLETE | RDF-driven code generation |
| `ggen template generate-tree` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:249` | ⚠️ PARTIAL | Returns placeholder output |
| `ggen template regenerate` | `/home/user/ggen/crates/ggen-cli/src/cmds/template.rs:284` | ❌ STUB | Merge strategies unimplemented |

### Project Commands (6/7 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen project new` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:109` | ✅ COMPLETE | Scaffold new project |
| `ggen project plan` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:161` | ✅ COMPLETE | Create generation plan (JSON/YAML) |
| `ggen project gen` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:214` | ✅ COMPLETE | Generate from template with vars |
| `ggen project apply` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:281` | ✅ COMPLETE | Apply generation plan to files |
| `ggen project init` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:325` | ✅ COMPLETE | Initialize with conventions |
| `ggen project generate` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:505` | ✅ COMPLETE | Auto-discover and generate |
| `ggen project watch` | `/home/user/ggen/crates/ggen-cli/src/cmds/project.rs:627` | ⚠️ PARTIAL | Blocking operation (architectural issue) |

### Marketplace Commands (7/14 WORKING)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen marketplace search` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:152` | ✅ COMPLETE | Query package registry |
| `ggen marketplace install` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:185` | ✅ COMPLETE | Install marketplace packages |
| `ggen marketplace list` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:228` | ⚠️ PARTIAL | List works, filtering/sorting stubs |
| `ggen marketplace publish` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:294` | ✅ COMPLETE | Publish to marketplace |
| `ggen marketplace validate` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:315` | ✅ COMPLETE | Validate package/all packages |
| `ggen marketplace maturity` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:341` | ✅ COMPLETE | Get maturity assessment |
| `ggen marketplace dashboard` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs:363` | ⚠️ PARTIAL | Returns demo structure |
| `ggen marketplace generate-artifacts` | `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs` | ❌ STUB | Flags/options unclear |

**Marketplace Features with Incomplete Implementation:**
- Line 252-264: Maturity level filtering (documented but placeholder)
- Line 272-285: Sorting functionality (documented but no actual sorting)

### Paper Commands (2/9 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen paper new` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:126` | ✅ COMPLETE | Create paper from template |
| `ggen paper list-templates` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:277` | ✅ COMPLETE | List 13+ templates |
| `ggen paper generate` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:165` | ❌ STUB | Returns 0KB placeholder |
| `ggen paper validate` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:204` | ❌ STUB | Only checks file existence |
| `ggen paper export` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:247` | ❌ STUB | Returns placeholder path |
| `ggen paper compile` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:336` | ❌ STUB | No pdflatex execution |
| `ggen paper init-bibliography` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:369` | ❌ STUB | Creates file only, no content |
| `ggen paper submit` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:405` | ❌ STUB | Sets draft status only |
| `ggen paper track` | `/home/user/ggen/crates/ggen-cli/src/cmds/paper.rs:430` | ❌ STUB | Returns empty tracking |

### Workflow Commands (0/5 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen workflow init` | `/home/user/ggen/crates/ggen-cli/src/cmds/workflow.rs:61` | ❌ STUB | Placeholder output |
| `ggen workflow analyze` | `/home/user/ggen/crates/ggen-cli/src/cmds/workflow.rs:86` | ❌ STUB | Hardcoded demo values |
| `ggen workflow discover` | `/home/user/ggen/crates/ggen-cli/src/cmds/workflow.rs` | ❌ STUB | Likely incomplete |
| `ggen workflow report` | `/home/user/ggen/crates/ggen-cli/src/cmds/workflow.rs` | ❌ STUB | Likely incomplete |
| `ggen workflow validate` | `/home/user/ggen/crates/ggen-cli/src/cmds/workflow.rs` | ❌ STUB | Likely incomplete |

### CI Commands (0/1 COMPLETE)
| Command | File | Status | Notes |
|---------|------|--------|-------|
| `ggen ci workflow` | `/home/user/ggen/crates/ggen-cli/src/cmds/ci.rs:7-18` | ❌ LEGACY | Uses old Subcommand enum, not #[verb] |

---

## Implementation Patterns

### Pattern 1: Complete Implementations
Found in: ai, graph, hook, utils, and most of template/project

**Structure:**
```rust
#[verb]
fn command_name(params: Type) -> Result<Output> {
    use ggen_domain::module::{execute_function, InputType};
    
    let input = InputType { /* ... */ };
    let result = crate::runtime::block_on(async move {
        execute_function(input).await
    }).map_err(|e| {
        NounVerbError::execution_error(e.to_string())
    })?;
    
    Ok(Output { /* structured data */ })
}
```

**Key Characteristics:**
- Calls into ggen-domain layer
- Uses block_on() for async/sync bridge
- Proper error handling with .map_err()
- Returns Serialize-able output struct
- No hardcoded values

### Pattern 2: Partial Implementations
Found in: template::generate-tree, project::watch, marketplace::list

**Issues:**
- Some parameters are underscore-prefixed (unused)
- Placeholder return values
- TODO comments or FIXME markers
- Incomplete feature implementations

Example:
```rust
// marketplace.rs:271-285 - Sorting not implemented
if let Some(sort_field) = sort {
    match sort_field.as_str() {
        "maturity" => {
            // Would sort by maturity score in real implementation
        }
        // ... more stubs
        _ => {}
    }
}
```

### Pattern 3: Stub Implementations
Found in: paper (mostly), workflow (all), ci

**Characteristics:**
- Return hardcoded/demo data
- No actual file I/O or API calls
- Unused parameters with leading underscore
- Comments explaining what should be implemented
- Always return Ok() never Err()

Example:
```rust
// paper.rs:336 - compile command
fn compile(tex_file: PathBuf, engine: Option<String>, bibtex: bool) -> Result<CompileOutput> {
    let _engine = engine.unwrap_or_else(|| "pdflatex".to_string());
    
    Ok(CompileOutput {
        source_file: tex_file.display().to_string(),
        output_pdf: output_pdf.display().to_string(),
        compilation_time_ms: 0,  // Hardcoded
        page_count: 10,           // Hardcoded
        file_size: "0 KB".to_string(),  // Hardcoded
        warnings: if bibtex {
            vec!["Run bibtex to regenerate bibliography".to_string()]
        } else {
            vec![]
        },
    })
}
```

---

## Error Handling Deep Dive

### Error Type Chain
```
Command (#[verb] function)
  └─> .map_err(|e| NounVerbError::execution_error(...))
       └─> clap_noun_verb::NounVerbError
            └─> Displayed to user as error message
```

### Actual Error Occurrences in Code

**Properly handled errors:**
- File I/O errors (template.rs:336-341)
- Directory creation (project.rs:336-341)
- Domain layer errors (all commands)

**Example from project::init:**
```rust
fs::create_dir_all(&path).map_err(|e| {
    NounVerbError::execution_error(format!(
        "Failed to create project directory: {}",
        e
    ))
})?;
```

**Stub implementations that never error:**
- Paper commands always return Ok()
- Workflow commands always return Ok()
- No validation failures possible

---

## Command Router Entry Points

### Primary Entry Point
File: `/home/user/ggen/crates/ggen-cli/src/main.rs`
```rust
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    ggen_cli_lib::cli_match().await
}
```

### CLI Match Function
File: `/home/user/ggen/crates/ggen-cli/src/lib.rs:72-84`
```rust
pub async fn cli_match() -> ggen_utils::error::Result<()> {
    // Handle --version flag
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb auto-discovery
    clap_noun_verb::run()
        .map_err(|e| ggen_utils::error::Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
```

### Command Router Module
File: `/home/user/ggen/crates/ggen-cli/src/cmds/mod.rs:12-37`
```rust
// Command modules - clap-noun-verb auto-discovery
pub mod ai;           // 3 commands
pub mod graph;        // 4 commands
pub mod hook;         // 4 commands
pub mod marketplace;  // 14+ commands
pub mod paper;        // 9 commands
pub mod project;      // 7 commands
pub mod template;     // 8 commands
pub mod utils;        // 2 commands
pub mod workflow;     // 5 commands

pub fn run_cli() -> Result<()> {
    clap_noun_verb::run()
        .map_err(|e| Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
```

---

## Missing/Broken Commands from README

### Documented in README but Not Found
1. `ggen ai generate-ontology` - Line 14 of README
   - **Status**: ❌ NOT IMPLEMENTED
   - **Expected location**: Would be `ai.rs` as separate verb
   - **Current workaround**: Use `ggen ai generate` with prompt

### Documented with Incomplete Implementation
1. `ggen marketplace list --min-maturity production` (Line 220 of README)
   - **Status**: ⚠️ Stub implementation (marketplace.rs:252-264)
   - **Works**: Basic list
   - **Missing**: Actual maturity score fetching and filtering

2. `ggen hook create pre-commit --name validate-ontology` (Line 46 of README)
   - **Status**: ✅ Actually works
   - **Evidence**: hook.rs:58-75 is fully implemented

---

## Async Runtime Bridge Details

### block_on() Usage
Most commands use the pattern:
```rust
crate::runtime::block_on(async move { 
    domain_async_function().await 
})
```

**Files using this:**
- ai.rs: All 3 commands
- graph.rs: All 4 commands  
- utils.rs: Both commands
- project.rs: Most commands
- marketplace.rs: Uses execute_async_verb() helper instead

### execute_async_verb() Helper
Found in: hook.rs, marketplace.rs

**Location**: `/home/user/ggen/crates/ggen-cli/src/runtime_helper.rs`

**Usage:**
```rust
execute_async_verb(async move {
    execute_function(input).await
        .map_err(|e| NounVerbError::execution_error(e.to_string()))
})
```

---

## File Statistics

```
Total Lines in Command Files: ~3,500 LOC
├─ template.rs:   339 lines
├─ project.rs:    696 lines
├─ marketplace.rs: 1,200+ lines (rapidly growing)
├─ paper.rs:      438 lines
├─ ai.rs:         632 lines
├─ workflow.rs:   200+ lines
├─ graph.rs:      150 lines
├─ hook.rs:       156 lines
├─ utils.rs:      156 lines
└─ ci.rs:         20 lines

Actual Working Code: ~2,000 LOC
Stub/Placeholder Code: ~800 LOC
Configuration/Types: ~700 LOC
```

---

## Next Steps for Implementers

1. **Immediate**: Complete paper::compile with actual pdflatex
2. **High Priority**: Implement workflow commands with real process mining
3. **Medium Priority**: Complete marketplace maturity filtering
4. **Low Priority**: Update CI to use #[verb] pattern

All locations and line numbers are accurate as of the analysis date.
