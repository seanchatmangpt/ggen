# LSP Tool Usage for Rust in Claude Code

## Quick Reference

**LSP Server**: rust-analyzer (plugin: `rust-analyzer-lsp@claude-plugins-official`)
**Installation**: `rustup component add rust-analyzer`
**Required**: rust-analyzer component must be installed before LSP operations work

## Parameter Formats

All LSP operations require these parameters:

- **filePath**: Absolute path to a `.rs` file (required)
- **line**: 1-based line number (required)
- **character**: 1-based character offset (required)
- **operation**: LSP operation type (required)

Example:
```
filePath: /Users/sac/ggen/crates/ggen-cli/src/main.rs
line: 8
character: 1
operation: goToDefinition
```

## LSP Operations for Rust

### 1. documentSymbol
**Purpose**: List all symbols (functions, structs, traits, impls) in a file

**Use when**: You need to understand the structure of a file

**Parameters**: Any line/character works (typically 1, 1)

**Example**:
```bash
# Find all symbols in main.rs
LSP: filePath=/Users/sac/ggen/crates/ggen-cli/src/main.rs, line=1, character=1, operation=documentSymbol
```

**Returns**: Tree of symbols with kinds (Function, Struct, Trait, Module, etc.)

### 2. workspaceSymbol
**Purpose**: Search for symbols across entire workspace

**Use when**: You know a symbol name but not its location

**Parameters**: Any line/character works (typically 1, 1)

**Example**:
```bash
# Find 'cli_match' function anywhere in workspace
LSP: filePath=/Users/sac/ggen/crates/ggen-cli/src/main.rs, line=1, character=1, operation=workspaceSymbol
```

**Returns**: List of symbol locations with file paths

### 3. goToDefinition
**Purpose**: Jump to where a symbol is defined

**Use when**: You want to see the implementation of a function/struct/trait

**Parameters**: Point to the symbol usage (line/character must be exact)

**Example**:
```bash
# Go to definition of 'ggen_cli_lib::cli_match' on line 8
LSP: filePath=/Users/sac/ggen/crates/ggen-cli/src/main.rs, line=8, character=8, operation=goToDefinition
```

**Returns**: Location of definition (file, line, character)

### 4. findReferences
**Purpose**: Find all usages of a symbol

**Use when**: You want to see where a function/struct is called

**Parameters**: Point to the symbol definition or usage

**Example**:
```bash
# Find all calls to 'main' function
LSP: filePath=/Users/sac/ggen/crates/ggen-cli/src/main.rs, line=7, character=1, operation=findReferences
```

**Returns**: List of all reference locations

### 5. hover
**Purpose**: Get type information and documentation

**Use when**: You need to know the type of a variable or signature of a function

**Parameters**: Point to the symbol you want info about

**Example**:
```bash
# Get type information for 'ggen_cli_lib::cli_match'
LSP: filePath=/Users/sac/ggen/crates/ggen-cli/src/main.rs, line=8, character=8, operation=hover
```

**Returns**: Type information, documentation, signature

### 6. goToImplementation
**Purpose**: Find implementations of a trait

**Use when**: You want to see all structs that implement a trait

**Parameters**: Point to the trait definition

**Example**:
```bash
# Find all implementations of 'Display' trait
LSP: filePath=/path/to/file.rs, line=10, character=15, operation=goToImplementation
```

**Returns**: List of implementation locations

### 7. prepareCallHierarchy + incomingCalls/outgoingCalls
**Purpose**: Trace call relationships

**Use when**: You need to understand call chains and dependencies

**Workflow**:
```bash
# Step 1: Prepare call hierarchy at a function
LSP: filePath=/path/to/file.rs, line=42, character=1, operation=prepareCallHierarchy

# Step 2: Get callers (who calls this function)
LSP: filePath=/path/to/file.rs, line=42, character=1, operation=incomingCalls

# Step 3: Get callees (what this function calls)
LSP: filePath=/path/to/file.rs, line=42, character=1, operation=outgoingCalls
```

**Returns**: Hierarchical call tree

## Common Workflows

### Workflow 1: Explore Unknown Code
```bash
1. Glob "**/*.rs"                    # Find files
2. LSP documentSymbol               # Get file structure
3. Read interesting functions       # Read implementation
```

### Workflow 2: Find Symbol Definition
```bash
1. LSP workspaceSymbol              # Search for symbol
2. LSP goToDefinition               # Jump to definition
3. LSP findReferences               # See all usages
```

### Workflow 3: Understand Call Chains
```bash
1. LSP prepareCallHierarchy         # At entry point
2. LSP outgoingCalls                # See what it calls
3. LSP goToDefinition               # Drill down
```

### Workflow 4: Refactor Safely
```bash
1. LSP findReferences               # Find all usages
2. LSP goToDefinition               # Check definition
3. Make changes                     # Edit with confidence
```

## Troubleshooting

### Error: "No LSP server available"

**Cause**: rust-analyzer component not installed

**Solution**:
```bash
rustup component add rust-analyzer
```

**Verification**:
```bash
rustup component list | grep rust-analyzer
# Should show: rust-analyzer (installed)
```

### Error: "server is starting"

**Cause**: LSP server still initializing (especially on first use)

**Solution**: Wait 10-30 seconds and retry

**Note**: Large workspaces (30+ crates) may take longer to index

### Error: "Failed to sync file open"

**Cause**: File path incorrect or file doesn't exist

**Solution**:
```bash
# Verify file exists
ls -la /Users/sac/ggen/crates/ggen-cli/src/main.rs

# Use absolute path, not relative
```

### Symbol Not Found

**Cause**: Line/character position doesn't point to a valid symbol

**Solution**:
```bash
# Use Read tool to find exact line/character
Read: /Users/sac/ggen/crates/ggen-cli/src/main.rs

# Count line numbers (1-based)
# Count character positions (1-based, from start of line)
```

## Best Practices

### 1. Use LSP Before Grep
- **LSP**: Semantic understanding (knows about types, traits, modules)
- **Grep**: Text search only (no semantic awareness)

**Rule**: Always try LSP first for .rs files, fall back to Grep only if LSP fails

### 2. Verify Line/Character Positions
```bash
# Wrong: Guessing positions
LSP: line=10, character=50  # Might point to whitespace

# Right: Read file first to get exact position
Read: file.rs  # See that symbol is on line 42, character 8
LSP: line=42, character=8
```

### 3. Start Broad, Then Narrow
```bash
1. workspaceSymbol      # Find symbol anywhere
2. goToDefinition       # Jump to definition
3. documentSymbol       # See surrounding context
4. findReferences       # Understand usage
```

### 4. Use documentSymbol for Context
Before diving into a function, get the full file structure:
```bash
LSP: operation=documentSymbol  # See all symbols
# Then choose what to investigate
```

### 5. Batch LSP Operations
LSP calls are independent - run multiple in one message:
```bash
LSP: operation=workspaceSymbol
LSP: operation=documentSymbol
LSP: operation=findReferences
# All in one message for efficiency
```

## Comparison: LSP vs Grep for Rust

| Task | LSP (Better) | Grep (Worse) |
|------|--------------|--------------|
| Find function definition | ✅ Semantic understanding | ❌ Text search, finds all mentions |
| List struct methods | ✅ Only shows impls for that struct | ❌ Shows method name everywhere |
| Find trait implementations | ✅ Knows trait relationships | ❌ Text search on "impl Trait" |
| Get function signature | ✅ Full type info | ❌ Need to read source |
| Find all callers | ✅ Knows call graph | ❌ Text search on function name |
| Search for string literal | ❌ Not designed for this | ✅ Perfect for text search |

**Rule**: Use LSP for code navigation, Grep for text/content search

## Workspace-Specific Notes

### 30-Crate Workspace
- **Indexing time**: 30-60 seconds on first load
- **Memory usage**: ~500MB-1GB for rust-analyzer
- **Best practice**: Let LSP fully index before intensive operations

### Cargo Workspace Layout
```
/Users/sac/ggen/
├── Cargo.toml           # Workspace root
├── crates/
│   ├── ggen-cli/        # CLI crate
│   ├── ggen-core/       # Core crate
│   └── ...              # 28 more crates
```

**LSP workspaceSymbol** searches across all crates automatically.

### Performance Tips
1. **First operation**: Slower (needs to build index)
2. **Subsequent operations**: Fast (index cached)
3. **Large changes**: Re-indexing triggered automatically
4. **Best performance**: Use after `cargo check` (index already built)

## Summary

**Required Setup**:
```bash
rustup component add rust-analyzer
```

**Core Operations**:
- `documentSymbol` - File structure
- `workspaceSymbol` - Search workspace
- `goToDefinition` - Find definition
- `findReferences` - Find usages
- `hover` - Type info

**Parameters**: Absolute file path, 1-based line, 1-based character

**Fallback**: If LSP fails, use Grep for .rs files (unlike .java files where Grep is forbidden)

---

**Version**: 1.0.0
**Date**: 2026-03-31
**Research Method**: Codebase exploration + LSP testing
**Status**: rust-analyzer component installed, LSP server functional
