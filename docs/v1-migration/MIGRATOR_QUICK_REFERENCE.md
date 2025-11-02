# V1 Migrator Quick Reference Guide

## 🎯 For Migrator Agents: How to Use These Extracted Files

### Location
All v1 command implementations: `/Users/sac/ggen/docs/v1-migration/*.rs.bak`

### Priority Order (80/20 Rule)

#### **TIER 1: Must Have (Start Here)**
1. `template-generate.rs.bak` - Core template generation
2. `project-new.rs.bak` - Project creation
3. `marketplace-search.rs.bak` - Package discovery

#### **TIER 2: Should Have**
4. `template-list.rs.bak` - Template browsing
5. `project-gen.rs.bak` - Template-based generation
6. `marketplace-install.rs.bak` - Package installation

#### **TIER 3: Nice to Have**
7. `template-show.rs.bak` - Template inspection
8. `template-new.rs.bak` - Template creation
9. `project-apply.rs.bak` - Existing project updates
10. Others as needed

### Migration Pattern: Copy-Adapt-Test

```rust
// STEP 1: Copy Args struct from .bak file
#[derive(Args, Debug, Clone)]
pub struct GenerateArgs {
    #[arg(short, long)]
    pub template: PathBuf,

    #[arg(short, long, default_value = ".")]
    pub output: PathBuf,

    #[arg(short, long)]
    pub vars: Vec<String>,
}

// STEP 2: Adapt run() function for new location
// OLD (v1): cli/src/commands/template/generate.rs
// NEW (v2): cli/src/cmds/template/generate.rs

pub fn run(args: &GenerateArgs) -> Result<()> {
    // Keep the sync wrapper pattern
    crate::runtime::execute(async {
        // UPDATE: Verify domain function exists
        crate::domain::template::generate_file(&options).await
    })
}

// STEP 3: Port tests
#[cfg(test)]
mod tests {
    // Copy test module from .bak
    // Update paths if needed
}
```

### Architectural Patterns (Use These!)

#### Pattern A: Sync Wrapper (RECOMMENDED)
```rust
// Best for: Commands with async domain logic
pub fn run(args: &Args) -> Result<()> {
    let options = build_options(args);

    crate::runtime::execute(async {
        crate::domain::noun::verb(&options).await?;
        println!("✅ Success!");
        Ok(())
    })
}
```

**Use for:**
- template generate, list, show, lint
- marketplace search, install, list
- utils doctor

#### Pattern B: Async with spawn_blocking
```rust
// Best for: Commands calling sync domain code
pub async fn run(args: &Args) -> Result<()> {
    let result = tokio::task::spawn_blocking({
        let args = args.clone();
        move || crate::domain::noun::verb(&args)
    }).await??;

    println!("✅ Success!");
    Ok(())
}
```

**Use for:**
- project new, init

#### Pattern C: Direct Domain Call
```rust
// Best for: Simple commands with sync domain logic
pub fn run(args: &Args) -> Result<()> {
    crate::domain::noun::verb(args)
}
```

**Use for:**
- Simple utilities

### Domain Layer Checklist

Before migrating a command, verify:

```bash
# 1. Check if domain function exists
rg "pub.*fn generate_file" cli/src/domain/

# 2. Check function signature
rg -A 5 "pub.*fn generate_file" cli/src/domain/

# 3. Verify return type matches
# Old: Result<GenerateResult>
# New: Result<GenerateResult> (should match)
```

### File Placement

```
v1 Location                          →  v2 Location
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
cli/src/commands/template/generate.rs  →  cli/src/cmds/template/generate.rs
cli/src/commands/project/new.rs        →  cli/src/cmds/project/new.rs
cli/src/commands/marketplace/search.rs →  cli/src/cmds/marketplace/search.rs
```

### Common Adaptations Needed

#### 1. Import Path Changes
```rust
// OLD v1
use crate::commands::template::GenerateArgs;

// NEW v2
use crate::cmds::template::GenerateArgs;
```

#### 2. Runtime Bridge
```rust
// Already exists in v2!
// No changes needed to runtime::execute()
```

#### 3. Domain Calls
```rust
// Check if domain module structure changed
// OLD: crate::domain::template::generate_file()
// NEW: Same! (verify with grep)
```

#### 4. Error Handling
```rust
// v2 uses same error system
use ggen_utils::error::Result;
// No changes needed!
```

### Testing Strategy

#### 1. Port Existing Tests First
```rust
// Copy from .bak file's #[cfg(test)] mod tests
// Update any changed paths
// Run: cargo test --package cli --lib cmds::template::generate
```

#### 2. Add Integration Tests
```bash
# Check cli/tests/ for existing integration tests
# Port relevant tests from v1
```

#### 3. Manual Testing
```bash
# Build and test each command
cargo build --release
./target/release/ggen template generate --help
./target/release/ggen template generate -t example.tmpl -o output/
```

### Coordination Protocol

Each migrator agent MUST:

```bash
# BEFORE migration
npx claude-flow@alpha hooks pre-task --description "migrate-template-generate"

# DURING migration (after each file)
npx claude-flow@alpha hooks post-edit \
  --file "cli/src/cmds/template/generate.rs" \
  --memory-key "hive/migration/template-generate"

# AFTER migration
npx claude-flow@alpha hooks post-task --task-id "<task-id>"
```

### Verification Checklist

Before marking a command as "migrated":

- [ ] Args struct copied and adapted
- [ ] run() function implemented with correct pattern
- [ ] Domain calls verified to exist
- [ ] Tests ported and passing
- [ ] Command builds without errors
- [ ] Manual test confirms it works
- [ ] Documentation updated (if needed)
- [ ] Coordination hooks executed

### Common Pitfalls

❌ **DON'T:**
- Change Args structure without good reason
- Skip tests (port them!)
- Forget to verify domain functions exist
- Use wrong architectural pattern
- Mix sync/async incorrectly

✅ **DO:**
- Keep same UX (args, output format)
- Reuse as much v1 code as possible
- Follow existing v2 patterns
- Test thoroughly
- Coordinate via hooks

### Quick Command Reference

```bash
# Find all .bak files
ls docs/v1-migration/*.bak

# View a specific backup
cat docs/v1-migration/template-generate.rs.bak

# Search for domain function
rg "pub.*fn generate_file" cli/src/domain/

# Check current v2 structure
tree cli/src/cmds/ -L 2

# Build and test
cargo build --package cli
cargo test --package cli --lib cmds::template
```

### Example Migration Workflow

```bash
# 1. Choose command (start with tier 1)
COMMAND="template-generate"

# 2. Read the backup
cat docs/v1-migration/template-generate.rs.bak

# 3. Create new file in v2 location
touch cli/src/cmds/template/generate.rs

# 4. Copy Args and run() with adaptations
# (Use Pattern A: Sync Wrapper)

# 5. Verify domain function exists
rg "generate_file" cli/src/domain/template/

# 6. Port tests
# Copy from .bak file's test module

# 7. Build and test
cargo test --package cli --lib cmds::template::generate

# 8. Manual verification
cargo run -- template generate --help

# 9. Coordination hooks
npx claude-flow@alpha hooks post-task --task-id "<id>"
```

---

**Status:** Ready for migration agents
**Files Available:** 14 commands
**Patterns Documented:** 3 architectural patterns
**Priority Queue:** Tier 1 → Tier 2 → Tier 3
