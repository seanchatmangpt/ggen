# Configuration Migration Plan

## Overview

This document provides a step-by-step plan to improve configuration management in ggen, based on the hardcoded configuration analysis.

## Current State ✅

**What's Working Well:**
- ✅ Most critical configs are already environment variable-based
- ✅ Good security practices (no hardcoded API keys)
- ✅ WIP integration has excellent TOML config example
- ✅ Test fixtures appropriately hardcoded for stability

**What Needs Improvement:**
- ⚠️ No centralized configuration documentation
- ⚠️ GitHub Enterprise support missing
- ⚠️ Watch/CI timeouts not easily configurable
- ⚠️ No TOML config file support for LLM providers
- ⚠️ Configuration discovery is difficult for new users

## Implementation Plan

### Phase 1: Documentation & Defaults (Immediate - This PR)

**Status:** ✅ COMPLETED

**Deliverables:**
1. ✅ `config/defaults.toml` - Complete configuration reference
2. ✅ `docs/hardcoded_config_analysis.md` - Detailed analysis
3. ✅ `docs/config_migration_plan.md` - This migration plan

**Changes:**
- No code changes required
- Documentation only
- Safe to merge immediately

**Benefits:**
- Users can now see all configurable options
- Clear documentation of environment variables
- Reference for future TOML implementation

---

### Phase 2: TOML Config Loader (Next PR)

**Priority:** HIGH
**Estimated Effort:** 2-4 hours
**Risk:** LOW (additive only, no breaking changes)

**Implementation Tasks:**

1. **Add config crate dependency**
   ```toml
   # ggen-ai/Cargo.toml
   [dependencies]
   config = "0.14"  # or similar
   figment = "0.10"  # alternative, supports profiles
   ```

2. **Create config loader module**
   ```rust
   // ggen-ai/src/config/loader.rs
   pub struct ConfigLoader {
       settings: Settings,
   }

   impl ConfigLoader {
       pub fn load() -> Result<Settings> {
           // 1. Load defaults
           // 2. Merge system config (/etc/ggen/config.toml)
           // 3. Merge user config (~/.config/ggen/config.toml)
           // 4. Merge project config (./ggen.toml)
           // 5. Merge environment variables
           // 6. Merge CLI args
       }
   }
   ```

3. **Update existing config structs**
   ```rust
   // Modify OllamaConfig, OpenAIConfig, etc. to use new loader
   impl OllamaConfig {
       pub fn from_config() -> Result<Self> {
           let loader = ConfigLoader::load()?;
           // Extract Ollama settings
       }
   }
   ```

4. **Add config file search paths**
   ```rust
   const SYSTEM_CONFIG: &str = "/etc/ggen/config.toml";
   const USER_CONFIG: &str = "~/.config/ggen/config.toml";
   const PROJECT_CONFIG: &str = "./ggen.toml";
   ```

**Testing:**
- Unit tests for config loading
- Integration tests for config merging
- Test all environment variable overrides

**Documentation:**
- Update README with TOML config examples
- Add config file location documentation
- Update environment variable docs

---

### Phase 3: GitHub Enterprise Support (Next PR)

**Priority:** HIGH
**Estimated Effort:** 1-2 hours
**Risk:** LOW (additive only)

**Implementation Tasks:**

1. **Add environment variable support**
   ```rust
   // ggen-core/src/github.rs
   let base_url = std::env::var("GGEN_GITHUB_BASE_URL")
       .or_else(|_| std::env::var("GITHUB_API_URL"))
       .unwrap_or_else(|_| "https://api.github.com".to_string());
   ```

2. **Add TOML config support**
   ```toml
   [github]
   base_url = "https://github.company.com/api/v3"
   ```

3. **Update GitHubClient constructor**
   ```rust
   pub fn new(repo: RepoInfo) -> Result<Self> {
       let base_url = get_github_base_url()?;  // From config
       // ... rest of implementation
   }
   ```

**Testing:**
- Test with public GitHub
- Test with mock GitHub Enterprise URL
- Test environment variable override

---

### Phase 4: Watch & CI Timeout Configuration (Future)

**Priority:** MEDIUM
**Estimated Effort:** 2-3 hours
**Risk:** LOW

**Implementation Tasks:**

1. **Add watch config to ggen.toml**
   ```toml
   [watch]
   poll_interval_ms = 1000
   debounce_ms = 500
   ```

2. **Add CI timeout config**
   ```toml
   [ci.release]
   timeout_seconds = 1800
   ```

3. **Update CLI commands to use config**
   ```rust
   // Use config values as defaults
   // CLI args override config
   ```

**Testing:**
- Test with various timeout values
- Test CLI arg overrides
- Test in different environments (local, CI)

---

### Phase 5: Config Management CLI (Future)

**Priority:** LOW
**Estimated Effort:** 4-6 hours
**Risk:** LOW

**New Commands:**

```bash
# Initialize config file
ggen config init [--system|--user|--project]

# Show effective configuration
ggen config show [--format=json|toml|yaml]

# Validate configuration
ggen config validate

# Get/set individual values
ggen config get llm.provider
ggen config set llm.provider openai

# List all config keys
ggen config list

# Show config file locations
ggen config paths
```

**Implementation:**
- Add `cli/src/cmds/config.rs`
- Add config subcommands
- Add validation logic
- Add config file generation

---

## Migration Guide for Users

### Current Usage (No Changes Required)

```bash
# Everything still works with environment variables
export OLLAMA_BASE_URL=http://localhost:11434
export OPENAI_API_KEY=sk-...
export GGEN_REGISTRY_URL=https://custom-registry.com/
```

### New Usage (Optional, After Phase 2)

**Create user config file:**
```bash
mkdir -p ~/.config/ggen
cat > ~/.config/ggen/config.toml << 'EOF'
[llm]
provider = "ollama"

[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "qwen3-coder:30b"

[github]
base_url = "https://github.company.com/api/v3"
EOF
```

**Create project config:**
```bash
# Add to your project's ggen.toml
[llm]
provider = "openai"

[generation]
temperature = 0.9
max_tokens = 8000
```

**Environment variables still override everything:**
```bash
# This overrides both TOML files
export GGEN_LLM_PROVIDER=anthropic
```

---

## Risk Assessment

### Low Risk ✅
- **Phase 1 (Documentation)** - No code changes
- **Phase 2 (TOML loader)** - Additive only, backwards compatible
- **Phase 3 (GitHub Enterprise)** - Additive only

### Medium Risk ⚠️
- **Phase 4 (Timeouts)** - Changes default behavior slightly
  - Mitigation: Keep same defaults, just make configurable

### Potential Issues
1. **Config file conflicts**: User sets both ENV and TOML
   - **Solution:** Clear priority order (ENV > TOML)

2. **Invalid config values**: User sets bad timeout/URL
   - **Solution:** Add validation with helpful error messages

3. **Config file location**: Users don't know where to put files
   - **Solution:** `ggen config init` command generates files

---

## Testing Strategy

### Unit Tests
```rust
#[test]
fn test_config_load_priority() {
    // Test: ENV > user config > defaults
}

#[test]
fn test_config_validation() {
    // Test: Invalid values rejected
}

#[test]
fn test_config_file_not_found() {
    // Test: Gracefully falls back to defaults
}
```

### Integration Tests
```rust
#[test]
fn test_github_enterprise_url() {
    // Test: Custom GitHub URL works
}

#[test]
fn test_multiple_config_sources() {
    // Test: System + user + project + ENV
}
```

### Manual Testing
- [ ] Test on Linux (XDG config paths)
- [ ] Test on macOS (~/.config)
- [ ] Test on Windows (%APPDATA%)
- [ ] Test with no config files
- [ ] Test with invalid config files
- [ ] Test with environment variables
- [ ] Test GitHub Enterprise integration

---

## Rollout Plan

### Week 1: Documentation (Phase 1) ✅
- Merge documentation PR
- No user impact
- Communicate available environment variables

### Week 2: TOML Support (Phase 2)
- Implement config loader
- Add TOML file support
- Test extensively
- Release as minor version (0.x.0)
- Update documentation

### Week 3: GitHub Enterprise (Phase 3)
- Add GitHub Enterprise support
- Test with real enterprise instances
- Release as patch version (0.x.1)

### Week 4: Timeouts (Phase 4)
- Add timeout configuration
- Test in various environments
- Release as minor version (0.x.0)

### Future: CLI Commands (Phase 5)
- Add config management CLI
- Release as minor version (0.x.0)

---

## Success Metrics

### User Experience
- ✅ Zero breaking changes
- ✅ Backwards compatible with all existing setups
- ✅ Clear documentation of all options
- ✅ Easy configuration for common scenarios

### Code Quality
- ✅ All hardcoded values documented
- ✅ Clear separation of config vs constants
- ✅ Comprehensive test coverage
- ✅ Validated configuration at startup

### Adoption
- Track usage of TOML config files
- Monitor GitHub issues for config questions
- Collect feedback on ease of configuration

---

## Decision Log

### Why TOML over YAML/JSON?
- ✅ Already using TOML for ggen.toml
- ✅ Better for configuration (less ambiguity than YAML)
- ✅ More readable than JSON
- ✅ Strong typing support

### Why Not Break Backwards Compatibility?
- ✅ No benefit to users
- ✅ Would require migration scripts
- ✅ Environment variables are standard practice
- ✅ Can support both ENV and TOML simultaneously

### Why Not Immediate Full Implementation?
- ✅ Incremental rollout reduces risk
- ✅ Allows for user feedback between phases
- ✅ Easier to test and validate
- ✅ Doesn't block immediate documentation value

---

## Next Actions

**For This PR (Phase 1):**
- [x] Review analysis document
- [x] Review defaults.toml
- [x] Review migration plan
- [ ] Merge documentation
- [ ] Announce configuration options to users

**For Next PR (Phase 2):**
- [ ] Choose config library (config-rs vs figment)
- [ ] Create feature branch
- [ ] Implement config loader
- [ ] Add tests
- [ ] Update documentation
- [ ] Create PR

**For Future PRs:**
- [ ] Implement GitHub Enterprise support
- [ ] Implement timeout configuration
- [ ] Implement config CLI commands

---

**Document Version:** 1.0
**Last Updated:** 2025-10-10
**Status:** Documentation Phase Complete ✅
