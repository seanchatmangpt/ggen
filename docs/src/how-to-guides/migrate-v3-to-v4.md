# Migrate from ggen v3.4.0 to v4.0.0

## Overview

**Good news**: Migration from ggen v3.4.0 to v4.0.0 is **completely transparent**. You do not need to change any commands or scripts.

v4.0.0 features major internal improvements (clap-noun-verb v5.3.0 upgrade, three-layer architecture enforcement, new AI introspection capabilities) while maintaining **100% backwards compatibility** with v3.4.0's CLI surface.

### What's Changing

**Internal Changes** (Developers):
- Verb registration now explicit (`#[verb("verb_name", "noun_name")]` instead of implicit)
- Three-layer architecture enforced across all modules
- clap-noun-verb v5.3.0 framework (improved discoverability, AI agent support)

**User-Facing Changes**: NONE ✅
- All existing commands work exactly as before
- CLI syntax unchanged
- Script compatibility 100%

## Installation

Simply update ggen:

```bash
# Using cargo
cargo install ggen

# Using homebrew (if available)
brew upgrade ggen

# Using docker
docker pull ggen:4.0.0
```

## Verification

After updating, verify v4.0.0 is installed:

```bash
ggen --version
# Output: ggen 4.0.0
```

Test existing commands to ensure compatibility:

```bash
# These commands work exactly as before
ggen template generate --template hello.tmpl
ggen ai generate-ontology --prompt "create a user model"
ggen graph load --source ontology.ttl
ggen ci workflow --name "build-test-deploy"
```

## What's New (Optional Features)

### AI Agent Introspection (New)

v4.0.0 adds new introspection flags for AI agents to discover capabilities programmatically:

```bash
# List all available verbs and their metadata
ggen --graph

# Get metadata for a specific verb
ggen --capabilities template generate
# Output: JSON with verb signature, arguments, return type

# Show detailed type information
ggen --introspect template generate
# Output: Human-readable verb details with type info
```

These flags enable AI agents and automation tools to:
- Discover all available commands
- Understand command signatures
- Plan workflows programmatically
- Integrate ggen as an autonomous agent

**For end users**: These flags are optional. Existing workflows are unaffected.

## Breaking Changes (For Developers)

### If you're a developer building ggen features:

1. **Verb Registration Pattern Changed**:
   ```rust
   // v3.4.0 (Old)
   #[verb]
   fn generate(template: Option<String>) -> Result<Output>

   // v4.0.0 (New)
   #[verb("generate", "template")]
   fn generate(template: Option<String>) -> Result<Output>
   ```

2. **clap Attributes Removed from Parameters**:
   ```rust
   // v3.4.0 (Old)
   #[verb]
   fn generate(
       #[clap(long, default_value = "default.tmpl")]
       template: Option<String>
   ) -> Result<Output>

   // v4.0.0 (New)
   #[verb("generate", "template")]
   fn generate(template: Option<String>) -> Result<Output> {
       let template = template.unwrap_or_else(|| "default.tmpl".to_string());
       // ... rest of implementation
   }
   ```

3. **Three-Layer Architecture Mandatory**:
   All modules must follow strict separation:
   - **Layer 3 (CLI)**: Input validation, output formatting
   - **Layer 2 (Integration)**: Async coordination, resource management
   - **Layer 1 (Domain)**: Pure business logic, NO CLI dependencies

See [Architecture Guide](../../explanations/architecture.md) for details.

## Feature Flags (Advanced)

v4.0.0 introduces optional feature flags for advanced users (coming in future releases):

```bash
# Default (minimal): Just core CLI
cargo install ggen

# With autonomic capabilities (future)
cargo install ggen --features autonomic

# With all features (future)
cargo install ggen --features full
```

Currently, all features are available by default. Feature flags are planned for v4.1.

## FAQ

### Q: Do I need to change my scripts?
**A**: No. All existing commands work unchanged. v4.0.0 is 100% backwards compatible with v3.4.0 scripts.

### Q: What about my configuration files?
**A**: No changes needed. ggen.toml, Makefile.toml, and all configuration formats remain unchanged.

### Q: Are there performance improvements?
**A**: Yes. The clap-noun-verb v5.3.0 upgrade brings performance improvements and better discoverability. You'll see faster CLI startup times.

### Q: Should I update immediately?
**A**: Yes, if you're actively using ggen. v4.0.0 is stable and ready for production. No migration effort required.

### Q: What about the 4 disabled modules (hook, marketplace, packs, utils)?
**A**: They remain disabled in v4.0.0 and will be restored in v4.1 after marketplace-v2 architecture completion. This does not affect core ggen functionality.

### Q: How do I use the new introspection flags?
**A**: These are primarily for AI agents and automation tools. As an end user, you don't need them. But if you're building AI-driven workflows:

```bash
# Get the complete command graph
ggen --graph | jq .

# Plan a workflow by discovering capabilities
ggen --capabilities ai generate-ontology | jq .arguments
```

## Troubleshooting

### Command not found after updating
```bash
# Verify installation
which ggen
ggen --version

# If not found, reinstall
cargo install ggen --force
```

### Old binaries still in use
If you installed via homebrew or system package manager:
```bash
# Clear caches
cargo clean
brew cleanup

# Reinstall
cargo install ggen
```

### Still having issues?
1. Check CHANGELOG.md for technical details
2. Review CLI reference: `docs/src/reference/cli.md`
3. File an issue: https://github.com/seanchatmangpt/ggen/issues

## Next Steps

After updating to v4.0.0:

1. **Verify Installation**: Run `ggen --version`
2. **Test Existing Workflows**: Run your scripts - they should work unchanged
3. **Explore New Features** (optional): Try `ggen --graph` and `ggen --capabilities`
4. **Stay Updated**: Follow releases at https://github.com/seanchatmangpt/ggen/releases

## Support

For questions or issues:
- **Documentation**: https://github.com/seanchatmangpt/ggen#readme
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions

---

**TL;DR**: Update ggen to v4.0.0, run your existing commands - they work exactly as before. Done! ✅
