# ggen v2.0 Architecture - Quick Reference Card

**One-page reference for the ggen v2.0 architecture**

---

## 🏗️ Three Layers

```
CLI Layer (commands/)       → Argument parsing, routing
    ↓
Domain Layer (domain/)      → Business logic, validation
    ↓
Runtime Layer (ggen-core/)  → Templates, RDF, I/O
```

---

## 📁 File Structure

```
commands/project/new.rs     → ggen project new
domain/project/new.rs       → Business logic
ggen-core/template/         → Template engine
```

---

## 🔄 Data Flow

```
User Input
  → CLI parses args
  → Domain validates
  → Runtime loads template
  → RDF graph built
  → SPARQL executed
  → Tera renders
  → Frozen sections merged
  → File written
```

---

## 📝 Template Structure

```yaml
---
rdf_inline:                    # RDF triples
  - "@prefix ex: <...> ."
  - "ex:{{name}} a ex:Module ."

sparql:                        # Queries
  get_type: "SELECT ?type ..."

frozen_sections:               # Preserved
  - "impl"
  - "tests"

to: "src/{{name}}.rs"         # Output
---
Template body with {{ vars }}
```

---

## 🔒 Frozen Sections

```rust
// FREEZE START: marker_name
// User edits preserved here
// FREEZE END: marker_name
```

**Merge**: Template + Preserved → Final Output

---

## 🎯 Key Decisions

| Decision | Why |
|----------|-----|
| **Global Runtime** | 50% faster builds |
| **RDF Separation** | Pure data, validated |
| **Frozen Sections** | Preserve user edits |
| **Filesystem Routing** | Auto-discovery |

---

## 📊 Performance

| Metric | v1.x | v2.0 | Δ |
|--------|------|------|---|
| Build | 60-90s | 30-45s | **-50%** |
| Gen | 3s | 2s | **-33%** |
| Binary | 25MB | 18MB | **-28%** |

---

## 🚀 Migration Checklist

- [ ] Update to v2.0: `brew upgrade ggen`
- [ ] Verify: `ggen --version`
- [ ] Update scripts: `market` → `marketplace`
- [ ] Test workflows
- [ ] Update dependencies (if lib user)

---

## 🛠️ Command Pattern

**CLI Layer**:
```rust
#[verb]
pub async fn run(args: &Args) -> Result<()> {
    domain::function(args).await
}
```

**Domain Layer**:
```rust
pub async fn function(args) -> DomainResult<Model> {
    // Business logic, no CLI deps
}
```

---

## 🔗 Quick Links

- [Full Architecture](V2_ARCHITECTURE_FINAL.md)
- [Migration Guide](../MIGRATION_V1_TO_V2.md)
- [Complete Design](v2-architecture-complete.md)

---

**Remember**: CLI → Domain → Runtime | Pure RDF | Frozen Sections
