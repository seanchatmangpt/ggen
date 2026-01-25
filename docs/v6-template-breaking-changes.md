<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [v6 Template System Breaking Changes Analysis](#v6-template-system-breaking-changes-analysis)
  - [Executive Summary](#executive-summary)
  - [1. Runtime vs Compile-Time Template Compilation](#1-runtime-vs-compile-time-template-compilation)
    - [Current State (v5.1.0)](#current-state-v510)
    - [Recommended v6 Breaking Changes](#recommended-v6-breaking-changes)
  - [2. Type-Safe Template Context](#2-type-safe-template-context)
    - [Current State (v5.1.0)](#current-state-v510-1)
    - [Recommended v6 Breaking Changes](#recommended-v6-breaking-changes-1)
  - [3. Template Security (Sandbox Violations)](#3-template-security-sandbox-violations)
    - [Current State (v5.1.0)](#current-state-v510-2)
      - [3.1 Disabled Autoescape](#31-disabled-autoescape)
      - [3.2 Unrestricted Filesystem Access](#32-unrestricted-filesystem-access)
      - [3.3 Shell Hook Execution](#33-shell-hook-execution)
    - [Recommended v6 Breaking Changes](#recommended-v6-breaking-changes-2)
  - [4. Template Composition Patterns](#4-template-composition-patterns)
    - [Current State (v5.1.0)](#current-state-v510-3)
    - [Recommended v6 Breaking Changes](#recommended-v6-breaking-changes-3)
  - [5. Error Reporting in Templates](#5-error-reporting-in-templates)
    - [Current State (v5.1.0)](#current-state-v510-4)
    - [Recommended v6 Breaking Changes](#recommended-v6-breaking-changes-4)
  - [6. Summary of Breaking Changes](#6-summary-of-breaking-changes)
    - [High Priority (v6.0 Release Blockers)](#high-priority-v60-release-blockers)
    - [Medium Priority (v6.1 Release)](#medium-priority-v61-release)
    - [Low Priority (v7.0 Future)](#low-priority-v70-future)
  - [7. Immediate Action Items (v6.0 Checklist)](#7-immediate-action-items-v60-checklist)
    - [Week 1: Security Hardening](#week-1-security-hardening)
    - [Week 2: Remove Shell Hooks](#week-2-remove-shell-hooks)
    - [Week 3: Type-Safe Contexts (Phase 1)](#week-3-type-safe-contexts-phase-1)
    - [Week 4: Error Reporting](#week-4-error-reporting)
  - [8. Migration Guide Template](#8-migration-guide-template)
    - [Automated Migration Tool](#automated-migration-tool)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# v6 Template System Breaking Changes Analysis

**Date**: 2026-01-24
**Scope**: `crates/ggen-domain/src/template/` and `crates/ggen-core/src/template.rs`
**Objective**: Identify type safety, security, and performance breaking changes for v6 production release

---

## Executive Summary

The current template system has **5 critical areas** requiring breaking changes for v6:

1. **Runtime-only template compilation** (no compile-time validation)
2. **Untyped template context** (`HashMap<String, Value>` without type guarantees)
3. **Security vulnerabilities** (disabled autoescape, shell hooks, unrestricted file access)
4. **Complex path resolution** (runtime-only, no validation)
5. **Lazy error detection** (errors discovered during generation, not upfront)

**Estimated Impact**: HIGH - These changes affect core template APIs across 10+ files and require migration path.

---

## 1. Runtime vs Compile-Time Template Compilation

### Current State (v5.1.0)

**Location**: `crates/ggen-core/src/template.rs:138-149`

```rust
// Templates are parsed at RUNTIME from strings
pub fn parse(input: &str) -> Result<Self> {
    let matter = Matter::<YAML>::new();
    let ParsedEntity { data, content, .. } = matter
        .parse::<serde_yaml::Value>(input)
        .map_err(|e| Error::new(&format!("Failed to parse template frontmatter: {}", e)))?;
    let raw_frontmatter = data.unwrap_or(serde_yaml::Value::Null);
    Ok(Self {
        raw_frontmatter,
        front: Frontmatter::default(),
        body: content,
    })
}
```

**Problems**:
- ❌ No compile-time validation of template syntax
- ❌ Errors discovered only during generation (runtime)
- ❌ No static analysis possible (IDE support, linting)
- ❌ Performance cost: parse on every use (mitigated by LRU cache, but cache misses expensive)

### Recommended v6 Breaking Changes

**BREAKING CHANGE 1.1**: Introduce compile-time template validation macro

```rust
// NEW: Compile-time template validation (zero-cost abstraction)
macro_rules! template {
    ($path:literal) => {{
        // Validate template at compile-time
        const TEMPLATE_STR: &str = include_str!($path);
        // Compile-time frontmatter parsing (const fn)
        const _: () = {
            // Validate YAML frontmatter syntax
            // Validate template variable references
            // Validate SPARQL query syntax
        };
        Template::from_validated(TEMPLATE_STR)
    }};
}
```

**BREAKING CHANGE 1.2**: Add `TemplateCompiler` for build-time compilation

```rust
// NEW: Build-time template compiler (ggen-macros crate)
#[derive(TemplateBundle)]
#[template_dir = "templates"]
pub struct MyTemplates {
    #[template(path = "user.tmpl")]
    user_template: CompiledTemplate<UserContext>,

    #[template(path = "project.tmpl")]
    project_template: CompiledTemplate<ProjectContext>,
}

// Generates:
// - Compile-time syntax validation
// - Type-safe context structs
// - Zero-cost template access (const)
```

**Migration Path**:
1. Phase 1 (v6.0): Add `Template::compile()` alongside existing `parse()` (opt-in)
2. Phase 2 (v6.1): Deprecate `parse()`, emit warnings
3. Phase 3 (v7.0): Remove `parse()`, make `compile()` default

---

## 2. Type-Safe Template Context

### Current State (v5.1.0)

**Location**: `crates/ggen-core/src/templates/context.rs:74-79`

```rust
#[derive(Debug, Clone)]
pub struct TemplateContext {
    /// Variables for template rendering (UNTYPED)
    variables: BTreeMap<String, Value>,  // ❌ serde_json::Value = stringly-typed
}

impl TemplateContext {
    pub fn set<K: Into<String>, V: Into<Value>>(&mut self, key: K, value: V) -> Result<()> {
        self.variables.insert(key.into(), value.into());  // ❌ No compile-time type checking
        Ok(())
    }
}
```

**Location**: `crates/ggen-domain/src/template/render_with_rdf.rs:162-165`

```rust
// Untyped context construction
let mut context = Context::new();
for (key, value) in &options.variables {
    context.insert(key, value);  // ❌ Runtime string insertion
}
```

**Problems**:
- ❌ No compile-time type checking of template variables
- ❌ Typos in variable names discovered at runtime
- ❌ No autocomplete/IDE support for template contexts
- ❌ Cannot statically verify all required variables are provided
- ❌ Type mismatches discovered during rendering, not construction

### Recommended v6 Breaking Changes

**BREAKING CHANGE 2.1**: Introduce strongly-typed template context trait

```rust
// NEW: Type-safe template context trait
pub trait TemplateContext: Serialize {
    /// Compile-time verification of required fields
    const REQUIRED_FIELDS: &'static [&'static str];

    /// Template-specific context type
    type Variables: Serialize + DeserializeOwned;

    /// Validate context completeness at compile-time
    fn validate() -> Result<()> {
        // Checked at compile-time via const fn
        Ok(())
    }
}

// Example usage:
#[derive(Serialize, Deserialize, TemplateContext)]
pub struct UserTemplateContext {
    #[required]  // ✅ Compile-time enforcement
    pub name: String,

    #[required]
    pub email: String,

    #[optional(default = "1.0.0")]
    pub version: String,
}

// Usage (type-safe):
let ctx = UserTemplateContext {
    name: "Alice".into(),
    email: "alice@example.com".into(),
    version: "2.0.0".into(),  // ✅ Typo caught by compiler
};

// ❌ Won't compile (missing required field):
// let ctx = UserTemplateContext { name: "Alice".into() };
```

**BREAKING CHANGE 2.2**: Add `#[derive(TemplateContext)]` macro

```rust
// NEW: Derive macro for template contexts (ggen-macros)
#[proc_macro_derive(TemplateContext, attributes(required, optional))]
pub fn derive_template_context(input: TokenStream) -> TokenStream {
    // Generate:
    // 1. REQUIRED_FIELDS constant
    // 2. Validation logic
    // 3. Conversion to/from Tera Context
    // 4. Compile-time checks
}
```

**BREAKING CHANGE 2.3**: Template context validation at parse time

```rust
// NEW: Associate context type with template at parse time
impl Template {
    pub fn parse_with_context<C: TemplateContext>(input: &str) -> Result<TypedTemplate<C>> {
        let template = Self::parse(input)?;

        // ✅ Validate template references only variables in C
        let referenced_vars = extract_template_variables(&template.body)?;
        C::validate_variables(&referenced_vars)?;

        Ok(TypedTemplate {
            template,
            _context: PhantomData,
        })
    }
}

pub struct TypedTemplate<C: TemplateContext> {
    template: Template,
    _context: PhantomData<C>,
}

impl<C: TemplateContext> TypedTemplate<C> {
    pub fn render(&self, context: &C) -> Result<String> {
        // ✅ Compile-time guarantee: context matches template requirements
        // ...
    }
}
```

**Migration Path**:
1. Phase 1 (v6.0): Add `TypedTemplate<C>` alongside existing `Template` (opt-in)
2. Phase 2 (v6.1): Convert all internal templates to typed contexts
3. Phase 3 (v7.0): Deprecate untyped `Template::render()`, require typed contexts

---

## 3. Template Security (Sandbox Violations)

### Current State (v5.1.0)

**Critical Security Issues**:

#### 3.1 Disabled Autoescape

**Location**: `crates/ggen-core/src/tera_env.rs:71-72`

```rust
// ❌ SECURITY RISK: Autoescape disabled globally
tera.autoescape_on(vec![]);  // Allows XSS in generated code
```

**Impact**: Templates can inject arbitrary content without escaping, including:
- Malicious code in generated files
- Command injection via shell hooks
- Path traversal attacks

#### 3.2 Unrestricted Filesystem Access

**Location**: `crates/ggen-domain/src/template/render_with_rdf.rs:257, 270`

```rust
// ❌ SECURITY RISK: Unrestricted file writes
std::fs::write(file_path, content).map_err(|e| { ... })?;

// ❌ No validation of file path (path traversal possible)
let rdf_path = template_path.parent().unwrap_or_else(|| Path::new(".")).join(&rendered_path);
let ttl_content = std::fs::read_to_string(&rdf_path)?;
```

**Attack Vector**:
```yaml
---
to: "../../../etc/passwd"  # ❌ Path traversal
---
Malicious content
```

#### 3.3 Shell Hook Execution

**Location**: `crates/ggen-core/src/template.rs:64-67`

```rust
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Frontmatter {
    // ❌ CRITICAL: Arbitrary shell command execution
    pub sh_before: Option<String>,
    pub sh_after: Option<String>,
    // ...
}
```

**Attack Vector**:
```yaml
---
to: "output.rs"
sh_before: "rm -rf /"  # ❌ CRITICAL: Unrestricted shell access
---
```

### Recommended v6 Breaking Changes

**BREAKING CHANGE 3.1**: Sandbox template filesystem access

```rust
// NEW: Sandboxed filesystem operations
pub struct TemplateSandbox {
    /// Allowed output directory (cannot write outside)
    allowed_output_dir: PathBuf,

    /// Allowed RDF source directories (cannot read outside)
    allowed_rdf_dirs: Vec<PathBuf>,

    /// Prohibited path patterns (e.g., "/etc", ".ssh")
    prohibited_patterns: Vec<Regex>,
}

impl TemplateSandbox {
    pub fn validate_output_path(&self, path: &Path) -> Result<()> {
        // ✅ Prevent path traversal
        let canonical = path.canonicalize()
            .map_err(|_| Error::new("Invalid output path"))?;

        if !canonical.starts_with(&self.allowed_output_dir) {
            return Err(Error::new(&format!(
                "Path traversal detected: {} is outside allowed directory {}",
                canonical.display(),
                self.allowed_output_dir.display()
            )));
        }

        // ✅ Check prohibited patterns
        for pattern in &self.prohibited_patterns {
            if pattern.is_match(&canonical.to_string_lossy()) {
                return Err(Error::new("Prohibited path pattern"));
            }
        }

        Ok(())
    }
}
```

**BREAKING CHANGE 3.2**: Remove shell hooks, replace with validated hooks

```rust
// ❌ REMOVE: Arbitrary shell execution
// pub sh_before: Option<String>,
// pub sh_after: Option<String>,

// ✅ ADD: Type-safe validated hooks
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TemplateHook {
    /// Format generated code (safe: no arbitrary commands)
    Format {
        formatter: FormatterType  // Enum: Rustfmt | Prettier | Black
    },

    /// Validate generated code (safe: read-only)
    Validate {
        validator: ValidatorType  // Enum: Clippy | ESLint | Ruff
    },

    /// Copy resource files (safe: sandboxed paths)
    CopyResources {
        from: PathBuf,  // Validated against sandbox
        to: PathBuf,    // Validated against sandbox
    },
}

impl Frontmatter {
    pub hooks_before: Vec<TemplateHook>,  // ✅ Type-safe hooks
    pub hooks_after: Vec<TemplateHook>,
}
```

**BREAKING CHANGE 3.3**: Enable autoescape by default, opt-out per template

```rust
// NEW: Autoescape enabled by default
impl TemplateEngine {
    pub fn new() -> Self {
        let mut tera = Tera::default();

        // ✅ Enable autoescape by default (HTML, XML, etc.)
        tera.autoescape_on(vec![".html", ".xml", ".svg"]);

        // Code generation templates opt-out explicitly
        Self { tera }
    }
}

// Template frontmatter:
// ---
// to: "output.rs"
// autoescape: false  # ✅ Explicit opt-out for code generation
// ---
```

**Migration Path**:
1. Phase 1 (v6.0): Add `TemplateSandbox` (opt-in, default disabled for backward compat)
2. Phase 2 (v6.1): Enable sandbox by default, deprecate `sh_before/sh_after` with warnings
3. Phase 3 (v7.0): Remove shell hooks entirely, require sandboxing

---

## 4. Template Composition Patterns

### Current State (v5.1.0)

**Location**: `crates/ggen-core/src/template.rs:413-444`

```rust
pub fn render(&self, tera: &mut Tera, vars: &Context) -> Result<String> {
    // ❌ No template composition support (each template isolated)
    // ❌ No shared partials/macros system
    // ❌ No template inheritance

    Ok(tera.render_str(&body_source, &final_vars)?)
}
```

**Problems**:
- ❌ No DRY (Don't Repeat Yourself) support across templates
- ❌ Difficult to share common patterns (headers, imports, etc.)
- ❌ No template inheritance (base templates + overrides)

### Recommended v6 Breaking Changes

**BREAKING CHANGE 4.1**: Add template composition support

```rust
// NEW: Template composition system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Frontmatter {
    // ...existing fields...

    /// Base template to extend (template inheritance)
    pub extends: Option<String>,

    /// Named blocks that can be overridden
    pub blocks: BTreeMap<String, String>,

    /// Partial templates to include
    pub partials: Vec<String>,

    /// Macros to import
    pub macros: Vec<String>,
}

// Example usage:
// base.tmpl:
// ---
// blocks:
//   header: "Default header"
//   content: ""
// ---
// {% block header %}{% endblock %}
// {% block content %}{% endblock %}

// child.tmpl:
// ---
// extends: "base.tmpl"
// blocks:
//   header: "Custom header"
//   content: "Custom content"
// ---
```

**BREAKING CHANGE 4.2**: Add macro/partial registration

```rust
// NEW: Macro and partial registration
impl Template {
    pub fn register_partials(&self, tera: &mut Tera, partials_dir: &Path) -> Result<()> {
        for partial in &self.front.partials {
            let partial_path = partials_dir.join(partial);
            let partial_content = std::fs::read_to_string(&partial_path)?;
            tera.add_raw_template(&partial, &partial_content)?;
        }
        Ok(())
    }

    pub fn register_macros(&self, tera: &mut Tera, macros_dir: &Path) -> Result<()> {
        for macro_file in &self.front.macros {
            let macro_path = macros_dir.join(macro_file);
            tera.add_template_file(&macro_path, Some(&macro_file))?;
        }
        Ok(())
    }
}
```

---

## 5. Error Reporting in Templates

### Current State (v5.1.0)

**Location**: `crates/ggen-core/src/template.rs:142`

```rust
.map_err(|e| Error::new(&format!("Failed to parse template frontmatter: {}", e)))?;
```

**Problems**:
- ❌ Generic error messages without line/column information
- ❌ No syntax highlighting of error location
- ❌ No suggestions for fixes
- ❌ Errors only at runtime (no compile-time detection)

### Recommended v6 Breaking Changes

**BREAKING CHANGE 5.1**: Rich error reporting with source location

```rust
// NEW: Rich error type with source location
#[derive(Debug, Clone)]
pub struct TemplateError {
    pub kind: TemplateErrorKind,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub source_excerpt: Option<String>,  // Lines around error
    pub suggestion: Option<String>,      // How to fix
}

impl std::fmt::Display for TemplateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Template error: {}", self.kind)?;

        if let (Some(line), Some(col)) = (self.line, self.column) {
            write!(f, " at line {}, column {}", line, col)?;
        }

        if let Some(excerpt) = &self.source_excerpt {
            write!(f, "\n\n{}", excerpt)?;
        }

        if let Some(suggestion) = &self.suggestion {
            write!(f, "\n\nSuggestion: {}", suggestion)?;
        }

        Ok(())
    }
}
```

**Example Output**:
```
Template error: Undefined variable 'usrname' at line 12, column 5

  10 | fn main() {
  11 |     println!("Hello, {{ name }}!");
  12 |     println!("User: {{ usrname }}");
     |                         ^^^^^^^ undefined variable
  13 | }

Suggestion: Did you mean 'username'? Available variables: name, email, version
```

**BREAKING CHANGE 5.2**: Pre-flight template validation

```rust
// NEW: Validate template before rendering
impl Template {
    pub fn validate(&self, available_vars: &[&str]) -> Result<Vec<TemplateWarning>> {
        let mut warnings = Vec::new();

        // ✅ Check for undefined variables
        let referenced_vars = self.extract_referenced_variables()?;
        for var in referenced_vars {
            if !available_vars.contains(&var.as_str()) {
                warnings.push(TemplateWarning {
                    kind: WarningKind::UndefinedVariable(var.clone()),
                    line: var.line,
                    suggestion: did_you_mean(&var.name, available_vars),
                });
            }
        }

        // ✅ Check for invalid SPARQL queries
        for (name, query) in &self.front.sparql {
            if let Err(e) = validate_sparql_syntax(query) {
                warnings.push(TemplateWarning {
                    kind: WarningKind::InvalidSparql(e),
                    line: None,
                    suggestion: Some("Check SPARQL syntax".into()),
                });
            }
        }

        Ok(warnings)
    }
}
```

---

## 6. Summary of Breaking Changes

### High Priority (v6.0 Release Blockers)

| ID | Breaking Change | Impact | Migration Effort |
|----|-----------------|--------|------------------|
| 3.1 | Template filesystem sandboxing | HIGH | Medium (2-3 days) |
| 3.2 | Remove shell hooks (`sh_before/sh_after`) | HIGH | Medium (refactor to safe hooks) |
| 3.3 | Enable autoescape by default | MEDIUM | Low (opt-out per template) |
| 2.1 | Type-safe template contexts | HIGH | High (4-5 days, all templates) |

### Medium Priority (v6.1 Release)

| ID | Breaking Change | Impact | Migration Effort |
|----|-----------------|--------|------------------|
| 1.1 | Compile-time template validation macro | MEDIUM | Medium (3-4 days) |
| 4.1 | Template composition (extends/blocks) | LOW | Low (opt-in feature) |
| 5.1 | Rich error reporting | LOW | Medium (error handling refactor) |

### Low Priority (v7.0 Future)

| ID | Breaking Change | Impact | Migration Effort |
|----|-----------------|--------|------------------|
| 1.2 | Build-time template compiler | LOW | High (new proc macro crate) |
| 2.2 | `#[derive(TemplateContext)]` macro | LOW | High (proc macro + docs) |
| 2.3 | Full typed template system | MEDIUM | Very High (1-2 weeks) |

---

## 7. Immediate Action Items (v6.0 Checklist)

### Week 1: Security Hardening
- [ ] Implement `TemplateSandbox` (BREAKING CHANGE 3.1)
- [ ] Add path traversal validation
- [ ] Add prohibited pattern checks
- [ ] Write security tests (path traversal, command injection)

### Week 2: Remove Shell Hooks
- [ ] Remove `sh_before/sh_after` from `Frontmatter` (BREAKING CHANGE 3.2)
- [ ] Implement safe `TemplateHook` enum
- [ ] Migrate existing templates to safe hooks
- [ ] Update documentation

### Week 3: Type-Safe Contexts (Phase 1)
- [ ] Add `TypedTemplate<C>` wrapper (BREAKING CHANGE 2.1)
- [ ] Add `TemplateContext` trait
- [ ] Convert 5 core templates to typed contexts (proof of concept)
- [ ] Document migration guide

### Week 4: Error Reporting
- [ ] Implement `TemplateError` with source location (BREAKING CHANGE 5.1)
- [ ] Add line/column tracking to parser
- [ ] Add syntax highlighting for errors
- [ ] Add "did you mean" suggestions

---

## 8. Migration Guide Template

For each breaking change, we'll provide:

```markdown
## Breaking Change: [ID] [Name]

### What Changed
- Old behavior: ...
- New behavior: ...

### Why It Changed
- Security: ...
- Type safety: ...
- Performance: ...

### Migration Steps
1. Step 1: ...
2. Step 2: ...

### Before/After Examples
**Before (v5.1.0)**:
```rust
// Old code
```

**After (v6.0.0)**:
```rust
// New code
```

### Automated Migration Tool
```bash
# Run migration script
ggen migrate v6 --dry-run  # Preview changes
ggen migrate v6 --apply    # Apply changes
```
```

---

## 9. Testing Strategy

### Security Tests (Priority: CRITICAL)
- [ ] Path traversal attack tests
- [ ] Command injection tests
- [ ] Autoescape bypass tests
- [ ] Sandbox escape attempts

### Type Safety Tests (Priority: HIGH)
- [ ] Compile-time context validation tests
- [ ] Missing variable detection tests
- [ ] Type mismatch detection tests

### Performance Tests (Priority: MEDIUM)
- [ ] Compile-time validation overhead
- [ ] Runtime performance (vs v5.1.0 baseline)
- [ ] Cache effectiveness

---

## 10. Files Requiring Changes

### Core Template System
- ✅ `crates/ggen-core/src/template.rs` (main template impl)
- ✅ `crates/ggen-core/src/templates/context.rs` (context types)
- ✅ `crates/ggen-core/src/tera_env.rs` (Tera configuration)
- ✅ `crates/ggen-core/src/template_cache.rs` (caching system)

### Domain Layer
- ✅ `crates/ggen-domain/src/template/mod.rs` (template service)
- ✅ `crates/ggen-domain/src/template/generate.rs` (generation logic)
- ✅ `crates/ggen-domain/src/template/render_with_rdf.rs` (RDF integration)
- ✅ `crates/ggen-domain/src/template/lint.rs` (template validation)

### New Files Required
- ❌ `crates/ggen-macros/src/template_context.rs` (NEW: derive macro)
- ❌ `crates/ggen-macros/src/template_compiler.rs` (NEW: compile-time validation)
- ❌ `crates/ggen-core/src/sandbox.rs` (NEW: filesystem sandboxing)
- ❌ `crates/ggen-core/src/template_errors.rs` (NEW: rich error types)

### Documentation Updates
- ❌ `docs/templates/v6-migration-guide.md` (NEW)
- ❌ `docs/templates/security.md` (NEW)
- ❌ `docs/templates/type-safe-contexts.md` (NEW)

---

## Conclusion

The template system requires **4 critical breaking changes** for v6 production release:

1. **Template Sandboxing** (security-critical)
2. **Remove Shell Hooks** (security-critical)
3. **Type-Safe Contexts** (type safety)
4. **Rich Error Reporting** (DX improvement)

**Estimated Timeline**: 4 weeks for v6.0 breaking changes
**Migration Impact**: HIGH - Affects all template users
**Security Impact**: CRITICAL - Prevents path traversal and command injection

**Next Steps**:
1. Create GitHub issues for each breaking change
2. Implement security fixes (Week 1-2)
3. Implement type safety (Week 3)
4. Implement error reporting (Week 4)
5. Write comprehensive migration guide
