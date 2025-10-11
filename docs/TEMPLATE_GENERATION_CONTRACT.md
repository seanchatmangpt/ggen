# Template Generation System - Functional Contract Documentation

## Purpose

This document defines what the template generation system **SHOULD DO**, not what it currently does. Use this as the authoritative specification for refactoring, testing, and validating behavior.

---

## System Overview

### What This System SHOULD DO

The template generation system SHOULD:

1. **Accept natural language** descriptions from users
2. **Generate ggen templates** with valid frontmatter and body content
3. **Parse generated content** using gray-matter (via ggen-core)
4. **Handle flexible input** from LLMs that may vary in format
5. **Validate outputs** against ggen template specifications
6. **Provide clear errors** when generation fails

### What This System SHOULD NOT DO

The system SHOULD NOT:

1. Reject valid YAML frontmatter due to strict parsing
2. Require specific field types (e.g., vars must be map-only)
3. Fail silently - always provide actionable error messages
4. Generate invalid ggen templates
5. Lose user data during parsing/transformation

---

## Component Contracts

### 1. TemplateGenerator (`ggen-ai/src/generators/template.rs`)

#### Purpose
Transform natural language descriptions into ggen templates using LLMs.

#### Public API Contract

##### `generate_template(description: &str, examples: Vec<&str>) -> Result<Template>`

**SHOULD DO**:
1. Send description + examples to configured LLM provider
2. Extract template content from LLM response (handle markdown, code blocks, etc.)
3. Parse extracted content using ggen-core's Template::parse()
4. Return fully-parsed Template with frontmatter populated
5. Provide clear errors if LLM output is unparseable

**SHOULD NOT**:
1. Crash on unexpected LLM output formats
2. Require specific YAML structure from LLM
3. Lose template body content during parsing
4. Return partially-initialized Template

**Error Handling**:
- MUST wrap parse errors with context about what failed
- MUST preserve original LLM output in error messages for debugging
- SHOULD suggest fixes when common parsing errors occur

**Example Valid Inputs**:
```rust
// Simple description
generate_template("Create a REST API controller", vec![])

// With examples
generate_template(
    "User authentication service",
    vec!["Include login/logout", "Use JWT tokens"]
)
```

**Example Valid Outputs**:
```yaml
---
to: "{{name}}_controller.rs"
vars:
  entity: "User"
  operations: ["create", "read", "update", "delete"]
---
pub struct {{entity}}Controller {
    // Implementation
}
```

##### `parse_template(content: &str) -> Result<Template>`

**SHOULD DO**:
1. Handle LLM output in multiple formats:
   - Direct YAML frontmatter (`---\nkey: value\n---\nbody`)
   - Markdown code blocks (` ```yaml\n...\n``` `)
   - Plain text with minimal frontmatter
   - Raw template body (wrap in default frontmatter)
2. Extract clean template content from wrapper markup
3. Delegate to ggen-core::Template::parse() for actual parsing
4. Render frontmatter to populate Template.front field
5. Return fully-initialized Template ready for use

**SHOULD NOT**:
1. Reject templates with non-standard frontmatter fields
2. Fail if vars is array instead of map (should auto-convert)
3. Require specific YAML formatting
4. Modify template body content

**Transformation Rules**:
```rust
// Rule 1: Extract from markdown
Input:  "```yaml\n---\nto: file.rs\n---\nbody\n```"
Output: "---\nto: file.rs\n---\nbody"

// Rule 2: Handle missing frontmatter
Input:  "Just template body"
Output: "---\nto: \"generated.tmpl\"\nvars:\n  name: \"example\"\n---\nJust template body"

// Rule 3: Preserve complete templates
Input:  "---\nto: file.rs\n---\nbody"
Output: "---\nto: file.rs\n---\nbody" (unchanged)
```

---

### 2. Template (`ggen-core/src/template.rs`)

#### Purpose
Core template representation with frontmatter parsing using gray-matter.

#### Data Structure Contract

##### `Frontmatter` Struct

**SHOULD ACCEPT**:
- Any valid YAML frontmatter
- Flexible field types (string, array, map, etc.)
- Unknown fields (forward compatibility)
- Missing optional fields (use defaults)

**SHOULD PROVIDE**:
- Type-safe access to known fields
- Default values for optional fields
- Custom deserializers for flexible fields

**Field Contracts**:

###### `vars: BTreeMap<String, serde_yaml::Value>`

**CURRENT PROBLEM**: Only accepts maps, rejects arrays
```yaml
# This CRASHES currently:
vars:
  - item1
  - item2
```

**SHOULD ACCEPT**:
```yaml
# Map format (preferred)
vars:
  name: "value"
  count: 42

# Array format (auto-convert)
vars:
  - "item1"
  - "item2"
# Should become: {var0: "item1", var1: "item2"}

# Single value (auto-convert)
vars: "single_value"
# Should become: {var0: "single_value"}

# Nested structures
vars:
  config:
    nested: true
  list:
    - a
    - b
```

**Implementation Contract**:
```rust
/// SHOULD DO: Accept any YAML value type
#[serde(default, deserialize_with = "deserialize_flexible_vars")]
pub vars: BTreeMap<String, serde_yaml::Value>,

fn deserialize_flexible_vars<'de, D>(
    deserializer: D
) -> Result<BTreeMap<String, serde_yaml::Value>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    // Accept: Map, Array, String, Number, Boolean, Null
    // Convert non-maps to indexed maps: {var0: val0, var1: val1, ...}
    // Preserve maps as-is
}
```

##### `Template::parse(input: &str) -> Result<Self>`

**SHOULD DO**:
1. Use gray-matter's Matter<YAML>::new() for parsing
2. Split frontmatter from body correctly
3. Store raw frontmatter for later rendering
4. Handle templates with no frontmatter (return default)
5. Handle empty frontmatter (return default)
6. Preserve exact body content (no trimming/modification)

**SHOULD NOT**:
1. Crash on malformed YAML (return descriptive error)
2. Modify whitespace in template body
3. Require frontmatter to be present
4. Lose data during parsing

**Error Messages SHOULD**:
```rust
// Good error example:
"Failed to parse template frontmatter at line 5:
 vars: invalid type: sequence, expected a map

 Suggestion: Change 'vars' to a map format:
 vars:
   key: value"

// Bad error example (current):
"vars: invalid type: sequence, expected a map at line 5 column 1"
```

##### `Template::render_frontmatter(&mut self, tera: &mut Tera, vars: &Context) -> Result<()>`

**SHOULD DO**:
1. Serialize raw_frontmatter to YAML string
2. Render YAML through Tera to resolve `{{ }}` variables
3. Deserialize rendered YAML into Frontmatter struct
4. Populate self.front with result
5. Handle deserialization errors gracefully

**SHOULD NOT**:
1. Fail if vars contains unexpected types
2. Crash on circular template references
3. Modify raw_frontmatter (keep immutable)
4. Skip rendering (must always render before use)

**Invariants**:
- MUST call render_frontmatter() before accessing self.front fields
- MUST NOT call render_frontmatter() twice (idempotent)
- MUST preserve template body during frontmatter rendering

---

### 3. Frontmatter CLI Command (`cli/src/cmds/ai/frontmatter.rs`)

#### Purpose
Generate blog/doc frontmatter using AI with SEO optimization.

#### Functional Contract

##### `run(args: &FrontmatterArgs) -> Result<()>`

**SHOULD DO**:
1. Accept natural language description of frontmatter needs
2. Generate structured YAML frontmatter via LLM
3. Serialize to valid YAML format (NOT Debug format)
4. Save to file or print to stdout
5. Validate output is valid YAML before returning

**SHOULD NOT**:
1. Output Rust Debug format (`Frontmatter { to: Some(...) }`)
2. Generate invalid YAML
3. Miss required SEO fields when requested
4. Crash on LLM output variations

**Current Bug**:
```rust
// WRONG (current):
fs::write(output_path, format!("{:?}\n---\n{}", template.front, template.body))?;
// Outputs: Frontmatter { to: Some("file"), ... }

// RIGHT (should be):
let yaml = serde_yaml::to_string(&template.front)?;
fs::write(output_path, format!("---\n{}\n---\n{}", yaml.trim(), template.body))?;
// Outputs:
// ---
// to: file
// vars:
//   name: value
// ---
// body content
```

**Output Format Contract**:
```yaml
# MUST output Jekyll/Hugo compatible frontmatter:
---
title: "Blog Post Title"
description: "SEO-optimized description"
date: 2025-01-15
author: "Author Name"
tags:
  - ai
  - ethics
keywords: "AI, ethics, technology"
image: "/images/featured.jpg"
---

Blog post content starts here...
```

**Validation**:
- MUST be parseable by Jekyll/Hugo/gray-matter
- MUST include title and description at minimum
- SHOULD include SEO fields when requested
- SHOULD validate YAML syntax before writing

---

### 4. Generate Command (`cli/src/cmds/ai/generate.rs`)

#### Purpose
Generate arbitrary templates/code from descriptions.

#### Functional Contract

**SHOULD DO**:
1. Accept any template description
2. Generate complete, usable ggen templates
3. Handle various output formats (code, config, docs, etc.)
4. Support iterative validation/improvement
5. Write valid ggen templates to disk

**SHOULD NOT**:
1. Crash on YAML parsing errors
2. Generate invalid ggen templates
3. Lose template content during processing
4. Skip validation when requested

**Validation Mode Contract**:
```rust
// When --validate flag is used:
// SHOULD DO:
1. Generate initial template
2. Validate with TemplateValidator
3. If issues found, regenerate with feedback
4. Repeat up to --max-iterations times
5. Return best template or fail with remaining issues

// SHOULD NOT:
1. Return invalid template as "good enough"
2. Ignore validation errors
3. Infinite loop on unfixable issues
```

---

## Integration Contract

### ggen-ai <-> ggen-core Integration

**SHOULD DO**:
1. ggen-ai generates string content via LLM
2. ggen-ai extracts/cleans content (remove markdown, etc.)
3. ggen-ai calls ggen-core::Template::parse() for parsing
4. ggen-ai calls Template::render_frontmatter() to populate fields
5. ggen-ai returns fully-initialized Template to user

**Data Flow**:
```
User Description
    ↓
LLM (OpenAI/Anthropic/Ollama)
    ↓
Raw Text Output (may have markdown, code blocks)
    ↓
TemplateGenerator::parse_template() [ggen-ai]
  - Extract clean template text
    ↓
Template::parse() [ggen-core]
  - Use gray-matter to split frontmatter/body
    ↓
Template::render_frontmatter() [ggen-core]
  - Render {{vars}} in frontmatter
  - Deserialize to Frontmatter struct
    ↓
Fully-Initialized Template
    ↓
User receives usable template
```

**SHOULD NOT**:
1. Parse YAML directly in ggen-ai (use ggen-core)
2. Duplicate gray-matter functionality
3. Have inconsistent parsing between commands
4. Skip render_frontmatter step

---

## Error Handling Contract

### Error Classification

#### User Errors (Recoverable)
```rust
// SHOULD provide helpful error messages:
Error::InvalidDescription("Description too vague. Try: 'Create REST API for user management'")
Error::ParseFailed("Generated template has invalid YAML. Retrying with stricter prompt...")
```

#### LLM Errors (Retryable)
```rust
// SHOULD retry with modified prompts:
Error::LlmOutputInvalid("LLM generated non-template content. Retry 1/3...")
```

#### System Errors (Fatal)
```rust
// SHOULD fail fast with context:
Error::ApiKeyMissing("OPENAI_API_KEY not set. See: docs/setup.md")
Error::NetworkFailed("Failed to reach Ollama at localhost:11434")
```

### Error Message Requirements

**MUST include**:
1. What went wrong (specific error)
2. Where it happened (file, line, field)
3. Why it happened (context)
4. How to fix it (suggestion)

**Example**:
```
❌ Template Generation Failed

What: Invalid YAML in frontmatter
Where: line 5, column 1, field 'vars'
Why: LLM generated vars as array, but map expected
Fix: Add custom deserializer to accept both formats

Original output:
---
vars:
  - item1
  - item2
---

See: docs/TEMPLATE_GENERATION_CONTRACT.md#vars-field
```

---

## Test Contract

### What Tests SHOULD Verify

#### Unit Tests
```rust
#[test]
fn should_accept_vars_as_map() { ... }

#[test]
fn should_accept_vars_as_array() { ... }

#[test]
fn should_accept_vars_as_string() { ... }

#[test]
fn should_handle_markdown_wrapped_templates() { ... }

#[test]
fn should_handle_missing_frontmatter() { ... }

#[test]
fn should_preserve_template_body_exactly() { ... }
```

#### Integration Tests
```rust
#[tokio::test]
async fn should_generate_valid_template_from_description() { ... }

#[tokio::test]
async fn should_output_valid_yaml_frontmatter() { ... }

#[tokio::test]
async fn should_handle_ollama_output_variations() { ... }
```

#### Property Tests
```rust
#[quickcheck]
fn should_parse_any_valid_yaml_frontmatter(yaml: ValidYaml) { ... }

#[quickcheck]
fn should_preserve_body_through_parse_render_cycle(body: String) { ... }
```

---

## Performance Contract

### Latency Requirements

**Template Generation**:
- Cold start: < 10 seconds (LLM call)
- Warm start: < 5 seconds (cached)
- Parsing: < 100ms
- Validation: < 200ms

**Memory Usage**:
- Template parsing: < 10 MB per template
- LLM cache: Configurable (default 100 MB)

---

## Compatibility Contract

### Input Compatibility

**MUST accept**:
- Hygen-style frontmatter
- Jekyll/Hugo-style frontmatter
- Custom ggen frontmatter extensions
- Minimal frontmatter (just `to:` field)
- No frontmatter (pure template body)

### Output Compatibility

**MUST generate**:
- Valid ggen templates
- Hygen-compatible templates (subset)
- Templates loadable by Template::parse()
- Templates usable by ggen's template engine

---

## Refactoring Guide

When refactoring this system:

1. **Read this contract first** - Understand what SHOULD happen
2. **Check current behavior** - See what DOES happen
3. **Identify gaps** - What's missing or broken?
4. **Write tests** - Cover contract requirements
5. **Implement changes** - Make code match contract
6. **Validate** - Run full test suite + manual validation
7. **Update docs** - Keep this contract current

### Migration Checklist

- [ ] Fix vars field to accept any YAML value
- [ ] Add custom deserializer for flexible vars
- [ ] Fix frontmatter CLI to output YAML not Debug
- [ ] Add validation for generated YAML
- [ ] Improve error messages with suggestions
- [ ] Add tests for all supported input formats
- [ ] Document breaking changes
- [ ] Update examples in docs

---

## Questions & Answers

**Q: Why accept vars as array if maps are standard?**
A: LLMs may generate arrays. Rather than fail, auto-convert to provide better UX. Always be lenient in what you accept.

**Q: Why use gray-matter instead of custom parser?**
A: Industry standard, battle-tested, handles edge cases. Don't reinvent wheels.

**Q: Should we validate LLM output before parsing?**
A: Yes, basic validation. But also handle invalid gracefully with helpful errors.

**Q: What if user provides invalid description?**
A: Prompt LLM to improve description, or ask user for clarification. Never silently fail.

---

**Document Version**: 1.0.0
**Last Updated**: 2025-10-11
**Maintained By**: Core Team
**Authority Level**: REQUIRED - This is the specification, not guidelines
