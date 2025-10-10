# Recipe Template Format

This template should be used for ALL cookbook recipes to ensure consistency and quality.

---

## Recipe X.Y: [Action-Oriented Title]

**Difficulty**: ⭐ Beginner | ⭐⭐ Intermediate | ⭐⭐⭐ Advanced
**Time**: 5min | 15min | 30min | 1hr
**Tags**: #tag1 #tag2 #tag3

### Problem
[What user problem does this solve? Be specific and relatable. 1-2 sentences.]

Example:
> You need to generate a Rust module with consistent structure, but manually creating the boilerplate is tedious and error-prone.

### Solution
[High-level approach to solving the problem. 2-3 sentences max.]

Example:
> Use ggen's template system to define the module structure once, then generate new modules with different names using command-line variables. The template ensures consistency and saves time.

### Prerequisites
- [Required knowledge or setup]
- [Dependencies needed]

Example:
- ggen installed (see Recipe 1.2)
- Basic understanding of YAML (see Recipe 1.4)
- Rust project structure knowledge

### Step-by-Step

#### 1. [First Action]
```bash
# Command with explanation
ggen command --option value
```

[Brief explanation of what happens in this step]

#### 2. [Second Action]
[Explanation of what to do next]

```yaml
# Template code with inline comments
---
frontmatter: here  # ← Explanation
---
template body
```

#### 3. [Third Action]
[Expected result or next action]

### Complete Example

**Input**: `rust-module.tmpl`
```yaml
---
to: "src/modules/{{name}}.rs"
vars:
  name: "example"
  author: "ggen"
---
//! {{name}} module
//! Author: {{author}}

pub struct {{name | capitalize}} {
    // Implementation
}

impl {{name | capitalize}} {
    pub fn new() -> Self {
        Self {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{name}}_creation() {
        let instance = {{name | capitalize}}::new();
        assert!(true);
    }
}
```

**Command**:
```bash
ggen gen rust-module.tmpl --vars name=user_auth author="Jane Doe"
```

**Output**: `src/modules/user_auth.rs`
```rust
//! user_auth module
//! Author: Jane Doe

pub struct UserAuth {
    // Implementation
}

impl UserAuth {
    pub fn new() -> Self {
        Self {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_auth_creation() {
        let instance = UserAuth::new();
        assert!(true);
    }
}
```

### Explanation
1. **Frontmatter (`to` field)**: Specifies output path with `{{name}}` variable for dynamic naming
2. **Variables section**: Defines default values that can be overridden via CLI
3. **Tera filters**: `capitalize` filter transforms `user_auth` to `UserAuth` for struct name
4. **Template body**: Uses variable substitution and filters throughout
5. **Result**: Generated file with all variables replaced and proper casing applied

### Expected Output
```
✅ Generated: src/modules/user_auth.rs
📄 Size: 342 bytes
🔒 Deterministic: true
⏱️  Time: 23ms
```

### Verification
```bash
# Verify the file was created
ls -lh src/modules/user_auth.rs

# Check the contents
cat src/modules/user_auth.rs

# Verify it compiles (for Rust)
cargo check --quiet
```

### Common Pitfalls

❌ **Mistake**: Forgetting to use the `capitalize` filter for struct names
- **Symptom**: Compiler error "expected type name, found `user_auth`"
- **Fix**: Add `| capitalize` filter: `{{name | capitalize}}`

❌ **Mistake**: Missing frontmatter separator (`---`)
- **Symptom**: Template error "Failed to parse frontmatter"
- **Fix**: Ensure three dashes (`---`) on their own lines before and after frontmatter

❌ **Mistake**: Using spaces in variable names
- **Symptom**: Template error "Invalid variable name"
- **Fix**: Use underscores or camelCase: `user_name` or `userName`

### Variations

💡 **Tip**: Add determinism for reproducible generation
```yaml
---
determinism: 42  # Add this to frontmatter
---
```

💡 **Tip**: Generate multiple related modules at once
```bash
# Create a loop script
for module in auth users posts; do
  ggen gen rust-module.tmpl --vars name=$module
done
```

💡 **Tip**: Use environment variables for author
```bash
export GGEN_AUTHOR="Your Name"
ggen gen rust-module.tmpl --vars author="$GGEN_AUTHOR"
```

### Troubleshooting

**Issue**: "Template not found" error
**Cause**: ggen can't locate `rust-module.tmpl`
**Solution**: Use absolute path or place in project templates directory:
```bash
ggen gen /full/path/to/rust-module.tmpl
# OR
mkdir -p .ggen/templates
mv rust-module.tmpl .ggen/templates/
ggen gen rust-module.tmpl
```

**Issue**: Output directory doesn't exist
**Cause**: `src/modules/` directory hasn't been created
**Solution**: Create directory first or update template to use existing path:
```bash
mkdir -p src/modules
# OR modify template's 'to' field to existing directory
```

**Issue**: Variables not substituting
**Cause**: Incorrect syntax for variable references
**Solution**: Ensure variables use double curly braces: `{{variable}}` not `{variable}`

### See Also
- Recipe 1.4: Understanding Template Structure - Template anatomy basics
- Recipe 2.3: Add Variables to Templates - Variable management in depth
- Recipe 3.5: Custom Filters - Complete Tera filter reference
- Recipe 6.1: Fixed Seeds for RNG - Deterministic generation details
- Recipe 13.1: Template Unit Testing - How to test your templates

### Next Steps
- Try generating modules with different names and authors
- Extend the template to include additional boilerplate (traits, constants, etc.)
- Create a template pack with multiple related templates (see Chapter 7)
- Add RDF metadata to track module relationships (see Chapter 4)
- Set up automated testing for generated modules (see Chapter 13)

---

## Template Checklist

Before publishing a recipe, verify:

- [ ] Recipe follows the standard structure
- [ ] Problem statement is clear and relatable
- [ ] Solution is concise (2-3 sentences)
- [ ] Code examples are complete and runnable
- [ ] All commands have been tested
- [ ] Expected output matches actual output
- [ ] At least 2 common pitfalls documented
- [ ] Minimum 3 "See Also" references
- [ ] Next steps provide clear progression
- [ ] Difficulty and time estimates are accurate
- [ ] Tags are relevant and searchable
- [ ] Emoji usage is consistent with style guide
- [ ] Code blocks have proper syntax highlighting
- [ ] Inline comments explain non-obvious parts
- [ ] Troubleshooting section covers common errors

---

## Metadata Guidelines

### Difficulty Levels
- **⭐ Beginner**: No prior ggen knowledge required, basic concepts only
- **⭐⭐ Intermediate**: Assumes familiarity with templates and basic features
- **⭐⭐⭐ Advanced**: Requires deep understanding of RDF, SPARQL, or complex workflows

### Time Estimates
- **5min**: Quick operations, single command, simple templates
- **15min**: Multiple steps, template creation, basic testing
- **30min**: Complex templates, RDF/SPARQL, multiple files
- **1hr**: Complete workflows, marketplace publishing, advanced integration

### Tag Categories
- **Feature**: #templates #rdf #sparql #determinism #injection #marketplace
- **Language**: #rust #python #typescript #go #sql
- **Workflow**: #testing #ci-cd #github #deployment #migration
- **Level**: #beginner #intermediate #advanced
- **Type**: #quickstart #reference #troubleshooting #integration

### See Also References
Format: `Recipe X.Y: [Title] - [Brief context]`

Good examples:
- ✅ Recipe 2.3: Add Variables to Templates - Learn about variable precedence
- ✅ Recipe 4.6: Variable Extraction from SPARQL - Query RDF for dynamic values
- ✅ Appendix B: Tera Filter Reference - Complete filter documentation

Bad examples:
- ❌ Recipe 2.3 (no context)
- ❌ See the variables chapter (not specific)
- ❌ Read about SPARQL (external link, not recipe reference)

---

## Visual Style Guide

### Emoji Usage
Use emojis consistently to improve scannability:

**Status Indicators**:
- ✅ Success, correct approach, valid
- ❌ Error, incorrect approach, invalid
- ⚠️ Warning, caution, important note
- 💡 Tip, insight, helpful information

**Content Types**:
- 📄 File, document, output
- 📦 Package, module, component
- 🔗 Link, reference, relationship
- 🔒 Security, determinism, immutability

**Actions**:
- 🚀 Performance, speed, optimization
- 🧪 Testing, experimental, validation
- 🎯 Goal, objective, target
- 🔍 Search, discovery, investigation

**Progress**:
- ⭐ Rating, difficulty level
- ⏱️ Time, duration
- 📊 Metrics, statistics
- 🔄 Process, workflow, iteration

### Code Block Formatting

**Always specify language**:
```bash  # ← Language identifier
ggen gen template.tmpl
```

**Use inline comments for complex code**:
```yaml
---
to: "src/{{name}}.rs"    # ← Dynamic output path
vars:
  name: "example"         # ← Default value
determinism: 42           # ← Fixed seed for reproducibility
---
```

**Annotate with arrows for clarity**:
```rust
pub struct UserAuth {
    //       ^^^^^^^^ ← Capitalized via Tera filter
    config: Config,
    //      ^^^^^^ ← Type defined elsewhere
}
```

### Section Headers

Use verb-based headers for action steps:
- ✅ "Creating the Template"
- ✅ "Running the Generation"
- ✅ "Verifying the Output"

Avoid noun-based headers:
- ❌ "Template Creation"
- ❌ "Generation Process"
- ❌ "Output Verification"

---

## Quality Assurance

### Testing Recipe Examples

Before publishing, test each recipe:

1. **Fresh environment**: Use clean directory or container
2. **Copy-paste test**: Ensure commands work verbatim
3. **Output verification**: Confirm expected output matches actual
4. **Error scenarios**: Verify common pitfalls are accurate
5. **Cross-references**: Check all "See Also" links are valid
6. **Version check**: Ensure commands work with current ggen version

### Validation Script

```bash
#!/bin/bash
# validate-recipe.sh - Test a recipe's commands

RECIPE_FILE=$1
TEMP_DIR=$(mktemp -d)

cd "$TEMP_DIR"
echo "Testing recipe in: $TEMP_DIR"

# Extract and run commands from recipe
grep -A1 "^\`\`\`bash" "$RECIPE_FILE" | \
  grep -v "^\`\`\`" | \
  grep -v "^--$" | \
  while read -r cmd; do
    echo "Running: $cmd"
    eval "$cmd" || echo "❌ Command failed: $cmd"
  done

echo "✅ Recipe validation complete"
cd - && rm -rf "$TEMP_DIR"
```

### Peer Review Checklist

Reviewers should verify:

- [ ] Recipe solves a real user problem
- [ ] Examples are practical, not contrived
- [ ] Code is idiomatic for the language
- [ ] Error messages match current ggen version
- [ ] Links and references are accurate
- [ ] Writing is clear and concise
- [ ] No typos or grammatical errors
- [ ] Formatting is consistent
- [ ] All commands execute successfully
- [ ] Recipe fits cookbook style and voice

---

This template ensures every recipe provides immediate value, clear guidance, and seamless navigation through the cookbook.
