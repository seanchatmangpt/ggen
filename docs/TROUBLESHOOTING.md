# Troubleshooting Guide

**Last Updated:** 2026-03-31
**Version:** 6.0.1
**Coverage:** 20 most common issues based on error analysis and improved error messages

## Quick Reference

| Issue Category | Quick Fix |
|---------------|-----------|
| **Build failures** | `cargo make check` → Check error code below |
| **Test failures** | `cargo make test` → See Test Failures section |
| **LLM/MCP issues** | Check API keys, verify OTEL spans |
| **Performance** | `cargo make slo-check` → See Performance section |
| **RDF/ontology** | `ggen validate <file>` → See RDF Issues section |

---

## 🔴 Critical Errors (Stop the Line)

### Error E0001: Invalid Merge Marker Order

**Symptom:**
```
error[E0001]: Invalid merge marker order
  --> GENERATED marker at line 3, ======= marker at line 1
```

**Cause:** Merge conflict markers in generated files are in wrong order.

**Solution:**
```bash
# 1. Check the merge marker order in the generated file
grep -n "<<<<<<<\|=======\|>>>>>>>" <generated_file>

# 2. Reorder markers to match this pattern:
#   <<<<<<< GENERATED
#   =======
#   >>>>>>> MANUAL

# 3. Regenerate from ontology
ggen sync --force
```

**Prevention:** Never edit generated files manually. Edit the `.ttl` source instead.

---

### Error E0002: Condition Query Must Return Boolean

**Symptom:**
```
error[E0002]: Condition query must return boolean (ASK), not results
  --> query used in WHEN condition
```

**Cause:** SPARQL condition query uses SELECT/CONSTRUCT instead of ASK.

**Solution:**
```sparql
# WRONG (SELECT):
SELECT ?x WHERE { ?x a :Type }

# RIGHT (ASK):
ASK { ?x a :Type }
```

**Reference:** See [SPARQL Query Types](#sparql-query-types) below.

---

### Error E0003: Generation Rule Query Type Error

**Symptom:**
```
error[E0003]: Generation rule query must return results (SELECT/CONSTRUCT), not boolean
  --> rule: 'my-rule', query type: ASK
```

**Cause:** Generation rule uses ASK query instead of SELECT/CONSTRUCT.

**Solution:**
```sparql
# WRONG (ASK):
ASK { ?x a :Type }

# RIGHT (SELECT):
SELECT ?x ?label WHERE {
  ?x a :Type .
  ?x rdfs:label ?label .
}

# RIGHT (CONSTRUCT for RDF structures):
CONSTRUCT {
  ?x a :GeneratedType .
  ?x rdfs:label ?label .
}
WHERE {
  ?x a :Type .
  ?x rdfs:label ?label .
}
```

---

### Error E0004: Empty Generated Content

**Symptom:**
```
error[E0004]: Generated content is empty
  --> rule: 'my-rule', query returned 0 results
```

**Cause:** SPARQL query returns no results.

**Solution:**
```bash
# 1. Test your query directly
ggen graph query --query <your_query.rq> --ontology ontology.ttl

# 2. Check if ontology has matching data
ggen graph stats --ontology ontology.ttl

# 3. Verify namespace prefixes in query match ontology
# Example: If ontology uses @prefix ex: <http://example.org/>
# Your query must also declare: PREFIX ex: <http://example.org/>

# 4. Check for typos in URIs or property names
```

**Debugging:**
```sparql
# Add this to your query to see all types:
SELECT DISTINCT ?type WHERE { ?s a ?type } LIMIT 10
```

---

### Error E0005: Generated File Too Large

**Symptom:**
```
error[E0005]: Generated file too large (11534336 bytes, limit: 10MB)
  --> rule: 'test_rule', output: 'large_output.txt'
```

**Cause:** SPARQL query returns too much data or template duplicates content.

**Solution:**
```bash
# 1. Add LIMIT to SPARQL query
SELECT ?x ?y WHERE { ... } LIMIT 1000

# 2. Split into multiple smaller files
# Edit ggen.toml to create multiple rules:
[[generation.rules]]
name = "part1"
query = { file = "queries/part1.rq" }
output_file = "generated/part1.rs"

[[generation.rules]]
name = "part2"
query = { file = "queries/part2.rq" }
output_file = "generated/part2.rs"

# 3. Check for unexpected data duplication
ggen graph query --query queries/duplicate_check.rq
```

---

### Error E0006: Directory Traversal Pattern Detected

**Symptom:**
```
error[E0006]: Directory traversal pattern detected in output path
  --> rule: 'test_rule', path: '../../../etc/passwd'
```

**Cause:** Template output path contains `..` (security violation).

**Solution:**
```toml
# WRONG (directory traversal):
output_file = "../../../etc/passwd"
output_file = "../config/secret.toml"

# RIGHT (relative from base directory):
output_file = "generated/config.rs"
output_file = "output/data.json"
```

**Security Note:** Directory traversal is blocked to prevent writing files outside the project directory.

---

### Error E0007: Watch System Stopped Unexpectedly

**Symptom:**
```
error[E0007]: Watch system stopped unexpectedly
```

**Cause:** Watch thread crashed or disconnected.

**Solution:**
```bash
# 1. Check logs for panic or error messages
RUST_LOG=trace ggen sync --watch 2>&1 | tee watch_debug.log

# 2. Restart watch mode
ggen sync --watch

# 3. If issue persists, run without --watch to debug
ggen sync

# 4. Check for file system issues
# - macOS: Check fsevents limits
# - Linux: Check inotify limits
```

**Prevention:** Avoid watching directories with thousands of files. Use `.gitignore` patterns to exclude generated files.

---

### Error E0008: Template Read Error

**Symptom:**
```
error[E0008]: Template not found or unreadable
  --> template: 'templates/my_template.tera'
```

**Cause:** Template file doesn't exist or has permission issues.

**Solution:**
```bash
# 1. Check template exists
ls -la templates/my_template.tera

# 2. Verify path in ggen.toml is correct
[[generation.rules]]
template = { file = "templates/my_template.tera" }  # Check this path

# 3. Check file permissions
chmod 644 templates/my_template.tera

# 4. Validate template syntax
ggen template lint my_template
```

---

### Error E0009: Watch Path Does Not Exist

**Symptom:**
```
error[E0009]: Watch path does not exist
  --> path: '/this/path/does/not/exist'
```

**Cause:** Ontology source or import path doesn't exist.

**Solution:**
```bash
# 1. Create missing directory
mkdir -p /this/path/does/not/exist

# 2. Or update ggen.toml to remove from watch list
[ontology]
source = "ontology.ttl"  # Check this path exists

imports = [
  "schema/base.ttl",     # Check these paths exist
  "vocab/extensions.ttl"
]

# 3. Verify all import paths
find . -name "*.ttl" -type f
```

**Where Watch Paths Come From:**
- `ontology.source` in `ggen.toml`
- `ontology.imports` array
- `generation.rules[].query` file paths

---

## 🟡 High Priority Issues

### Compiler Errors

**Symptom:** `error[E...]` during `cargo make check`

**Solution:**
```bash
# 1. Clean build cache
cargo clean

# 2. Check for Rust version mismatch
rustc --version  # Should be 1.91.1

# 3. Update dependencies if needed
cargo update

# 4. Re-run check
cargo make check
```

**Common Compiler Errors:**

| Error | Cause | Fix |
|-------|-------|-----|
| `E0433` | Import not found | Check `use` statements, verify module path |
| `E0599` | Method not found | Check trait imports, verify type |
| `E0277` | Trait not implemented | Add `use` for required trait |
| `E0382` | Use after move | Use references instead of owned values |

---

### Test Failures

**Symptom:** `test ... FAILED` during `cargo make test`

**Solution:**
```bash
# 1. Run with verbose output
cargo test -- --nocapture

# 2. Run specific test
cargo test test_name

# 3. Check for missing environment variables
echo $GROQ_API_KEY
echo $TEST_DATABASE_URL

# 4. Run tests with backtrace
RUST_BACKTRACE=1 cargo test
```

**Chicago TDD Test Failures:**

If tests fail because they use mocks (London TDD), convert to Chicago TDD:

```rust
// WRONG (London TDD - forbidden):
#[cfg(test)]
mock! {
    pub HttpClient {}
    impl HttpClient for MockHttpClient {
        fn get(&self, url: &str) -> Result<String>;
    }
}

// RIGHT (Chicago TDD - required):
let client = reqwest::Client::new();
let response = client.get("https://example.com").await?;
assert_eq!(response.status(), 200);
```

See [Testing Policy](#chicago-tdd-policy) below.

---

### Clippy Warnings

**Symptom:** `warning:` during `cargo make lint`

**Solution:**
```bash
# 1. Auto-fix simple issues
cargo clippy --fix --allow-dirty

# 2. Run lint again
cargo make lint

# 3. Fix remaining warnings manually
# Common warnings:
# - unused_variables: Prefix with `_` or remove
# - dead_code: Add `#[allow(dead_code)]` or remove
# - println!: Use logging instead
```

---

## 🟢 Common Issues

### Installation Issues

#### cargo install fails

**Symptom:** `error: failed to compile ggen-cli`

**Solution:**
```bash
# 1. Check Rust version
rustc --version  # Must be 1.91.1 or later

# 2. Update Rust if needed
rustup update

# 3. Install with features
cargo install ggen-cli --features all

# 4. If still failing, install from source
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo make install
```

---

#### ggen command not found

**Symptom:** `ggen: command not found`

**Solution:**
```bash
# 1. Check cargo bin directory is in PATH
echo $PATH | grep cargo

# 2. Add to PATH if missing
export PATH="$HOME/.cargo/bin:$PATH"

# 3. Make permanent (add to ~/.zshrc or ~/.bashrc)
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# 4. Verify installation
which ggen
ggen --version
```

---

### Build Performance Issues

#### Slow build times

**Symptom:** Build takes >15 seconds

**Solution:**
```bash
# 1. Check SLO compliance
cargo make slo-check

# 2. Use incremental builds (default in dev mode)
cargo check

# 3. Reduce dependencies
cargo tree --duplicates

# 4. Use cargo make for parallel builds
cargo make -j 8 check

# 5. Profile build to find bottlenecks
cargo build --timings
```

**Target SLOs:**
- First build: ≤15s
- Incremental build: ≤2s

---

#### High memory usage

**Symptom:** Build uses >100MB RAM

**Solution:**
```bash
# 1. Limit parallel jobs
CARGO_BUILD_JOBS=2 cargo build

# 2. Use release mode for faster builds
cargo build --release

# 3. Check for memory leaks
valgrind --leak-check=full cargo test
```

---

### LLM/MCP Issues

#### LLM API calls fail

**Symptom:** `Error: LLM API request failed`

**Solution:**
```bash
# 1. Check API key is set
echo $GROQ_API_KEY
echo $OPENAI_API_KEY

# 2. Test API key manually
curl -H "Authorization: Bearer $GROQ_API_KEY" \
  https://api.groq.com/openai/v1/models

# 3. Verify OTEL spans exist (required for LLM features)
RUST_LOG=trace,ggen_ai=trace cargo test -p ggen-cli-lib --test llm_e2e_test 2>&1 | grep llm

# Expected output:
# llm.complete request
# llm.model=groq::openai/gpt-oss-20b
# llm.total_tokens=770
```

**OTEL Validation Required:** LLM features must emit OTEL spans. If no spans appear, the feature is not working.

See [OTEL Validation](#opentelemetry-validation) below.

---

#### MCP server won't start

**Symptom:** `Error: MCP server failed to start`

**Solution:**
```bash
# 1. Check MCP configuration
ggen mcp validate-config

# 2. Initialize config if missing
ggen mcp init-config

# 3. Start with verbose output
RUST_LOG=trace ggen mcp start-server --transport stdio

# 4. Check port conflicts (HTTP transport)
lsof -i :8080  # Default HTTP port
```

---

#### MCP tools not available

**Symptom:** MCP client can't see ggen tools

**Solution:**
```bash
# 1. List available tools
ggen mcp list

# 2. Test specific tool
ggen mcp test validate_pipeline

# 3. Check server is running
ggen mcp status

# 4. Restart MCP server
ggen mcp start-server --transport stdio
```

---

### RDF/Ontology Issues

#### SHACL validation fails

**Symptom:** `Error: SHACL validation failed`

**Solution:**
```bash
# 1. Validate ontology
ggen validate ontology.ttl

# 2. Check for common SHACL violations
# - Missing required properties
# - Invalid data types
# - Constraint violations

# 3. Validate with detailed output
ggen validate --verbose ontology.ttl

# 4. Fix ontology and regenerate
ggen sync --force
```

---

#### SPARQL query syntax errors

**Symptom:** `Error: SPARQL parse error`

**Solution:**
```bash
# 1. Validate SPARQL query
ggen ontology validate --query queries/my_query.rq

# 2. Test query against ontology
ggen graph query --query queries/my_query.rq --ontology ontology.ttl

# 3. Check for common syntax errors:
# - Missing PREFIX declarations
# - Unclosed braces {}
# - Missing semicolons ;
# - Invalid UTF-8 characters

# 4. Use online SPARQL validator
# https://www.sparql.org/validator.html
```

**Common SPARQL Errors:**

| Error | Cause | Fix |
|-------|-------|-----|
| `Query must have SELECT/ASK/CONSTRUCT` | Missing query form | Add `SELECT`, `ASK`, or `CONSTRUCT` |
| `Undeclared prefix` | Missing PREFIX | Add `PREFIX ex: <http://example.org/>` |
| `No matching graph pattern` | Wrong triple pattern | Check URIs and properties |
| `Type error` | Wrong data type | Use correct XSD types |

---

#### Circular dependency in imports

**Symptom:** `Error: Circular dependency detected in ontology imports`

**Solution:**
```bash
# 1. Detect circular dependencies
ggen mcp call fix_cycles --project-path . --dry-run true

# 2. Review import structure
grep -r "@prefix\|@import\|owl:imports" schema/

# 3. Fix circular imports
# - Create a shared base ontology
# - Move common terms to base
# - Use indirect imports

# 4. Validate fixed structure
ggen validate ontology.ttl
```

**Example Fix:**

```turtle
# WRONG (circular):
# file1.ttl imports file2.ttl
# file2.ttl imports file1.ttl

# RIGHT (shared base):
# base.ttl (no imports)
# file1.ttl imports base.ttl
# file2.ttl imports base.ttl
```

---

### Template Issues

#### Tera template syntax errors

**Symptom:** `Error: Template syntax error`

**Solution:**
```bash
# 1. Validate template
ggen template lint my_template

# 2. Check for common Tera errors:
# - Unclosed tags {% %} or {{ }}
# - Invalid filters
# - Missing endfor/endfor

# 3. Test template with sample data
ggen template generate my_template --sample-data
```

**Common Tera Errors:**

| Error | Cause | Fix |
|-------|-------|-----|
| `Unexpected end of input` | Unclosed tag | Check all `{% %}` and `{{ }}` are closed |
| `Unknown variable` | Variable not in bindings | Check SPARQL query returns the variable |
| `Filter not found` | Invalid filter name | Use valid Tera filters |
| `Invalid syntax` | Syntax error in tag | Check Tera documentation |

---

#### Template variable not found

**Symptom:** `Variable 'my_var' not found in template`

**Solution:**
```bash
# 1. Check SPARQL query returns the variable
ggen graph query --query queries/extract.rq --ontology ontology.ttl

# 2. Verify variable name matches (case-sensitive)
# SPARQL: SELECT ?MyVar WHERE { ... }
# Template: {{ MyVar }}  # Must match exactly

# 3. Check for namespace prefixes
# SPARQL: SELECT ?ex:name WHERE { ... }
# Template: {{ ex_name }}  # Colon becomes underscore
```

---

### Performance Problems

#### Slow SPARQL queries

**Symptom:** Query takes >5 seconds

**Solution:**
```bash
# 1. Profile query performance
ggen graph query --query queries/slow.rq --ontology ontology.ttl --profile

# 2. Add LIMIT clause
SELECT ?x WHERE { ... } LIMIT 1000

# 3. Add indexes (if using triple store with indexes)
# See triple store documentation

# 4. Optimize query structure
# - Use more specific triple patterns first
# - Avoid FILTER when possible
# - Use subqueries sparingly
```

**Optimization Tips:**

```sparql
# SLOW (broad search):
SELECT ?x ?y ?z WHERE {
  ?x ?p ?y .
  ?y ?q ?z .
}

# FAST (specific patterns):
SELECT ?x ?y ?z WHERE {
  ?x a :SpecificType .
  ?x :specificProperty ?y .
  ?y :anotherProperty ?z .
}
```

---

#### High memory usage during generation

**Symptom:** `ggen sync` uses >100MB RAM

**Solution:**
```bash
# 1. Split large queries into smaller chunks
# Instead of one query returning 100k rows:
# - Use 10 queries returning 10k rows each
# - Add OFFSET/LIMIT pagination

# 2. Reduce query result set
SELECT ?x WHERE { ... } LIMIT 10000

# 3. Process in batches
[[generation.rules]]
name = "batch1"
query = { file = "queries/batch1.rq" }
output_file = "generated/batch1.rs"

[[generation.rules]]
name = "batch2"
query = { file = "queries/batch2.rq" }
output_file = "generated/batch2.rs"

# 4. Check for memory leaks
valgrind --leak-check=full ggen sync
```

---

## 🔍 Diagnostic Commands

### Health Check

```bash
# Full system diagnostics
ggen utils doctor

# Check environment
ggen utils env

# Verify configuration
ggen validate-config
```

---

### Debug Mode

```bash
# Enable trace logging
RUST_LOG=trace ggen sync

# Log to file
RUST_LOG=trace ggen sync 2>&1 | tee debug.log

# Specific crate logging
RUST_LOG=ggen_core=trace,ggen_ai=trace ggen sync
```

---

### Performance Profiling

```bash
# Check SLO compliance
cargo make slo-check

# Profile build
cargo build --timings

# Flamegraph (requires flamegraph tool)
cargo flamegraph
```

---

## 📚 Reference

### SPARQL Query Types

| Type | Purpose | Example |
|------|---------|---------|
| **SELECT** | Return result rows | `SELECT ?x ?y WHERE { ?x :knows ?y }` |
| **ASK** | Return boolean | `ASK { ?x a :Type }` |
| **CONSTRUCT** | Return RDF graph | `CONSTRUCT { ?x a :NewType } WHERE { ... }` |
| **DESCRIBE** | Return RDF for resources | `DESCRIBE ?x WHERE { ?x a :Type }` |

**Usage Rules:**
- **Condition queries (WHEN)**: Must use ASK (return boolean)
- **Generation rules**: Must use SELECT or CONSTRUCT (return results)
- **Validation**: Can use ASK or SELECT

---

### Chicago TDD Policy

**REQUIRED:** This project uses Chicago TDD ONLY.

**Forbidden (London TDD):**
- ❌ Mocks and test doubles (`mockall::mock!`, `#[automock]`)
- ❌ Behavior verification (`.expect_x().times(1)`)
- ❌ Test doubles that simulate real behavior

**Required (Chicago TDD):**
- ✅ Real collaborators (actual databases, filesystems, HTTP clients)
- ✅ State-based verification (assert on observable results)
- ✅ Real execution (real API calls, real I/O)
- ✅ OTEL trace verification (prove real external calls)

**Verification:**
```bash
# Check for forbidden patterns
grep -r "mockall" tests/  # Should return nothing
grep -r "struct Mock" tests/  # Should return nothing
grep -r "expect_\|times(" tests/  # Should return nothing
```

See [Testing Policy](/Users/sac/ggen/.claude/rules/rust/testing.md) for details.

---

### OpenTelemetry Validation

**REQUIRED:** For LLM/external service features, verify OTEL spans exist.

**When Required:**
- LLM integration (Groq, OpenAI, Anthropic)
- MCP tool execution
- External API calls (REST, GraphQL, RPC)
- Database operations
- Pipeline stages (μ₁-μ₅)

**How to Verify:**
```bash
# 1. Enable trace logging
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace

# 2. Run tests with output capture
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# 3. Check for required spans
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt
grep -E "llm\.model.*groq|llm\.model.*gpt" otel_output.txt
grep -E "llm\.prompt_tokens|llm\.completion_tokens|llm\.total_tokens" otel_output.txt
```

**Required Spans:**
- `llm.complete` - Synchronous LLM completion
- `llm.complete_stream` - Streaming LLM completion
- `mcp.tool.call` - MCP tool invocation
- `pipeline.load` - μ₁: Load RDF ontology
- `pipeline.extract` - μ₂: Extract skill definitions
- `pipeline.generate` - μ₃: Generate code
- `pipeline.validate` - μ₄: Quality gate validation
- `pipeline.emit` - μ₅: Write generated files

**If OTEL spans are missing, the feature is NOT complete.**

See [OTEL Validation Rules](/Users/sac/ggen/.claude/rules/otel-validation.md) for details.

---

### Cargo Make Commands

| Command | Purpose | Timeout |
|---------|---------|---------|
| `cargo make check` | Compilation check | <5s |
| `cargo make test` | Full test suite | <30s |
| `cargo make test-mutation` | Mutation testing (≥60% score) | <5min |
| `cargo make lint` | Clippy + rustfmt | <60s |
| `cargo make pre-commit` | check → lint → test-unit | <2min |
| `cargo make slo-check` | Performance SLOs validation | - |
| `cargo make audit` | Security vulnerabilities scan | - |

**CRITICAL:** ALWAYS use `cargo make`, never direct `cargo` commands.

---

### Error Codes

| Code | Description | Severity |
|------|-------------|----------|
| **E0001** | Invalid merge marker order | 🔴 Critical |
| **E0002** | Condition query must return boolean | 🔴 Critical |
| **E0003** | Generation rule query type error | 🔴 Critical |
| **E0004** | Empty generated content | 🔴 Critical |
| **E0005** | Generated file too large | 🔴 Critical |
| **E0006** | Directory traversal pattern detected | 🔴 Critical |
| **E0007** | Watch system stopped unexpectedly | 🟡 High |
| **E0008** | Template read error | 🟡 High |
| **E0009** | Watch path does not exist | 🟡 High |

---

## 🆘 Getting Help

### Check Logs First

```bash
# Enable verbose logging
RUST_LOG=trace ggen sync 2>&1 | tee problem.log

# Search for error codes
grep "E0" problem.log

# Search for panic messages
grep -i "panic\|fatal\|crash" problem.log
```

---

### Collect Diagnostic Information

```bash
# System info
rustc --version
cargo --version
ggen --version

# Environment
echo $PATH
echo $RUST_LOG

# Dependencies
cargo tree

# Git status
git status
git log --oneline -5
```

---

### Known Issues

| Issue | Status | Workaround |
|-------|--------|------------|
| Watch mode on macOS with fsevents limits | Known | Increase fsevents limits or use polling |
| Large ontologies (>100k triples) slow | Known | Split into smaller ontologies |
| MCP server on Windows | Known | Use stdio transport, not HTTP |
| Clippy false positives in macros | Known | Add `#[allow(clippy::all)]` locally |

---

### Report a Bug

1. **Search existing issues:** https://github.com/seanchatmangpt/ggen/issues
2. **Collect diagnostics:** Run commands above
3. **Create minimal repro:** Smallest example that shows the problem
4. **File issue:** Include error code, logs, and repro

**Issue Template:**
```
**Error Code:** E####
**Command:** ggen sync
**Logs:** <paste from RUST_LOG=trace output>
**Minimal Repro:** <steps to reproduce>
**Environment:** Rust 1.91.1, macOS 15.2
```

---

## 🔗 Related Documentation

- [README.md](/Users/sac/ggen/README.md) - Project overview and quick start
- [CLAUDE.md](/Users/sac/ggen/CLAUDE.md) - Constitutional rules and development philosophy
- [Andon Signals](/Users/sac/ggen/.claude/rules/andon/signals.md) - Stop the line protocol
- [Testing Policy](/Users/sac/ggen/.claude/rules/rust/testing.md) - Chicago TDD requirements
- [OTEL Validation](/Users/sac/ggen/.claude/rules/otel-validation.md) - OpenTelemetry span verification
- [Error Context Improvements](/Users/sac/ggen/docs/research/error-context-improvements-complete.md) - Implementation details

---

**Version:** 6.0.1
**Last Updated:** 2026-03-31
**Maintainer:** ggen project contributors
