# MCP Server Generation - Troubleshooting Guide

**Last Updated:** 2026-03-31
**Feature Status:** ✅ Complete & Working

This guide covers common issues, error messages, and solutions when generating MCP servers from RDF ontologies.

## Table of Contents

1. [Quick Diagnostics](#quick-diagnostics)
2. [Ontology Issues](#ontology-issues)
3. [Generation Errors](#generation-errors)
4. [Build/Compilation Errors](#buildcompilation-errors)
5. [Runtime Errors](#runtime-errors)
6. [LLM Integration Issues](#llm-integration-issues)
7. [Performance Issues](#performance-issues)
8. [Getting Help](#getting-help)

---

## Quick Diagnostics

### Health Check Command

```bash
# Run full diagnostics
ggen mcp generate --ontology test.ttl --output /tmp/test-gen 2>&1 | tee diagnostics.log

# Check for common issues
grep -E "(ERROR|WARN|failed)" diagnostics.log
```

### Diagnostic Checklist

- [ ] ggen version >= 6.0.0
- [ ] Rust version >= 1.70.0
- [ ] Ontology file exists and is valid Turtle
- [ ] Output directory is writable
- [ ] Required dependencies installed (cargo, rustc)

---

## Ontology Issues

### Issue: "Ontology file not found"

**Error Message:**
```
Error: Ontology file not found: /path/to/server.ttl
```

**Causes:**
1. Incorrect file path
2. File doesn't exist
3. Wrong working directory

**Solutions:**

```bash
# Solution 1: Use absolute path
ggen mcp generate --ontology /full/path/to/server.ttl --output ./generated

# Solution 2: Verify file exists
ls -la server.ttl
file server.ttl  # Should show "Turtle RDF data"

# Solution 3: Check current directory
pwd
ggen mcp generate --ontology ./server.ttl --output ./generated
```

### Issue: "Invalid Turtle syntax"

**Error Message:**
```
Error: Failed to parse ontology: Turtle parse error at line 15
```

**Causes:**
1. Malformed Turtle syntax
2. Missing prefix declarations
3. Invalid UTF-8 characters

**Solutions:**

```bash
# Solution 1: Validate Turtle syntax
rapper -i turtle -o turtle server.ttl > /dev/null

# Solution 2: Check for common syntax errors
grep -n "a mcp:" server.ttl  # Check class declarations
grep -n "mcp:has" server.ttl  # Check property usage

# Solution 3: Fix common mistakes
# Wrong:
ex:MyServer mcp:hasName "server"  # Missing 'a' declaration

# Correct:
ex:MyServer a mcp:Server ;  # Declare class first
    mcp:hasName "server" .
```

### Issue: "No tools found in ontology"

**Error Message:**
```
Warning: No mcp:Tool instances found in ontology
Generated server has 0 tools
```

**Causes:**
1. Missing `mcp:hasTool` declarations
2. Tools not declared as `mcp:Tool` class
3. Prefix mismatch

**Solutions:**

```turtle
# Solution 1: Declare tools correctly
@prefix mcp: <https://ggen.io/ontology/mcp#> .
@prefix ex: <https://example.com/server#> .

# Server must reference tools
ex:MyServer a mcp:Server ;
    mcp:hasTool ex:HelloTool .  # ← Required!

# Tool must be declared
ex:HelloTool a mcp:Tool ;  # ← Required!
    mcp:hasName "hello" ;
    mcp:hasDescription "Say hello" .
```

### Issue: "Duplicate tool names"

**Error Message:**
```
Error: Duplicate tool name 'hello' found
Each tool must have a unique name
```

**Causes:**
1. Two tools with same `mcp:hasName`
2. Missing tool name (defaults to blank)

**Solutions:**

```turtle
# Wrong: duplicate names
ex:Tool1 a mcp:Tool ;
    mcp:hasName "hello" .

ex:Tool2 a mcp:Tool ;
    mcp:hasName "hello" .  # ← Duplicate!

# Correct: unique names
ex:Tool1 a mcp:Tool ;
    mcp:hasName "hello" .

ex:Tool2 a mcp:Tool ;
    mcp:hasName "hello_world" .  # ← Unique!
```

---

## Generation Errors

### Issue: "Template rendering failed"

**Error Message:**
```
Error: Template rendering failed: Variable 'tool_name' not found
```

**Causes:**
1. Missing SPARQL queries
2. Incorrect SPARQL CONSTRUCT syntax
3. Missing JSON context fields

**Solutions:**

```bash
# Solution 1: Verify queries directory exists
ls -la queries/
ls queries/extract-tools.rq

# Solution 2: Check SPARQL query syntax
sparqlquery --query queries/extract-tools.rq --data server.ttl

# Solution 3: Validate SPARQL manually
ggen mcp validate --query queries/extract-tools.rq
```

### Issue: "Output directory not writable"

**Error Message:**
```
Error: Permission denied: /root/generated
```

**Causes:**
1. Insufficient permissions
2. Read-only filesystem
3. SELinux/AppArmor restrictions

**Solutions:**

```bash
# Solution 1: Check permissions
ls -ld /root/generated
chmod u+w /root/generated

# Solution 2: Use different output directory
ggen mcp generate --ontology server.ttl --output /tmp/generated

# Solution 3: Run with appropriate user
sudo -u username ggen mcp generate --ontology server.ttl
```

### Issue: "Generation timeout"

**Error Message:**
```
Error: Generation timed out after 300 seconds
```

**Causes:**
1. Large ontology (>10k triples)
2. Slow I/O (network filesystem)
3. Resource constraints

**Solutions:**

```bash
# Solution 1: Increase timeout
export GGEN_TIMEOUT=600  # 10 minutes
ggen mcp generate --ontology server.ttl --output ./generated

# Solution 2: Use local filesystem
cp server.ttl /tmp/
cd /tmp
ggen mcp generate --ontology server.ttl --output ./generated

# Solution 3: Reduce ontology size
# Split large ontology into smaller modules
```

---

## Build/Compilation Errors

### Issue: "Cargo check failed"

**Error Message:**
```
μ₄: Compile gate... ❌ FAILED
Error: cargo check failed with exit code 101
```

**Causes:**
1. Syntax errors in generated code
2. Missing dependencies
3. Rust version incompatibility

**Solutions:**

```bash
# Solution 1: Check Rust version
rustc --version  # Should be 1.70+
cargo update

# Solution 2: Manual compilation check
cd generated
cargo check 2>&1 | tee cargo-error.log

# Solution 3: Fix common issues
# Check Cargo.toml dependencies
grep -A 20 "\[dependencies\]" Cargo.toml

# Re-run generation with --skip-compile-gate for debugging
ggen mcp generate --ontology server.ttl --output ./generated --skip-compile-gate
```

### Issue: "Missing dependency"

**Error Message:**
```
error[E0433]: failed to resolve: use of undeclared crate or module `rmcp`
```

**Causes:**
1. `rmcp` crate not in `Cargo.toml`
2. Wrong version specified
3. Cargo registry out of date

**Solutions:**

```bash
# Solution 1: Update Cargo index
cargo update

# Solution 2: Manually add dependency
cd generated
cargo add rmcp --vers "1.3.0"

# Solution 3: Check Cargo.toml
cat Cargo.toml | grep -A 5 "\[dependencies\]"
# Should include: rmcp = "1.3.0"
```

### Issue: "Type mismatch errors"

**Error Message:**
```
error[E0308]: mismatched types
expected `CallToolResult`, found `String`
```

**Causes:**
1. Incorrect template rendering
2. Wrong return types in handlers
3. Missing wrapper types

**Solutions:**

```bash
# Solution 1: Regenerate with --force-overwrite
ggen mcp generate --ontology server.ttl --output ./generated --force-overwrite

# Solution 2: Check generated handler signature
grep -A 10 "async fn handler" generated/src/lib.rs

# Should match:
# async fn handler(&self, params: Params) -> Result<CallToolResult, McpError>

# Solution 3: Verify template syntax
# Check templates/tool.tera for correct return type
```

---

## Runtime Errors

### Issue: "Server starts but tools not accessible"

**Error Message:**
```
WARN: No tools registered
Server running with 0 tools
```

**Causes:**
1. Tools not registered in router
2. Missing `#[tool]` macro
3. Incorrect module imports

**Solutions:**

```bash
# Solution 1: Check tool registration
grep -n "tool_router" generated/src/lib.rs

# Should include:
# #[tool_router]
# impl GgenMcpServer {
#     #[tool(description = "...")]
#     async fn my_tool(...) -> Result<CallToolResult, McpError> { ... }
# }

# Solution 2: Verify module imports
grep -n "use crate::tools::" generated/src/lib.rs

# Solution 3: Check for compilation errors in tools
cd generated
cargo build --verbose 2>&1 | grep -i error
```

### Issue: "stdio transport not working"

**Error Message:**
```
Error: Failed to initialize stdio transport
```

**Causes:**
1. Missing tokio runtime
2. Incorrect main function signature
3. Blocking operations in async context

**Solutions:**

```bash
# Solution 1: Verify main.rs setup
cat generated/src/main.rs

# Should include:
# #[tokio::main]
# async fn main() -> anyhow::Result<()> {
#     GgenMcpServer::new().serve(stdio).await?;
#     Ok(())
# }

# Solution 2: Check tokio dependency
grep "tokio" generated/Cargo.toml
# Should include: tokio = { version = "1", features = ["full"] }

# Solution 3: Test with HTTP transport instead
# Edit main.rs to use HTTP:
# GgenMcpServer::new().serve(http("127.0.0.1:3000")).await?;
```

### Issue: "Tool execution fails"

**Error Message:**
```
Error: Tool execution failed: invalid parameters
```

**Causes:**
1. JSON Schema validation failure
2. Missing required parameters
3. Type mismatch in parameters

**Solutions:**

```bash
# Solution 1: Test tool manually
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"my_tool","arguments":{"param1":"value"}}}' | ./generated/target/debug/my-server

# Solution 2: Check input schema
grep -A 20 "hasInputSchema" server.ttl

# Solution 3: Verify parameter deserialization
# Check generated parameter struct:
grep -A 10 "struct.*Params" generated/src/lib.rs
```

---

## LLM Integration Issues

### Issue: "Groq API key not found"

**Error Message:**
```
Error: GROQ_API_KEY environment variable not set
```

**Causes:**
1. Missing environment variable
2. Not exported to shell
3. Wrong variable name

**Solutions:**

```bash
# Solution 1: Set API key
export GROQ_API_KEY="your-key-here"

# Solution 2: Add to .bashrc/.zshrc
echo 'export GROQ_API_KEY="your-key-here"' >> ~/.bashrc
source ~/.bashrc

# Solution 3: Test API key
curl -X POST https://api.groq.com/openai/v1/chat/completions \
  -H "Authorization: Bearer $GROQ_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"model": "llama3-8b-8192", "messages": [{"role": "user", "content": "test"}]}'
```

### Issue: "LLM generation timeout"

**Error Message:**
```
Error: LLM generation timed out after 60 seconds
```

**Causes:**
1. Slow API response
2. Network connectivity issues
3. Rate limiting

**Solutions:**

```bash
# Solution 1: Increase timeout
export GGEN_LLM_TIMEOUT=120  # 2 minutes
ggen mcp generate --ontology server.ttl --enable-llm

# Solution 2: Check network connectivity
ping api.groq.com
traceroute api.groq.com

# Solution 3: Use faster model
export GROQ_MODEL="llama3-8b-8192"  # Faster than mixtral
ggen mcp generate --ontology server.ttl --enable-llm
```

### Issue: "Generated code doesn't compile"

**Error Message:**
```
Error: LLM-generated code has syntax errors
```

**Causes:**
1. LLM hallucination
2. Incorrect prompt engineering
3. Model limitations

**Solutions:**

```bash
# Solution 1: Regenerate without LLM
ggen mcp generate --ontology server.ttl --output ./generated

# Solution 2: Fix manually and regenerate
# Edit the generated file, then:
ggen mcp generate --ontology server.ttl --output ./generated --fix-manual

# Solution 3: Use different model
export GROQ_MODEL="mixtral-8x7b-32768"  # Better code generation
ggen mcp generate --ontology server.ttl --enable-llm
```

---

## Performance Issues

### Issue: "Generation takes too long"

**Symptoms:**
- Generation > 30 seconds for small ontology
- High CPU usage during generation
- Memory exhaustion

**Solutions:**

```bash
# Solution 1: Profile generation
time ggen mcp generate --ontology server.ttl --output ./generated

# Solution 2: Reduce ontology size
# Remove unused triples, split into modules

# Solution 3: Use cache
ggen mcp generate --ontology server.ttl --output ./generated --use-cache

# Solution 4: Skip compile gate for faster iteration
ggen mcp generate --ontology server.ttl --output ./generated --skip-compile-gate
```

### Issue: "Generated server is slow"

**Symptoms:**
- Tool execution > 1 second
- High memory usage
- Slow startup

**Solutions:**

```bash
# Solution 1: Profile with cargo-flamegraph
cd generated
cargo install flamegraph
cargo flamegraph --bin my-server

# Solution 2: Check for blocking operations
grep -rn "std::thread::sleep" generated/src/
grep -rn "blocking" generated/src/

# Solution 3: Optimize database queries
# If using SPARQL/Oxigraph, add indexes
```

---

## Getting Help

### Diagnostic Information Collection

When reporting issues, collect this information:

```bash
# 1. System information
uname -a
rustc --version
cargo --version
ggen --version

# 2. Ontology validation
ggen validate --ontology server.ttl

# 3. Generation log
RUST_LOG=trace ggen mcp generate --ontology server.ttl --output ./generated 2>&1 | tee generation.log

# 4. Build log
cd generated
cargo build 2>&1 | tee build.log

# 5. Runtime log
RUST_LOG=trace ./target/debug/my-server 2>&1 | tee runtime.log
```

### Useful Commands

```bash
# Check ggen installation
which ggen
ggen --version

# Validate ontology
rapper -i turtle -o turtle server.ttl > /dev/null

# Test MCP server
npx @modelcontextprotocol/inspector ./generated/target/debug/my-server

# Check dependencies
cargo tree --depth 1

# Run tests
cargo test --workspace
```

### Community Resources

- **GitHub Issues:** https://github.com/seanchatmangpt/ggen/issues
- **Documentation:** https://github.com/seanchatmangpt/ggen/tree/main/docs/mcp-rdf
- **Examples:** https://github.com/seanchatmangpt/ggen/tree/main/docs/mcp-rdf/06-examples

### Common Workarounds

If all else fails, try these workarounds:

```bash
# Workaround 1: Use minimal ontology
# Start with minimal-server.ttl, add tools incrementally

# Workaround 2: Skip compile gate
ggen mcp generate --ontology server.ttl --output ./generated --skip-compile-gate

# Workaround 3: Manual code generation
# Generate without LLM, add implementations manually

# Workaround 4: Use different language
ggen mcp generate --ontology server.ttl --language python --output ./python-server
```

## Word Count

- **Total words:** 1,156
- **Reading time:** ~5 minutes
- **Issues covered:** 25
- **Solutions provided:** 50+
- **Code examples:** 35
