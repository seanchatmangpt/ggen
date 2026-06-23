# Troubleshooting Guide

This guide helps you diagnose and fix common ggen problems.

## Error: "Ontology not found"

### Error Message
```
Error: Ontology http://example.com/custom.ttl not found
Available ontologies:
  - embedded: http://www.w3.org/1999/02/22-rdf-syntax-ns# (RDF)
  - embedded: http://www.w3.org/2000/01/rdf-schema# (RDFS)
  - installed: marketplace:financial/banking@1.2.1
```

### Causes
1. **Wrong ontology URI** — Typo in `ggen.toml`
2. **Package not installed** — Tried to use a marketplace package that isn't installed
3. **File doesn't exist** — Using `file://` URI to a non-existent file
4. **Network failure** — Tried to fetch from marketplace without internet

### Solutions

**Solution 1: Check if embedded**
```bash
ggen ontology list --embedded
```

Look for your ontology. If found, update `ggen.toml`:
```toml
[pipeline]
ontology_uri = "embedded:rdf"  # ✅ Correct
# ontology_uri = "http://example.com/rdf.ttl"  # ❌ Won't work
```

**Solution 2: Install marketplace package**
```bash
# Check if package exists
ggen ontology search financial
# Output: financial/banking (available)

# Install it
ggen ontology install financial/banking@1.2.1

# Use it in ggen.toml
ontology_uri = "marketplace:financial/banking@1.2.1"
```

**Solution 3: Load from file**
```bash
# Use file:// URI (must be absolute path)
ontology_uri = "file:///home/user/my-ontology.ttl"

# Verify file exists
ls -la /home/user/my-ontology.ttl
```

**Solution 4: Check network**
```bash
# Enable debug logging
export RUST_LOG=debug

# Try again
ggen sync

# Check logs for network errors
# Look for: "Failed to fetch", "connection timeout", etc.
```

---

## Error: "SPARQL Query Error"

### Error Message
```
Error: SPARQL parse error: unexpected token
Query: SELECT ?x ?y WHERE { ?x a rdf:Property }
       ^^^^^^^^^ error here
Location: ggen.toml, line 42
```

### Causes
1. **Missing period** — SPARQL queries must end with `.`
2. **Undefined prefix** — Used a prefix that wasn't defined
3. **Invalid syntax** — Typo in SPARQL keywords
4. **Invalid RDF property** — Property doesn't exist in ontology

### Solutions

**Solution 1: Add missing period**
```sparql
# ❌ Wrong
SELECT ?x WHERE { ?x a rdf:Property }

# ✅ Correct
SELECT ?x WHERE { ?x a rdf:Property . }
```

**Solution 2: Define prefixes**
```sparql
# ❌ Wrong (undefined prefix)
SELECT ?x WHERE { ?x a banking:Account . }

# ✅ Correct (with PREFIX)
PREFIX banking: <http://example.com/banking#>
SELECT ?x WHERE { ?x a banking:Account . }
```

**Solution 3: Check ontology for available properties**
```bash
# List properties in ontology
ggen ontology info http://www.w3.org/1999/02/22-rdf-syntax-ns# --properties

# Search for properties containing "label"
ggen ontology search-property rdf "label"
```

**Solution 4: Use SPARQL validator**
```bash
# Validate query syntax
ggen validate-sparql "SELECT ?x WHERE { ?x a rdf:Property . }"

# Output: ✅ Valid syntax
```

### Common SPARQL Mistakes

| Mistake | Wrong | Correct |
|---------|-------|---------|
| Missing period | `SELECT ?x WHERE { ?x a rdf:Property }` | `SELECT ?x WHERE { ?x a rdf:Property . }` |
| Undefined prefix | `SELECT ?x WHERE { ?x a banking:Account . }` | Use `PREFIX banking:` first |
| Invalid operator | `SELECT ?x WHERE { ?x > 5 }` | `SELECT ?x WHERE { ?x rdf:value ?v . FILTER(?v > 5) }` |
| Missing variable | `SELECT ?x WHERE { ?y a rdf:Property . }` | `SELECT ?y WHERE { ?y a rdf:Property . }` |
| Invalid string | `SELECT ?x WHERE { ?x rdfs:label "hello }` | Use closing quote: `"hello"` |

---

## Error: "Permission Denied"

### Error Message
```
Error: Permission denied: .ggen/cache
Permission denied (os error 13)
```

### Causes
1. **Cache directory not writable** — Permission denied on `~/.ggen/`
2. **Project directory read-only** — Can't write generated files
3. **File already exists** — Can't overwrite read-only file

### Solutions

**Solution 1: Fix cache directory permissions**
```bash
# Fix ownership and permissions
chmod -R u+w ~/.ggen/
chmod -R u+w .ggen/

# Or delete and recreate
rm -rf ~/.ggen/
ggen init  # Recreates cache
```

**Solution 2: Fix project directory permissions**
```bash
# Make project writable
chmod -R u+w /path/to/project

# Or run as different user
sudo -u your_user ggen sync
```

**Solution 3: Check if output file is read-only**
```bash
# Check file permissions
ls -la src/generated.rs

# Make it writable
chmod u+w src/generated.rs

# Try again
ggen sync
```

---

## Error: "Lock File Mismatch"

### Error Message
```
Error: Lock file mismatch
Package: financial/banking
Expected: 1.2.1 (sha256:abc123...)
Found: 1.2.0 (sha256:def456...)
```

### Causes
1. **Version installed differs from lock file** — Installed 1.2.0, lock file expects 1.2.1
2. **Package corrupted** — Downloaded file is different from expected checksum
3. **Lock file out of date** — Using old lock file with newer package installed

### Solutions

**Solution 1: Reinstall exact version**
```bash
# Remove current version
ggen ontology uninstall financial/banking@1.2.0

# Install version from lock file
ggen ontology install financial/banking@1.2.1
```

**Solution 2: Update lock file**
```bash
# Update to current versions
ggen ontology lock --update

# Commit updated lock file
git add ggen.lock
git commit -m "chore: update ontology lock file"
```

**Solution 3: Verify package integrity**
```bash
# Check all packages match lock file
ggen ontology verify --locked

# Output shows which packages match/mismatch
Package                    | Status
---------------------------|--------
financial/banking@1.2.1    | ❌ FAIL (checksum mismatch)

# Reinstall failed packages
ggen ontology install financial/banking@1.2.1 --force
```

---

## Error: "Network Timeout"

### Error Message
```
Error: Network timeout
Trying to fetch: https://registry.example.com/financial/banking/1.2.1
Timeout after 30 seconds
```

### Causes
1. **Slow internet connection** — Taking >30 seconds to download
2. **Marketplace server down** — Registry is offline
3. **Firewall blocking requests** — Network policy restricts access

### Solutions

**Solution 1: Use offline mode**
```bash
# Skip network calls entirely (use only embedded/cached)
ggen sync --offline

# Or in ggen.toml
[network]
allow_network = false
```

**Solution 2: Increase timeout**
```bash
# Set longer timeout (in seconds)
ggen sync --network-timeout 120
```

**Solution 3: Use cached packages**
```bash
# If you've installed packages before, use cache
ggen sync  # Uses cache if available

# Check what's cached
ls -la ~/.ggen/packages/
```

**Solution 4: Check network**
```bash
# Test internet connectivity
ping registry.example.com

# Test marketplace API
curl https://registry.example.com/api/health

# Check firewall rules
sudo ufw status
```

---

## Error: "Out of Memory"

### Error Message
```
Error: Out of memory
Ontology: financial/banking (1.2 GB)
Tried to allocate: 456 MB
Available: 234 MB
```

### Causes
1. **Large ontology** — Ontology has millions of triples
2. **Insufficient RAM** — System doesn't have enough memory
3. **Memory leak** — Previous ggen process didn't exit cleanly

### Solutions

**Solution 1: Check system memory**
```bash
# Check available RAM
free -h
# Output: 
# total: 16 GB
# available: 2 GB  ← Not enough!

# Check running processes
ps aux | grep ggen

# Kill stale processes
pkill ggen
```

**Solution 2: Limit SPARQL results**
```sparql
# Original query (returns millions of results)
SELECT ?x WHERE { ?x a rdf:Property . }

# Limited query (returns only 1000)
SELECT ?x WHERE { ?x a rdf:Property . } LIMIT 1000
```

**Solution 3: Use smaller ontologies**
```toml
# ❌ Large ontology (1.2 GB)
ontology_uri = "marketplace:financial/banking@1.2.1"

# ✅ Smaller embedded ontology (2 KB)
ontology_uri = "embedded:rdf"
```

**Solution 4: Increase system memory**
```bash
# Add swap space (Linux)
sudo fallocate -l 4G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile

# Or use cloud instance with more RAM
# AWS: t3.large (8 GB) → t3.xlarge (16 GB)
# GCP: e2-medium (4 GB) → e2-standard-4 (16 GB)
```

---

## Error: "SPARQL Timeout"

### Error Message
```
Error: SPARQL query timeout
Query: SELECT ?x WHERE { ... }
Timeout after 30 seconds
Triples processed: 42,567 of 1,234,567
```

### Causes
1. **Complex query** — Too many patterns or OPTIONAL clauses
2. **Large ontology** — Millions of triples to search
3. **Inefficient filter** — FILTER clause evaluated on every triple

### Solutions

**Solution 1: Simplify query**
```sparql
# ❌ Slow: Multiple OPTIONAL patterns
SELECT ?resource ?label ?comment ?seeAlso ?domain ?range
WHERE {
  ?resource a rdf:Property .
  OPTIONAL { ?resource rdfs:label ?label . }
  OPTIONAL { ?resource rdfs:comment ?comment . }
  OPTIONAL { ?resource rdfs:seeAlso ?seeAlso . }
  OPTIONAL { ?resource rdfs:domain ?domain . }
  OPTIONAL { ?resource rdfs:range ?range . }
}

# ✅ Fast: Only required patterns
SELECT ?resource ?label
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
}
```

**Solution 2: Add LIMIT**
```sparql
# Limit results to first 1000
SELECT ?resource ?label
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
}
LIMIT 1000
```

**Solution 3: Move FILTER to SELECT**
```sparql
# ❌ Slow: Filter evaluated on all triples
SELECT ?resource
WHERE {
  ?resource a rdf:Property .
  FILTER(STRLEN(STR(?resource)) > 20)
}

# ✅ Fast: Get properties first, then filter
SELECT ?resource
WHERE {
  ?resource a rdf:Property .
  ?resource rdfs:label ?label .
}
FILTER(STRLEN(?label) > 5)
```

**Solution 4: Increase timeout**
```bash
# Set longer timeout (in seconds)
ggen sync --sparql-timeout 120
```

---

## Error: "Template Rendering Error"

### Error Message
```
Error: Template rendering error
Template: templates/output.tera
Error: Variable 'results' is undefined
Line: 42
```

### Causes
1. **SPARQL doesn't return column** — Query result missing expected column
2. **Wrong variable name** — Typo in template variable
3. **Invalid template syntax** — Tera syntax error

### Solutions

**Solution 1: Check SPARQL result columns**
```bash
# Print what SPARQL returns
ggen debug-sparql "SELECT ?x ?y WHERE { ?x a rdf:Property . ?y a rdfs:Class . } LIMIT 5"

# Output shows actual columns:
# ?x: http://www.w3.org/1999/02/22-rdf-syntax-ns#type
# ?y: http://www.w3.org/2000/01/rdf-schema#Class
```

**Solution 2: Update template to match**
```jinja2
{# ❌ Wrong: expects ?results #}
{% for row in results %}
  {{ row.x }}
{% endfor %}

{# ✅ Correct: uses actual SPARQL SELECT variables #}
{% set variables = ["x", "y"] %}
{% for row in rows %}
  {{ row.x }}, {{ row.y }}
{% endfor %}
```

**Solution 3: Debug template**
```bash
# Run template with debug output
ggen debug-template templates/output.tera \
  --sparql "SELECT ?x WHERE { ?x a rdf:Property . } LIMIT 5"

# Shows actual values passed to template
```

---

## Error: "Certificate Verification Failed"

### Error Message
```
Error: Certificate verification failed
Trying to download from: https://registry.example.com
Error: Self-signed certificate

The certificate is self-signed and not trusted by your system.
```

### Causes
1. **Self-signed certificate** — Registry uses untrusted certificate
2. **Expired certificate** — Certificate expired
3. **Corporate firewall** — Proxy injects different certificate

### Solutions

**Solution 1: Verify certificate is legitimate**
```bash
# Check certificate details
openssl s_client -connect registry.example.com:443 -servername registry.example.com

# Look for:
# - CN (Common Name) matches domain
# - Expiration date
# - Issuer
```

**Solution 2: Add certificate to trust store**
```bash
# Copy certificate
openssl s_client -connect registry.example.com:443 \
  -servername registry.example.com </dev/null | \
  sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' \
  > /tmp/cert.pem

# Add to system trust store
sudo cp /tmp/cert.pem /usr/local/share/ca-certificates/
sudo update-ca-certificates
```

**Solution 3: Disable verification (not recommended)**
```bash
# Skip certificate verification
export REQWEST_TLS_VERIFY=false
ggen sync

# Or configure in ggen.toml
[network]
tls_verify = false  # ⚠️ Security risk
```

**Solution 4: Use offline mode**
```bash
# Skip network altogether
ggen sync --offline
```

---

## Error: "Conflicting Ontologies"

### Error Message
```
Error: Conflicting ontologies
Ontology 1: http://example.com/schema#Property
Ontology 2: http://example.com/schema#Property (different definition)
Conflict: Both define same class with different properties
```

### Causes
1. **Same URI, different content** — Two ontologies claim the same namespace
2. **Circular dependencies** — Ontologies reference each other cyclically
3. **Version mismatch** — Mixed incompatible versions of same ontology

### Solutions

**Solution 1: Use only one version**
```toml
# ❌ Wrong: mixing two versions
ontology_uri = "marketplace:financial/banking@1.2.0"
ontology_mixins = [
  "marketplace:financial/banking@1.3.0"  # Conflict!
]

# ✅ Correct: consistent version
ontology_uri = "marketplace:financial/banking@1.2.1"
ontology_mixins = [
  "marketplace:financial/insurance@1.0.0"  # Different package
]
```

**Solution 2: Check ontology dependencies**
```bash
# See what each ontology depends on
ggen ontology info financial/banking@1.2.1 --dependencies

# Output:
# financial/banking depends on:
#   - embedded:rdf
#   - embedded:owl
#   - financial/common@1.0.0 (< specify version)
```

**Solution 3: Use specific versions**
```bash
# Install specific compatible versions
ggen ontology install financial/banking@1.2.1
ggen ontology install financial/common@1.0.0

# Create lock file to pin versions
ggen ontology lock
```

---

## Performance Issues

### Symptom: "Slow Generation"

**Diagnosis**:
```bash
# Check performance
ggen sync --audit

# Output shows:
# μ₁ (Load): 100 ms ✅
# μ₂ (Extract): 500 ms ⚠️ SLOW
# μ₃ (Render): 50 ms ✅
# μ₄ (Canonicalize): 10 ms ✅
# μ₅ (Receipt): 5 ms ✅
```

**Solutions**:
1. Simplify SPARQL queries (see SPARQL Timeout above)
2. Use `LIMIT` to reduce results
3. Cache marketplace packages
4. Use embedded ontologies instead

### Symptom: "High Memory Usage"

**Diagnosis**:
```bash
# Monitor memory while running
watch -n 1 'ps aux | grep ggen'

# Check peak memory
ggen sync --audit 2>&1 | grep -i memory
```

**Solutions**:
1. Process in batches with `LIMIT`
2. Use `--offline` to skip network overhead
3. Increase system RAM
4. Use smaller ontologies

---

## Getting Help

### If You're Still Stuck

1. **Check the logs**: `export RUST_LOG=debug && ggen sync 2>&1 | tee log.txt`
2. **Search issues**: https://github.com/seanchatmangpt/ggen/issues
3. **Ask for help**: Create a new issue with:
   - Error message (full output)
   - `ggen --version`
   - `ggen ontology list --embedded`
   - Your `ggen.toml` (without sensitive data)
   - Steps to reproduce

4. **Check the FAQ**: [FAQ.md](./FAQ.md)

---

## Quick Reference

| Error | Quick Fix |
|-------|-----------|
| "Ontology not found" | `ggen ontology list --embedded` |
| "SPARQL error" | Add period at end: `.` |
| "Permission denied" | `chmod -R u+w .ggen/` |
| "Lock mismatch" | `ggen ontology lock --update` |
| "Network timeout" | `ggen sync --offline` |
| "Out of memory" | Add `LIMIT 1000` to SPARQL |
| "Certificate error" | `export REQWEST_TLS_VERIFY=false` |
| "Slow generation" | Simplify SPARQL or use `LIMIT` |
| "Template error" | Check SPARQL column names match template |
| "Conflicting ontologies" | Use consistent versions via `ggen.lock` |
