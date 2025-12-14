<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to: Debug Pack Installation](#how-to-debug-pack-installation)
  - [Enable Verbose Logging](#enable-verbose-logging)
  - [Check Pack Integrity](#check-pack-integrity)
  - [Validate Before Installation](#validate-before-installation)
  - [Common Issues](#common-issues)
    - [Issue 1: Signature Verification Failed](#issue-1-signature-verification-failed)
    - [Issue 2: Template Not Found](#issue-2-template-not-found)
    - [Issue 3: SPARQL Query Fails](#issue-3-sparql-query-fails)
    - [Issue 4: Type Generation Incorrect](#issue-4-type-generation-incorrect)
    - [Issue 5: Generation Crashes](#issue-5-generation-crashes)
  - [Debugging Tools](#debugging-tools)
    - [Inspect Pack Contents](#inspect-pack-contents)
    - [Dry Run Generation](#dry-run-generation)
    - [Compare Pack Versions](#compare-pack-versions)
    - [Test in Isolated Environment](#test-in-isolated-environment)
  - [Check Logs](#check-logs)
  - [Clean Cache and Retry](#clean-cache-and-retry)
  - [Report Issue](#report-issue)
  - [Related Guides](#related-guides)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to: Debug Pack Installation

**Problem:** Pack installation fails or generates incorrect code.

**Solution:** Use debugging tools and troubleshooting steps.

---

## Enable Verbose Logging

```bash
ggen ontology install schema.org --verbose
```

**Output shows:**
- Download progress
- Signature verification steps
- File extraction
- Validation checks
- Error stack traces

---

## Check Pack Integrity

Verify pack file isn't corrupted:

```bash
ggen ontology verify ./schema.org-1.0.0.gpack
```

**Checks:**
- File format validity
- Signature verification
- Metadata schema
- Required files present

---

## Validate Before Installation

```bash
ggen ontology validate ./my-pack --strict
```

**Validation checks:**
- `pack.yaml` schema compliance
- Ontology syntax (RDF/Turtle/XML)
- Template syntax (Handlebars)
- SPARQL query validity
- File path references
- Version format
- License declaration

---

## Common Issues

### Issue 1: Signature Verification Failed

**Error:**
```
Error: Signature verification failed
  Pack: schema.org-1.0.0.gpack
  Public key fingerprint mismatch
```

**Solutions:**

1. Re-download the pack:
   ```bash
   rm -rf ~/.ggen/cache/schema.org-1.0.0.gpack
   ggen ontology install schema.org
   ```

2. Update marketplace index:
   ```bash
   ggen marketplace refresh
   ggen ontology install schema.org
   ```

3. Trust specific key (if from known source):
   ```bash
   ggen keys trust ./publisher-key.pub
   ggen ontology install ./schema.org-1.0.0.gpack --force
   ```

---

### Issue 2: Template Not Found

**Error:**
```
Error: Template 'typescript' not found in pack
  Pack: my-ontology
  Available templates: python, rust
```

**Solutions:**

1. List available templates:
   ```bash
   ggen ontology info my-ontology
   ```

2. Use available template:
   ```bash
   ggen ontology generate my-ontology --template python
   ```

3. Install template separately:
   ```bash
   ggen template install typescript
   ```

---

### Issue 3: SPARQL Query Fails

**Error:**
```
Error: SPARQL query execution failed
  Query: classes
  Message: Syntax error at line 5
```

**Solutions:**

1. Test SPARQL query manually:
   ```bash
   ggen ontology query my-ontology \
     --query-file ./test-query.sparql \
     --format json
   ```

2. Validate SPARQL syntax:
   ```bash
   ggen ontology validate my-ontology --check-queries
   ```

3. Use query debugger:
   ```bash
   ggen ontology generate my-ontology \
     --template typescript \
     --debug-queries \
     --output ./test
   ```

---

### Issue 4: Type Generation Incorrect

**Error:**
Types are generated but with wrong TypeScript types.

**Solutions:**

1. Check type mappings:
   ```bash
   ggen ontology info my-ontology --show-type-mappings
   ```

2. Override type mappings:
   ```bash
   ggen ontology generate my-ontology \
     --template typescript \
     --config '{
       "custom_type_mappings": {
         "xsd:dateTime": "Date"
       }
     }'
   ```

3. Inspect intermediate data:
   ```bash
   ggen ontology generate my-ontology \
     --template typescript \
     --dump-intermediate ./debug.json
   ```

   Then inspect `debug.json`:
   ```json
   {
     "classes": [...],
     "properties": [...],
     "type_mappings": {...}
   }
   ```

---

### Issue 5: Generation Crashes

**Error:**
```
Error: Generation failed
  Handlebars template error: Missing helper 'zodType'
```

**Solutions:**

1. Verify template helpers exist:
   ```bash
   ggen ontology template verify ./my-templates/typescript
   ```

2. Test template locally:
   ```bash
   ggen ontology template test \
     --template ./my-templates/typescript \
     --sample-data ./test-data.json
   ```

3. Use safe mode (skip custom helpers):
   ```bash
   ggen ontology generate my-ontology \
     --template typescript \
     --safe-mode
   ```

---

## Debugging Tools

### Inspect Pack Contents

```bash
ggen ontology inspect my-ontology
```

**Shows:**
- All files in pack
- File sizes
- Template details
- Ontology statistics

---

### Dry Run Generation

Test generation without writing files:

```bash
ggen ontology generate my-ontology \
  --template typescript \
  --dry-run
```

**Output:**
- What files would be created
- Preview of generated content
- Template variables used

---

### Compare Pack Versions

```bash
ggen ontology diff my-ontology@1.0.0 my-ontology@1.1.0
```

**Shows:**
- New/removed classes
- Changed properties
- Template differences
- Metadata changes

---

### Test in Isolated Environment

```bash
# Create test directory
mkdir /tmp/ggen-test
cd /tmp/ggen-test

# Install with fresh cache
GGEN_HOME=/tmp/ggen-test/.ggen ggen ontology install my-ontology

# Test generation
GGEN_HOME=/tmp/ggen-test/.ggen ggen ontology generate my-ontology \
  --template typescript \
  --output ./test
```

---

## Check Logs

View installation logs:

```bash
cat ~/.ggen/logs/install.log
```

View generation logs:

```bash
cat ~/.ggen/logs/generate.log
```

Filter for errors:

```bash
grep ERROR ~/.ggen/logs/*.log
```

---

## Clean Cache and Retry

```bash
# Clear all caches
ggen cache clear

# Reinstall pack
ggen ontology install my-ontology

# Regenerate code
ggen ontology generate my-ontology --template typescript --output ./src
```

---

## Report Issue

If issue persists, gather debug info:

```bash
ggen debug-info my-ontology > debug-report.txt
```

**debug-report.txt contains:**
- ggen version
- Platform info
- Pack metadata
- Installation logs
- Generation logs
- Environment variables

Submit to pack author or marketplace support.

---

## Related Guides

- [How to: Customize Code Generation](customize-generation.md)
- [Reference: CLI Commands](../reference/cli-commands.md)
- [Tutorial: Building Your Own Pack](../tutorials/03-building-custom-pack.md)
