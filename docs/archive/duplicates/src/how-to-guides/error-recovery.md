<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Error Recovery and Rollback Strategies](#error-recovery-and-rollback-strategies)
  - [Prevention: Validation Before Generation](#prevention-validation-before-generation)
    - [Pre-generation Validation](#pre-generation-validation)
    - [Backup Before Generation](#backup-before-generation)
  - [Graceful Error Handling](#graceful-error-handling)
    - [Timeout Protection](#timeout-protection)
    - [Retry with Exponential Backoff](#retry-with-exponential-backoff)
  - [Recovery Strategies](#recovery-strategies)
    - [Strategy 1: Atomic Writes](#strategy-1-atomic-writes)
    - [Strategy 2: Version Control Checkpoints](#strategy-2-version-control-checkpoints)
    - [Strategy 3: Blue-Green Deployment](#strategy-3-blue-green-deployment)
    - [Strategy 4: Circuit Breaker Pattern](#strategy-4-circuit-breaker-pattern)
  - [Debugging Failed Generation](#debugging-failed-generation)
    - [Capture Full Error Information](#capture-full-error-information)
    - [Analyze Error Logs](#analyze-error-logs)
    - [Minimal Reproduction](#minimal-reproduction)
  - [Recovery Checklist](#recovery-checklist)
  - [Common Failures and Solutions](#common-failures-and-solutions)
    - [Failure: Out of Memory](#failure-out-of-memory)
    - [Failure: File Permission Denied](#failure-file-permission-denied)
    - [Failure: Schema Validation Failed](#failure-schema-validation-failed)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Error Recovery and Rollback Strategies

Handle generation failures gracefully and recover when things go wrong.

## Prevention: Validation Before Generation

### Pre-generation Validation

Always validate before generating:

```bash
#!/bin/bash
set -e

echo "Pre-generation validation..."

# 1. Validate ontology syntax
ggen ontology validate schema.ttl || exit 1

# 2. Check for known issues
ggen utils doctor || exit 1

# 3. Verify dependencies
cargo check 2>/dev/null || true

# 4. Run tests
npm test 2>/dev/null || true

# Safe to generate
echo "✓ All validations passed"
ggen ontology generate schema.ttl --language typescript --output models.ts
```

### Backup Before Generation

```bash
#!/bin/bash

# Create backup
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
cp models.ts models.ts.backup.$TIMESTAMP

# Generate
ggen ontology generate schema.ttl --language typescript --output models.ts

# Verify new code compiles
if ! npx tsc --noEmit models.ts; then
  echo "❌ Generated code doesn't compile!"
  cp models.ts.backup.$TIMESTAMP models.ts
  exit 1
fi

echo "✓ Generated code verified"
```

## Graceful Error Handling

### Timeout Protection

```bash
#!/bin/bash

# Prevent hanging generation
timeout 60s ggen ontology generate schema.ttl \
  --language typescript \
  --output models.ts

if [ $? -eq 124 ]; then
  echo "ERROR: Generation timed out"
  exit 1
fi
```

### Retry with Exponential Backoff

```bash
#!/bin/bash

retry_with_backoff() {
  local max_attempts=3
  local timeout=1
  local attempt=1

  while [ $attempt -le $max_attempts ]; do
    echo "Attempt $attempt of $max_attempts..."

    if timeout $((timeout * 30))s ggen ontology generate "$@"; then
      return 0
    fi

    if [ $attempt -lt $max_attempts ]; then
      echo "Backing off ${timeout}m before retry..."
      sleep $((timeout * 60))
      timeout=$((timeout * 2))
    fi

    attempt=$((attempt + 1))
  done

  echo "ERROR: All retry attempts failed"
  return 1
}

# Use it
retry_with_backoff schema.ttl --language typescript --output models.ts
```

## Recovery Strategies

### Strategy 1: Atomic Writes

Never overwrite existing code directly - use atomic operations:

```bash
#!/bin/bash

OUTPUT_FILE="models.ts"
TEMP_FILE=".models.ts.tmp"

# Generate to temporary file
ggen ontology generate schema.ttl \
  --language typescript \
  --output "$TEMP_FILE" \
|| {
  rm -f "$TEMP_FILE"
  exit 1
}

# Verify generated code
if ! npx tsc --noEmit "$TEMP_FILE"; then
  echo "ERROR: Generated code has type errors"
  rm -f "$TEMP_FILE"
  exit 1
fi

# Atomic replacement
mv "$TEMP_FILE" "$OUTPUT_FILE"
echo "✓ Updated $OUTPUT_FILE atomically"
```

### Strategy 2: Version Control Checkpoints

```bash
#!/bin/bash

# Current commit
BEFORE=$(git rev-parse HEAD)

# Generate
ggen ontology generate schema.ttl \
  --language typescript \
  --output models.ts \
|| exit 1

# Verify
npm test 2>&1 | tee test-output.txt || {
  # Tests failed - rollback
  git checkout HEAD -- models.ts
  echo "❌ Tests failed - rolled back to $BEFORE"
  exit 1
}

# Commit if tests pass
git add models.ts
git commit -m "chore: regenerate models"
echo "✓ Changes committed"
```

### Strategy 3: Blue-Green Deployment

Keep two versions running during migration:

```typescript
// src/models.blue.ts (old version)
import { User as UserBlue } from './generated/models.old';

// src/models.green.ts (new version)
import { User as UserGreen } from './generated/models.new';

// Use feature flag to switch
const USE_NEW_MODELS = process.env.USE_GREEN === 'true';

export const User = USE_NEW_MODELS ? UserGreen : UserBlue;

// Usage: no code changes needed
import { User } from './models';
```

Switch between versions without downtime:

```bash
# Deploy new models
export USE_GREEN=true
npm run deploy

# If issues, quick rollback
export USE_GREEN=false
npm run deploy
```

### Strategy 4: Circuit Breaker Pattern

Fail gracefully if generation fails:

```typescript
import { User as FallbackUser } from './fallback-models';

async function getOrGenerateUser(id: string): Promise<User> {
  try {
    // Try with newly generated models
    const generated = await import('./generated-models');
    return new generated.User(id);
  } catch (error) {
    // Fall back to last known good version
    console.warn('Generation failed, using fallback:', error.message);
    return new FallbackUser(id);
  }
}
```

## Debugging Failed Generation

### Capture Full Error Information

```bash
#!/bin/bash

LOG_FILE="generation-$(date +%Y%m%d-%H%M%S).log"

# Capture everything
ggen ontology generate schema.ttl \
  --language typescript \
  --output models.ts \
  --verbose \
  2>&1 | tee "$LOG_FILE" || {
    ERROR_CODE=$?
    echo "ERROR: Generation failed with code $ERROR_CODE"
    echo "Details in: $LOG_FILE"
    exit $ERROR_CODE
  }
```

### Analyze Error Logs

```bash
# Find common errors
grep "ERROR\|WARN" generation-*.log

# Get last 50 lines of error
tail -50 generation-$(date +%Y%m%d)*.log

# Count error types
grep "ERROR:" generation-*.log | cut -d: -f3 | sort | uniq -c
```

### Minimal Reproduction

```bash
#!/bin/bash

# Test with minimal schema
cat > minimal.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:SimpleClass a rdfs:Class .
ex:simpleProperty a rdf:Property ;
  rdfs:domain ex:SimpleClass .
EOF

# Test minimal generation
ggen ontology generate minimal.ttl \
  --language typescript \
  --output test-models.ts \
  --verbose

# If minimal works, add complexity incrementally
```

## Recovery Checklist

When generation fails:

- [ ] Check error message carefully
- [ ] Validate ontology: `ggen ontology validate schema.ttl`
- [ ] Try with minimal schema
- [ ] Check system resources: `free -h`, `df -h`
- [ ] Review recent changes to schema
- [ ] Check ggen version: `ggen --version`
- [ ] Look at logs: tail generation logs
- [ ] Try upgrading ggen: `cargo install ggen --force`
- [ ] Check for known issues in docs
- [ ] Report if bug: include logs and minimal example

## Common Failures and Solutions

### Failure: Out of Memory

```bash
# Symptom: OOM error, generation hangs
# Solution 1: Split large ontology
split -l 1000 large-schema.ttl part-
ggen ontology generate part-aa.ttl --output part-aa.ts

# Solution 2: Use streaming
ggen ontology generate schema.ttl \
  --language typescript \
  --output models.ts \
  --streaming \
  --buffer-size 100MB
```

### Failure: File Permission Denied

```bash
# Symptom: Permission denied writing to file
# Solution: Fix permissions
ls -la models.ts
chmod 644 models.ts

# Or write to different location
ggen ontology generate schema.ttl \
  --output /tmp/models.ts
cp /tmp/models.ts ./models.ts
```

### Failure: Schema Validation Failed

```bash
# Symptom: Ontology validation errors
# Solution: Fix schema
ggen ontology validate schema.ttl --detailed

# Make corrections
nano schema.ttl

# Validate again
ggen ontology validate schema.ttl
```

## Summary

You now know how to:
- ✅ Validate before generation
- ✅ Backup critical files
- ✅ Implement recovery strategies
- ✅ Use atomic operations
- ✅ Debug generation failures
- ✅ Rollback safely
- ✅ Minimize impact of failures

Your generation pipeline is now resilient!
