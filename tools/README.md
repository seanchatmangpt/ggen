# ggen Tools

Utility tools for validating, testing, and maintaining ggen documentation and code.

## Diataxis Validator

Validates that all Diataxis documentation follows the correct structure, contains required metadata, and maintains referential integrity.

### What It Validates

✅ **Category Detection**
- Ensures files are in correct directories (tutorials/, how-to/, explanations/, reference/)
- Identifies misplaced files

✅ **Content Structure**
- Checks for H1 title
- Verifies minimum section count
- Validates word count against category minimums:
  - Tutorial: ≥800 words
  - How-To: ≥600 words
  - Explanation: ≥700 words
  - Reference: ≥400 words

✅ **Required Metadata**
- Tutorials: Learning Objectives, Estimated Time, Difficulty
- How-To: Estimated Time, Difficulty
- All: Next Steps section (warning if missing)

✅ **Cross-References**
- Verifies all markdown links to `.md` files are valid
- Detects broken internal links
- Allows external URLs (http/https)

✅ **Index Completeness**
- Validates document counts match expectations:
  - 5 Tutorials
  - 5 How-To guides
  - 5 Explanations
  - 7 References
- Ensures all documents are referenced in diataxis-index.md

### Usage

#### Option 1: Standalone (Recommended - No Dependencies)

```bash
# Compile
rustc --edition 2021 tools/diataxis-validator-standalone.rs -o diataxis-validator

# Run from ggen root
./diataxis-validator

# Or directly with rust-script (if installed)
rust-script tools/diataxis-validator-standalone.rs
```

#### Option 2: Using Cargo

```bash
# From ggen root
cd tools
cargo run --release --bin diataxis-validator

# Or with cargo directly from ggen root
cargo run --manifest-path tools/Cargo.toml --release --bin diataxis-validator
```

### Output

The validator produces a detailed report:

```
================================================================================
  DIATAXIS DOCUMENTATION VALIDATION REPORT
================================================================================

📊 SUMMARY
  ├─ Total files found:       22
  ├─ Valid files:            22
  ├─ Errors:                  0
  └─ Warnings:                2

📈 DOCUMENT COUNTS BY CATEGORY
  ├─ Tutorial:                5
  ├─ How-To:                  5
  ├─ Explanation:             5
  └─ Reference:               7

❌ ERRORS (if any)
  ├─ [1] Path/to/file.md: Broken reference to 'nonexistent.md'
  ├─ [2] Path/to/file.md: Missing H1 title

⚠️  WARNINGS (if any)
  ├─ [1] Path/to/file.md: Only 500 words (minimum 600)
  ├─ [2] Path/to/file.md: Tutorial missing 'Learning Objectives'

================================================================================
  STATUS: ✅ VALIDATION PASSED
================================================================================
```

### Exit Codes

- **0**: All validations passed
- **1**: One or more errors detected

### Requirements

**Standalone version:**
- Rust compiler (any recent version)
- No external dependencies

**Cargo version:**
- Cargo
- Dependencies (automatically managed):
  - walkdir 2.x
  - regex 1.x

### Performance

- **Standalone:** ~50-100ms for full diataxis documentation
- **Cargo:** ~200-300ms (includes compilation overhead)

### Integration with CI/CD

Add to your CI pipeline:

```yaml
# GitHub Actions
- name: Validate Diataxis Documentation
  run: |
    rustc --edition 2021 tools/diataxis-validator-standalone.rs -o validator
    ./validator
```

```yaml
# GitLab CI
validate_docs:
  script:
    - rustc --edition 2021 tools/diataxis-validator-standalone.rs -o validator
    - ./validator
```

### Common Validation Errors

| Error | Cause | Fix |
|-------|-------|-----|
| `File not in diataxis structure` | File not in correct subdirectory | Move file to tutorials/, how-to/, explanations/, or reference/ |
| `Missing H1 title` | No `# Title` at start of file | Add title as first markdown heading |
| `Only X words (minimum Y)` | Content too short | Expand content or check word count requirements |
| `Broken reference to 'X'` | Link points to non-existent file | Update link or create referenced file |
| `Tutorial missing 'Learning Objectives'` | Required section missing | Add "## Learning Objectives" section |
| `Document not referenced in index` | File not linked in diataxis-index.md | Add entry to diataxis-index.md |

### Testing

Both validators include unit tests:

```bash
# Standalone
rustc --test tools/diataxis-validator-standalone.rs -o validator_test && ./validator_test

# Cargo
cd tools && cargo test
```

Test coverage includes:
- Category detection from file paths
- Minimum word requirements per category
- Title extraction
- Reference parsing
- Word counting

### Implementation Details

**Validator Algorithm:**

1. **Scan Phase**: Walk directory tree, collect all `.md` files
2. **Parse Phase**: For each file:
   - Extract title (first H1)
   - Count words
   - Count sections
   - Extract headers
   - Extract references
   - Detect metadata sections
3. **Validate Phase**: Check:
   - Category-specific metadata
   - Word count minimums
   - Section structure
   - Header consistency
4. **Reference Phase**: Validate:
   - All internal links point to real files
   - All documents in index
   - Count totals match expectations
5. **Report Phase**: Generate summary with errors and warnings

**Standalone Advantages:**
- No dependency management
- Faster startup time
- Can be run with plain `rustc`
- No Cargo.lock required
- Self-contained executable

**Cargo Advantages:**
- Better dependency resolution
- Release optimization
- Test framework integration
- Package management

### Customization

To modify validation rules, edit the validator source:

1. **Change category minimums**: Update `Category::min_words()`
2. **Add new checks**: Extend `validate_document()` function
3. **Customize report**: Modify `ValidationResult::print_report()`
4. **Change expectations**: Update expected counts in `validate_index_file()`

### Contributing

To improve the validator:

1. Add tests for new validation rules
2. Update README with new error types
3. Consider performance implications
4. Ensure backward compatibility

### License

Same as ggen project
