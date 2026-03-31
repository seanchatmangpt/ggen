# LLM Integration Test - End-to-End Code Generation

## Purpose

This test validates the complete LLM-assisted code generation pipeline in ggen v6.0.0:
1. SPARQL extraction of behavior predicates (`a2a:hasSystemPrompt`, `a2a:hasImplementationHint`)
2. Template rendering with `{{ generated_impl }}` blocks
3. LLM bridge integration for generating implementation code
4. Generated code compilation and validation

## Test Architecture

```
ontology.ttl (Behavior Predicates)
    ↓
SPARQL Extraction (μ₁)
    ↓
Template Rendering (μ₃) with {{ generated_impl }}
    ↓
LLM Bridge (Groq API)
    ↓
Generated Rust Code (μ₅)
    ↓
Compilation Verification (cargo check)
```

## Test Skills

### 1. Data Analysis Skill
- **System Prompt**: "You are a data analysis expert. Analyze structured data and provide insights using statistical methods."
- **Implementation Hint**: "Use pandas for data manipulation and implement statistical analysis functions"
- **Expected Output**: Rust code with statistical analysis functions

### 2. Text Processing Skill
- **System Prompt**: "You are a natural language processing expert. Process text data, extract entities, and perform sentiment analysis."
- **Implementation Hint**: "Implement tokenization, entity extraction, and sentiment scoring using regex and pattern matching"
- **Expected Output**: Rust code with NLP processing functions

### 3. Decision Making Skill
- **System Prompt**: "You are a decision support system. Evaluate options based on weighted criteria and provide recommendations with confidence scores."
- **Implementation Hint**: "Implement weighted decision matrix with confidence scoring and explanation generation"
- **Expected Output**: Rust code with decision matrix logic

## Verification Steps

### Step 1: Run Code Generation
```bash
cd /Users/sac/ggen/examples/llm-full-integration
ggen sync --ontology
```

**Expected Output:**
- SPARQL extraction log showing behavior predicates
- LLM API calls to Groq
- Generated Rust code in `./generated/`

### Step 2: Verify SPARQL Extraction
Check that the following predicates are extracted:
- `a2a:hasSystemPrompt` for all 3 skills
- `a2a:hasImplementationHint` for all 3 skills
- `a2a:hasBehavior` for all 3 skills

**Verification Command:**
```bash
ggen query --sparql "SELECT ?s ?p ?o WHERE { ?s a2a:hasSystemPrompt ?o }"
```

### Step 3: Verify Template Rendering
Check generated code contains:
- Struct definitions for `AnalysisResult`, `TextEntity`, `DecisionCriteria`
- Trait implementations with `{{ generated_impl }}` blocks replaced
- LLM-generated implementation code

**Files to Check:**
- `./generated/src/agent.rs` - Main agent struct
- `./generated/src/skills/` - Individual skill implementations
- `./generated/src/types.rs` - Schema type definitions

### Step 4: Verify Compilation
```bash
cd ./generated
cargo check
```

**Expected Result:**
- ✅ Compiles without errors
- ✅ All dependencies resolve
- ✅ No warnings from clippy

### Step 5: Run Tests (if applicable)
```bash
cargo test
```

## Success Criteria

- [x] Ontology with behavior predicates created
- [ ] SPARQL extraction logs show behavior predicates
- [ ] Templates use `{{ generated_impl }}` correctly
- [ ] LLM bridge makes API calls to Groq
- [ ] Generated code compiles without errors
- [ ] Generated code contains expected skill implementations
- [ ] No compilation warnings or errors

## Expected Generated Code Structure

```
generated/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── agent.rs           # Main agent with skills
│   ├── skills/
│   │   ├── mod.rs
│   │   ├── data_analysis.rs
│   │   ├── text_processing.rs
│   │   └── decision_making.rs
│   └── types.rs           # Schema types
└── tests/
    └── integration_test.rs
```

## Known Limitations

1. **LLM API Required**: Test requires valid Groq API credentials
2. **Network Dependency**: Requires internet access for LLM calls
3. **Rate Limiting**: Groq API may have rate limits
4. **Non-Deterministic**: LLM output may vary between runs

## Troubleshooting

### Issue: SPARQL extraction fails
**Solution**: Check ontology.ttl syntax with `ggen validate ontology.ttl`

### Issue: LLM API errors
**Solution**: Verify GROQ_API_KEY is set in environment

### Issue: Generated code doesn't compile
**Solution**: Check template syntax and LLM output formatting

### Issue: Missing behavior predicates
**Solution**: Verify namespace prefix `a2a:` is correctly defined

## Results Documentation

After running the test, document results here:

- **Test Run Date**: [YYYY-MM-DD]
- **ggen Version**: [version]
- **SPARQL Extraction**: ✅/❌
- **LLM Integration**: ✅/❌
- **Generated Code Compiles**: ✅/❌
- **Total Generation Time**: [seconds]
- **LLM API Calls**: [count]
- **Generated LOC**: [lines of code]

## Next Steps

Based on test results:
1. If successful: Document as working example
2. If failed: Debug specific component (SPARQL, Template, LLM)
3. If partial: Document limitations and workarounds
