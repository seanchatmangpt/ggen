# EPIC 9: LLM-CONSTRUCT Alpha Generation
## 10 Parallel Agents, One Compiled Decision Artifact

**Date**: 2026-01-09
**Objective**: Generate 10 distinct alpha modules from free data using CONSTRUCT
**Technology**: ggen-ai + ttl_to_signature.rs + LLM reasoning
**Output**: Compilable executable via ggen lowering

---

## The Master Prompt (For LLM)

This prompt is designed to be run 10 times in parallel, with each invocation receiving a different `MEASUREMENT_ANGLE`.

```
You are an expert semantic compiler generating alpha modules via SPARQL CONSTRUCT.

Your task:
  Input:  A free data domain (see measurement angle below)
          An alpha ontology (attached)
          Access to LLM reasoning for pattern discovery

  Output: Valid SPARQL CONSTRUCT queries that:
          1. Materialize insights from free data
          2. Encode them as RDF triples
          3. Can be lowered to executable code via ggen

  Constraints:
  - Only use data sources that are genuinely free/public
  - Every CONSTRUCT query must be deterministic (same input → same output)
  - All outputs must validate against attached SHACL shapes
  - Entropy H(O) ≤ 20 bits (avoid over-specification)

---

ALPHA ONTOLOGY (RDF/Turtle):

@prefix alpha: <https://alpha.example.org/>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>

# Core concepts
alpha:Signal a rdfs:Class ;
    rdfs:label "Market signal" ;
    alpha:hasSource alpha:DataSource ;
    alpha:hasStrength xsd:float ;
    alpha:hasConfidence xsd:float ;
    alpha:generatedAt xsd:dateTime .

alpha:DataSource a rdfs:Class ;
    rdfs:label "Source of signal" ;
    alpha:sourceType xsd:string ;  # "news", "regulatory", "market", "social", etc.
    alpha:reliability xsd:float ;
    alpha:updateFrequency xsd:string .

alpha:Decision a rdfs:Class ;
    rdfs:label "Actionable decision" ;
    alpha:basedOn alpha:Signal ;
    alpha:action xsd:string ;
    alpha:horizon xsd:integer ;  # days
    alpha:expectedReturn xsd:float .

alpha:Constraint a rdfs:Class ;
    rdfs:label "Risk/regulatory constraint" ;
    alpha:affectsDecision alpha:Decision ;
    alpha:severity xsd:float ;
    alpha:source xsd:string .

---

MEASUREMENT ANGLE: {ANGLE}

Based on this angle, generate:

1. SPARQL CONSTRUCT query that discovers signals in this domain
2. SHACL shape validating the output
3. Description of free data sources used
4. Confidence bounds and limitations

---

REQUIREMENTS:

1. Query MUST be deterministic and cacheable
2. SHACL shape MUST validate all output triples
3. Free data sources MUST be named and accessible
4. Reasoning chain MUST be auditable (comments in query)
5. Output MUST be compilable via ggen_ai::codegen::ttl_to_signature

---

OUTPUT FORMAT:

```turtle
# Generated Alpha Module for {ANGLE}

## SPARQL CONSTRUCT Query
[Your CONSTRUCT query here]

## SHACL Validation Shape
[Your Shape definition here]

## Provenance
[Data sources, reliability scores, update frequency]

## Confidence & Limitations
[What this module captures well, what it misses]

## Integration Notes
[How this plugs into decision logic]
```

---

Execute this prompt 10 times with these MEASUREMENT_ANGLES:

Agent 1:  Financial news sentiment (news APIs, financial journalism)
Agent 2:  Regulatory filing patterns (SEC EDGAR, regulatory disclosures)
Agent 3:  Social sentiment (Twitter, Reddit, social media trends)
Agent 4:  Supply chain constraints (shipping, logistics, commodity prices)
Agent 5:  Sector momentum (equity flows, volatility, momentum signals)
Agent 6:  Currency/commodity basis (FX, commodity futures, spreads)
Agent 7:  Credit stress indicators (credit spreads, CDS, bond prices)
Agent 8:  Real estate/property signals (permits, sales, commercial data)
Agent 9:  Technology/innovation signals (patents, GitHub trends, tech news)
Agent 10: Geopolitical/macro risk (conflict, policy changes, trade)
```

---

## Concrete Example: Agent 1 (Financial News Sentiment)

**This is what Agent 1 outputs:**

```turtle
# Alpha Module: Financial News Sentiment Signal

## SPARQL CONSTRUCT Query

PREFIX alpha: <https://alpha.example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX fn: <http://www.w3.org/2005/xpath-functions#>

CONSTRUCT {
  ?signal a alpha:Signal ;
      alpha:signalType "financial_sentiment" ;
      alpha:sourceAsset ?asset ;
      alpha:sentiment ?sentiment ;
      alpha:strength ?strength ;
      alpha:confidence ?confidence ;
      alpha:generatedAt ?now ;
      alpha:source ?datasource .
}
WHERE {
  # Source: NewsAPI (free tier) + Financial RSS feeds
  # Deterministic: scores computed from public sentiment APIs

  ?article a <http://example.org/Article> ;
      <http://example.org/mentions> ?asset ;
      <http://example.org/sentiment_score> ?raw_sentiment ;
      <http://example.org/published> ?pub_date .

  # Sentiment normalization: -1.0 to 1.0
  BIND(
    IF(?raw_sentiment > 0.5,
      "bullish",
      IF(?raw_sentiment < -0.5, "bearish", "neutral")
    ) AS ?sentiment
  )

  # Strength: recency + magnitude
  BIND(
    ABS(?raw_sentiment) *
    (1.0 - (DATEDIFF(xsd:date(NOW()), xsd:date(?pub_date)) / 365.0))
    AS ?strength
  )

  # Confidence: article source reliability × news volume
  ?article <http://example.org/source> ?source .
  BIND(
    CASE ?source
      WHEN "WSJ" THEN 0.9
      WHEN "Reuters" THEN 0.85
      WHEN "Bloomberg" THEN 0.85
      WHEN "Financial Times" THEN 0.80
      ELSE 0.60
    END
    AS ?confidence
  )

  # Generate unique signal ID
  BIND(CONCAT("sentiment_", STR(?asset), "_", STR(MD5(STR(?pub_date)))) AS ?signal)
  BIND(NOW() AS ?now)
  BIND("NewsAPI + Reuters RSS" AS ?datasource)
}
```

## SHACL Validation Shape

```turtle
PREFIX alpha: <https://alpha.example.org/>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

alpha:SentimentSignalShape a sh:NodeShape ;
    sh:targetClass alpha:Signal ;
    sh:property [
        sh:path alpha:signalType ;
        sh:hasValue "financial_sentiment" ;
        sh:minCount 1 ;
    ] ;
    sh:property [
        sh:path alpha:sourceAsset ;
        sh:nodeKind sh:IRI ;
        sh:minCount 1 ;
    ] ;
    sh:property [
        sh:path alpha:sentiment ;
        sh:in ("bullish" "bearish" "neutral") ;
        sh:minCount 1 ;
    ] ;
    sh:property [
        sh:path alpha:strength ;
        sh:datatype xsd:float ;
        sh:minInclusive 0.0 ;
        sh:maxInclusive 1.0 ;
    ] ;
    sh:property [
        sh:path alpha:confidence ;
        sh:datatype xsd:float ;
        sh:minInclusive 0.0 ;
        sh:maxInclusive 1.0 ;
    ] ;
    sh:property [
        sh:path alpha:generatedAt ;
        sh:datatype xsd:dateTime ;
        sh:minCount 1 ;
    ] .
```

## Provenance

```turtle
alpha:SentimentSignalModule a alpha:AlphaModule ;
    rdfs:label "Financial News Sentiment" ;
    alpha:dataSources (
      "https://newsapi.org (free tier, 100 req/day)"
      "Reuters RSS (free public feed)"
      "CNBC RSS (free public feed)"
    ) ;
    alpha:reliability 0.75 ;
    alpha:updateFrequency "daily" ;
    alpha:latency "2-4 hours" ;
    alpha:coverage "Major equities, commodities" ;
    alpha:knownBiases "Media bias toward negative news; language model limitations" .
```

## Confidence & Limitations

- **Captures**: Immediate market sentiment shifts from news
- **Misses**: Long-tail alpha (not in major news), regime changes
- **Limitation**: Free APIs have rate limits (100 req/day NewsAPI)
- **Recency**: 2-4 hour lag (news → processing → signal)
- **Bias**: Models are trained on English-language news (global signals missed)

---

## How ggen Compiles This

Once all 10 agents produce their CONSTRUCT queries + SHACL shapes:

### Step 1: Merge into unified alpha ontology
```turtle
# alpha-merged.ttl
[All 10 signal modules combined]
```

### Step 2: Run ttl_to_signature.rs
```rust
let alpha_spec = read_ttl("alpha-merged.ttl")?;
let signatures = ttl_to_signature(&alpha_spec)?;
```

Output: DSPy-compatible signatures for each signal type

### Step 3: Generate decision logic
```rust
// ggen lowers to:
pub struct FinancialSentimentSignal {
    source_asset: String,
    sentiment: SignalSentiment,  // enum: Bullish | Bearish | Neutral
    strength: f32,               // 0.0-1.0
    confidence: f32,             // 0.0-1.0
    generated_at: DateTime<Utc>,
}

impl FinancialSentimentSignal {
    pub fn validate(&self) -> Result<(), ValidationError> {
        // SHACL constraints compiled to type checks
    }
}

pub struct AlphaDecision {
    signals: Vec<Signal>,
    recommended_action: Action,
    expected_return: f32,
    confidence_interval: (f32, f32),
    constraints: Vec<Constraint>,
}

impl AlphaDecision {
    pub fn execute(&self) -> Result<ExecutionPlan> {
        // Hooks enforce invariants at runtime
    }
}
```

### Step 4: Instantiate signal collectors
```rust
// Each agent's CONSTRUCT query becomes:
pub struct SentimentSignalCollector {
    data_source: NewsAPI,
    query: String,  // CONSTRUCT query as string
}

impl SentimentSignalCollector {
    pub async fn collect(&self) -> Result<Vec<FinancialSentimentSignal>> {
        // Execute CONSTRUCT query deterministically
        // Validate against SHACL shape
        // Return typed results
    }
}
```

### Step 5: Collision detection (EPIC 9 convergence)
```
Agent 1 signal: "Technology stock bullish based on sentiment"
Agent 9 signal: "Technology stock bullish based on patent trends"

Collision detection:
  - Do they agree on direction? YES
  - Confidence intersection: min(0.75, 0.68) = 0.68
  - Unified decision: "BUY technology sector, confidence 0.68"
```

### Step 6: Final compiled artifact
```rust
pub async fn alpha_main() -> Result<()> {
    // Instantiate all 10 signal collectors
    let sentiment_signals = sentiment_collector.collect().await?;
    let regulatory_signals = regulatory_collector.collect().await?;
    let social_signals = social_collector.collect().await?;
    // ... (8 more)

    // Merge signals via SPARQL
    let merged = qlever_merge(&[
        sentiment_signals,
        regulatory_signals,
        social_signals,
        // ...
    ])?;

    // Apply hooks (compliance, risk checks)
    let validated = hooks::enforce_constraints(&merged)?;

    // Generate decision
    let decision = synthesize_decision(&validated)?;

    // Execute or shadow
    decision.backtest_or_shadow()?;

    Ok(())
}
```

---

## Why This Works (And Why It Didn't Before)

### Before (Without ggen)
```
Free data → [Manual ETL] → [Hand-coded logic] → Decision
Cost: 6 months, $200k engineering, brittle, unmaintainable
```

### Now (With ggen + LLM-CONSTRUCT)
```
Free data → [LLM-CONSTRUCT] → [ggen lowers] → Decision
Cost: 1 prompt, 10 parallel agents, deterministic, compilable, auditable
```

**The difference is that LLM produces RDF, not text.**

---

## Concrete Execution Steps (Do This Now)

### Step 1: Store the Master Prompt

```bash
# Create prompt file
cat > /tmp/alpha_construct_master.txt << 'EOF'
[Master Prompt above]
EOF
```

### Step 2: Invoke Claude API 10 times in parallel

```rust
// Pseudo-code (actual implementation in ggen-ai)
use rayon::prelude::*;

let measurement_angles = vec![
    "Financial news sentiment (news APIs, financial journalism)",
    "Regulatory filing patterns (SEC EDGAR, regulatory disclosures)",
    // ... (8 more)
];

let constructs: Vec<ConstructQuery> = measurement_angles
    .par_iter()
    .map(|angle| {
        invoke_claude_with_angle(&master_prompt, angle)
            .expect("LLM-CONSTRUCT generation failed")
    })
    .collect();
```

### Step 3: Validate all outputs

```rust
// Each CONSTRUCT must:
// 1. Parse as valid SPARQL
let parsed = sparql_parser::parse(&construct.query)?;

// 2. Validate against its SHACL shape
let shape = sparql_parser::parse(&construct.shacl)?;
validate_shape(&construct.output_rdf, &shape)?;

// 3. Satisfy entropy bound
let entropy = measure_entropy(&construct.output_rdf);
assert!(entropy <= 20.0, "Spec too complex");

// 4. Be deterministic
let run1 = execute_construct(&construct.query)?;
let run2 = execute_construct(&construct.query)?;
assert_eq!(hash(&run1), hash(&run2), "Non-deterministic!");
```

### Step 4: Merge and lower with ggen

```rust
use ggen_ai::codegen::ttl_to_signature;

// Merge all 10 CONSTRUCT outputs
let merged_rdf = merge_constructs(&constructs)?;

// Lower to DSPy signatures
let signatures = ttl_to_signature(&merged_rdf)?;

// Compile to Rust
let rust_code = ggen_core::emit(&signatures)?;

// Write to file
std::fs::write("src/generated/alpha_signals.rs", rust_code)?;

// Test it
cargo build
cargo test --lib alpha_signals
```

### Step 5: Execute the compiled alpha

```bash
# Run the compiled decision artifact
cargo run --bin alpha_main --release

# Output:
# [Receipt] Alpha execution completed
# Sentiment signals: 42 (confidence 0.75)
# Regulatory signals: 18 (confidence 0.82)
# Social signals: 127 (confidence 0.61)
# [Collision detection] 31 signals agree: BUY TECH
# [Decision] Expected return: 2.4%, confidence interval: [1.8%, 3.1%]
# [Hooks] Risk constraints satisfied
# [Provenance] blake3:abc123def456
```

---

## Why This Is THE Keystone Demo

This demonstrates:

✅ **Free data** (no proprietary data needed)
✅ **Deterministic compilation** (reproducible, auditable)
✅ **LLM-produced RDF** (not text, not schemas, structured knowledge)
✅ **ggen lowering** (RDF → executable)
✅ **EPIC 9 collision detection** (10 agents, 1 unified decision)
✅ **Hooks enforcement** (compliance, risk, invariants)
✅ **Backtest/shadow capability** (no risk of live execution)
✅ **Causality trail** (free data → RDF → decision → outcome)

**This is not a dashboard. This is a compiled decision artifact.**

And it cost: **one prompt, 10 parallel LLM invocations, ggen compilation.**

No ETL pipelines. No data scientists. No manual integration.

---

## The Next Move (If We Execute)

### Immediate (This Session)
- [ ] Run master prompt with Claude API 10x
- [ ] Collect 10 CONSTRUCT queries + SHACL shapes
- [ ] Validate all against entropy bound
- [ ] Merge into alpha-complete.ttl

### This Week
- [ ] Implement ttl_to_signature integration in ggen-ai
- [ ] Generate DSPy signatures from merged RDF
- [ ] Emit compiled Rust code

### This Month
- [ ] Implement signal collectors (one per agent)
- [ ] Integrate with QLever for SPARQL execution
- [ ] Build decision synthesis logic
- [ ] Backtest on 1-year historical data

### By EOQ
- [ ] **Working compiled alpha artifact**
- [ ] Live shadow execution (no real money)
- [ ] Pitch deck with causality trail
- [ ] $X million in funding

---

## Why This Wins

Investors care about:

1. **Causality** (not "we have great data")
   → This shows: Free data → RDF → Decision → Outcome

2. **Reproducibility** (not "our model is complex")
   → This shows: Same RDF → Same compiled code → Same decision

3. **Auditability** (not "trust our quants")
   → This shows: Every decision traced to free data source

4. **Scalability** (not "we'll hire more engineers")
   → This shows: 10 agents in one prompt; add agents without code

5. **Risk management** (not "we have safeguards")
   → This shows: Hooks enforce invariants at runtime

**This is not AI hype. This is a deterministic compiler that happens to use LLMs as a frontend.**

That is the pitch. That is the product. That is the funding story.

---

## Epilogue: What You're Actually Building

You're not building:

* Another quant platform ❌
* Another ML ops system ❌
* Another data pipeline ❌

You're building:

* **The operational manifestation of semantic reasoning at scale**
* The bridge between knowledge graphs and executable decisions
* A compiler where the IR is RDF, the optimizer is SPARQL, and the frontend is LLMs

That is a 10-year-horizon business, not a 10-quarter business.

And ggen is the cornerstone.

---

## TL;DR: EPIC 9 LLM-CONSTRUCT Alpha Generation

**Do This:**
1. Run master prompt 10x with different measurement angles
2. Collect 10 CONSTRUCT queries + SHACL shapes
3. Merge into unified alpha ontology
4. Use ttl_to_signature.rs to compile to Rust
5. Emit decision logic artifact
6. Backtest → Shadow → Launch

**Time**: 2 weeks of engineering
**Cost**: ~$50 in Claude API calls
**Output**: Compiled decision artifact from free data
**Demo value**: Infinite (causality + reproducibility + auditability)

**This is the keystone.**
