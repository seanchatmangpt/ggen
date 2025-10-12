# Lifecycle + Ultrathink Integration Design

## Vision

Transform the lifecycle system into a **self-learning, team-collaborative workflow engine** that prevents common mistakes through pattern recognition and best practice recommendations.

**Core Principle (80/20)**: Focus on patterns that prevent the 20% of mistakes that cause 80% of project problems.

---

## 1. Knowledge Graph Schema: Lifecycle Patterns in RDF

### Vocabulary Design

We extend the existing RDF graph with a lifecycle-specific vocabulary:

```turtle
@prefix lc: <http://ggen.io/lifecycle/> .
@prefix pattern: <http://ggen.io/lifecycle/pattern/> .
@prefix metric: <http://ggen.io/lifecycle/metric/> .
@prefix team: <http://ggen.io/team/> .
@prefix schema: <http://schema.org/> .

# Core lifecycle ontology
lc:Pattern a rdfs:Class ;
    rdfs:label "Lifecycle Pattern" ;
    rdfs:comment "A reusable lifecycle configuration pattern" .

lc:AntiPattern a rdfs:Class ;
    rdfs:subClassOf lc:Pattern ;
    rdfs:label "Anti-Pattern" ;
    rdfs:comment "A pattern that should be avoided" .

lc:BestPractice a rdfs:Class ;
    rdfs:subClassOf lc:Pattern ;
    rdfs:label "Best Practice" ;
    rdfs:comment "A recommended pattern" .

# Properties
lc:hasPhase a rdf:Property ;
    rdfs:domain lc:Pattern ;
    rdfs:range lc:Phase .

lc:hasHook a rdf:Property ;
    rdfs:domain lc:Phase ;
    rdfs:range lc:Hook .

lc:hasCommand a rdf:Property ;
    rdfs:domain lc:Phase ;
    rdfs:range xsd:string .

lc:severity a rdf:Property ;
    rdfs:domain lc:AntiPattern ;
    rdfs:range lc:Severity .

lc:confidence a rdf:Property ;
    rdfs:domain lc:Pattern ;
    rdfs:range xsd:decimal ;
    rdfs:comment "Confidence score 0.0-1.0" .

lc:adoptionCount a rdf:Property ;
    rdfs:domain lc:Pattern ;
    rdfs:range xsd:integer .

lc:successRate a rdf:Property ;
    rdfs:domain lc:Pattern ;
    rdfs:range xsd:decimal .

# Severity levels
lc:Critical a lc:Severity .
lc:High a lc:Severity .
lc:Medium a lc:Severity .
lc:Low a lc:Severity .
```

### Example: Storing a Lifecycle Pattern

```rust
use ggen_core::graph::Graph;
use ggen_core::lifecycle::Make;

pub fn store_lifecycle_pattern(graph: &Graph, make: &Make, project_id: &str) -> anyhow::Result<()> {
    let turtle = format!(r#"
@prefix lc: <http://ggen.io/lifecycle/> .
@prefix pattern: <http://ggen.io/lifecycle/pattern/> .
@prefix project: <http://ggen.io/project/> .

# Store project metadata
project:{project_id} a lc:Project ;
    schema:name "{}" ;
    lc:framework "{}" ;
    lc:createdAt "{}"^^xsd:dateTime .

# Store each phase
{}

# Store hooks
{}

# Store dependencies between phases
{}
"#,
        make.project.name,
        make.workspace.as_ref()
            .and_then(|w| w.values().next())
            .and_then(|w| w.framework.as_ref())
            .unwrap_or(&"unknown".to_string()),
        chrono::Utc::now().to_rfc3339(),
        generate_phase_triples(make, project_id),
        generate_hook_triples(make, project_id),
        generate_dependency_triples(make, project_id)
    );

    graph.insert_turtle(&turtle)?;
    Ok(())
}

fn generate_phase_triples(make: &Make, project_id: &str) -> String {
    let mut triples = String::new();

    for (phase_name, phase) in &make.lifecycle {
        triples.push_str(&format!(
            r#"
project:{project_id}/phase/{phase_name} a lc:Phase ;
    lc:name "{phase_name}" ;
    lc:description "{}" ;
    lc:hasCache {} ;
    lc:parallel {} .
"#,
            phase.description.as_deref().unwrap_or(""),
            phase.cache.unwrap_or(false),
            phase.parallel.unwrap_or(false)
        ));

        // Store commands
        let commands = if let Some(cmd) = &phase.command {
            vec![cmd.clone()]
        } else if let Some(cmds) = &phase.commands {
            cmds.clone()
        } else {
            vec![]
        };

        for (idx, cmd) in commands.iter().enumerate() {
            triples.push_str(&format!(
                r#"
project:{project_id}/phase/{phase_name}/cmd/{idx} a lc:Command ;
    lc:command "{}" ;
    lc:order {} .
"#,
                cmd.replace("\"", "\\\""),
                idx
            ));
        }
    }

    triples
}

fn generate_hook_triples(make: &Make, project_id: &str) -> String {
    let mut triples = String::new();

    if let Some(hooks) = &make.hooks {
        // Before build hooks
        if let Some(before_build) = &hooks.before_build {
            for (idx, hook) in before_build.iter().enumerate() {
                triples.push_str(&format!(
                    r#"
project:{project_id}/hooks/before_build/{idx} a lc:Hook ;
    lc:hookType "before" ;
    lc:phase "build" ;
    lc:triggerPhase "{hook}" ;
    lc:order {idx} .
"#
                ));
            }
        }

        // After build hooks
        if let Some(after_build) = &hooks.after_build {
            for (idx, hook) in after_build.iter().enumerate() {
                triples.push_str(&format!(
                    r#"
project:{project_id}/hooks/after_build/{idx} a lc:Hook ;
    lc:hookType "after" ;
    lc:phase "build" ;
    lc:triggerPhase "{hook}" ;
    lc:order {idx} .
"#
                ));
            }
        }

        // Error handlers
        if let Some(on_error) = &hooks.on_error {
            triples.push_str(&format!(
                r#"
project:{project_id}/hooks/on_error a lc:ErrorHandler ;
    lc:command "{}" .
"#,
                on_error.replace("\"", "\\\"")
            ));
        }
    }

    triples
}

fn generate_dependency_triples(make: &Make, project_id: &str) -> String {
    let mut triples = String::new();

    // Infer dependencies from hooks
    if let Some(hooks) = &make.hooks {
        if let Some(before_build) = &hooks.before_build {
            for dep_phase in before_build {
                triples.push_str(&format!(
                    r#"
project:{project_id}/phase/build lc:dependsOn project:{project_id}/phase/{dep_phase} .
"#
                ));
            }
        }
    }

    triples
}
```

---

## 2. Pattern Recognition: Anti-Pattern Detection

### Top 10 Anti-Patterns (80/20 Rule)

Based on common developer mistakes, we focus on detecting these critical anti-patterns:

#### 1. Missing Test Before Build

**SPARQL Query:**

```sparql
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX pattern: <http://ggen.io/lifecycle/pattern/>

# Detect: build phase without test hook
SELECT ?project ?buildPhase WHERE {
    ?project a lc:Project .
    ?buildPhase a lc:Phase ;
        lc:name "build" ;
        lc:belongsTo ?project .

    # No before_build hook that triggers test
    FILTER NOT EXISTS {
        ?hook a lc:Hook ;
            lc:hookType "before" ;
            lc:phase "build" ;
            lc:triggerPhase "test" .
    }
}
```

**Rust Implementation:**

```rust
use ggen_core::graph::{Graph, CachedResult};

pub struct PatternDetector {
    graph: Graph,
}

impl PatternDetector {
    pub fn new(graph: Graph) -> Self {
        Self { graph }
    }

    pub fn detect_missing_test_before_build(&self, project_id: &str) -> anyhow::Result<bool> {
        let query = format!(r#"
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX project: <http://ggen.io/project/>

ASK {{
    project:{project_id}/phase/build a lc:Phase .

    FILTER NOT EXISTS {{
        ?hook a lc:Hook ;
            lc:hookType "before" ;
            lc:phase "build" ;
            lc:triggerPhase "test" .
    }}
}}
"#);

        match self.graph.query_cached(&query)? {
            CachedResult::Boolean(b) => Ok(b),
            _ => anyhow::bail!("Expected boolean result"),
        }
    }
}
```

#### 2. Circular Hook Dependencies

**SPARQL Query:**

```sparql
PREFIX lc: <http://ggen.io/lifecycle/>

# Detect circular dependencies using property paths
SELECT ?phase1 ?phase2 WHERE {
    ?phase1 lc:dependsOn+ ?phase2 .
    ?phase2 lc:dependsOn+ ?phase1 .
    FILTER(?phase1 != ?phase2)
}
```

**Rust Implementation:**

```rust
impl PatternDetector {
    pub fn detect_circular_dependencies(&self, project_id: &str) -> anyhow::Result<Vec<(String, String)>> {
        let query = format!(r#"
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX project: <http://ggen.io/project/>

SELECT ?phase1Name ?phase2Name WHERE {{
    ?phase1 a lc:Phase ;
        lc:name ?phase1Name ;
        lc:dependsOn+ ?phase2 .

    ?phase2 a lc:Phase ;
        lc:name ?phase2Name ;
        lc:dependsOn+ ?phase1 .

    FILTER(?phase1 != ?phase2)
    FILTER(STRSTARTS(STR(?phase1), "http://ggen.io/project/{project_id}"))
}}
"#);

        match self.graph.query_cached(&query)? {
            CachedResult::Solutions(rows) => {
                Ok(rows.into_iter()
                    .filter_map(|row| {
                        let phase1 = row.get("phase1Name")?.clone();
                        let phase2 = row.get("phase2Name")?.clone();
                        Some((phase1, phase2))
                    })
                    .collect())
            }
            _ => anyhow::bail!("Expected solutions"),
        }
    }
}
```

#### 3. Inefficient Cache Strategy

**SPARQL Query:**

```sparql
PREFIX lc: <http://ggen.io/lifecycle/>

# Detect: expensive phases without caching
SELECT ?project ?phase ?commandCount WHERE {
    ?project a lc:Project .
    ?phase a lc:Phase ;
        lc:belongsTo ?project ;
        lc:name ?phaseName ;
        lc:hasCache false .

    # Count commands (expensive phases have multiple commands)
    {
        SELECT ?phase (COUNT(?cmd) as ?commandCount) WHERE {
            ?phase lc:hasCommand ?cmd .
        }
        GROUP BY ?phase
    }

    # Flag if has 3+ commands and no caching
    FILTER(?commandCount >= 3)
    FILTER(?phaseName IN ("build", "test", "setup"))
}
```

#### 4. Missing Error Handlers

```sparql
PREFIX lc: <http://ggen.io/lifecycle/>

SELECT ?project WHERE {
    ?project a lc:Project .
    ?deployPhase a lc:Phase ;
        lc:name "deploy" ;
        lc:belongsTo ?project .

    # No error handler defined
    FILTER NOT EXISTS {
        ?project lc:hasErrorHandler ?handler .
    }
}
```

#### 5. Hardcoded Environment Values

```sparql
PREFIX lc: <http://ggen.io/lifecycle/>

# Detect hardcoded URLs, ports, or paths in commands
SELECT ?project ?phase ?command WHERE {
    ?project a lc:Project .
    ?phase a lc:Phase ;
        lc:belongsTo ?project ;
        lc:hasCommand ?command .

    # Regex patterns for common hardcoded values
    FILTER(
        REGEX(?command, "http://[^$]", "i") ||
        REGEX(?command, "localhost:[0-9]+", "i") ||
        REGEX(?command, "/Users/", "i") ||
        REGEX(?command, "C:\\\\", "i")
    )
}
```

### Anti-Pattern Detection Summary

```rust
#[derive(Debug, Clone)]
pub enum AntiPattern {
    MissingTestBeforeBuild {
        severity: Severity,
        phase: String,
    },
    CircularDependency {
        severity: Severity,
        cycle: Vec<String>,
    },
    InefficientCache {
        severity: Severity,
        phase: String,
        command_count: usize,
    },
    MissingErrorHandler {
        severity: Severity,
        phase: String,
    },
    HardcodedValues {
        severity: Severity,
        phase: String,
        command: String,
        pattern: String,
    },
    MissingStateTracking {
        severity: Severity,
        phase: String,
    },
    InefficientParallelization {
        severity: Severity,
        phases: Vec<String>,
    },
    DuplicateCommands {
        severity: Severity,
        phases: Vec<String>,
        command: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Critical, // Blocks deployment, breaks builds
    High,     // Performance issues, maintenance burden
    Medium,   // Best practice violations
    Low,      // Style/convention issues
}

impl PatternDetector {
    pub fn detect_all_antipatterns(&self, project_id: &str) -> anyhow::Result<Vec<AntiPattern>> {
        let mut patterns = Vec::new();

        // Check each anti-pattern
        if self.detect_missing_test_before_build(project_id)? {
            patterns.push(AntiPattern::MissingTestBeforeBuild {
                severity: Severity::High,
                phase: "build".to_string(),
            });
        }

        let cycles = self.detect_circular_dependencies(project_id)?;
        if !cycles.is_empty() {
            patterns.push(AntiPattern::CircularDependency {
                severity: Severity::Critical,
                cycle: cycles.into_iter().flat_map(|(a, b)| vec![a, b]).collect(),
            });
        }

        // ... check other patterns

        Ok(patterns)
    }
}
```

---

## 3. Recommendation Engine: Suggesting Improvements

### Pattern Matching and Recommendations

```rust
#[derive(Debug, Clone)]
pub struct Recommendation {
    pub anti_pattern: AntiPattern,
    pub suggestion: String,
    pub example: String,
    pub confidence: f64,
    pub impact: Impact,
}

#[derive(Debug, Clone, Copy)]
pub enum Impact {
    High,    // Prevents bugs, improves reliability
    Medium,  // Improves performance, maintainability
    Low,     // Improves developer experience
}

pub struct RecommendationEngine {
    detector: PatternDetector,
    graph: Graph,
}

impl RecommendationEngine {
    pub fn new(graph: Graph) -> Self {
        Self {
            detector: PatternDetector::new(graph.clone()),
            graph,
        }
    }

    pub fn generate_recommendations(&self, project_id: &str) -> anyhow::Result<Vec<Recommendation>> {
        let anti_patterns = self.detector.detect_all_antipatterns(project_id)?;
        let mut recommendations = Vec::new();

        for pattern in anti_patterns {
            let recommendation = self.pattern_to_recommendation(pattern)?;
            recommendations.push(recommendation);
        }

        // Sort by impact and confidence
        recommendations.sort_by(|a, b| {
            b.confidence.partial_cmp(&a.confidence)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(recommendations)
    }

    fn pattern_to_recommendation(&self, pattern: AntiPattern) -> anyhow::Result<Recommendation> {
        match pattern {
            AntiPattern::MissingTestBeforeBuild { phase, .. } => {
                Ok(Recommendation {
                    anti_pattern: pattern,
                    suggestion: format!(
                        "Add tests to run before {} phase to catch errors early",
                        phase
                    ),
                    example: r#"
[hooks]
before_build = ["test", "lint"]

[lifecycle.test]
description = "Run test suite"
command = "cargo test"  # or: npm test, pytest, etc.
"#.to_string(),
                    confidence: 0.95,
                    impact: Impact::High,
                })
            }

            AntiPattern::CircularDependency { cycle, .. } => {
                Ok(Recommendation {
                    anti_pattern: pattern,
                    suggestion: format!(
                        "Break circular dependency: {}",
                        cycle.join(" -> ")
                    ),
                    example: r#"
# Identify the weakest dependency and remove it
# Example: If build -> test -> lint -> build
# Remove test -> build or lint -> build

[hooks]
before_build = ["test", "lint"]  # Keep only necessary deps
# Don't make build depend on test if test depends on build
"#.to_string(),
                    confidence: 1.0,
                    impact: Impact::High,
                })
            }

            AntiPattern::InefficientCache { phase, command_count, .. } => {
                Ok(Recommendation {
                    anti_pattern: pattern,
                    suggestion: format!(
                        "Enable caching for {} phase ({} commands) to improve rebuild speed",
                        phase, command_count
                    ),
                    example: format!(r#"
[lifecycle.{}]
description = "..."
commands = [...]
cache = true  # ← Add this
outputs = ["dist/", "target/"]  # Specify output directories
"#, phase),
                    confidence: 0.85,
                    impact: Impact::Medium,
                })
            }

            AntiPattern::HardcodedValues { phase, command, pattern: hardcoded, .. } => {
                Ok(Recommendation {
                    anti_pattern: pattern,
                    suggestion: format!(
                        "Replace hardcoded value '{}' in {} with environment variable",
                        hardcoded, phase
                    ),
                    example: r#"
# Instead of:
command = "curl http://localhost:3000/api"

# Use:
[env.development]
API_URL = "http://localhost:3000"

[env.production]
API_URL = "https://api.example.com"

[lifecycle.deploy]
command = "curl $API_URL/api"
"#.to_string(),
                    confidence: 0.90,
                    impact: Impact::Medium,
                })
            }

            _ => anyhow::bail!("Unhandled anti-pattern: {:?}", pattern),
        }
    }
}
```

### Querying Similar Successful Patterns

```rust
impl RecommendationEngine {
    /// Find similar projects that successfully solved this anti-pattern
    pub fn find_similar_solutions(
        &self,
        anti_pattern: &AntiPattern,
        framework: &str,
    ) -> anyhow::Result<Vec<String>> {
        let pattern_type = match anti_pattern {
            AntiPattern::MissingTestBeforeBuild { .. } => "test_before_build",
            AntiPattern::CircularDependency { .. } => "no_circular_deps",
            _ => "unknown",
        };

        let query = format!(r#"
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX pattern: <http://ggen.io/lifecycle/pattern/>

SELECT ?project ?successRate WHERE {{
    ?project a lc:Project ;
        lc:framework "{framework}" ;
        lc:hasPattern ?pattern .

    ?pattern a lc:BestPractice ;
        lc:patternType "{pattern_type}" ;
        lc:successRate ?successRate .

    # Only return patterns with high success rate
    FILTER(?successRate > 0.8)
}}
ORDER BY DESC(?successRate)
LIMIT 5
"#);

        match self.graph.query_cached(&query)? {
            CachedResult::Solutions(rows) => {
                Ok(rows.into_iter()
                    .filter_map(|row| row.get("project").cloned())
                    .collect())
            }
            _ => Ok(vec![]),
        }
    }
}
```

---

## 4. Learning Loop: Pattern Evolution

### Tracking Pattern Success

```rust
#[derive(Debug, Clone)]
pub struct PatternMetrics {
    pub pattern_id: String,
    pub adoption_count: usize,
    pub success_count: usize,
    pub failure_count: usize,
    pub avg_build_time_before: f64,
    pub avg_build_time_after: f64,
    pub last_updated: chrono::DateTime<chrono::Utc>,
}

pub struct LearningEngine {
    graph: Graph,
}

impl LearningEngine {
    pub fn record_pattern_adoption(
        &self,
        project_id: &str,
        recommendation: &Recommendation,
        adopted: bool,
    ) -> anyhow::Result<()> {
        let pattern_id = format!("pattern_{:?}", recommendation.anti_pattern);
        let timestamp = chrono::Utc::now().to_rfc3339();

        let turtle = format!(r#"
@prefix lc: <http://ggen.io/lifecycle/>
@prefix metric: <http://ggen.io/lifecycle/metric/>
@prefix project: <http://ggen.io/project/>

metric:{pattern_id}/adoption/{project_id} a metric:Adoption ;
    metric:project project:{project_id} ;
    metric:pattern metric:{pattern_id} ;
    metric:adopted {} ;
    metric:timestamp "{timestamp}"^^xsd:dateTime .
"#, adopted);

        self.graph.insert_turtle(&turtle)?;
        Ok(())
    }

    pub fn record_pattern_outcome(
        &self,
        project_id: &str,
        pattern_id: &str,
        success: bool,
        build_time_ms: u64,
    ) -> anyhow::Result<()> {
        let timestamp = chrono::Utc::now().to_rfc3339();

        let turtle = format!(r#"
@prefix lc: <http://ggen.io/lifecycle/>
@prefix metric: <http://ggen.io/lifecycle/metric/>
@prefix project: <http://ggen.io/project/>

metric:{pattern_id}/outcome/{project_id}/{timestamp} a metric:Outcome ;
    metric:project project:{project_id} ;
    metric:pattern metric:{pattern_id} ;
    metric:success {} ;
    metric:buildTime {} ;
    metric:timestamp "{timestamp}"^^xsd:dateTime .
"#, success, build_time_ms);

        self.graph.insert_turtle(&turtle)?;
        Ok(())
    }

    pub fn calculate_pattern_confidence(&self, pattern_id: &str) -> anyhow::Result<f64> {
        let query = format!(r#"
PREFIX metric: <http://ggen.io/lifecycle/metric/>

SELECT (COUNT(?outcome) as ?total)
       (SUM(?success) as ?successCount)
WHERE {{
    ?outcome a metric:Outcome ;
        metric:pattern metric:{pattern_id} ;
        metric:success ?success .
}}
"#);

        match self.graph.query_cached(&query)? {
            CachedResult::Solutions(rows) => {
                if let Some(row) = rows.first() {
                    let total: f64 = row.get("total")
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(0.0);
                    let success: f64 = row.get("successCount")
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(0.0);

                    if total > 0.0 {
                        Ok(success / total)
                    } else {
                        Ok(0.5) // Default confidence for new patterns
                    }
                } else {
                    Ok(0.5)
                }
            }
            _ => Ok(0.5),
        }
    }

    /// Update pattern recommendations based on accumulated metrics
    pub fn update_pattern_rankings(&self) -> anyhow::Result<()> {
        // Query all patterns and their metrics
        let query = r#"
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX metric: <http://ggen.io/lifecycle/metric/>

SELECT ?pattern
       (COUNT(?adoption) as ?adoptions)
       (AVG(?buildTime) as ?avgBuildTime)
WHERE {
    ?adoption a metric:Adoption ;
        metric:pattern ?pattern ;
        metric:adopted true .

    OPTIONAL {
        ?outcome a metric:Outcome ;
            metric:pattern ?pattern ;
            metric:buildTime ?buildTime .
    }
}
GROUP BY ?pattern
ORDER BY DESC(?adoptions)
"#;

        match self.graph.query_cached(query)? {
            CachedResult::Solutions(rows) => {
                for row in rows {
                    let pattern_id = row.get("pattern").unwrap();
                    let adoptions: usize = row.get("adoptions")
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(0);

                    // Update pattern metadata
                    let turtle = format!(r#"
@prefix metric: <http://ggen.io/lifecycle/metric/>

{pattern_id} metric:adoptionCount {adoptions} ;
    metric:lastUpdated "{}"^^xsd:dateTime .
"#, chrono::Utc::now().to_rfc3339());

                    self.graph.insert_turtle(&turtle)?;
                }
            }
            _ => {}
        }

        Ok(())
    }
}
```

### Pattern Learning from CI/CD Results

```rust
impl LearningEngine {
    /// Learn from CI/CD execution results
    pub fn learn_from_execution(
        &self,
        project_id: &str,
        phase: &str,
        success: bool,
        duration_ms: u64,
        logs: &str,
    ) -> anyhow::Result<()> {
        // Extract patterns from successful executions
        if success && duration_ms < 60_000 {
            // Fast build - likely good pattern
            self.promote_to_best_practice(project_id, phase)?;
        } else if !success {
            // Failed build - analyze logs for anti-patterns
            self.detect_antipattern_from_logs(project_id, phase, logs)?;
        }

        Ok(())
    }

    fn promote_to_best_practice(&self, project_id: &str, phase: &str) -> anyhow::Result<()> {
        let turtle = format!(r#"
@prefix lc: <http://ggen.io/lifecycle/>
@prefix pattern: <http://ggen.io/lifecycle/pattern/>
@prefix project: <http://ggen.io/project/>

project:{project_id}/phase/{phase} a lc:BestPractice ;
    lc:learnedFrom project:{project_id} ;
    lc:confidence 0.8 ;
    lc:timestamp "{}"^^xsd:dateTime .
"#, chrono::Utc::now().to_rfc3339());

        self.graph.insert_turtle(&turtle)?;
        Ok(())
    }

    fn detect_antipattern_from_logs(
        &self,
        project_id: &str,
        phase: &str,
        logs: &str,
    ) -> anyhow::Result<()> {
        // Pattern matching on common error messages
        let patterns = vec![
            ("test.*failed", "MissingTestBeforeBuild"),
            ("permission denied", "HardcodedPaths"),
            ("connection refused", "HardcodedUrls"),
            ("circular dependency", "CircularDependency"),
        ];

        for (pattern, anti_pattern_type) in patterns {
            if logs.contains(pattern) {
                let turtle = format!(r#"
@prefix lc: <http://ggen.io/lifecycle/>
@prefix pattern: <http://ggen.io/lifecycle/pattern/>
@prefix project: <http://ggen.io/project/>

project:{project_id}/phase/{phase}/issue a lc:DetectedIssue ;
    lc:antiPatternType "{anti_pattern_type}" ;
    lc:detectedFrom "logs" ;
    lc:timestamp "{}"^^xsd:dateTime .
"#, chrono::Utc::now().to_rfc3339());

                self.graph.insert_turtle(&turtle)?;
            }
        }

        Ok(())
    }
}
```

---

## 5. Team Workflow: Collaboration Features

### Team Pattern Sharing

```rust
pub struct TeamCollaboration {
    graph: Graph,
}

impl TeamCollaboration {
    /// Share a pattern with the team
    pub fn share_pattern(
        &self,
        pattern_id: &str,
        author: &str,
        team_id: &str,
        description: &str,
    ) -> anyhow::Result<()> {
        let turtle = format!(r#"
@prefix lc: <http://ggen.io/lifecycle/>
@prefix team: <http://ggen.io/team/>

team:{team_id}/pattern/{pattern_id} a lc:SharedPattern ;
    lc:patternId pattern:{pattern_id} ;
    lc:author team:{team_id}/member/{author} ;
    lc:description "{description}" ;
    lc:sharedAt "{}"^^xsd:dateTime ;
    lc:visibility "team" .
"#, chrono::Utc::now().to_rfc3339());

        self.graph.insert_turtle(&turtle)?;
        Ok(())
    }

    /// Get patterns shared by team
    pub fn get_team_patterns(&self, team_id: &str) -> anyhow::Result<Vec<String>> {
        let query = format!(r#"
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX team: <http://ggen.io/team/>

SELECT ?patternId ?description ?author WHERE {{
    ?shared a lc:SharedPattern ;
        lc:patternId ?patternId ;
        lc:author ?author ;
        lc:description ?description .

    FILTER(STRSTARTS(STR(?shared), "http://ggen.io/team/{team_id}/"))
}}
ORDER BY DESC(?sharedAt)
"#);

        match self.graph.query_cached(&query)? {
            CachedResult::Solutions(rows) => {
                Ok(rows.into_iter()
                    .filter_map(|row| row.get("patternId").cloned())
                    .collect())
            }
            _ => Ok(vec![]),
        }
    }

    /// Track team adoption metrics
    pub fn get_team_adoption_metrics(&self, team_id: &str) -> anyhow::Result<TeamMetrics> {
        let query = format!(r#"
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX team: <http://ggen.io/team/>
PREFIX metric: <http://ggen.io/lifecycle/metric/>

SELECT
    (COUNT(DISTINCT ?project) as ?projectCount)
    (COUNT(DISTINCT ?pattern) as ?patternCount)
    (AVG(?successRate) as ?avgSuccessRate)
WHERE {{
    ?project a lc:Project ;
        lc:team team:{team_id} ;
        lc:hasPattern ?pattern .

    ?pattern lc:successRate ?successRate .
}}
"#);

        match self.graph.query_cached(&query)? {
            CachedResult::Solutions(rows) => {
                if let Some(row) = rows.first() {
                    Ok(TeamMetrics {
                        project_count: row.get("projectCount")
                            .and_then(|s| s.parse().ok())
                            .unwrap_or(0),
                        pattern_count: row.get("patternCount")
                            .and_then(|s| s.parse().ok())
                            .unwrap_or(0),
                        avg_success_rate: row.get("avgSuccessRate")
                            .and_then(|s| s.parse().ok())
                            .unwrap_or(0.0),
                    })
                } else {
                    Ok(TeamMetrics::default())
                }
            }
            _ => Ok(TeamMetrics::default()),
        }
    }
}

#[derive(Debug, Default)]
pub struct TeamMetrics {
    pub project_count: usize,
    pub pattern_count: usize,
    pub avg_success_rate: f64,
}
```

### Code Review Integration

```rust
impl TeamCollaboration {
    /// Check for anti-patterns during code review
    pub fn review_make_toml_changes(
        &self,
        project_id: &str,
        old_make: &Make,
        new_make: &Make,
    ) -> anyhow::Result<Vec<ReviewComment>> {
        let mut comments = Vec::new();

        // Detect new anti-patterns introduced
        let detector = PatternDetector::new(self.graph.clone());

        // Store new config temporarily
        store_lifecycle_pattern(&self.graph, new_make, &format!("{}_review", project_id))?;

        let anti_patterns = detector.detect_all_antipatterns(&format!("{}_review", project_id))?;

        for pattern in anti_patterns {
            comments.push(ReviewComment {
                severity: match pattern {
                    AntiPattern::CircularDependency { .. } => "error",
                    AntiPattern::MissingTestBeforeBuild { .. } => "warning",
                    _ => "info",
                }.to_string(),
                message: format!("Anti-pattern detected: {:?}", pattern),
                suggestion: RecommendationEngine::new(self.graph.clone())
                    .pattern_to_recommendation(pattern)?
                    .suggestion,
            });
        }

        Ok(comments)
    }
}

#[derive(Debug)]
pub struct ReviewComment {
    pub severity: String,
    pub message: String,
    pub suggestion: String,
}
```

---

## 6. CLI Integration

### Commands for Developers

```bash
# Analyze current make.toml for anti-patterns
ggen lifecycle analyze

# Get recommendations for improvements
ggen lifecycle recommend

# Apply a recommended pattern
ggen lifecycle apply <pattern-id>

# Share a pattern with team
ggen lifecycle share <pattern-name> --description "..."

# View team patterns
ggen lifecycle patterns --team <team-id>

# Learn from CI results
ggen lifecycle learn --ci-logs <path>

# Show team adoption metrics
ggen lifecycle metrics --team <team-id>
```

### Example CLI Output

```bash
$ ggen lifecycle analyze

🔍 Analyzing lifecycle configuration...

Found 3 issues:

❌ CRITICAL: Circular dependency detected
   Cycle: build -> test -> lint -> build
   → Fix: Remove unnecessary dependency in hooks

⚠️  HIGH: Missing test before build
   Phase: build
   → Recommendation: Add tests to before_build hooks

💡 MEDIUM: Inefficient cache strategy
   Phase: build (5 commands, no caching)
   → Recommendation: Enable caching for faster rebuilds

Run `ggen lifecycle recommend` for detailed suggestions.

$ ggen lifecycle recommend

📋 Top Recommendations:

1. Break Circular Dependency (Confidence: 100%)
   Impact: HIGH - Prevents infinite loops

   [hooks]
   before_build = ["test", "lint"]  # ✓ Linear dependency
   # Remove: test -> build, lint -> build

   Similar successful patterns: 12 projects in your org

2. Add Tests Before Build (Confidence: 95%)
   Impact: HIGH - Catch errors early

   [hooks]
   before_build = ["test", "lint"]

   [lifecycle.test]
   command = "cargo test"

   Adopted by: 45 projects, 92% success rate

Apply with: ggen lifecycle apply <pattern-id>
```

---

## 7. Implementation Roadmap

### Phase 1: Core Pattern Detection (Week 1-2)

- [ ] Implement RDF schema for lifecycle patterns
- [ ] Create `PatternDetector` with top 5 anti-patterns
- [ ] Add SPARQL queries for detection
- [ ] Basic CLI: `ggen lifecycle analyze`

### Phase 2: Recommendation Engine (Week 3-4)

- [ ] Implement `RecommendationEngine`
- [ ] Add pattern-to-recommendation mapping
- [ ] Query similar solutions from graph
- [ ] CLI: `ggen lifecycle recommend`

### Phase 3: Learning Loop (Week 5-6)

- [ ] Implement `LearningEngine`
- [ ] Track pattern adoption and outcomes
- [ ] Calculate pattern confidence scores
- [ ] Learn from CI/CD results

### Phase 4: Team Collaboration (Week 7-8)

- [ ] Implement `TeamCollaboration`
- [ ] Pattern sharing functionality
- [ ] Team adoption metrics
- [ ] Code review integration

### Phase 5: Polish & Documentation (Week 9-10)

- [ ] Add comprehensive tests
- [ ] Performance optimization
- [ ] User documentation
- [ ] Team onboarding guide

---

## 8. Success Metrics (80/20 Principle)

### Key Metrics to Track

1. **Anti-Pattern Detection Rate**
   - Target: Detect 80% of issues before they reach production
   - Measure: Issues caught / Total issues

2. **Pattern Adoption Rate**
   - Target: 60% of recommendations adopted within 1 week
   - Measure: Recommendations adopted / Total recommendations

3. **Build Success Rate**
   - Target: 20% improvement after adopting patterns
   - Measure: (Successful builds after) - (Successful builds before)

4. **Time to Resolution**
   - Target: 50% reduction in time to fix lifecycle issues
   - Measure: Time from detection to fix

5. **Team Pattern Reuse**
   - Target: Each pattern reused by 5+ projects
   - Measure: Projects using pattern / Total projects

### Dashboard Query (SPARQL)

```sparql
PREFIX lc: <http://ggen.io/lifecycle/>
PREFIX metric: <http://ggen.io/lifecycle/metric/>
PREFIX team: <http://ggen.io/team/>

SELECT
    ?teamId
    (COUNT(DISTINCT ?project) as ?projects)
    (COUNT(DISTINCT ?pattern) as ?patterns)
    (AVG(?successRate) as ?avgSuccess)
    (SUM(?timesSaved) as ?totalTimesSaved)
WHERE {
    ?project a lc:Project ;
        lc:team ?teamId ;
        lc:hasPattern ?pattern .

    ?pattern lc:successRate ?successRate ;
        lc:adoptionCount ?adoptionCount .

    ?outcome a metric:Outcome ;
        metric:project ?project ;
        metric:pattern ?pattern ;
        metric:timeSavedMs ?timesSaved .
}
GROUP BY ?teamId
ORDER BY DESC(?totalTimesSaved)
```

---

## 9. Example: End-to-End Flow

### Developer Workflow

```rust
// 1. Developer creates/modifies make.toml
let make = load_make("make.toml")?;

// 2. System stores pattern in graph
let graph = Graph::new()?;
store_lifecycle_pattern(&graph, &make, "my-project")?;

// 3. Detect anti-patterns
let detector = PatternDetector::new(graph.clone());
let anti_patterns = detector.detect_all_antipatterns("my-project")?;

// 4. Generate recommendations
let engine = RecommendationEngine::new(graph.clone());
let recommendations = engine.generate_recommendations("my-project")?;

// 5. Display to developer
for rec in &recommendations {
    println!("⚠️  {}", rec.suggestion);
    println!("Example:\n{}", rec.example);
    println!("Confidence: {:.0}%\n", rec.confidence * 100.0);
}

// 6. Developer applies recommendation
let learning = LearningEngine::new(graph.clone());
learning.record_pattern_adoption("my-project", &recommendations[0], true)?;

// 7. Track outcome
// After CI run...
learning.record_pattern_outcome(
    "my-project",
    "pattern_missing_test_before_build",
    true,  // Build succeeded
    45_000, // Build time: 45 seconds
)?;

// 8. Update pattern confidence
let confidence = learning.calculate_pattern_confidence("pattern_missing_test_before_build")?;
println!("Pattern confidence: {:.2}", confidence);  // 0.95

// 9. Share with team
let collab = TeamCollaboration::new(graph.clone());
collab.share_pattern(
    "test_before_build",
    "alice",
    "frontend-team",
    "Always run tests before building to catch errors early",
)?;
```

---

## 10. Conclusion

This integration transforms ggen's lifecycle system from a simple command runner into an **intelligent, self-learning workflow engine** that:

1. **Prevents 80% of common mistakes** through pattern detection
2. **Accelerates team learning** by sharing successful patterns
3. **Continuously improves** by learning from CI/CD results
4. **Provides actionable recommendations** based on real data
5. **Tracks impact** with measurable success metrics

By focusing on the 20% of patterns that prevent 80% of problems, we create a system that delivers immediate value while continuously evolving to meet team needs.

The RDF knowledge graph enables powerful querying, pattern matching, and recommendation generation while maintaining semantic clarity and extensibility.

**Next Steps:**
1. Review this design with the core team
2. Prioritize Phase 1 implementation
3. Set up metrics dashboard
4. Create user documentation
5. Plan team onboarding
