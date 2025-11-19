//! Basic Kaizen Engine Example
//!
//! Demonstrates a complete PDCA cycle for ontology improvement.

use chrono::Utc;
use ggen_kaizen::{
    KaizenConfig, KaizenOrchestrator,
    pdca::ImprovementTarget,
    suggestion::{PainPoint, PainPointCategory, Severity, Evidence, EvidenceSource},
};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing for visibility
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("🌱 Kaizen Ontology Improvement Engine");
    println!("=====================================\n");

    // Create Kaizen orchestrator with custom configuration
    let config = KaizenConfig {
        cycle_interval_secs: 60, // 1 minute for demo
        min_improvement_threshold: 0.05,
        auto_apply_threshold: 0.3,
        compound_factor: 1.05,
        ..Default::default()
    };

    let orchestrator = KaizenOrchestrator::new(config)?;
    println!("✅ Kaizen orchestrator initialized\n");

    // Step 1: Record pain points from developer feedback
    println!("Step 1: Recording pain points...");

    let pain_point1 = PainPoint {
        id: "pp-001".to_string(),
        category: PainPointCategory::Performance,
        description: "SPARQL queries on class hierarchy are slow".to_string(),
        frequency: 15,
        severity: Severity::High,
        first_observed: Utc::now(),
        last_observed: Utc::now(),
        affected_components: vec!["query-engine".to_string(), "class-resolver".to_string()],
        evidence: vec![
            Evidence {
                source: EvidenceSource::PerformanceMetrics,
                description: "Average query time: 2.5s (target: <500ms)".to_string(),
                timestamp: Utc::now(),
                metadata: {
                    let mut map = HashMap::new();
                    map.insert("query_type".to_string(), "hierarchy".to_string());
                    map.insert("avg_time_ms".to_string(), "2500".to_string());
                    map
                },
            },
        ],
    };

    orchestrator.record_pain_point(pain_point1).await?;

    let pain_point2 = PainPoint {
        id: "pp-002".to_string(),
        category: PainPointCategory::SemanticClarity,
        description: "Property relationships are ambiguous".to_string(),
        frequency: 8,
        severity: Severity::Medium,
        first_observed: Utc::now(),
        last_observed: Utc::now(),
        affected_components: vec!["property-definitions".to_string()],
        evidence: vec![
            Evidence {
                source: EvidenceSource::UserFeedback,
                description: "Developers confused about property usage".to_string(),
                timestamp: Utc::now(),
                metadata: HashMap::new(),
            },
        ],
    };

    orchestrator.record_pain_point(pain_point2).await?;
    println!("✅ Recorded 2 pain points\n");

    // Step 2: Generate improvement suggestions
    println!("Step 2: Generating improvement suggestions...");
    let suggestions = orchestrator.get_suggestions().await;

    println!("✅ Generated {} suggestions:", suggestions.len());
    for (i, suggestion) in suggestions.iter().enumerate() {
        println!("\n  Suggestion {}:", i + 1);
        println!("    Title: {}", suggestion.title);
        println!("    Priority: {:?}", suggestion.priority);
        println!("    ROI: {:.2}", suggestion.estimated_impact.roi());
        println!("    Impact Score: {:.2}", suggestion.estimated_impact.overall_score());
        println!("    Estimated Effort: {:.1}h", suggestion.estimated_impact.effort_hours);
    }
    println!();

    // Step 3: Start improvement cycle
    println!("Step 3: Starting PDCA improvement cycle...");
    let target = ImprovementTarget::OntologyStructure {
        ontology_uri: "http://example.org/ontology".to_string(),
        focus_area: "class hierarchy optimization".to_string(),
    };

    let cycle_id = orchestrator.start_cycle(target).await?;
    println!("✅ Started PDCA cycle: {}\n", cycle_id);

    // Step 4: Check quality score
    println!("Step 4: Checking ontology quality...");
    let quality_score = orchestrator.get_quality_score().await;
    println!("✅ Current ontology quality score: {:.1}%\n", quality_score * 100.0);

    // Step 5: Generate standard work documentation
    println!("Step 5: Generating standard work documentation...");
    let documentation = orchestrator.generate_documentation(
        "Ontology Development Standards".to_string(),
        "Standardized procedures for ontology development and maintenance".to_string(),
        "All ontology development activities".to_string(),
    ).await?;

    println!("✅ Generated documentation ({} bytes)", documentation.len());
    println!("\nFirst 500 characters of documentation:");
    println!("---");
    println!("{}", &documentation[..documentation.len().min(500)]);
    if documentation.len() > 500 {
        println!("...(truncated)");
    }
    println!("---\n");

    println!("🎉 Kaizen improvement cycle complete!");
    println!("\nKey Principles:");
    println!("  • Small, incremental changes compound over time");
    println!("  • Evidence-based improvements from real pain points");
    println!("  • PDCA cycles ensure continuous improvement");
    println!("  • Automated standard work documentation");
    println!("  • Quality metrics track progress");

    Ok(())
}
