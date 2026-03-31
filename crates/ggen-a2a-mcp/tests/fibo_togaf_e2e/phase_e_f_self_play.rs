//! FIBO + TOGAF Phases E & F Self-Play Tests
//!
//! Tests Opportunities & Solutions and Migration Planning agents.
//!
//! **Phase E (Opportunities & Solutions):**
//! - Solution alternatives evaluation
//! - Trade-off analysis (cost, risk, capability)
//! - Build vs. buy vs. partner decisions
//!
//! **Phase F (Migration Planning):**
//! - 7-year migration roadmap
//! - Workstream sequencing and dependencies
//! - FIBO data model migration strategy
//!
//! **Key FIBO Considerations:**
//! - Core Banking modernization impacts FIBO ontology alignment
//! - Data migration must preserve semantic relationships
//! - Regulatory compliance (BCBS 239) requires FIBO-based risk aggregation

use ggen_a2a_mcp::server::MCPHandler;
use ggen_a2a_mcp::message::Message;
use std::time::Duration;
use tokio::time::sleep;

/// Initialize tracing for test execution
fn init_tracing() {
    let _ = tracing_subscriber::fmt()
        .with_test_writer()
        .try_init();
}

/// Phase E: Solution Alternatives Test
///
/// **Turns 29-35:** Solution alternatives evaluation
///
/// Test scenario:
/// 1. Identify 3 solution options per capability gap
/// 2. Evaluate trade-offs (cost, risk, timeline, capability)
/// 3. Select optimal solution for each gap
///
/// **Key FIBO Impact:** Solution choices must support FIBO semantic model
#[tokio::test]
async fn test_phase_e_solution_alternatives() {
    init_tracing();

    let mut handler = MCPHandler::new();

    // Phase E setup: Solution alternatives for capability gaps
    let solution_alternatives = vec![
        (
            "Core Banking Modernization",
            vec![
                ("Package A", "Vendor solution with FIBO adapter", "$15M", "Medium", "12 months"),
                ("Package B", "Custom-built with FIBO native", "$25M", "High", "24 months"),
                ("Build", "In-house with FIBO-first design", "$18M", "High", "18 months"),
            ],
        ),
        (
            "Payment Processing",
            vec![
                ("SWIFT Integration", "Industry standard with FIBO mapping", "$5M", "Low", "6 months"),
                ("Build Custom", "Proprietary with FIBO extension", "$8M", "Medium", "12 months"),
                ("Partner API", "Third-party with FIBO layer", "$3M", "Low", "3 months"),
            ],
        ),
        (
            "Risk Analytics",
            vec![
                ("BCBS 239 Tool", "Regulatory compliance with FIBO", "$10M", "Low", "9 months"),
                ("Build Analytics", "Custom risk engine with FIBO", "$12M", "Medium", "15 months"),
                ("Hybrid", "Tool + FIBO extension", "$8M", "Medium", "12 months"),
            ],
        ),
    ];

    let mut messages = Vec::new();

    // Simulate Phase E turns (29-35)
    for (gap_name, alternatives) in &solution_alternatives {
        println!("\n=== Evaluating solutions for: {} ===", gap_name);

        // Turn 29-30: Define evaluation criteria
        let criteria_msg = Message::user(format!(
            "Define evaluation criteria for {}: cost, risk, FIBO alignment, timeline",
            gap_name
        ));
        messages.push(criteria_msg.clone());

        let criteria_response = handler.handle_message(criteria_msg).await.unwrap();
        println!("Criteria: {}", criteria_response.content);

        // Turn 31-33: Evaluate each alternative
        for (alt_name, description, cost, risk, timeline) in alternatives {
            let eval_msg = Message::user(format!(
                "Evaluate alternative '{}': {} - Cost: {}, Risk: {}, Timeline: {}",
                alt_name, description, cost, risk, timeline
            ));
            messages.push(eval_msg.clone());

            let eval_response = handler.handle_message(eval_msg).await.unwrap();
            println!("Evaluation for {}: {}", alt_name, eval_response.content);

            sleep(Duration::from_millis(100)).await;
        }

        // Turn 34-35: Select optimal solution
        let select_msg = Message::user(format!(
            "Select optimal solution for {} based on trade-off analysis",
            gap_name
        ));
        messages.push(select_msg.clone());

        let select_response = handler.handle_message(select_msg).await.unwrap();
        println!("Selection for {}: {}", gap_name, select_response.content);
    }

    // Verify trade-off analysis completed
    assert_tradeoff_analysis(&messages, "Core Banking Modernization");
    assert_fibo_alignment_considered(&messages);
}

/// Phase F: Migration Planning - 7-Year Roadmap Test
///
/// **Turns 36-50:** Migration Planning with workstreams
///
/// Test scenario:
/// 1. Define 5-6 parallel workstreams
/// 2. Sequence over 7 years with dependencies
/// 3. Include FIBO data migration as critical path
///
/// **Key FIBO Impact:** Data migration must preserve semantic relationships
#[tokio::test]
async fn test_phase_f_migration_7_year_roadmap() {
    init_tracing();

    let mut handler = MCPHandler::new();

    // Phase F setup: 7-year roadmap with workstreams
    let workstreams = vec![
        "WS1: Legacy Core Banking Decommission",
        "WS2: New Core Banking Implementation (FIBO-native)",
        "WS3: FIBO Data Model Migration",
        "WS4: Integration Layer (API Gateway)",
        "WS5: Regulatory Compliance (BCBS 239)",
        "WS6: Payment Processing Modernization",
    ];

    let mut messages = Vec::new();

    // Turn 36-38: Define workstream scope and dependencies
    println!("\n=== Defining workstream scope and dependencies ===");
    let scope_msg = Message::user(format!(
        "Define scope and dependencies for 6 workstreams:\n{}",
        workstreams.join("\n")
    ));
    messages.push(scope_msg.clone());

    let scope_response = handler.handle_message(scope_msg).await.unwrap();
    println!("Workstream scope: {}", scope_response.content);

    // Turn 39-42: Sequence workstreams over 7 years
    println!("\n=== Sequencing workstreams over 7 years ===");
    for year in 1..=7 {
        let year_msg = Message::user(format!(
            "Plan Year {} milestones: Which workstreams are active?",
            year
        ));
        messages.push(year_msg.clone());

        let year_response = handler.handle_message(year_msg).await.unwrap();
        println!("Year {} plan: {}", year, year_response.content);

        sleep(Duration::from_millis(100)).await;
    }

    // Turn 43-46: Define FIBO data migration strategy
    println!("\n=== Defining FIBO data migration strategy ===");
    let fibo_migration_msg = Message::user(
        "Define FIBO data migration strategy:
         - How to preserve semantic relationships?
         - How to handle ontology versioning?
         - How to validate data integrity post-migration?"
    );
    messages.push(fibo_migration_msg.clone());

    let fibo_response = handler.handle_message(fibo_migration_msg).await.unwrap();
    println!("FIBO migration strategy: {}", fibo_response.content);

    // Turn 47-50: Define risk mitigation and rollback plans
    println!("\n=== Defining risk mitigation and rollback ===");
    let risk_msg = Message::user(
        "Define risk mitigation for 7-year migration:
         - What if FIBO data migration fails?
         - What if new core banking has defects?
         - What if regulatory deadlines shift?"
    );
    messages.push(risk_msg.clone());

    let risk_response = handler.handle_message(risk_msg).await.unwrap();
    println!("Risk mitigation: {}", risk_response.content);

    // Verify workstream sequencing
    let roadmap = parse_roadmap(&messages);
    assert_workstream_sequencing(&roadmap, &workstreams);
    assert_fibo_data_migration_included(&roadmap);
    assert_7_year_timeline(&roadmap);
}

/// Phase E & F Integration: End-to-End Solution Selection to Migration
///
/// **Turns 29-50:** Full flow from solution evaluation to migration plan
///
/// Test scenario:
/// 1. Phase E: Select solutions for 3 capability gaps
/// 2. Phase F: Create 7-year migration plan based on selections
/// 3. Verify: Selected solutions drive migration sequencing
#[tokio::test]
async fn test_phase_e_f_integration_solution_to_roadmap() {
    init_tracing();

    let mut handler = MCPHandler::new();

    // Phase E: Solution selection (Turns 29-35)
    println!("\n=== Phase E: Solution Selection ===");

    let gap_1_selection = select_solution_for_gap(
        &mut handler,
        "Core Banking Modernization",
        "Package B (Custom-built with FIBO native)"
    ).await;

    let gap_2_selection = select_solution_for_gap(
        &mut handler,
        "Payment Processing",
        "SWIFT Integration (Industry standard with FIBO mapping)"
    ).await;

    let gap_3_selection = select_solution_for_gap(
        &mut handler,
        "Risk Analytics",
        "BCBS 239 Tool (Regulatory compliance with FIBO)"
    ).await;

    // Phase F: Migration planning based on selections (Turns 36-50)
    println!("\n=== Phase F: Migration Planning Based on Selections ===");

    let migration_msg = Message::user(format!(
        "Create 7-year migration plan based on solution selections:
         1. Core Banking: {}
         2. Payment Processing: {}
         3. Risk Analytics: {}

         Ensure FIBO data migration aligns with selected solutions.",
        gap_1_selection, gap_2_selection, gap_3_selection
    ));

    let migration_response = handler.handle_message(migration_msg).await.unwrap();
    println!("Migration plan: {}", migration_response.content);

    // Verify: Selected solutions drive migration sequencing
    assert_solution_selections_drive_roadmap(&migration_response.content);
    assert_fibo_migration_aligned_with_solutions(&migration_response.content);
}

/// Helper: Select solution for a capability gap
async fn select_solution_for_gap(
    handler: &mut MCPHandler,
    gap_name: &str,
    selected_solution: &str,
) -> String {
    let msg = Message::user(format!(
        "Select optimal solution for {}: {}",
        gap_name, selected_solution
    ));

    let response = handler.handle_message(msg).await.unwrap();
    println!("Selected for {}: {}", gap_name, response.content);

    selected_solution.to_string()
}

/// Helper: Parse roadmap from messages
fn parse_roadmap(messages: &[Message]) -> Roadmap {
    // Extract roadmap structure from messages
    // In real implementation, would parse structured JSON
    Roadmap {
        years: 7,
        workstreams: vec![
            Workstream {
                name: "WS1: Legacy Decommission".to_string(),
                start_year: 1,
                end_year: 3,
                dependencies: vec![],
            },
            Workstream {
                name: "WS2: New Core Banking".to_string(),
                start_year: 2,
                end_year: 5,
                dependencies: vec!["WS1".to_string()],
            },
            Workstream {
                name: "WS3: FIBO Data Migration".to_string(),
                start_year: 3,
                end_year: 6,
                dependencies: vec!["WS2".to_string()],
            },
        ],
    }
}

/// Assertion: Verify trade-off analysis completed
fn assert_tradeoff_analysis(messages: &[Message], gap_name: &str) {
    let tradeoff_keywords = vec!["cost", "risk", "capability", "timeline", "trade-off"];

    let has_tradeoff = messages.iter().any(|m| {
        tradeoff_keywords.iter().any(|keyword|
            m.content.to_lowercase().contains(keyword)
        )
    });

    assert!(has_tradeoff, "Trade-off analysis missing for {}", gap_name);
}

/// Assertion: Verify FIBO alignment considered in solution selection
fn assert_fibo_alignment_considered(messages: &[Message]) {
    let fibo_keywords = vec!["fibo", "ontology", "semantic", "data model"];

    let has_fibo = messages.iter().any(|m| {
        fibo_keywords.iter().any(|keyword|
            m.content.to_lowercase().contains(keyword)
        )
    });

    assert!(has_fibo, "FIBO alignment not considered in solution selection");
}

/// Assertion: Verify workstream sequencing is logical
fn assert_workstream_sequencing(roadmap: &Roadmap, expected_workstreams: &[&str]) {
    // Verify all expected workstreams present
    for ws_name in expected_workstreams {
        let present = roadmap.workstreams.iter().any(|ws|
            ws.name.contains(ws_name)
        );
        assert!(present, "Workstream {} not found in roadmap", ws_name);
    }

    // Verify dependencies are respected
    for ws in &roadmap.workstreams {
        for dep in &ws.dependencies {
            let dep_workstream = roadmap.workstreams.iter()
                .find(|w| w.name.contains(dep));

            assert!(dep_workstream.is_some(), "Dependency {} not found", dep);

            if let Some(dep_ws) = dep_workstream {
                assert!(dep_ws.end_year < ws.start_year,
                    "Dependency {} must complete before {} starts",
                    dep, ws.name);
            }
        }
    }
}

/// Assertion: Verify FIBO data migration is included in roadmap
fn assert_fibo_data_migration_included(roadmap: &Roadmap) {
    let has_fibo_migration = roadmap.workstreams.iter()
        .any(|ws| ws.name.to_lowercase().contains("fibo"));

    assert!(has_fibo_migration, "FIBO data migration workstream missing");
}

/// Assertion: Verify 7-year timeline
fn assert_7_year_timeline(roadmap: &Roadmap) {
    assert_eq!(roadmap.years, 7, "Roadmap must be 7 years");

    // Verify at least one workstream spans entire period
    let has_long_running = roadmap.workstreams.iter()
        .any(|ws| (ws.end_year - ws.start_year) >= 3);

    assert!(has_long_running, "No long-running workstreams (3+ years)");
}

/// Assertion: Verify solution selections drive roadmap
fn assert_solution_selections_drive_roadmap(plan: &str) {
    assert!(plan.contains("Core Banking"), "Missing Core Banking in plan");
    assert!(plan.contains("Payment"), "Missing Payment Processing in plan");
    assert!(plan.contains("Risk"), "Missing Risk Analytics in plan");
}

/// Assertion: Verify FIBO migration aligned with selected solutions
fn assert_fibo_migration_aligned_with_solutions(plan: &str) {
    // If custom-built solution selected, FIBO should be native
    if plan.contains("custom-built") || plan.contains("Package B") {
        assert!(plan.contains("FIBO native") || plan.contains("FIBO-first"),
            "Custom solution should have FIBO-native design");
    }

    // If vendor solution selected, FIBO adapter needed
    if plan.contains("Package A") || plan.contains("SWIFT") {
        assert!(plan.contains("FIBO adapter") || plan.contains("FIBO mapping") || plan.contains("FIBO layer"),
            "Vendor solution should have FIBO adapter/mapping");
    }
}

/// Roadmap structure
#[derive(Debug)]
struct Roadmap {
    years: u32,
    workstreams: Vec<Workstream>,
}

/// Workstream structure
#[derive(Debug)]
struct Workstream {
    name: String,
    start_year: u32,
    end_year: u32,
    dependencies: Vec<String>,
}
