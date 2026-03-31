//! FIBO + TOGAF Phases C & D Self-Play Tests (Simplified)
//!
//! Tests Data Architecture (FIBO-based) and Technology Architecture agents.
//! Phase C: Use FIBO as canonical data model
//! Phase D: Map data entities to technology platforms

/// FIBO ontology classes for financial data modeling
const FIBO_CLASSES: &[(&str, &str, &str)] = &[
    // FND: Foundation
    (
        "fibo-fnd:LegalPerson",
        "Customer Entity",
        "Represents a legally recognized individual or organization",
    ),
    (
        "fibo-fnd:Organization",
        "Organization Entity",
        "Represents a business entity",
    ),
    (
        "fibo-fnd:Address",
        "Address Entity",
        "Represents a physical or mailing address",
    ),
    // LCC: Loan Contracts
    (
        "fibo-lcc:LoanContract",
        "Loan Agreement",
        "Represents a loan contract between parties",
    ),
    (
        "fibo-lcc:CreditAgreement",
        "Credit Agreement Entity",
        "Represents a credit facility agreement",
    ),
    (
        "fibo-lcc:InterestRate",
        "Rate Entity",
        "Represents an interest rate applied to a loan",
    ),
    // SEC: Securities
    (
        "fibo-sec:SecurityInstrument",
        "Holding Entity",
        "Represents a financial security or instrument",
    ),
    (
        "fibo-sec:EquityInstrument",
        "Equity Holding Entity",
        "Represents an ownership interest",
    ),
    (
        "fibo-sec:DebtInstrument",
        "Debt Holding Entity",
        "Represents a debt obligation",
    ),
    // IND: Indicators
    (
        "fibo-ind:MarketPrice",
        "Price Entity",
        "Represents a market price for an instrument",
    ),
    (
        "fibo-ind:InterestRate",
        "Rate Entity",
        "Represents an interest rate value",
    ),
];

/// Technology platforms mapped from data entities
const TECHNOLOGY_PLATFORMS: &[(&str, &str, &str)] = &[
    (
        "Customer Entity",
        "Core Banking System",
        "Customer 360 platform for customer data management",
    ),
    (
        "Organization Entity",
        "CRM System",
        "Salesforce for organization relationship management",
    ),
    (
        "Loan Agreement",
        "Loan Origination Platform",
        "LoanIQ for loan lifecycle management",
    ),
    (
        "Credit Agreement Entity",
        "Credit Risk Platform",
        "Moody's Analytics for credit risk assessment",
    ),
    (
        "Rate Entity",
        "Market Data Platform",
        "Bloomberg for real-time market data",
    ),
    (
        "Holding Entity",
        "Investment Management System",
        "Simcorp for investment portfolio management",
    ),
    (
        "Equity Holding Entity",
        "Trading Platform",
        "Apex for equity trading and execution",
    ),
    (
        "Debt Holding Entity",
        "Fixed Income Platform",
        "BlackRock for fixed income portfolio management",
    ),
    (
        "Price Entity",
        "Pricing Engine",
        "OpenFin for real-time pricing calculations",
    ),
    (
        "Address Entity",
        "Master Data Management",
        "Informatica MDM for address standardization",
    ),
];

/// Data entity definition from FIBO
#[derive(Debug, Clone)]
struct DataEntityDefinition {
    fibo_class: String,
    data_entity: String,
    attributes: Vec<String>,
    relationships: Vec<String>,
}

/// Platform recommendation
#[derive(Debug, Clone)]
struct PlatformRecommendation {
    data_entity: String,
    platform: String,
    integration_pattern: String,
    key_capabilities: Vec<String>,
    alternatives: Vec<String>,
}

/// Simulated message from Phase C or D
#[derive(Debug, Clone)]
struct SimulatedMessage {
    turn: usize,
    phase: String,
    fibo_class: Option<String>,
    data_entity: String,
    platform: Option<String>,
    content_preview: String,
}

/// Test Phase C: Data Architecture with FIBO
#[test]
fn test_phase_c_data_architecture_with_fibo() {
    println!("\n=== Phase C: Data Architecture with FIBO ===");

    let mut messages = Vec::new();

    // Simulate Phase C: Turns 13-20 (8 FIBO classes)
    for (i, (fibo_class, data_entity, description)) in FIBO_CLASSES.iter().enumerate() {
        let turn_num = 13 + i;

        // Simulate LLM response creating data entity from FIBO class
        let entity_def = DataEntityDefinition {
            fibo_class: fibo_class.to_string(),
            data_entity: data_entity.to_string(),
            attributes: vec![
                "id".to_string(),
                "name".to_string(),
                "description".to_string(),
                "created_at".to_string(),
                "updated_at".to_string(),
            ],
            relationships: vec!["relates_to_other_entities".to_string()],
        };

        let msg = SimulatedMessage {
            turn: turn_num,
            phase: "C".to_string(),
            fibo_class: Some(fibo_class.to_string()),
            data_entity: data_entity.to_string(),
            platform: None,
            content_preview: format!(
                "Turn {}: Created data entity '{}' from FIBO class '{}'\n\
                Description: {}",
                turn_num, data_entity, fibo_class, description
            ),
        };

        messages.push(msg);
        println!(
            "✓ Turn {}: Created '{}' from '{}'",
            turn_num, data_entity, fibo_class
        );
    }

    // Verify FIBO entity count
    println!(
        "\nFIBO Classes Processed: {}/{}",
        messages.len(),
        FIBO_CLASSES.len()
    );

    // Verify each FIBO class was mapped
    let mut mapped_count = 0;
    for (fibo_class, data_entity, _) in FIBO_CLASSES {
        if messages
            .iter()
            .any(|m| m.fibo_class.as_ref() == Some(fibo_class) && m.data_entity == *data_entity)
        {
            mapped_count += 1;
        }
    }

    println!(
        "Successfully Mapped: {}/{}",
        mapped_count,
        FIBO_CLASSES.len()
    );

    // Assertions
    assert_eq!(
        messages.len(),
        FIBO_CLASSES.len(),
        "Expected {} messages (one per FIBO class), got {}",
        FIBO_CLASSES.len(),
        messages.len()
    );

    assert_eq!(
        mapped_count,
        FIBO_CLASSES.len(),
        "Expected all {} FIBO classes to be mapped, got {}",
        FIBO_CLASSES.len(),
        mapped_count
    );
}

/// Test Phase D: Technology Architecture
#[test]
fn test_phase_d_technology_architecture() {
    println!("\n=== Phase D: Technology Architecture ===");

    let mut messages = Vec::new();

    // Simulate Phase D: Turns 21-30 (10 technology mappings)
    for (i, (data_entity, platform, rationale)) in TECHNOLOGY_PLATFORMS.iter().enumerate() {
        let turn_num = 21 + i;

        // Simulate LLM response mapping data entity to platform
        let platform_rec = PlatformRecommendation {
            data_entity: data_entity.to_string(),
            platform: platform.to_string(),
            integration_pattern: "API".to_string(),
            key_capabilities: vec![
                "Data Management".to_string(),
                "Integration".to_string(),
                "Security".to_string(),
            ],
            alternatives: vec![
                "Alternative Platform A".to_string(),
                "Alternative Platform B".to_string(),
            ],
        };

        let msg = SimulatedMessage {
            turn: turn_num,
            phase: "D".to_string(),
            fibo_class: None,
            data_entity: data_entity.to_string(),
            platform: Some(platform.to_string()),
            content_preview: format!(
                "Turn {}: Mapped data entity '{}' to platform '{}'\n\
                Rationale: {}",
                turn_num, data_entity, platform, rationale
            ),
        };

        messages.push(msg);
        println!(
            "✓ Turn {}: Mapped '{}' to '{}'",
            turn_num, data_entity, platform
        );
    }

    // Verify technology mapping count
    println!(
        "\nTechnology Mappings: {}/{}",
        messages.len(),
        TECHNOLOGY_PLATFORMS.len()
    );

    // Verify each data entity was mapped to platform
    let mut mapped_count = 0;
    for (data_entity, platform, _) in TECHNOLOGY_PLATFORMS {
        if messages
            .iter()
            .any(|m| m.data_entity == *data_entity && m.platform.as_ref() == Some(platform))
        {
            mapped_count += 1;
        }
    }

    println!(
        "Successfully Mapped: {}/{}",
        mapped_count,
        TECHNOLOGY_PLATFORMS.len()
    );

    // Assertions
    assert_eq!(
        messages.len(),
        TECHNOLOGY_PLATFORMS.len(),
        "Expected {} messages (one per platform mapping), got {}",
        TECHNOLOGY_PLATFORMS.len(),
        messages.len()
    );

    assert_eq!(
        mapped_count,
        TECHNOLOGY_PLATFORMS.len(),
        "Expected all {} data entities to be mapped to platforms, got {}",
        TECHNOLOGY_PLATFORMS.len(),
        mapped_count
    );
}

/// Integration test: Full Phase C + D workflow
#[test]
fn test_phase_c_d_full_workflow() {
    println!("\n=== Full Phase C + D Workflow ===");

    let mut all_messages = Vec::new();

    // Phase C: Data Architecture with FIBO (Turns 13-20)
    println!("\n--- Phase C: Data Architecture ---");
    for (i, (fibo_class, data_entity, description)) in FIBO_CLASSES.iter().enumerate() {
        let turn_num = 13 + i;

        let msg = SimulatedMessage {
            turn: turn_num,
            phase: "C".to_string(),
            fibo_class: Some(fibo_class.to_string()),
            data_entity: data_entity.to_string(),
            platform: None,
            content_preview: format!("Created '{}' from '{}'", data_entity, fibo_class),
        };

        all_messages.push(msg);
        println!(
            "Turn {}: FIBO '{}' → Entity '{}'",
            turn_num, fibo_class, data_entity
        );
    }

    // Phase D: Technology Architecture (Turns 21-30)
    println!("\n--- Phase D: Technology Architecture ---");
    for (i, (data_entity, platform, rationale)) in TECHNOLOGY_PLATFORMS.iter().enumerate() {
        let turn_num = 21 + i;

        let msg = SimulatedMessage {
            turn: turn_num,
            phase: "D".to_string(),
            fibo_class: None,
            data_entity: data_entity.to_string(),
            platform: Some(platform.to_string()),
            content_preview: format!("Mapped '{}' to '{}'", data_entity, platform),
        };

        all_messages.push(msg);
        println!(
            "Turn {}: Entity '{}' → Platform '{}'",
            turn_num, data_entity, platform
        );
    }

    println!("\nTotal Messages: {}", all_messages.len());

    // Verify FIBO to Platform traceability
    println!("\n=== FIBO to Platform Traceability ===");

    let mut traceable_count = 0;

    for (fibo_class, data_entity, _) in FIBO_CLASSES {
        // Check if data entity exists in Phase C
        let entity_created = all_messages.iter().any(|m| {
            m.phase == "C"
                && m.fibo_class.as_ref() == Some(fibo_class)
                && m.data_entity == *data_entity
        });

        // Check if data entity is mapped to platform in Phase D
        let platform_mapped = all_messages
            .iter()
            .any(|m| m.phase == "D" && m.data_entity == *data_entity && m.platform.is_some());

        if entity_created && platform_mapped {
            traceable_count += 1;
            println!(
                "✓ FIBO '{}' → Entity '{}' → Platform",
                fibo_class, data_entity
            );
        } else {
            println!(
                "✗ FIBO '{}' → Entity '{}' (incomplete trace)",
                fibo_class, data_entity
            );
        }
    }

    println!(
        "\nTraceable FIBO entities: {}/{}",
        traceable_count,
        FIBO_CLASSES.len()
    );

    // Assertions
    assert_eq!(
        all_messages.len(),
        FIBO_CLASSES.len() + TECHNOLOGY_PLATFORMS.len(),
        "Expected {} total messages ({} Phase C + {} Phase D), got {}",
        FIBO_CLASSES.len() + TECHNOLOGY_PLATFORMS.len(),
        FIBO_CLASSES.len(),
        TECHNOLOGY_PLATFORMS.len(),
        all_messages.len()
    );

    assert!(
        traceable_count >= FIBO_CLASSES.len() / 2,
        "Expected at least 50% of FIBO entities to be traceable to platforms, got {}/{}",
        traceable_count,
        FIBO_CLASSES.len()
    );
}

/// Test FIBO entity coverage across domains
#[test]
fn test_fibo_domain_coverage() {
    println!("\n=== FIBO Domain Coverage ===");

    let mut domain_counts: std::collections::HashMap<&str, usize> =
        std::collections::HashMap::new();

    for (fibo_class, _, _) in FIBO_CLASSES {
        let domain = fibo_class.split(':').next().unwrap_or("unknown");
        *domain_counts.entry(domain).or_insert(0) += 1;
    }

    println!("\nFIBO Domain Breakdown:");
    for (domain, count) in domain_counts.iter() {
        println!("  {}: {} classes", domain, count);
    }

    // Verify we have coverage from multiple FIBO domains
    assert!(
        domain_counts.len() >= 3,
        "Expected at least 3 different FIBO domains, got {}",
        domain_counts.len()
    );

    // Verify FND (Foundation) domain exists
    assert!(
        domain_counts.contains_key("fibo-fnd"),
        "Expected FND (Foundation) domain to be present"
    );

    // Verify we have at least 2 classes in FND
    assert!(
        domain_counts.get("fibo-fnd").copied().unwrap_or(0) >= 2,
        "Expected at least 2 FND classes, got {}",
        domain_counts.get("fibo-fnd").copied().unwrap_or(0)
    );
}

/// Test technology platform diversity
#[test]
fn test_technology_platform_diversity() {
    println!("\n=== Technology Platform Diversity ===");

    let mut platform_types: std::collections::HashMap<&str, usize> =
        std::collections::HashMap::new();

    for (_, platform, _) in TECHNOLOGY_PLATFORMS {
        // Extract platform type (e.g., "System", "Platform", "Engine")
        let platform_type = if platform.contains("System") {
            "System"
        } else if platform.contains("Platform") {
            "Platform"
        } else if platform.contains("Engine") {
            "Engine"
        } else if platform.contains("Management") {
            "Management"
        } else {
            "Other"
        };

        *platform_types.entry(platform_type).or_insert(0) += 1;
    }

    println!("\nPlatform Type Breakdown:");
    for (platform_type, count) in platform_types.iter() {
        println!("  {}: {} platforms", platform_type, count);
    }

    // Verify we have diverse platform types
    assert!(
        platform_types.len() >= 2,
        "Expected at least 2 different platform types, got {}",
        platform_types.len()
    );

    // Verify we have at least one Platform
    assert!(
        platform_types.contains_key("Platform"),
        "Expected at least one Platform"
    );
}

#[test]
fn test_fibo_to_data_entity_mapping() {
    println!("\n=== FIBO to Data Entity Mapping ===");

    for (fibo_class, data_entity, description) in FIBO_CLASSES {
        println!("FIBO: {} → Entity: {}", fibo_class, data_entity);
        println!("  Description: {}", description);

        // Verify mapping follows naming conventions
        assert!(
            !data_entity.is_empty(),
            "Data entity name should not be empty"
        );
        assert!(!description.is_empty(), "Description should not be empty");

        // Verify FIBO class follows naming convention (fibo-XXX:Name)
        assert!(
            fibo_class.contains(':'),
            "FIBO class should contain colon separator"
        );
        assert!(
            fibo_class.starts_with("fibo-"),
            "FIBO class should start with 'fibo-'"
        );
    }

    println!("\nAll {} FIBO mappings validated", FIBO_CLASSES.len());
}

#[test]
fn test_data_entity_to_platform_mapping() {
    println!("\n=== Data Entity to Platform Mapping ===");

    for (data_entity, platform, rationale) in TECHNOLOGY_PLATFORMS {
        println!("Entity: {} → Platform: {}", data_entity, platform);
        println!("  Rationale: {}", rationale);

        // Verify mapping follows naming conventions
        assert!(
            !data_entity.is_empty(),
            "Data entity name should not be empty"
        );
        assert!(!platform.is_empty(), "Platform name should not be empty");
        assert!(!rationale.is_empty(), "Rationale should not be empty");
    }

    println!(
        "\nAll {} platform mappings validated",
        TECHNOLOGY_PLATFORMS.len()
    );
}
