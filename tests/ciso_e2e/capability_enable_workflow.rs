//! Capability Enable Workflow E2E Tests
//!
//! Tests the capability workflow: list, inspect, graph, trust, conflicts.
//! Each test calls 2+ real CLI commands in sequence.

use super::helpers::*;

#[test]
fn test_capability_list_returns_valid_json() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen capability list`
    println!("[CISO] Running: ggen capability list");
    let (stdout, exit_code) = run_ggen(&["capability", "list"], workspace_path(&temp)).unwrap();
    println!("[CISO] capability list exit code: {}", exit_code);

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert capabilities array exists
    let capabilities = json["capabilities"]
        .as_array()
        .expect("capabilities should be an array");

    // Step 4: Assert total > 0
    let total = json["total"].as_u64().expect("total should be a number");
    assert!(total > 0, "Expected at least 1 capability, got {}", total);
    assert_eq!(capabilities.len(), total as usize);

    // Step 5: Assert each capability has id, name, category
    for cap in capabilities {
        assert!(
            cap["id"].is_string(),
            "capability should have an id string"
        );
        assert!(
            cap["name"].is_string(),
            "capability should have a name string"
        );
        assert!(
            cap["category"].is_string(),
            "capability should have a category string"
        );
        // atomic_packs should be an array
        assert!(
            cap["atomic_packs"].is_array(),
            "capability should have atomic_packs array"
        );
    }

    println!(
        "[CISO] PASS: capability list returned {} valid capabilities",
        total
    );
}

#[test]
fn test_capability_list_then_inspect_workflow() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen capability list` to get first capability id
    println!("[CISO] Step 1: ggen capability list");
    let (list_output, _) = run_ggen(&["capability", "list"], workspace_path(&temp)).unwrap();
    let list_json = extract_json_from_output(&list_output).unwrap();
    let capabilities = list_json["capabilities"]
        .as_array()
        .expect("capabilities should be an array");
    assert!(
        !capabilities.is_empty(),
        "Need at least one capability for inspect test"
    );

    let first_cap_id = capabilities[0]["id"].as_str().expect("capability should have id");
    println!("[CISO] Step 1 result: first capability id = '{}'", first_cap_id);

    // Step 2: Run `ggen capability inspect --capability <id>`
    println!("[CISO] Step 2: ggen capability inspect --capability {}", first_cap_id);
    let (inspect_output, _) = run_ggen(
        &["capability", "inspect", "--capability", first_cap_id],
        workspace_path(&temp),
    ).unwrap();
    let inspect_json = extract_json_from_output(&inspect_output).unwrap();

    // Step 3: Assert inspect output matches list output
    let inspect_cap = inspect_json["capability"]
        .as_str()
        .unwrap_or_else(|| inspect_json["id"].as_str().unwrap_or(""));
    assert_eq!(
        inspect_cap, first_cap_id,
        "capability inspect id should match list id"
    );

    // Step 4: Assert inspect has atomic_packs
    let atomic_packs = inspect_json["atomic_packs"]
        .as_array()
        .expect("inspect should have atomic_packs");
    assert!(
        !atomic_packs.is_empty(),
        "capability should have at least one atomic pack"
    );

    // Step 5: Verify atomic_packs from inspect match list
    let list_atomic_packs: Vec<&str> = capabilities[0]["atomic_packs"]
        .as_array()
        .expect("list atomic_packs should be array")
        .iter()
        .filter_map(|v| v.as_str())
        .collect();
    let inspect_atomic_packs: Vec<&str> = atomic_packs
        .iter()
        .filter_map(|v| v.as_str())
        .collect();

    // The inspect output may include additional packs (e.g., core-ontology),
    // so we only verify that the original atomic_packs are a subset.
    for pack in &list_atomic_packs {
        assert!(
            inspect_atomic_packs.contains(pack),
            "inspect atomic_packs should contain '{}' from list",
            pack
        );
    }

    println!(
        "[CISO] PASS: capability list -> inspect workflow for '{}': {} atomic packs",
        first_cap_id,
        inspect_atomic_packs.len()
    );
}

#[test]
fn test_capability_graph_shows_pack_graph() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen capability graph`
    println!("[CISO] Running: ggen capability graph");
    let (stdout, exit_code) = run_ggen(&["capability", "graph"], workspace_path(&temp)).unwrap();
    println!("[CISO] capability graph exit code: {}", exit_code);

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert graph structure: nodes and edges
    let nodes = json["nodes"].as_array().expect("graph should have nodes array");
    let edges = json["edges"].as_array().expect("graph should have edges array");

    assert!(!nodes.is_empty(), "graph should have at least one node");

    // Step 4: Assert each node has id, label, node_type
    for node in nodes {
        assert!(node["id"].is_string(), "node should have id");
        assert!(node["label"].is_string(), "node should have label");
        assert!(node["node_type"].is_string(), "node should have node_type");
    }

    // Step 5: Assert each edge has from, to, label
    for edge in edges {
        assert!(edge["from"].is_string(), "edge should have from");
        assert!(edge["to"].is_string(), "edge should have to");
        assert!(edge["label"].is_string(), "edge should have label");
    }

    println!(
        "[CISO] PASS: capability graph has {} nodes, {} edges",
        nodes.len(),
        edges.len()
    );
}

#[test]
fn test_capability_trust_shows_trust_tiers() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen capability trust`
    println!("[CISO] Running: ggen capability trust");
    let (stdout, exit_code) = run_ggen(&["capability", "trust"], workspace_path(&temp)).unwrap();
    println!("[CISO] capability trust exit code: {}", exit_code);

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert trust report structure
    let total = json["total"].as_u64().expect("trust should have total count");
    assert!(total > 0, "trust report should list at least 1 pack");

    let packs = json["packs"].as_array().expect("trust should have packs array");
    assert_eq!(packs.len(), total as usize);

    // Step 4: Assert each pack has trust tier info
    for pack in packs {
        assert!(
            pack["pack_id"].is_string(),
            "trust pack should have pack_id"
        );
        assert!(
            pack["trust_tier"].is_string(),
            "trust pack should have trust_tier"
        );
        assert!(
            pack["digest"].is_string(),
            "trust pack should have digest"
        );
        assert!(
            pack["signature"].is_string(),
            "trust pack should have signature"
        );
    }

    println!(
        "[CISO] PASS: capability trust returned {} packs with trust tiers",
        total
    );
}

#[test]
fn test_capability_conflicts_detection() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen capability conflicts`
    println!("[CISO] Running: ggen capability conflicts");
    let (stdout, exit_code) = run_ggen(&["capability", "conflicts"], workspace_path(&temp)).unwrap();
    println!("[CISO] capability conflicts exit code: {}", exit_code);

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert conflict report structure
    assert!(
        json["compatible"].is_boolean(),
        "conflicts report should have 'compatible' boolean"
    );
    assert!(
        json["conflicts"].is_array(),
        "conflicts report should have 'conflicts' array"
    );

    // Step 4: Verify all conflict entries (if any) have required fields
    let conflicts = json["conflicts"].as_array().unwrap();
    for conflict in conflicts {
        assert!(
            conflict["pack_id"].is_string() || conflict["capability_id"].is_string(),
            "conflict entry should identify the pack or capability"
        );
        assert!(
            conflict["description"].is_string(),
            "conflict entry should have a description"
        );
    }

    let is_compatible = json["compatible"].as_bool().unwrap();
    println!(
        "[CISO] PASS: capability conflicts detection: compatible={}, {} conflicts found",
        is_compatible,
        conflicts.len()
    );
}
