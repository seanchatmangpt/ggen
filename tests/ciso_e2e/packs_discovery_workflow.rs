//! Packs Discovery Workflow E2E Tests
//!
//! Tests the pack discovery workflow: list, show, search, compatibility.
//! Each test calls 2+ real CLI commands in sequence.

use anyhow::Result;

use crate::helpers::*;

#[test]
fn test_packs_list_returns_valid_json() -> Result<()> {
    let temp = create_temp_workspace()?;

    // Step 1: Run `ggen packs list`
    println!("[CISO] Running: ggen packs list");
    let (stdout, exit_code) = run_ggen(&["packs", "list"], workspace_path(&temp))?;

    println!("[CISO] packs list exit code: {}", exit_code);

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout)?;
    println!("[CISO] packs list JSON keys: {:?}", json.as_object().map(|o| o.keys().collect::<Vec<_>>()));

    // Step 3: Assert total > 0
    let total = json["total"].as_u64().expect("total should be a number");
    assert!(total > 0, "Expected at least 1 pack, got {}", total);

    // Step 4: Assert packs array exists
    let packs = json["packs"].as_array().expect("packs should be an array");
    assert_eq!(packs.len(), total as usize, "packs.len() should match total");

    // Step 5: Assert each pack has id, name, version
    for pack in packs {
        assert!(pack["id"].is_string(), "pack should have an id string");
        assert!(pack["name"].is_string(), "pack should have a name string");
        assert!(pack["version"].is_string(), "pack should have a version string");
    }

    println!("[CISO] PASS: packs list returned {} valid packs", total);
    Ok(())
}

#[test]
fn test_packs_list_then_show_workflow() -> Result<()> {
    let temp = create_temp_workspace()?;

    // Step 1: Run `ggen packs list` to get the first pack id
    println!("[CISO] Step 1: ggen packs list");
    let (list_output, _) = run_ggen(&["packs", "list"], workspace_path(&temp))?;
    let list_json = extract_json_from_output(&list_output)?;
    let packs = list_json["packs"].as_array().expect("packs should be an array");
    assert!(!packs.is_empty(), "Need at least one pack for show test");

    let first_pack_id = packs[0]["id"].as_str().expect("first pack should have id");
    println!("[CISO] Step 1 result: first pack id = '{}'", first_pack_id);

    // Step 2: Run `ggen packs show --pack_id <id>` to get pack details
    println!("[CISO] Step 2: ggen packs show --pack_id {}", first_pack_id);
    let (show_output, _show_exit) =
        run_ggen(&["packs", "show", "--pack_id", first_pack_id], workspace_path(&temp))?;
    let show_json = extract_json_from_output(&show_output)?;

    // Step 3: Assert show output has matching id
    let show_pack_id = show_json["pack_id"]
        .as_str()
        .unwrap_or_else(|| show_json["id"].as_str().unwrap_or(""));
    assert_eq!(
        show_pack_id, first_pack_id,
        "packs show id should match the id from packs list"
    );

    // Step 4: Assert show output has description
    let description = show_json["description"]
        .as_str()
        .expect("packs show should have a description");
    assert!(!description.is_empty(), "description should not be empty");

    // Step 5: Assert show output has name
    let name = show_json["name"]
        .as_str()
        .expect("packs show should have a name");
    assert!(!name.is_empty(), "name should not be empty");

    println!(
        "[CISO] PASS: packs list -> show workflow for '{}': name={}",
        first_pack_id, name
    );
    Ok(())
}

#[test]
fn test_packs_search_returns_matching_results() -> Result<()> {
    let temp = create_temp_workspace()?;

    // Step 1: Run `ggen packs search --query web`
    println!("[CISO] Step 1: ggen packs search --query web");
    let (stdout, _exit_code) = run_ggen(&["packs", "search", "--query", "web"], workspace_path(&temp))?;

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout)?;

    // Step 3: Assert results exist
    let total = json["total"].as_u64().expect("total should be a number");
    assert!(total > 0, "Search for 'web' should return at least 1 result, got {}", total);

    let results = json["results"].as_array().expect("results should be an array");
    assert_eq!(results.len(), total as usize);

    // Step 4: Assert query field is present
    let query = json["query"].as_str().expect("query should be a string");
    assert_eq!(query, "web");

    // Step 5: Assert each result has pack_id, name, score
    for result in results {
        assert!(result["pack_id"].is_string(), "result should have pack_id");
        assert!(result["name"].is_string(), "result should have name");
        assert!(result["score"].is_number(), "result should have score");
    }

    println!(
        "[CISO] PASS: packs search 'web' returned {} results",
        total
    );
    Ok(())
}

#[test]
fn test_packs_compatibility() -> Result<()> {
    let temp = create_temp_workspace()?;

    // Step 1: Get two pack ids from packs list
    println!("[CISO] Step 1: ggen packs list (to get pack ids)");
    let (list_output, _) = run_ggen(&["packs", "list"], workspace_path(&temp))?;
    let list_json = extract_json_from_output(&list_output)?;
    let packs = list_json["packs"].as_array().expect("packs should be an array");

    if packs.len() < 2 {
        println!("[CISO] SKIP: need at least 2 packs for compatibility test, got {}", packs.len());
        return Ok(());
    }

    let pack_id_1 = packs[0]["id"].as_str().unwrap();
    let pack_id_2 = packs[1]["id"].as_str().unwrap();
    let pack_ids_arg = format!("{},{}", pack_id_1, pack_id_2);

    // Step 2: Run compatibility check
    println!(
        "[CISO] Step 2: ggen packs compatibility --pack_ids {}",
        pack_ids_arg
    );
    let (compat_output, _) = run_ggen(
        &["packs", "compatibility", "--pack_ids", &pack_ids_arg],
        workspace_path(&temp),
    )?;
    let compat_json = extract_json_from_output(&compat_output)?;

    // Step 3: Assert compatibility report structure
    assert!(
        compat_json["compatible"].is_boolean(),
        "compatibility report should have a 'compatible' boolean"
    );
    assert!(
        compat_json["conflicts"].is_array(),
        "compatibility report should have a 'conflicts' array"
    );
    assert!(
        compat_json["pack_ids"].is_array(),
        "compatibility report should have a 'pack_ids' array"
    );

    let is_compatible = compat_json["compatible"].as_bool().unwrap();
    println!(
        "[CISO] PASS: packs compatibility '{}' and '{}' -> compatible={}",
        pack_id_1, pack_id_2, is_compatible
    );
    Ok(())
}
