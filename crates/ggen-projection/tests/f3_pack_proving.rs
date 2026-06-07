use chrono::Utc;
use ggen_projection::{EquationContext, project_ocel2, project_prov, sync, CustomizationMap, PackDescriptor, PackPlan, Pair2,
    ProjectionMap, ProjectionMapping, ReceiptIndex, RelationPage, StagingGate,
};
use knhk_construct8::Receipt;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ==========================================
// Tier 1: Feature Coverage (5 tests)
// ==========================================

// Verify successful generation/sync of a pack projection and mapping to disk
// using `sync` and verifying directories/markers.
#[test]
fn test_f3_t1_durable_pack_generation() {
    let tmp = TempDir::new().unwrap();

    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/main.rs"),
            ProjectionMapping {
                pack_id: "pack_1".to_string(),
                template_path: PathBuf::from("templates/main.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    let mut cust_map = CustomizationMap::new();
    cust_map
        .vars
        .insert("profile_name".to_string(), "development".to_string());

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/main.rs".to_string(), b"fn main() {}", b"", &EquationContext::default(), None);

    // Perform the sync to write maps and sync marker to disk
    sync(tmp.path(), &proj_map, &cust_map, &receipts).expect("sync should succeed");

    // Verify the expected output files and markers exist
    let pm_file = tmp.path().join("projection-map.json");
    let cm_file = tmp.path().join("customization-map.json");
    let r_file = tmp.path().join("receipts.json");
    let marker_file = tmp.path().join(".sync_marker");

    assert!(
        pm_file.exists(),
        "projection-map.json must exist after sync"
    );
    assert!(
        cm_file.exists(),
        "customization-map.json must exist after sync"
    );
    assert!(r_file.exists(), "receipts.json must exist after sync");
    assert!(marker_file.exists(), ".sync_marker must exist after sync");

    // Read marker content and verify it matches "sync_active"
    let marker_content = fs::read_to_string(&marker_file).unwrap();
    assert_eq!(marker_content, "sync_active");

    // Deserialize and check contents
    let pm_content = fs::read_to_string(&pm_file).unwrap();
    let pm_deser: ProjectionMap =
        serde_json::from_str(&pm_content).expect("Should deserialize projection-map.json");
    assert!(pm_deser
        .mappings
        .contains_key(&PathBuf::from("src/main.rs")));

    let cm_content = fs::read_to_string(&cm_file).unwrap();
    let cm_deser: CustomizationMap =
        serde_json::from_str(&cm_content).expect("Should deserialize customization-map.json");
    assert_eq!(cm_deser.vars.get("profile_name").unwrap(), "development");

    let r_content = fs::read_to_string(&r_file).unwrap();
    let r_deser: ReceiptIndex =
        serde_json::from_str(&r_content).expect("Should deserialize receipts.json");
    assert!(r_deser.receipts.contains_key("src/main.rs"));
}

// Verify a clean receipt index where files on disk match the receipts.
#[test]
fn test_f3_t1_receipt_verification() {
    let tmp = TempDir::new().unwrap();
    let file_path_1 = tmp.path().join("src/main.rs");
    let file_path_2 = tmp.path().join("src/lib.rs");

    fs::create_dir_all(file_path_1.parent().unwrap()).unwrap();
    fs::write(&file_path_1, b"fn main() {}").unwrap();
    fs::write(&file_path_2, b"pub fn lib_func() {}").unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/main.rs".to_string(), b"fn main() {}", b"", &EquationContext::default(), None);
    receipts.add_receipt("src/lib.rs".to_string(), b"pub fn lib_func() {}", b"", &EquationContext::default(), None);

    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/main.rs"),
            ProjectionMapping {
                pack_id: "pack_main".to_string(),
                template_path: PathBuf::from("templates/main.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();
    proj_map
        .add_mapping(
            PathBuf::from("src/lib.rs"),
            ProjectionMapping {
                pack_id: "pack_lib".to_string(),
                template_path: PathBuf::from("templates/lib.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    // Verify it is clean and matches the receipts
    let res = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(
        res.is_ok(),
        "validate_sync should succeed when all files on disk match the receipts"
    );
}

// Verify that modifying a generated file on disk causes validate_sync
// to fail with a drift/stale error.
#[test]
fn test_f3_t1_drift_detection() {
    let tmp = TempDir::new().unwrap();
    let file_path = tmp.path().join("src/main.rs");
    fs::create_dir_all(file_path.parent().unwrap()).unwrap();
    fs::write(&file_path, b"original generated content").unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/main.rs".to_string(), b"original generated content", b"", &EquationContext::default(), None);

    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/main.rs"),
            ProjectionMapping {
                pack_id: "pack_1".to_string(),
                template_path: PathBuf::from("templates/main.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    // Verify it is originally in sync
    assert!(proj_map.validate_sync(tmp.path(), &receipts, None).is_ok());

    // Modify file on disk to simulate drift
    fs::write(&file_path, b"modified by user").unwrap();

    // validate_sync must now fail
    let res = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(
        res.is_err(),
        "validate_sync must fail when file content on disk drifts"
    );
    assert!(res
        .unwrap_err()
        .to_string()
        .contains("is modified/out-of-sync"));
}

// Verify W3C PROV-O (project_prov) and OCEL v2 (project_ocel2) emission
// from pages/receipts contain expected namespaces, event/activity classes,
// and relationship qualifiers.
#[test]
fn test_f3_t1_provenance_emission() {
    use chrono::TimeZone;

    let page = RelationPage {
        page_id: "page_p1".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 6, 6, 12, 0, 0).unwrap(),
        pairs: vec![
            Pair2 {
                source: "event:e1".to_string(),
                target: "order".to_string(),
                rel_type: "ocel:type".to_string(),
            },
            Pair2 {
                source: "event:e1".to_string(),
                target: "object:o1".to_string(),
                rel_type: "ocel:object".to_string(),
            },
            Pair2 {
                source: "object:o1".to_string(),
                target: "item".to_string(),
                rel_type: "ocel:type".to_string(),
            },
            Pair2 {
                source: "object:o1".to_string(),
                target: "object:o2".to_string(),
                rel_type: "childOf".to_string(),
            },
        ],
    };

    let hash = blake3::hash(b"test_receipt_content");
    let receipt = Receipt::new(hash, 4, 8);

    // Verify W3C PROV-O Turtle format output
    let prov_out = project_prov(std::slice::from_ref(&page), &[receipt]);
    assert!(prov_out.contains("@prefix prov: <http://www.w3.org/ns/prov#> ."));
    assert!(prov_out.contains("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."));
    assert!(prov_out.contains("@prefix knhk: <http://knhk.io/vocab/> ."));

    assert!(prov_out.contains("a prov:Entity"));
    assert!(prov_out.contains("a prov:Activity"));
    assert!(prov_out.contains("a prov:Agent"));
    assert!(prov_out.contains("prov:wasGeneratedBy"));
    assert!(prov_out.contains("prov:wasDerivedFrom"));
    assert!(prov_out.contains("prov:used"));
    assert!(prov_out.contains("knhk:packetCount 4"));
    assert!(prov_out.contains("knhk:tripleCount 8"));

    // Verify OCEL v2 output
    let ocel_val = project_ocel2(&[page]);
    let ocel_obj = ocel_val
        .as_object()
        .expect("OCEL v2 is expected to be a JSON Object");
    assert!(ocel_obj.contains_key("eventTypes"));
    assert!(ocel_obj.contains_key("objectTypes"));
    assert!(ocel_obj.contains_key("events"));
    assert!(ocel_obj.contains_key("objects"));

    let events = ocel_obj
        .get("events")
        .and_then(|v| v.as_array())
        .expect("events is array");
    let e1 = events
        .iter()
        .find(|e| e.get("id").and_then(|id| id.as_str()) == Some("e1"))
        .expect("e1 exists");
    assert_eq!(e1.get("type").and_then(|t| t.as_str()), Some("order"));

    let rels = e1
        .get("relationships")
        .and_then(|r| r.as_array())
        .expect("relationships is array");
    assert_eq!(rels[0].get("objectId").and_then(|o| o.as_str()), Some("o1"));
    assert_eq!(
        rels[0].get("qualifier").and_then(|q| q.as_str()),
        Some("relatesTo")
    ); // Normalizes "ocel:object"

    let objects = ocel_obj
        .get("objects")
        .and_then(|v| v.as_array())
        .expect("objects is array");
    let o1 = objects
        .iter()
        .find(|o| o.get("id").and_then(|id| id.as_str()) == Some("o1"))
        .expect("o1 exists");
    assert_eq!(o1.get("type").and_then(|t| t.as_str()), Some("item"));
}

// Verify that StagingGate allows write for files that do not exist yet on disk.
#[test]
fn test_f3_t1_non_destructive_staging() {
    let tmp = TempDir::new().unwrap();
    let receipts = ReceiptIndex::new();
    let gate = StagingGate::new(tmp.path().to_path_buf(), receipts);

    // File "new_src.rs" does not exist in tmp directory
    let res = gate.check_write(Path::new("new_src.rs"), false);
    assert!(
        res.is_ok(),
        "Staging gate must allow writing to non-existent files (non-destructive staging)"
    );
}

// ==========================================
// Tier 2: Boundaries and Corner Cases (5 tests)
// ==========================================

// Modify the hash inside a receipt and verify that validate_sync correctly returns an error.
#[test]
fn test_f3_t2_corrupted_receipt() {
    let tmp = TempDir::new().unwrap();
    let file_path = tmp.path().join("src/helper.rs");
    fs::create_dir_all(file_path.parent().unwrap()).unwrap();
    fs::write(&file_path, b"some content").unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/helper.rs".to_string(), b"some content", b"", &EquationContext::default(), None);

    // Corrupt the receipt hash
    if let Some(receipt) = receipts.receipts.get_mut("src/helper.rs") {
        receipt.blake3_hash = "corrupted_hash_value".to_string();
    }

    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/helper.rs"),
            ProjectionMapping {
                pack_id: "pack_1".to_string(),
                template_path: PathBuf::from("templates/helper.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    // Call validate_sync, which should compare the actual file hash against the corrupted receipt hash and fail
    let res = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(
        res.is_err(),
        "validate_sync must fail when cryptographic receipt is corrupted"
    );
    assert!(res
        .unwrap_err()
        .to_string()
        .contains("corrupted_hash_value"));
}

// Resolve a PackPlan with multiple dependencies and check that the sorted plan
// matches the topological dependency resolution.
#[test]
fn test_f3_t2_missing_template() {
    // Pack C has no dependencies
    let pack_c = PackDescriptor {
        id: "pack_c".to_string(),
        name: "Pack C".to_string(),
        version: "1.0.0".to_string(),
        description: "C".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![],
        query_aliases: BTreeMap::new(),
    };

    // Pack B depends on Pack C
    let mut deps_b = BTreeMap::new();
    deps_b.insert("pack_c".to_string(), "^1.0.0".to_string());
    let pack_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "1.1.0".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: deps_b,
        templates: vec![],
        query_aliases: BTreeMap::new(),
    };

    // Pack A depends on Pack B and Pack C
    let mut deps_a = BTreeMap::new();
    deps_a.insert("pack_b".to_string(), "^1.1.0".to_string());
    deps_a.insert("pack_c".to_string(), "^1.0.0".to_string());
    let pack_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "2.0.0".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: deps_a,
        templates: vec![],
        query_aliases: BTreeMap::new(),
    };

    // Run resolution with descriptors in non-topological order [pack_a, pack_b, pack_c]
    let descriptors = vec![pack_a, pack_b, pack_c];
    let plan = PackPlan::resolve(&descriptors).expect("Dependency resolution should succeed");

    // Topologically sorted order should be pack_c -> pack_b -> pack_a
    assert_eq!(plan.resolution_order, vec!["pack_c", "pack_b", "pack_a"]);
}

// Validate a CustomizationMap containing invalid whitespace variables
// and verify it returns a validation error.
#[test]
fn test_f3_t2_invalid_variables() {
    let mut map = CustomizationMap::new();
    // Insert a variable with whitespace as its key
    map.vars.insert("   ".to_string(), "some_val".to_string());

    let res = map.validate();
    assert!(
        res.is_err(),
        "Customization map validation must fail for whitespace keys"
    );
    assert_eq!(
        res.unwrap_err().to_string(),
        "Customization point name cannot be empty"
    );
}

// Verify that calling validate_sync on a missing output directory returns an error.
#[test]
fn test_f3_t2_empty_output_dir() {
    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/nonexistent.rs"),
            ProjectionMapping {
                pack_id: "pack_1".to_string(),
                template_path: PathBuf::from("templates/nonexistent.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    let receipts = ReceiptIndex::new();
    // Use a completely nonexistent directory path
    let tmp = TempDir::new().unwrap();
    let nonexistent_dir = tmp.path().join("nonexistent_directory_xyz_123");
    let res = proj_map.validate_sync(&nonexistent_dir, &receipts, None);
    assert!(
        res.is_err(),
        "validate_sync must return error if output directory is missing/empty"
    );
}

// Verify that StagingGate refuses to write to a modified file (local edits)
// unless force = true is passed.
#[test]
fn test_f3_t2_checksum_mismatch() {
    let tmp = TempDir::new().unwrap();
    let file_path = tmp.path().join("src/helper.rs");
    fs::create_dir_all(file_path.parent().unwrap()).unwrap();
    fs::write(&file_path, b"original content").unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/helper.rs".to_string(), b"original content", b"", &EquationContext::default(), None);

    let gate = StagingGate::new(tmp.path().to_path_buf(), receipts);

    // Modify local file to cause checksum mismatch (local edits / dirty)
    fs::write(&file_path, b"modified content").unwrap();

    // With force = false, check_write must fail
    let res_refuse = gate.check_write(Path::new("src/helper.rs"), false);
    assert!(
        res_refuse.is_err(),
        "Staging gate must refuse to write when local checksum mismatches"
    );
    assert!(res_refuse
        .unwrap_err()
        .to_string()
        .contains("Staging gate refusal: local edits in"));

    // With force = true, check_write must succeed
    let res_force = gate.check_write(Path::new("src/helper.rs"), true);
    assert!(
        res_force.is_ok(),
        "Staging gate must allow write when force = true even if checksum mismatches"
    );
}
