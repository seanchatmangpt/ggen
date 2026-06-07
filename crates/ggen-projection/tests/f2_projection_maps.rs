use ggen_projection::{EquationContext, sync, CustomizationMap, ProjectionMap, ProjectionMapping, ReceiptIndex, StagingGate,
};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

#[test]
fn test_f2_t1_generate_projection_map() {
    let mut map = ProjectionMap::new();
    let mapping = ProjectionMapping {
        pack_id: "pack_1".to_string(),
        template_path: PathBuf::from("templates/src.tmpl"),
        query_path: None,
        bound_variables: vec!["var_x".to_string()],
        merge_strategy: "Exclusive".to_string(),
        start_line: Some(1),
        end_line: Some(10),
    };
    map.add_mapping(PathBuf::from("target/output.rs"), mapping.clone())
        .unwrap();

    let entry = map
        .mappings
        .get(&PathBuf::from("target/output.rs"))
        .unwrap();
    assert_eq!(entry.pack_id, "pack_1");
    assert_eq!(entry.start_line, Some(1));
    assert_eq!(entry.end_line, Some(10));
}

#[test]
fn test_f2_t1_create_custom_map() {
    let mut map = CustomizationMap::new();
    map.vars
        .insert("api_key".to_string(), "TODO_API_KEY".to_string());
    map.vars
        .insert("port".to_string(), "placeholder_port".to_string());
    map.vars.insert("host".to_string(), "localhost".to_string());

    let incomplete = map.incomplete_slots();
    assert!(incomplete.contains(&"api_key".to_string()));
    assert!(incomplete.contains(&"port".to_string()));
    assert!(!incomplete.contains(&"host".to_string()));
}

#[test]
fn test_f2_t1_receipt_index_creation() {
    let mut index = ReceiptIndex::new();
    let content = b"fn main() {}";
    index.add_receipt("src/main.rs".to_string(), content, b"", &EquationContext::default(), None);

    let receipt = index.receipts.get("src/main.rs").unwrap();
    assert_eq!(receipt.target_id, "src/main.rs");
    assert_eq!(
        receipt.blake3_hash,
        blake3::hash(content).to_hex().to_string()
    );
    assert!(!index.index_hash.is_empty());
}

#[test]
fn test_f2_t1_staging_gate_non_destructive() {
    let tmp = TempDir::new().unwrap();
    let index = ReceiptIndex::new();
    let gate = StagingGate::new(tmp.path().to_path_buf(), index);

    // File does not exist, so writing is allowed
    let res = gate.check_write(Path::new("src/main.rs"), false);
    assert!(res.is_ok());
}

#[test]
fn test_f2_t1_sync_writes_maps_to_disk() {
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
    cust_map.vars.insert("var1".to_string(), "val1".to_string());

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/main.rs".to_string(), b"some content", b"", &EquationContext::default(), None);

    sync(tmp.path(), &proj_map, &cust_map, &receipts).unwrap();

    assert!(tmp.path().join("projection-map.json").exists());
    assert!(tmp.path().join("customization-map.json").exists());
    assert!(tmp.path().join("receipts.json").exists());
    assert!(tmp.path().join(".sync_marker").exists());
}

#[test]
fn test_f2_t2_stale_projection_map_read() {
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

    let mut receipts = ReceiptIndex::new();
    // 1. If file does not exist on disk, validate_sync should return Err (stale)
    let res_missing = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(res_missing.is_err());

    // 2. Write file to disk
    let file_path = tmp.path().join("src/main.rs");
    std::fs::create_dir_all(file_path.parent().unwrap()).unwrap();
    std::fs::write(&file_path, b"content A").unwrap();

    // Add receipt with matching hash
    receipts.add_receipt("src/main.rs".to_string(), b"content A", b"", &EquationContext::default(), None);
    let res_ok = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(res_ok.is_ok());

    // Modify file on disk so it is out of sync with receipt
    std::fs::write(&file_path, b"content B").unwrap();
    let res_stale = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(res_stale.is_err());
}

#[test]
fn test_f2_t2_overlapping_ranges() {
    let mut map = ProjectionMap::new();
    let mapping_1 = ProjectionMapping {
        pack_id: "pack_1".to_string(),
        template_path: PathBuf::from("templates/main.tmpl"),
        query_path: None,
        bound_variables: vec![],
        merge_strategy: "Exclusive".to_string(),
        start_line: Some(10),
        end_line: Some(20),
    };
    map.add_mapping(PathBuf::from("src/main.rs"), mapping_1)
        .unwrap();

    // Overlapping range [15, 25] in same file
    let mapping_2 = ProjectionMapping {
        pack_id: "pack_2".to_string(),
        template_path: PathBuf::from("templates/other.tmpl"),
        query_path: None,
        bound_variables: vec![],
        merge_strategy: "Exclusive".to_string(),
        start_line: Some(15),
        end_line: Some(25),
    };
    let res = map.add_mapping(PathBuf::from("src/main.rs"), mapping_2);
    assert!(res.is_err());
}

#[test]
fn test_f2_t2_staging_gate_refusal_on_dirty() {
    let tmp = TempDir::new().unwrap();
    let file_path = tmp.path().join("src/main.rs");
    std::fs::create_dir_all(file_path.parent().unwrap()).unwrap();
    std::fs::write(&file_path, b"content original").unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/main.rs".to_string(), b"content original", b"", &EquationContext::default(), None);

    let gate = StagingGate::new(tmp.path().to_path_buf(), receipts.clone());

    // Modify local file to make it dirty
    std::fs::write(&file_path, b"content edited").unwrap();

    // check_write should refuse
    let res_refused = gate.check_write(Path::new("src/main.rs"), false);
    assert!(res_refused.is_err());

    // check_write with force = true should allow
    let res_forced = gate.check_write(Path::new("src/main.rs"), true);
    assert!(res_forced.is_ok());
}

#[test]
fn test_f2_t2_non_writable_directory() {
    // Non-writable directory check
    let tmp = TempDir::new().unwrap();
    let read_only_dir = tmp.path().join("readonly");
    std::fs::create_dir(&read_only_dir).unwrap();

    let proj_map = ProjectionMap::new();
    let cust_map = CustomizationMap::new();
    let receipts = ReceiptIndex::new();

    // Set read-only permissions
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&read_only_dir, std::fs::Permissions::from_mode(0o400)).unwrap();
    }

    // Attempting to sync/write inside the read-only directory should return an error
    let res = sync(
        &read_only_dir.join("subdir"),
        &proj_map,
        &cust_map,
        &receipts,
    );
    assert!(res.is_err());

    // Restore permissions so cleanup doesn't fail
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(&read_only_dir, std::fs::Permissions::from_mode(0o700));
    }
}

#[test]
fn test_f2_t2_empty_customization_point_name() {
    let mut map = CustomizationMap::new();
    map.vars.insert(" ".to_string(), "some_value".to_string());
    assert!(map.validate().is_err());
}
