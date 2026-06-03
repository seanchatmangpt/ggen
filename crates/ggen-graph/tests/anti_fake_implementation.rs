#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
use std::error::Error;
use std::fs;
use std::path::Path;

#[test]
fn test_anti_fake_implementation_scan() -> Result<(), Box<dyn Error>> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let src_dir = manifest_dir.join("src");
    let tests_dir = manifest_dir.join("tests");

    let forbidden_patterns = [
        "mockall",
        "mock!",
        "#[automock]",
        "TODO",
        "FIXME",
        "unimplemented!",
        "hash_placeholder",
        "uuid_placeholder",
        "fake_signature",
    ];

    let mut violations = Vec::new();

    let mut scan_dir = |dir: &Path| -> Result<(), Box<dyn Error>> {
        if !dir.exists() {
            return Ok(());
        }
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
                if path
                    .file_name()
                    .is_some_and(|name| name == "anti_fake_implementation.rs")
                {
                    continue;
                }
                let content = fs::read_to_string(&path)?;
                for pattern in &forbidden_patterns {
                    if content.contains(pattern) {
                        violations.push(format!(
                            "File {:?} contains forbidden fake/stub marker: '{}'",
                            path.strip_prefix(manifest_dir).unwrap_or(&path),
                            pattern
                        ));
                    }
                }
            }
        }
        Ok(())
    };

    scan_dir(&src_dir)?;
    scan_dir(&tests_dir)?;

    if !violations.is_empty() {
        for violation in &violations {
            eprintln!("{}", violation);
        }
        return Err(format!(
            "Anti-fake implementation check failed: {} violations found",
            violations.len()
        )
        .into());
    }

    Ok(())
}
