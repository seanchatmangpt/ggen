//! Generate YAWL Java code (Rules 3-10) and write to disk for Maven compilation

use ggen_yawl::codegen::{
    create_controller_rule, create_dto_rule, create_enum_rule, create_hbm_mapping_rule,
    create_jackson_serializer_rule, create_jpa_entity_rule, create_repository_rule,
    create_service_rule,
};
use std::fs;
use std::path::Path;

#[test]
#[ignore] // Run with: cargo test generate_for_maven -- --ignored --nocapture
fn generate_for_maven() {
    let output_dir = "/tmp/yawl-generated-test/src/main/java";
    fs::create_dir_all(output_dir).expect("Failed to create output directory");

    println!("\n=== Generating YAWL Java Code (Rules 3-10) for Maven ===\n");

    let mut total_files = 0;
    let mut total_lines = 0;

    // Rule 3: JPA Entities
    {
        let rule = create_jpa_entity_rule().expect("Rule 3 should create");
        let files = rule.execute().expect("Rule 3 should execute");
        println!("Rule 3: JPA Entities - {} files", files.len());
        total_files += files.len();
        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("Failed to create directory");
            }
            fs::write(&path, &file.content).expect("Failed to write file");
            let lines = file.content.lines().count();
            total_lines += lines;
            println!("  ✓ {} ({} lines)", file.path.display(), lines);
        }
    }

    // Rule 4: Repositories
    {
        let rule = create_repository_rule().expect("Rule 4 should create");
        let files = rule.execute().expect("Rule 4 should execute");
        println!("Rule 4: Repositories - {} files", files.len());
        total_files += files.len();
        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("Failed to create directory");
            }
            fs::write(&path, &file.content).expect("Failed to write file");
            let lines = file.content.lines().count();
            total_lines += lines;
            println!("  ✓ {} ({} lines)", file.path.display(), lines);
        }
    }

    // Rule 5: DTOs
    {
        let rule = create_dto_rule().expect("Rule 5 should create");
        let files = rule.execute().expect("Rule 5 should execute");
        println!("Rule 5: DTOs - {} files", files.len());
        total_files += files.len();
        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("Failed to create directory");
            }
            fs::write(&path, &file.content).expect("Failed to write file");
            let lines = file.content.lines().count();
            total_lines += lines;
            println!("  ✓ {} ({} lines)", file.path.display(), lines);
        }
    }

    // Rule 6: Controllers
    {
        let rule = create_controller_rule().expect("Rule 6 should create");
        let files = rule.execute().expect("Rule 6 should execute");
        println!("Rule 6: Controllers - {} files", files.len());
        total_files += files.len();
        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("Failed to create directory");
            }
            fs::write(&path, &file.content).expect("Failed to write file");
            let lines = file.content.lines().count();
            total_lines += lines;
            println!("  ✓ {} ({} lines)", file.path.display(), lines);
        }
    }

    // Rule 7: Enums
    {
        let rule = create_enum_rule().expect("Rule 7 should create");
        let files = rule.execute().expect("Rule 7 should execute");
        println!("Rule 7: Enums - {} files", files.len());
        total_files += files.len();
        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("Failed to create directory");
            }
            fs::write(&path, &file.content).expect("Failed to write file");
            let lines = file.content.lines().count();
            total_lines += lines;
            println!("  ✓ {} ({} lines)", file.path.display(), lines);
        }
    }

    // Rule 8: Services
    {
        let rule = create_service_rule().expect("Rule 8 should create");
        let files = rule.execute().expect("Rule 8 should execute");
        println!("Rule 8: Services - {} files", files.len());
        total_files += files.len();
        for file in files {
            let path = Path::new(output_dir).join(&file.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).expect("Failed to create directory");
            }
            fs::write(&path, &file.content).expect("Failed to write file");
            let lines = file.content.lines().count();
            total_lines += lines;
            println!("  ✓ {} ({} lines)", file.path.display(), lines);
        }
    }

    // Rule 9: HBM Mappings (Placeholder - not fully integrated with Rule<Q,T> framework)
    {
        let _rule = create_hbm_mapping_rule();
        println!("Rule 9: HBM Mappings - [Placeholder implementation - not yet integrated]");
    }

    // Rule 10: Jackson Serializers (Placeholder - not fully integrated with Rule<Q,T> framework)
    {
        let _rule = create_jackson_serializer_rule();
        println!(
            "Rule 10: Jackson Serializers - [Placeholder implementation - not yet integrated]"
        );
    }

    println!("\n=== Code Generation Summary ===");
    println!("Total files generated: {}", total_files);
    println!("Total lines of code: {}", total_lines);
    println!("Output directory: {}", output_dir);
    println!("\nFiles ready for Maven compilation");
}
