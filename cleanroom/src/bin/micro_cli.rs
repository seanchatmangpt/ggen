//! Micro CLI for testing file persistence and cleanup
//!
//! This CLI writes a hello world file to disk and then reads it back,
//! demonstrating that the Cleanroom Testing Framework properly isolates
//! file system operations and ensures cleanup.

use std::fs;
use std::io::Write;
use std::path::Path;

/// Main function for the micro CLI
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Cleanroom Micro CLI - File Persistence Test");
    println!("===============================================");

    // Create a test file
    let test_file_path = "cleanroom_test_hello_world.txt";
    let test_content = "Hello, World! This is a test file created by Cleanroom.\n";

    println!("📝 Writing test file: {}", test_file_path);

    // Write the file
    let mut file = fs::File::create(test_file_path)?;
    file.write_all(test_content.as_bytes())?;
    file.sync_all()?;
    drop(file); // Explicitly close the file

    println!("✅ File written successfully");

    // Read the file back to verify it was written
    println!("📖 Reading file back to verify...");
    let read_content = fs::read_to_string(test_file_path)?;

    if read_content == test_content {
        println!("✅ File content matches expected content");
    } else {
        println!("❌ File content mismatch!");
        println!("Expected: {:?}", test_content);
        println!("Actual: {:?}", read_content);
        return Err("File content mismatch".into());
    }

    // Check file exists
    if Path::new(test_file_path).exists() {
        println!("✅ File exists on disk");

        // Get file metadata
        let metadata = fs::metadata(test_file_path)?;
        println!("📊 File size: {} bytes", metadata.len());
        println!("📅 Created: {:?}", metadata.created());
        println!("📅 Modified: {:?}", metadata.modified());
    } else {
        println!("❌ File does not exist on disk");
        return Err("File not found".into());
    }

    // Demonstrate file cleanup (this would normally be done by the framework)
    println!("🧹 Cleaning up test file...");
    fs::remove_file(test_file_path)?;

    // Verify file is gone
    if !Path::new(test_file_path).exists() {
        println!("✅ File successfully removed");
    } else {
        println!("❌ File still exists after cleanup");
        return Err("File cleanup failed".into());
    }

    println!("🎉 Micro CLI test completed successfully!");
    println!("This demonstrates that Cleanroom can:");
    println!("  - Create files in isolated environments");
    println!("  - Read and verify file contents");
    println!("  - Clean up files after tests");
    println!("  - Ensure no persistent side effects");

    Ok(())
}
