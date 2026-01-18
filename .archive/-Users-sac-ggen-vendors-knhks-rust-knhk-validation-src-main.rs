// knhk-validation
// Release validation binary for v0.4.0

use knhk_validation::*;

#[cfg(feature = "std")]
fn main() {
    let mut report = ValidationReport::new();

    println!("KNHKS v0.4.0 Validation");
    println!("========================");
    println!();

    // CLI validation
    println!("CLI Validation:");
    report.add_result(cli_validation::validate_cli_binary_exists());
    report.add_result(cli_validation::validate_cli_command("--help", &[]));
    report.add_result(cli_validation::validate_cli_command("hook", &["--help"]));
    report.add_result(cli_validation::validate_cli_command("connect", &["--help"]));
    report.add_result(cli_validation::validate_cli_command("receipt", &["--help"]));

    // Network validation
    println!("Network Validation:");
    report.add_result(network_validation::validate_http_client_exists());
    report.add_result(network_validation::validate_otel_exporter_exists());

    // Configuration validation
    println!("Configuration Validation:");
    report.add_result(configuration_validation::validate_config_file_parsing());

    // Property validation
    println!("Property Validation:");
    report.add_result(property_validation::validate_receipt_merging_properties());
    report.add_result(property_validation::validate_iri_hashing_properties());
    report.add_result(property_validation::validate_guard_constraints());

    // Performance validation
    println!("Performance Validation:");
    report.add_result(performance_validation::validate_hot_path_performance());
    report.add_result(performance_validation::validate_cli_latency());

    // Print results
    println!();
    println!("Summary:");
    println!("  Total: {}", report.total);
    println!("  Passed: {}", report.passed);
    println!("  Failed: {}", report.failed);
    println!("  Warnings: {}", report.warnings);
    println!();

    for result in &report.results {
        if result.passed {
            println!("  ✓ {}", result.message);
        } else {
            println!("  ✗ {}", result.message);
        }
    }

    println!();

    if report.is_success() {
        println!("All validation checks passed!");
        std::process::exit(0);
    } else {
        println!("Some validation checks failed!");
        std::process::exit(1);
    }
}

#[cfg(not(feature = "std"))]
fn main() {
    println!("Validation requires std feature");
}

