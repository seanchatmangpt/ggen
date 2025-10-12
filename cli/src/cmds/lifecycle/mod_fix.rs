/// Validate production readiness for deployment
fn validate_for_deployment(root: &Path, env: &str, _strict: bool) -> ggen_utils::error::Result<()> {
    println!("🚀 Production Readiness Validation for {} environment", env);

    let validator = ReadinessValidator::new();

    match validator.validate_for_deployment(root) {
        Ok(result) => {
            println!("📊 Overall Score: {:.1}%", result.score);

            if result.passed {
                println!("✅ VALIDATION PASSED - Ready for {} deployment", env);
            } else {
                println!("❌ VALIDATION FAILED - Not ready for {} deployment", env);
            }

            if !result.recommendations.is_empty() {
                println!("\n🎯 Recommendations:");
                for rec in &result.recommendations {
                    println!("  • {}", rec);
                }
            }

            Ok(())
        }
        Err(e) => {
            println!("❌ Validation error: {}", e);
            Ok(())
        }
    }
}

/// Build environment variables
fn build_env(env: Option<String>) -> Vec<(String, String)> {
    env.map(|e| vec![("GGEN_ENV".to_string(), e)])
        .unwrap_or_default()
}
