use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;

#[derive(Args, Debug)]
pub struct GenArgs {
    /// Template reference (e.g., "gpack-id:path/to.tmpl" or "local/path.tmpl")
    pub template_ref: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Perform dry-run without creating files
    #[arg(long)]
    pub dry_run: bool,

    /// Deterministic seed for reproducible generation
    #[arg(long)]
    pub seed: Option<u64>,

    /// Force overwrite existing files
    #[arg(short = 'f', long)]
    pub force: bool,
}

/// London TDD: Define trait boundaries for testability
#[cfg_attr(test, mockall::automock)]
pub trait TemplateResolver {
    fn resolve(&self, template_ref: &str) -> Result<Template>;
}

#[cfg_attr(test, mockall::automock)]
pub trait PlanGenerator {
    fn generate_plan(&self, template: &Template, vars: &HashMap<String, String>) -> Result<Plan>;
}

#[cfg_attr(test, mockall::automock)]
pub trait PlanApplier {
    fn apply(&self, plan: &Plan) -> Result<()>;
}

/// Placeholder types (will be replaced with actual ggen-core types)
#[derive(Debug, Clone)]
pub struct Template {
    pub content: String,
    pub frontmatter: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct Plan {
    pub operations: Vec<Operation>,
}

#[derive(Debug, Clone)]
pub enum Operation {
    Create { path: String, content: String },
    Update { path: String, content: String },
    Delete { path: String },
}

/// Parse key=value pairs into HashMap
fn parse_vars(vars: &[String]) -> Result<HashMap<String, String>> {
    let mut map = HashMap::new();
    for var in vars {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Expected 'key=value'",
                var
            )));
        }
        map.insert(parts[0].to_string(), parts[1].to_string());
    }
    Ok(map)
}

/// Validate and sanitize generation input
fn validate_gen_input(args: &GenArgs) -> Result<()> {
    // Validate template reference is not empty
    if args.template_ref.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    // Validate template reference length
    if args.template_ref.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Template reference too long (max 500 characters)",
        ));
    }

    // Validate variables format
    for var in &args.vars {
        if !var.contains('=') {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Expected format: key=value",
                var
            )));
        }

        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 || parts[0].trim().is_empty() {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Key cannot be empty",
                var
            )));
        }
    }

    Ok(())
}

/// Main entry point for `ggen project gen`
pub async fn run(args: &GenArgs) -> Result<()> {
    // Validate input
    validate_gen_input(args)?;

    // TODO: Replace with real implementations from ggen-core
    println!("🚧 Placeholder: project gen");
    println!("  Template: {}", args.template_ref.trim());
    println!("  Vars: {:?}", args.vars);
    println!("  Dry run: {}", args.dry_run);

    Ok(())
}

/// Testable implementation with dependency injection (London TDD)
pub async fn run_with_deps(
    args: &GenArgs, resolver: &dyn TemplateResolver, generator: &dyn PlanGenerator,
    applier: &dyn PlanApplier,
) -> Result<()> {
    // Step 1: Resolve template
    println!("🔍 Resolving template...");
    let template = resolver.resolve(&args.template_ref)?;

    // Step 2: Parse variables
    println!("⚙️  Processing variables...");
    let vars = parse_vars(&args.vars)?;

    // Step 3: Generate plan
    println!("📋 Generating plan...");
    let plan = generator.generate_plan(&template, &vars)?;

    // Step 4: Apply plan (unless dry-run)
    if !args.dry_run {
        println!("🚀 Applying changes...");
        applier.apply(&plan)?;
        println!("✅ Generation completed successfully!");
    } else {
        println!("🔍 Dry run completed - no changes applied");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_gen_resolves_template() {
        // Arrange: Create mock resolver
        let mut mock_resolver = MockTemplateResolver::new();
        mock_resolver
            .expect_resolve()
            .with(eq(String::from("hello.tmpl")))
            .times(1)
            .returning(|_| {
                Ok(Template {
                    content: "Hello, {{ name }}!".to_string(),
                    frontmatter: HashMap::new(),
                })
            });

        let mut mock_generator = MockPlanGenerator::new();
        mock_generator
            .expect_generate_plan()
            .times(1)
            .returning(|_, _| Ok(Plan { operations: vec![] }));

        let mut mock_applier = MockPlanApplier::new();
        mock_applier.expect_apply().times(1).returning(|_| Ok(()));

        let args = GenArgs {
            template_ref: "hello.tmpl".to_string(),
            vars: vec!["name=World".to_string()],
            dry_run: false,
            seed: None,
            force: false,
        };

        // Act
        let result = run_with_deps(&args, &mock_resolver, &mock_generator, &mock_applier).await;

        // Assert
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_gen_parses_variables() {
        let mut mock_resolver = MockTemplateResolver::new();
        mock_resolver.expect_resolve().returning(|_| {
            Ok(Template {
                content: String::new(),
                frontmatter: HashMap::new(),
            })
        });

        let mut mock_generator = MockPlanGenerator::new();
        mock_generator
            .expect_generate_plan()
            .withf(|_, vars| {
                vars.get("name") == Some(&"World".to_string())
                    && vars.get("age") == Some(&"30".to_string())
            })
            .times(1)
            .returning(|_, _| Ok(Plan { operations: vec![] }));

        let mut mock_applier = MockPlanApplier::new();
        mock_applier.expect_apply().times(1).returning(|_| Ok(()));

        let args = GenArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec!["name=World".to_string(), "age=30".to_string()],
            dry_run: false,
            seed: None,
            force: false,
        };

        let result = run_with_deps(&args, &mock_resolver, &mock_generator, &mock_applier).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_gen_dry_run_skips_apply() {
        let mut mock_resolver = MockTemplateResolver::new();
        mock_resolver.expect_resolve().returning(|_| {
            Ok(Template {
                content: String::new(),
                frontmatter: HashMap::new(),
            })
        });

        let mut mock_generator = MockPlanGenerator::new();
        mock_generator
            .expect_generate_plan()
            .times(1)
            .returning(|_, _| Ok(Plan { operations: vec![] }));

        let mut mock_applier = MockPlanApplier::new();
        // Expect apply to NOT be called in dry-run mode
        mock_applier.expect_apply().times(0);

        let args = GenArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec![],
            dry_run: true, // Dry run enabled
            seed: None,
            force: false,
        };

        let result = run_with_deps(&args, &mock_resolver, &mock_generator, &mock_applier).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_vars_valid() {
        let vars = vec!["name=Alice".to_string(), "age=30".to_string()];
        let result = parse_vars(&vars).unwrap();

        assert_eq!(result.get("name"), Some(&"Alice".to_string()));
        assert_eq!(result.get("age"), Some(&"30".to_string()));
    }

    #[test]
    fn test_parse_vars_invalid_format() {
        let vars = vec!["invalid".to_string()];
        let result = parse_vars(&vars);

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid variable format"));
    }

    #[test]
    fn test_parse_vars_with_equals_in_value() {
        let vars = vec!["url=https://example.com?foo=bar".to_string()];
        let result = parse_vars(&vars).unwrap();

        assert_eq!(
            result.get("url"),
            Some(&"https://example.com?foo=bar".to_string())
        );
    }
}
