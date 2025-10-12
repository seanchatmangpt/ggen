//! Generate complete template projects

use anyhow;
use clap::Args;
use ggen_ai::TemplateGenerator;
use ggen_utils::error::Result;
use std::fs;
use std::sync::Arc;

#[derive(Debug, Args)]
pub struct ProjectArgs {
    /// Project description
    #[arg(short, long)]
    pub description: String,

    /// Project name
    #[arg(short, long)]
    pub name: String,

    /// Target programming language
    #[arg(short, long, default_value = "rust")]
    pub language: String,

    /// Target framework
    #[arg(short, long)]
    pub framework: Option<String>,

    /// Output directory
    #[arg(short, long, default_value = "./generated-project")]
    pub output: String,

    /// Publish to marketplace after generation
    #[arg(long)]
    pub publish: bool,

    /// Include tests
    #[arg(long)]
    pub tests: bool,

    /// Include documentation
    #[arg(long)]
    pub docs: bool,

    /// Include CI/CD configuration
    #[arg(long)]
    pub ci: bool,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,

    /// Use mock client for testing
    #[arg(long)]
    pub mock: bool,
}

pub async fn run(args: &ProjectArgs) -> Result<()> {
    println!("🏗️ Generating project structure with AI...");
    println!("Project: {}", args.name);
    println!("Description: {}", args.description);
    println!("Language: {}", args.language);

    if let Some(framework) = &args.framework {
        println!("Framework: {}", framework);
    }

    println!("Output directory: {}", args.output);
    println!("Include tests: {}", args.tests);
    println!("Include docs: {}", args.docs);
    println!("Include CI: {}", args.ci);
    println!("Publish to marketplace: {}", args.publish);

    let global_config = ggen_ai::get_global_config();

    use ggen_ai::MockClient;
    let client: Arc<dyn ggen_ai::client::LlmClient> = if args.mock {
        println!("ℹ️  Using mock client for testing");
        Arc::new(MockClient::with_response("Generated project structure content"))
    } else if args.openai {
        println!("ℹ️  Using OpenAI provider");
        let _config = global_config
            .get_provider_config(&ggen_ai::config::LlmProvider::OpenAI)
            .ok_or_else(|| ggen_utils::error::Error::new("OpenAI configuration not found"))?
            .clone();
        global_config
            .create_provider_client(&ggen_ai::config::LlmProvider::OpenAI)
            .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?
    } else if args.anthropic {
        println!("ℹ️  Using Anthropic provider");
        global_config
            .create_provider_client(&ggen_ai::config::LlmProvider::Anthropic)
            .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?
    } else if args.ollama {
        println!("ℹ️  Using Ollama provider");
        global_config
            .create_provider_client(&ggen_ai::config::LlmProvider::Ollama)
            .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?
    } else {
        // Use contextual client (auto-detect or mock for testing)
        println!(
            "ℹ️  Using {} provider (auto-detected)",
            global_config.provider_name()
        );
        global_config
            .create_contextual_client()
            .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?
    };

    let generator = TemplateGenerator::with_client(client);

    // Generate project description
    let project_description = format!(
        "Generate a complete {} project structure for '{}' with the following requirements: {}",
        args.language, args.name, args.description
    );

    let mut examples = vec![
        format!("Project name: {}", args.name),
        format!("Language: {}", args.language),
    ];

    if let Some(framework) = &args.framework {
        examples.push(format!("Framework: {}", framework));
    }

    if args.tests {
        examples.push("Include comprehensive test suite".to_string());
    }

    if args.docs {
        examples.push("Include documentation with examples".to_string());
    }

    if args.ci {
        examples.push("Include CI/CD configuration".to_string());
    }

    // Generate the project structure template
    let template = generator
        .generate_template(
            &project_description,
            examples.iter().map(|s| s.as_str()).collect(),
        )
        .await
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("✅ Project structure generated successfully!");

    // Create output directory
    std::fs::create_dir_all(&args.output).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create output directory: {}", e))
    })?;

    // Write the main project template
    let main_template_path = format!("{}/project.tmpl", args.output);
    fs::write(
            &main_template_path,
            format!("{:?}\n---\n{}", template.front, template.body),
        )?;
        println!("📁 Saved project template to: {}", main_template_path);

        // Generate additional project files
        let project_files = vec![
            (
                "README.md",
                "Generate a comprehensive README.md for the project",
            ),
            (
                "Cargo.toml",
                "Generate Cargo.toml configuration for Rust project",
            ),
            ("src/main.rs", "Generate main.rs entry point"),
        ];

        // ⚠️  COMMENTED OUT - Individual file generation loop
        for (file_path, description) in &project_files {
            let file_description = format!(
                "{} for project '{}': {}",
                description, args.name, args.description
            );
            let file_template = generator
                .generate_template(
                    &file_description,
                    examples.iter().map(|s| s.as_str()).collect(),
                )
                .await
                .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

            let full_path = format!("{}/{}", args.output, file_path);
            std::fs::create_dir_all(std::path::Path::new(&full_path).parent().unwrap()).map_err(
                |e| ggen_utils::error::Error::new(&format!("Failed to create directory: {}", e)),
            )?;

            fs::write(
                &full_path,
                format!("{:?}\n---\n{}", file_template.front, file_template.body),
            )?;
            println!("📁 Generated: {}", full_path);
        }

        // Generate project manifest
        let manifest_content = format!(
            r#"# Generated Project: {}
    Description: {}
    Language: {}
    Framework: {}
    Generated at: {}
    Generated by: ggen-ai

    ## Project Structure
    - project.tmpl: Main project template
    - README.md: Project documentation
    - Cargo.toml: Rust project configuration
    - src/main.rs: Entry point

    ## Features
    - Tests: {}
    - Documentation: {}
    - CI/CD: {}
    - Marketplace ready: {}

    ## Usage
    To use this project:
    1. Navigate to the project directory: cd {}
    2. Review the generated files
    3. Customize as needed
    4. Run the project: cargo run

    ## Next Steps
    - Review and customize the generated code
    - Add your specific business logic
    - Run tests: cargo test
    - Build for production: cargo build --release
    "#,
            args.name,
            args.description,
            args.language,
            args.framework.as_deref().unwrap_or("None"),
            chrono::Utc::now().to_rfc3339(),
            args.tests,
            args.docs,
            args.ci,
            args.publish,
            args.output
        );

        let manifest_path = format!("{}/PROJECT_MANIFEST.md", args.output);
        fs::write(&manifest_path, manifest_content)?;
        println!("📋 Generated project manifest: {}", manifest_path);

        println!("✅ Project generation completed successfully!");
        println!("📋 Summary:");
        println!("   • Project: {}", args.name);
        println!("   • Language: {}", args.language);
        println!("   • Output: {}", args.output);
        println!("   • Files generated: {}", project_files.len() + 2); // +2 for template and manifest

        Ok(())
}
