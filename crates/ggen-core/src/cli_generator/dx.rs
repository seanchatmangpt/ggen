//! Hyper-Advanced Developer Experience (DX) Enhancements
//!
//! This module provides advanced DX features for the CLI generator:
//! - IDE-friendly completions and suggestions
//! - Enhanced error messages with fix suggestions
//! - Live template preview
//! - Auto-fix suggestions
//! - Type-aware code generation hints
//! - Progressive disclosure of complexity

use crate::cli_generator::types::{Argument, CliProject, Noun, Verb};

/// Enhanced error messages with fix suggestions
pub struct ErrorEnhancer;

impl ErrorEnhancer {
    /// Generate a helpful error message with fix suggestions
    pub fn enhance_error(err: &anyhow::Error, context: &ErrorContext) -> String {
        let mut msg = format!("❌ Error: {}\n\n", err);

        // Context-aware suggestions
        if let Some(suggestion) = Self::suggest_fix(err, context) {
            msg.push_str(&format!("💡 Suggestion: {}\n\n", suggestion));
        }

        // Show related documentation
        if let Some(doc_link) = Self::get_doc_link(context) {
            msg.push_str(&format!("📚 Documentation: {}\n", doc_link));
        }

        msg
    }

    fn suggest_fix(err: &anyhow::Error, _context: &ErrorContext) -> Option<String> {
        let err_msg = err.to_string().to_lowercase();

        if err_msg.contains("template not found") {
            Some("Check that templates exist in the templates/cli/ directory. Run 'ggen template list' to see available templates.".to_string())
        } else if err_msg.contains("domain function") {
            Some("Ensure domain function path matches the generated module structure. Use format: {crate}::{module}::{verb}".to_string())
        } else if err_msg.contains("circular dependency") {
            Some("Ensure CLI crate depends on domain crate, not vice versa. Domain crate should not import clap or clap-noun-verb.".to_string())
        } else {
            None
        }
    }

    fn get_doc_link(context: &ErrorContext) -> Option<String> {
        match context {
            ErrorContext::TemplateGeneration => {
                Some("See docs/CLI_GENERATOR_V2_2026_BEST_PRACTICES.md".to_string())
            }
            ErrorContext::WorkspaceStructure => Some(
                "See docs/CLI_GENERATOR_V2_2026_BEST_PRACTICES.md#workspace-pattern".to_string(),
            ),
            ErrorContext::DomainFunctionReference => Some(
                "See docs/CLI_GENERATOR_V2_2026_BEST_PRACTICES.md#domain-function-references"
                    .to_string(),
            ),
            _ => None,
        }
    }
}

pub enum ErrorContext {
    TemplateGeneration,
    WorkspaceStructure,
    DomainFunctionReference,
    Unknown,
}

/// IDE-friendly code generation hints
pub struct CodeHints;

impl CodeHints {
    /// Generate type-aware hints for domain functions
    pub fn generate_domain_function_hint(verb: &Verb, noun: &Noun, project: &CliProject) -> String {
        let core_crate = project
            .domain_crate
            .as_ref()
            .map(|c| c.replace("-", "_"))
            .unwrap_or_else(|| format!("{}_core", project.name.replace("-", "_")));

        let default_path = format!("{}::{}::{}", core_crate, noun.name, verb.name);
        let function_path = verb.domain_function.as_deref().unwrap_or(&default_path);

        format!(
            "/// Domain function reference: `{}`\n\
             /// Expected signature:\n\
             /// ```rust\n\
             /// pub async fn execute(input: Input) -> Result<Output>;\n\
             /// ```",
            function_path
        )
    }

    /// Generate import suggestions for CLI layer
    pub fn generate_import_suggestion(verb: &Verb, noun: &Noun, project: &CliProject) -> String {
        let core_crate = project
            .domain_crate
            .as_ref()
            .map(|c| c.replace("-", "_"))
            .unwrap_or_else(|| format!("{}_core", project.name.replace("-", "_")));

        format!(
            "use {}::{}::{}::{{Input, Output, execute as domain_{}}};\n\
             use crate::runtime::execute as sync_execute;",
            core_crate, noun.name, verb.name, verb.name
        )
    }

    /// Generate argument mapping hints
    pub fn generate_argument_mapping(verb: &Verb) -> String {
        let mut mapping =
            String::from("// Map CLI arguments to domain input:\nlet input = Input {\n");

        for arg in &verb.arguments {
            mapping.push_str(&format!("    {}: args.{}.clone(),\n", arg.name, arg.name));
        }

        mapping.push_str("};");
        mapping
    }
}

/// Live template preview
pub struct TemplatePreview;

impl TemplatePreview {
    /// Generate a preview of what will be generated
    pub fn preview_workspace_structure(project: &CliProject) -> String {
        let default_cli = format!("{}-cli", project.name);
        let default_domain = format!("{}-core", project.name);
        let cli_crate = project.cli_crate.as_ref().unwrap_or(&default_cli);
        let domain_crate = project.domain_crate.as_ref().unwrap_or(&default_domain);

        format!(
            "📁 Workspace Structure Preview:\n\n\
             {project_name}/\n\
             ├── Cargo.toml              # Workspace manifest\n\
             └── crates/\n\
                 ├── {cli_crate}/        # CLI presentation layer\n\
                 │   ├── Cargo.toml\n\
                 │   └── src/\n\
                 │       ├── main.rs     # Entry point with clap-noun-verb v3.3.0\n\
                 │       ├── lib.rs\n\
                 │       ├── runtime.rs  # Async/sync bridge\n\
                 │       └── cmds/       # Command definitions\n\
                 │           └── {{nouns}}\n\
                 └── {domain_crate}/    # Domain/business logic layer\n\
                     ├── Cargo.toml\n\
                     └── src/\n\
                         └── {{noun}}/   # Domain modules\n\n\
             🎯 Nouns: {nouns}\n\
             🔧 Verbs per noun: {verbs}\n\
             📦 Dependencies: {deps}",
            project_name = project.name,
            cli_crate = cli_crate,
            domain_crate = domain_crate,
            nouns = project.nouns.len(),
            verbs = project.nouns.iter().map(|n| n.verbs.len()).sum::<usize>(),
            deps = project.dependencies.len(),
        )
    }

    /// Preview a specific verb implementation
    pub fn preview_verb_implementation(verb: &Verb, noun: &Noun, project: &CliProject) -> String {
        let core_crate = project
            .domain_crate
            .as_ref()
            .map(|c| c.replace("-", "_"))
            .unwrap_or_else(|| format!("{}_core", project.name.replace("-", "_")));

        format!(
            "📝 Verb Implementation Preview:\n\n\
             ```rust\n\
             // crates/{cli_crate}/src/cmds/{noun_name}/{verb_name}.rs\n\
             \n\
             use clap::Args;\n\
             use anyhow::Result;\n\
             use crate::runtime::execute;\n\
             use {core_crate}::{noun_name}::{verb_name}::{{Input, execute as domain_{verb_name}}};\n\
             \n\
             #[derive(Args, Debug)]\n\
             pub struct {verb_args} {{\n\
             {args}\
             }}\n\
             \n\
             #[verb(\"{verb_name}\", \"{noun_name}\")]\n\
             pub fn run(args: &{verb_args}) -> Result<()> {{\n\
                 let input = Input {{\n\
             {input_mapping}\
                 }};\n\
                 \n\
                 let result = execute(async move {{\n\
                     domain_{verb_name}(input).await\n\
                 }})?;\n\
                 \n\
                 println!(\"✅ {noun_name} {verb_name}d: {{}}\", result.id);\n\
                 Ok(())\n\
             }}\n\
             ```",
            cli_crate = project.cli_crate.as_ref().unwrap_or(&format!("{}-cli", project.name)),
            core_crate = core_crate,
            noun_name = noun.name,
            verb_name = verb.name,
            verb_args = format!("{}Args", verb.name.chars().next().unwrap().to_uppercase().to_string() + &verb.name[1..]),
            args = verb.arguments.iter().map(|a| {
                format!("    /// {}\n    #[arg(long)]\n    pub {}: {},\n\n",
                        a.help, a.name, Self::rust_type_for_arg(a))
            }).collect::<String>(),
            input_mapping = verb.arguments.iter().map(|a| {
                format!("        {}: args.{}.clone(),\n", a.name, a.name)
            }).collect::<String>(),
        )
    }

    fn rust_type_for_arg(arg: &Argument) -> String {
        match arg.arg_type.name.as_str() {
            "String" => "String".to_string(),
            "bool" => "bool".to_string(),
            "PathBuf" => "std::path::PathBuf".to_string(),
            _ => {
                if arg.required {
                    arg.arg_type.name.to_string()
                } else {
                    format!("Option<{}>", arg.arg_type.name)
                }
            }
        }
    }
}

/// Auto-fix suggestions for common issues
pub struct AutoFix;

impl AutoFix {
    /// Suggest fixes for missing domain function references
    pub fn suggest_domain_function_fix(verb: &Verb, noun: &Noun, project: &CliProject) -> String {
        let core_crate = project
            .domain_crate
            .as_ref()
            .map(|c| c.replace("-", "_"))
            .unwrap_or_else(|| format!("{}_core", project.name.replace("-", "_")));

        let suggested_path = format!("{}::{}::{}", core_crate, noun.name, verb.name);

        format!(
            "💡 Add to your RDF ontology:\n\n\
             <#{verb}-{noun}> a cnv:Verb ;\n\
                 cnv:domainFunction \"{suggested_path}\" .\n\n\
             Or update the generated CLI to use:\n\
             ```rust\n\
             use {core_crate}::{noun}::{verb}::{{Input, Output, execute}};\n\
             ```",
            verb = verb.name,
            noun = noun.name,
            suggested_path = suggested_path,
            core_crate = core_crate,
        )
    }

    /// Suggest fixes for workspace structure issues
    pub fn suggest_workspace_fix(project: &CliProject) -> String {
        format!(
            "💡 Ensure workspace Cargo.toml includes:\n\n\
             ```toml\n\
             [workspace]\n\
             members = [\n\
                 \"crates/{cli_crate}\",\n\
                 \"crates/{domain_crate}\",\n\
             ]\n\
             resolver = \"{resolver}\"\n\
             ```",
            cli_crate = project
                .cli_crate
                .as_ref()
                .unwrap_or(&format!("{}-cli", project.name)),
            domain_crate = project
                .domain_crate
                .as_ref()
                .unwrap_or(&format!("{}-core", project.name)),
            resolver = project.resolver,
        )
    }
}

/// Progressive disclosure helpers
pub struct ProgressiveDisclosure;

impl ProgressiveDisclosure {
    /// Get basic info for beginners
    pub fn beginner_info(project: &CliProject) -> String {
        format!(
            "✨ Generated CLI project: {}\n\
             📦 {} noun(s), {} verb(s) total\n\
             🚀 Next steps:\n\
               1. cd {}\n\
               2. cargo build\n\
               3. cargo run -- --help",
            project.name,
            project.nouns.len(),
            project.nouns.iter().map(|n| n.verbs.len()).sum::<usize>(),
            project.name,
        )
    }

    /// Get advanced info for power users
    pub fn advanced_info(project: &CliProject) -> String {
        format!(
            "🔧 Advanced Details:\n\n\
             Workspace:\n\
             - CLI crate: {}\n\
             - Domain crate: {}\n\
             - Resolver: {}\n\
             \n\
             Dependencies:\n\
             {}\n\
             \n\
             Domain Functions:\n\
             {}",
            project
                .cli_crate
                .as_ref()
                .unwrap_or(&format!("{}-cli", project.name)),
            project
                .domain_crate
                .as_ref()
                .unwrap_or(&format!("{}-core", project.name)),
            project.resolver,
            project
                .dependencies
                .iter()
                .map(|d| { format!("  - {} = \"{}\"", d.name, d.version) })
                .collect::<Vec<_>>()
                .join("\n"),
            project
                .nouns
                .iter()
                .flat_map(|n| {
                    n.verbs.iter().map(move |v| {
                        format!(
                            "  - {}::{}::{}",
                            project
                                .domain_crate
                                .as_ref()
                                .unwrap_or(&format!("{}-core", project.name)),
                            n.name,
                            v.name
                        )
                    })
                })
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }
}
