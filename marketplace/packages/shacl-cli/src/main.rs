use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::*;
use shacl_cli::*;

#[derive(Parser)]
#[command(name = "shacl-cli")]
#[command(about = "SHACL validation, constraint enforcement, and reporting", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Manage SHACL shapes
    Shape {
        #[command(subcommand)]
        action: ShapeCommands,
    },

    /// Manage constraints
    Constraint {
        #[command(subcommand)]
        action: ConstraintCommands,
    },

    /// Run validation
    Validation {
        #[command(subcommand)]
        action: ValidationCommands,
    },

    /// Generate and manage reports
    Report {
        #[command(subcommand)]
        action: ReportCommands,
    },
}

#[derive(Subcommand)]
enum ShapeCommands {
    /// Create a new SHACL shape
    Create {
        /// Shape name
        #[arg(long)]
        name: String,

        /// Target class IRI
        #[arg(long)]
        target_class: Option<String>,

        /// Shape type (node or property)
        #[arg(long, default_value = "node")]
        shape_type: String,

        /// Property path (for property shapes)
        #[arg(long)]
        property_path: Option<String>,

        /// Output format
        #[arg(short, long, default_value = "turtle")]
        format: String,
    },

    /// List all shapes
    List {
        /// Filter by type
        #[arg(long)]
        filter_type: Option<String>,

        /// Output format
        #[arg(short, long, default_value = "table")]
        format: String,
    },

    /// Show shape details
    Show {
        /// Shape name
        name: String,

        /// Include constraints
        #[arg(long)]
        include_constraints: bool,

        /// Output format
        #[arg(short, long, default_value = "turtle")]
        format: String,
    },

    /// Compile shapes
    Compile {
        /// Shapes file
        shape_file: std::path::PathBuf,

        /// Output path
        #[arg(long)]
        output: Option<std::path::PathBuf>,

        /// Optimization level (0-3)
        #[arg(long, default_value = "2")]
        optimize: u8,
    },

    /// Validate shape definition
    ValidateShape {
        /// Shape name
        name: String,

        /// Strict mode
        #[arg(long)]
        strict: bool,
    },
}

#[derive(Subcommand)]
enum ConstraintCommands {
    /// Add constraint to shape
    Add {
        /// Shape name
        shape: String,

        /// Constraint type
        #[arg(long)]
        r#type: String,

        /// Property path
        #[arg(long)]
        path: Option<String>,

        /// Constraint value
        #[arg(long)]
        value: Option<String>,

        /// Severity level
        #[arg(long, default_value = "violation")]
        severity: String,

        /// Validation message
        #[arg(long)]
        message: Option<String>,
    },

    /// Remove constraint
    Remove {
        /// Shape name
        shape: String,

        /// Constraint ID
        constraint_id: String,
    },

    /// Test constraint
    Test {
        /// Constraint ID
        constraint_id: String,

        /// Data file
        data_file: std::path::PathBuf,

        /// Show violations
        #[arg(long)]
        show_violations: bool,
    },

    /// Explain constraint
    Explain {
        /// Constraint ID
        constraint_id: String,

        /// Include examples
        #[arg(long)]
        include_examples: bool,
    },

    /// Create custom SPARQL constraint
    Customize {
        /// Shape name
        shape: String,

        /// SPARQL query file
        sparql_query: std::path::PathBuf,

        /// Constraint name
        #[arg(long)]
        name: Option<String>,

        /// Severity level
        #[arg(long, default_value = "violation")]
        severity: String,

        /// Message
        #[arg(long)]
        message: Option<String>,
    },
}

#[derive(Subcommand)]
enum ValidationCommands {
    /// Validate data against shapes
    Validate {
        /// Data file
        data_file: std::path::PathBuf,

        /// Shapes file
        #[arg(long)]
        shapes: std::path::PathBuf,

        /// Output format
        #[arg(short, long, default_value = "turtle")]
        format: String,

        /// Fail on warning
        #[arg(long)]
        fail_on_warning: bool,

        /// Maximum violations to report
        #[arg(long)]
        max_violations: Option<usize>,
    },

    /// Batch validate multiple files
    Batch {
        /// Data directory
        data_directory: std::path::PathBuf,

        /// Shapes file
        #[arg(long)]
        shapes: std::path::PathBuf,

        /// Process subdirectories
        #[arg(long)]
        recursive: bool,

        /// Number of parallel workers
        #[arg(long)]
        parallel: Option<usize>,

        /// Output directory
        #[arg(long)]
        output_directory: Option<std::path::PathBuf>,
    },

    /// Continuous validation with file watching
    Continuous {
        /// Path to watch
        watch_path: std::path::PathBuf,

        /// Shapes file
        #[arg(long)]
        shapes: std::path::PathBuf,

        /// Check interval in seconds
        #[arg(long, default_value = "5")]
        interval: u64,

        /// Send notifications on change
        #[arg(long)]
        notify_on_change: bool,
    },

    /// Incremental validation
    Incremental {
        /// Data file
        data_file: std::path::PathBuf,

        /// Shapes file
        #[arg(long)]
        shapes: std::path::PathBuf,

        /// Baseline results
        #[arg(long)]
        baseline: Option<std::path::PathBuf>,

        /// Enable caching
        #[arg(long)]
        cache_enabled: bool,
    },
}

#[derive(Subcommand)]
enum ReportCommands {
    /// Generate validation report
    Generate {
        /// Validation results file
        validation_file: std::path::PathBuf,

        /// Output format
        #[arg(short, long, default_value = "html")]
        format: String,

        /// Group by field
        #[arg(long)]
        group_by: Option<String>,

        /// Include valid nodes
        #[arg(long)]
        include_valid: bool,
    },

    /// Export report
    Export {
        /// Report file
        report_file: std::path::PathBuf,

        /// Output path
        output_path: std::path::PathBuf,

        /// Output format
        #[arg(short, long, default_value = "json")]
        format: String,

        /// Compress output
        #[arg(long)]
        compress: bool,
    },

    /// Summarize report
    Summarize {
        /// Report file
        report_file: std::path::PathBuf,

        /// Group by field
        #[arg(long)]
        group_by: Option<String>,

        /// Include details
        #[arg(long)]
        include_details: bool,
    },

    /// Visualize report
    Visualize {
        /// Report file
        report_file: std::path::PathBuf,

        /// Chart type
        #[arg(long, default_value = "bar")]
        chart_type: String,

        /// Output format
        #[arg(short, long, default_value = "svg")]
        format: String,

        /// Interactive mode
        #[arg(long)]
        interactive: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    if cli.verbose {
        tracing_subscriber::fmt()
            .with_env_filter("shacl_cli=debug")
            .init();
    } else {
        tracing_subscriber::fmt()
            .with_env_filter("shacl_cli=info")
            .init();
    }

    match cli.command {
        Commands::Shape { action } => handle_shape_command(action),
        Commands::Constraint { action } => handle_constraint_command(action),
        Commands::Validation { action } => handle_validation_command(action),
        Commands::Report { action } => handle_report_command(action),
    }
}

fn handle_shape_command(action: ShapeCommands) -> Result<()> {
    match action {
        ShapeCommands::Create {
            name,
            target_class,
            shape_type,
            property_path,
            format,
        } => {
            println!(
                "{}",
                "Creating shape...".bright_blue().bold()
            );
            shapes::create_shape(&name, target_class.as_deref(), &shape_type, property_path.as_deref(), &format)?;
            println!("{} Shape '{}' created successfully", "✓".green(), name);
            Ok(())
        }
        ShapeCommands::List { filter_type, format } => {
            shapes::list_shapes(filter_type.as_deref(), &format)?;
            Ok(())
        }
        ShapeCommands::Show {
            name,
            include_constraints,
            format,
        } => {
            shapes::show_shape(&name, include_constraints, &format)?;
            Ok(())
        }
        ShapeCommands::Compile {
            shape_file,
            output,
            optimize,
        } => {
            println!("{}", "Compiling shapes...".bright_blue().bold());
            shapes::compile_shape(&shape_file, output.as_deref(), optimize)?;
            println!("{} Shapes compiled successfully", "✓".green());
            Ok(())
        }
        ShapeCommands::ValidateShape { name, strict } => {
            println!("{}", "Validating shape...".bright_blue().bold());
            shapes::validate_shape(&name, strict)?;
            println!("{} Shape validation passed", "✓".green());
            Ok(())
        }
    }
}

fn handle_constraint_command(action: ConstraintCommands) -> Result<()> {
    match action {
        ConstraintCommands::Add {
            shape,
            r#type,
            path,
            value,
            severity,
            message,
        } => {
            println!("{}", "Adding constraint...".bright_blue().bold());
            constraints::add_constraint(&shape, &r#type, path.as_deref(), value.as_deref(), &severity, message.as_deref())?;
            println!("{} Constraint added to shape '{}'", "✓".green(), shape);
            Ok(())
        }
        ConstraintCommands::Remove { shape, constraint_id } => {
            println!("{}", "Removing constraint...".bright_blue().bold());
            constraints::remove_constraint(&shape, &constraint_id)?;
            println!("{} Constraint removed", "✓".green());
            Ok(())
        }
        ConstraintCommands::Test {
            constraint_id,
            data_file,
            show_violations,
        } => {
            constraints::test_constraint(&constraint_id, &data_file, show_violations)?;
            Ok(())
        }
        ConstraintCommands::Explain {
            constraint_id,
            include_examples,
        } => {
            constraints::explain_constraint(&constraint_id, include_examples)?;
            Ok(())
        }
        ConstraintCommands::Customize {
            shape,
            sparql_query,
            name,
            severity,
            message,
        } => {
            println!("{}", "Creating custom constraint...".bright_blue().bold());
            constraints::customize_constraint(&shape, &sparql_query, name.as_deref(), &severity, message.as_deref())?;
            println!("{} Custom constraint added", "✓".green());
            Ok(())
        }
    }
}

fn handle_validation_command(action: ValidationCommands) -> Result<()> {
    match action {
        ValidationCommands::Validate {
            data_file,
            shapes,
            format,
            fail_on_warning,
            max_violations,
        } => {
            println!("{}", "Running validation...".bright_blue().bold());
            let result = validation::validate_data(&data_file, &shapes, &format, fail_on_warning, max_violations)?;
            if result {
                println!("{} Validation passed", "✓".green());
            } else {
                println!("{} Validation failed", "✗".red());
            }
            Ok(())
        }
        ValidationCommands::Batch {
            data_directory,
            shapes,
            recursive,
            parallel,
            output_directory,
        } => {
            println!("{}", "Running batch validation...".bright_blue().bold());
            validation::batch_validate(&data_directory, &shapes, recursive, parallel, output_directory.as_deref())?;
            println!("{} Batch validation completed", "✓".green());
            Ok(())
        }
        ValidationCommands::Continuous {
            watch_path,
            shapes,
            interval,
            notify_on_change,
        } => {
            println!("{}", "Starting continuous validation...".bright_blue().bold());
            validation::continuous_validate(&watch_path, &shapes, interval, notify_on_change)?;
            Ok(())
        }
        ValidationCommands::Incremental {
            data_file,
            shapes,
            baseline,
            cache_enabled,
        } => {
            println!("{}", "Running incremental validation...".bright_blue().bold());
            validation::incremental_validate(&data_file, &shapes, baseline.as_deref(), cache_enabled)?;
            println!("{} Incremental validation completed", "✓".green());
            Ok(())
        }
    }
}

fn handle_report_command(action: ReportCommands) -> Result<()> {
    match action {
        ReportCommands::Generate {
            validation_file,
            format,
            group_by,
            include_valid,
        } => {
            println!("{}", "Generating report...".bright_blue().bold());
            reports::generate_report(&validation_file, &format, group_by.as_deref(), include_valid)?;
            println!("{} Report generated", "✓".green());
            Ok(())
        }
        ReportCommands::Export {
            report_file,
            output_path,
            format,
            compress,
        } => {
            println!("{}", "Exporting report...".bright_blue().bold());
            reports::export_report(&report_file, &output_path, &format, compress)?;
            println!("{} Report exported to {:?}", "✓".green(), output_path);
            Ok(())
        }
        ReportCommands::Summarize {
            report_file,
            group_by,
            include_details,
        } => {
            reports::summarize_report(&report_file, group_by.as_deref(), include_details)?;
            Ok(())
        }
        ReportCommands::Visualize {
            report_file,
            chart_type,
            format,
            interactive,
        } => {
            println!("{}", "Creating visualization...".bright_blue().bold());
            reports::visualize_report(&report_file, &chart_type, &format, interactive)?;
            println!("{} Visualization created", "✓".green());
            Ok(())
        }
    }
}
