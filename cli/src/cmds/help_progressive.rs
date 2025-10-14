//! Progressive help system that adapts to user experience level

use clap::Args;
use colored::Colorize;
use ggen_utils::error::Result;
use ggen_utils::user_level::{ProgressiveHelp, UserActivity};

#[derive(Args, Debug)]
pub struct HelpProgressiveArgs {
    /// Command to get help for
    #[arg(value_name = "COMMAND")]
    pub command: Option<String>,

    /// Show tips based on your usage patterns
    #[arg(long)]
    pub tips: bool,
}

pub async fn run(args: &HelpProgressiveArgs) -> Result<()> {
    let activity = UserActivity::load().unwrap_or_default();
    let level = activity.get_level();

    println!();
    println!(
        "{} {} {}",
        "📚".cyan(),
        "Your level:".bold(),
        level.as_str().green().bold()
    );
    println!(
        "   {} {}",
        "Commands run:".dimmed(),
        activity.total_commands.to_string().cyan()
    );
    println!();

    if args.tips {
        // Show contextual tips
        let tips = ProgressiveHelp::get_contextual_tips(&activity);
        if !tips.is_empty() {
            println!("{}", "💡 Tips for you:".yellow().bold());
            for tip in tips {
                println!("   {}", tip);
            }
            println!();
        }

        // Show next suggested command
        if let Some(suggestion) =
            ProgressiveHelp::suggest_next_command(activity.last_command.as_deref(), level)
        {
            println!("{} {}", "🎯 Suggested next:".green().bold(), suggestion);
            println!();
        }

        // Show most used commands
        let top_commands = activity.get_top_commands(3);
        if !top_commands.is_empty() {
            println!("{}", "🔝 Your most used commands:".blue().bold());
            for (i, (cmd, count)) in top_commands.iter().enumerate() {
                println!("   {}. {} ({} times)", i + 1, cmd.cyan(), count);
            }
            println!();
        }
    } else if let Some(command) = &args.command {
        // Show help for specific command
        let help_text = ProgressiveHelp::get_command_help(command, level);
        println!("{} {}", "Help for".bold(), command.cyan().bold());
        println!();
        println!("{}", help_text);
        println!();
    } else {
        // Show general help adapted to level
        match level {
            ggen_utils::user_level::UserLevel::Newcomer => {
                println!("{}", "🚀 Welcome to ggen!".green().bold());
                println!();
                println!("Here are the essential commands to get started:");
                println!();
                println!("  {} - Check if your environment is ready", "ggen doctor".cyan());
                println!(
                    "  {} - Generate a demo project",
                    "ggen quickstart demo".cyan()
                );
                println!("  {} - See available templates", "ggen list".cyan());
                println!(
                    "  {} - Generate from a template",
                    "ggen gen <template>".cyan()
                );
                println!();
                println!(
                    "💡 Tip: Run '{}' to see all commands",
                    "ggen --help".yellow()
                );
            }
            ggen_utils::user_level::UserLevel::Intermediate => {
                println!("{}", "🎯 Common Workflows".green().bold());
                println!();
                println!("Now that you know the basics, try these workflows:");
                println!();
                println!(
                    "  {} - Find templates by description",
                    "ggen search <query>".cyan()
                );
                println!(
                    "  {} - Install a template package",
                    "ggen add <package>".cyan()
                );
                println!(
                    "  {} - AI-powered code generation",
                    "ggen ai generate".cyan()
                );
                println!(
                    "  {} - Create a full project",
                    "ggen ai project \"idea\"".cyan()
                );
                println!();
            }
            ggen_utils::user_level::UserLevel::Advanced => {
                println!("{}", "⚡ Advanced Features".green().bold());
                println!();
                println!("Explore these advanced capabilities:");
                println!();
                println!(
                    "  {} - Project lifecycle management",
                    "ggen lifecycle".cyan()
                );
                println!(
                    "  {} - RDF graph generation",
                    "ggen ai graph".cyan()
                );
                println!(
                    "  {} - SPARQL query generation",
                    "ggen ai sparql".cyan()
                );
                println!(
                    "  {} - GitHub integration",
                    "ggen github".cyan()
                );
                println!();
            }
            ggen_utils::user_level::UserLevel::Expert => {
                println!("{}", "🚀 Power User Mode".green().bold());
                println!();
                println!("You're an expert! Here are some pro tips:");
                println!();
                println!("  • Create custom templates with RDF + SPARQL");
                println!("  • Use injection modes for idempotent updates");
                println!("  • Leverage deterministic generation for testing");
                println!("  • Integrate ggen into CI/CD pipelines");
                println!();
                println!("Consider contributing your templates to the marketplace!");
            }
        }
        println!();
    }

    Ok(())
}
