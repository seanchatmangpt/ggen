use clap::Args;
use colored::Colorize;
use ggen_utils::error::Result;
use std::process::Command;

#[derive(Args, Debug)]
pub struct DoctorArgs {
    /// Show verbose output with fix instructions
    #[arg(short, long)]
    verbose: bool,
}

struct Check {
    name: &'static str,
    command: &'static str,
    args: Vec<&'static str>,
    optional: bool,
    fix_message: &'static str,
}

impl Check {
    fn new(name: &'static str, command: &'static str, args: Vec<&'static str>) -> Self {
        Self {
            name,
            command,
            args,
            optional: false,
            fix_message: "",
        }
    }

    fn optional(mut self, fix_message: &'static str) -> Self {
        self.optional = true;
        self.fix_message = fix_message;
        self
    }

    fn run(&self) -> std::result::Result<String, String> {
        let output = Command::new(self.command)
            .args(&self.args)
            .output()
            .map_err(|e| format!("not found: {}", e))?;

        if output.status.success() {
            let version = String::from_utf8_lossy(&output.stdout)
                .lines()
                .next()
                .unwrap_or("installed")
                .to_string();
            Ok(version)
        } else {
            Err("not available".to_string())
        }
    }
}

pub async fn run(args: &DoctorArgs) -> Result<()> {
    println!("\n{}", "🔍 Checking your environment...".cyan().bold());
    println!();

    let checks = vec![
        Check::new("Rust toolchain", "rustc", vec!["--version"]),
        Check::new("Cargo", "cargo", vec!["--version"]),
        Check::new("Git", "git", vec!["--version"]),
        Check::new("Ollama", "ollama", vec!["--version"])
            .optional("Install from https://ollama.ai for local AI generation"),
        Check::new("Docker", "docker", vec!["--version"])
            .optional("Install from https://docker.com for cleanroom testing"),
    ];

    let mut all_required_ok = true;
    let mut optional_missing = vec![];

    for check in checks {
        match check.run() {
            Ok(version) => {
                println!("✅ {} ({})", check.name.green(), version.trim());
            }
            Err(e) if check.optional => {
                println!("⚠️  {} ({}) - optional", check.name.yellow(), e);
                if args.verbose || !check.fix_message.is_empty() {
                    println!("   {}", check.fix_message.dimmed());
                }
                optional_missing.push(check.name);
            }
            Err(e) => {
                println!("❌ {} ({})", check.name.red(), e);
                all_required_ok = false;

                // Provide fix instructions
                match check.name {
                    "Rust toolchain" | "Cargo" => {
                        println!("   {}", "Fix: Install Rust".yellow());
                        println!(
                            "   {}",
                            "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
                                .dimmed()
                        );
                    }
                    "Git" => {
                        println!("   {}", "Fix: Install Git".yellow());
                        #[cfg(target_os = "macos")]
                        println!("   {}", "xcode-select --install".dimmed());
                        #[cfg(target_os = "linux")]
                        println!(
                            "   {}",
                            "sudo apt install git  # or: sudo yum install git".dimmed()
                        );
                    }
                    _ => {}
                }
            }
        }
    }

    println!();
    println!(
        "{}",
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━".dimmed()
    );
    println!();

    if all_required_ok {
        println!("{}", "🎉 You're ready to use ggen!".green().bold());
        println!();
        println!("Next steps:");
        println!("  • {}", "ggen quickstart demo".cyan());
        println!(
            "  • {}",
            "ggen ai project \"your idea\" --name my-project".cyan()
        );
        println!("  • {}", "ggen search \"rust web\"".cyan());

        if !optional_missing.is_empty() {
            println!();
            println!("{}", "ℹ️  Optional tools not installed:".yellow());
            for tool in optional_missing {
                println!("   • {}", tool);
            }
            println!();
            println!("These tools enable advanced features but are not required.");
        }
    } else {
        println!(
            "{}",
            "🔧 Please install missing requirements".yellow().bold()
        );
        println!();
        println!("Quick setup:");
        println!("  {}", "curl -fsSL https://raw.githubusercontent.com/seanchatmangpt/ggen/master/scripts/quickstart.sh | bash".cyan());
    }

    println!();
    Ok(())
}
