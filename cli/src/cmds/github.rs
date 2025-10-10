use anyhow::Result;
use clap::{Args, Subcommand};
use colored::*;
use ggen_core::{GitHubClient, RepoInfo};

#[derive(Args, Debug)]
pub struct GitHubArgs {
    #[command(subcommand)]
    pub command: GitHubCommand,
}

#[derive(Subcommand, Debug)]
pub enum GitHubCommand {
    /// Check GitHub Pages configuration and status
    PagesStatus(PagesStatusArgs),
    /// View GitHub Actions workflow runs
    WorkflowStatus(WorkflowStatusArgs),
    /// Trigger a workflow manually
    TriggerWorkflow(TriggerWorkflowArgs),
}

#[derive(Args, Debug)]
pub struct PagesStatusArgs {
    /// Repository in owner/repo format (defaults to current repo from git remote)
    #[arg(short, long)]
    pub repo: Option<String>,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,
}

#[derive(Args, Debug)]
pub struct WorkflowStatusArgs {
    /// Workflow file name (e.g., "publish-registry.yml")
    #[arg(short, long, default_value = "publish-registry.yml")]
    pub workflow: String,

    /// Repository in owner/repo format (defaults to current repo from git remote)
    #[arg(short, long)]
    pub repo: Option<String>,

    /// Number of runs to show
    #[arg(short, long, default_value = "10")]
    pub limit: u32,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,
}

#[derive(Args, Debug)]
pub struct TriggerWorkflowArgs {
    /// Workflow file name (e.g., "publish-registry.yml")
    #[arg(short, long, default_value = "publish-registry.yml")]
    pub workflow: String,

    /// Repository in owner/repo format (defaults to current repo from git remote)
    #[arg(short, long)]
    pub repo: Option<String>,

    /// Branch, tag, or SHA to run workflow on
    #[arg(short = 'b', long, default_value = "master")]
    pub ref_name: String,
}

pub async fn run(args: &GitHubArgs) -> Result<()> {
    match &args.command {
        GitHubCommand::PagesStatus(cmd_args) => pages_status(cmd_args).await,
        GitHubCommand::WorkflowStatus(cmd_args) => workflow_status(cmd_args).await,
        GitHubCommand::TriggerWorkflow(cmd_args) => trigger_workflow(cmd_args).await,
    }
}

async fn pages_status(args: &PagesStatusArgs) -> Result<()> {
    let repo_str = get_repository(&args.repo)?;
    let repo = RepoInfo::parse(&repo_str)?;
    let client = GitHubClient::new(repo.clone())?;

    if !client.is_authenticated() {
        eprintln!(
            "{} Not authenticated. Set GITHUB_TOKEN or GH_TOKEN for full access.",
            "Warning:".yellow()
        );
        eprintln!();
    }

    // Get Pages configuration
    let pages_config_result = client.get_pages_config(&repo).await;

    let mut site_url = None;

    match &pages_config_result {
        Ok(config) => {
            site_url = config.html_url.clone();
            if args.json {
                println!("{}", serde_json::to_string_pretty(config)?);
            } else {
                print_pages_status(config, &repo);
            }
        }
        Err(e) => {
            if e.to_string().contains("not configured") {
                if args.json {
                    println!("{{\"status\": \"not_configured\"}}");
                } else {
                    println!("{}", "GitHub Pages Configuration:".bold());
                    println!("{}", "─".repeat(60).dimmed());
                    println!(
                        "{} GitHub Pages is not configured for {}",
                        "❌".red(),
                        repo.as_str()
                    );
                    println!();
                    println!("{}", "To enable GitHub Pages:".yellow());
                    println!(
                        "  1. Go to https://github.com/{}/settings/pages",
                        repo.as_str()
                    );
                    println!("  2. Under 'Build and deployment', set Source to 'GitHub Actions'");
                    println!("  3. Save settings");
                }
            } else {
                anyhow::bail!("Failed to get Pages configuration: {}", e);
            }
        }
    }

    // Check if site is accessible
    if let Some(url) = site_url {
        let status = client.check_site_status(&url).await?;

        if !args.json {
            println!();
            println!("{}", "Site Accessibility:".bold());
            println!("{}", "─".repeat(60).dimmed());
            if status == 200 {
                println!("{} Site is live: {}", "✅".green(), url.cyan());
            } else if status == 404 {
                println!("{} Site returns 404: {}", "❌".red(), url.dimmed());
                println!("  {}", "Deployment may still be in progress...".yellow());
            } else {
                println!(
                    "{} Unexpected status {}: {}",
                    "⚠️".yellow(),
                    status,
                    url.dimmed()
                );
            }
        }
    }

    Ok(())
}

async fn workflow_status(args: &WorkflowStatusArgs) -> Result<()> {
    let repo_str = get_repository(&args.repo)?;
    let repo = RepoInfo::parse(&repo_str)?;
    let client = GitHubClient::new(repo.clone())?;

    let runs = client
        .get_workflow_runs(&repo, &args.workflow, args.limit)
        .await?;

    if args.json {
        println!("{}", serde_json::to_string_pretty(&runs)?);
    } else {
        print_workflow_runs(&runs, &args.workflow, &repo);
    }

    Ok(())
}

async fn trigger_workflow(args: &TriggerWorkflowArgs) -> Result<()> {
    let repo_str = get_repository(&args.repo)?;
    let repo = RepoInfo::parse(&repo_str)?;
    let client = GitHubClient::new(repo.clone())?;

    if !client.is_authenticated() {
        anyhow::bail!("GitHub token required to trigger workflows. Set GITHUB_TOKEN or GH_TOKEN environment variable.");
    }

    println!(
        "{} Triggering workflow {} on branch {}...",
        "🚀".bold(),
        args.workflow.cyan(),
        args.ref_name.yellow()
    );

    client
        .trigger_workflow(&repo, &args.workflow, &args.ref_name)
        .await?;

    println!("{} Workflow triggered successfully!", "✅".green());
    println!();
    println!(
        "View status at: https://github.com/{}/actions",
        repo.as_str()
    );

    Ok(())
}

fn print_pages_status(config: &ggen_core::PagesConfig, repo: &RepoInfo) {
    println!("{}", "GitHub Pages Configuration:".bold());
    println!("{}", "─".repeat(60).dimmed());

    if let Some(url) = &config.url {
        println!("URL:    {}", url.cyan());
    }

    if let Some(status) = &config.status {
        let status_icon = match status.as_str() {
            "built" => "✅",
            "building" => "🔄",
            _ => "❓",
        };
        println!("Status: {} {}", status_icon, status);
    }

    if let Some(source) = &config.source {
        if let Some(branch) = &source.branch {
            println!("Branch: {}", branch.yellow());
        }
        if let Some(path) = &source.path {
            println!("Path:   {}", path);
        }
    }

    if let Some(https) = config.https_enforced {
        println!(
            "HTTPS:  {}",
            if https {
                "✅ Enforced".green()
            } else {
                "❌ Not enforced".red()
            }
        );
    }

    println!();
    println!("{}", "Repository:".bold());
    println!("{}", "─".repeat(60).dimmed());
    println!("https://github.com/{}", repo.as_str());
}

fn print_workflow_runs(runs: &ggen_core::WorkflowRunsResponse, workflow: &str, repo: &RepoInfo) {
    println!(
        "{} {} {}",
        "Workflow Runs for".bold(),
        workflow.cyan(),
        format!("({})", repo.as_str()).dimmed()
    );
    println!("{}", "─".repeat(80).dimmed());

    if runs.workflow_runs.is_empty() {
        println!("No runs found for this workflow");
        return;
    }

    println!(
        "{:<8} {:<12} {:<12} {:<15} {:<25} {}",
        "RUN".bold(),
        "STATUS".bold(),
        "CONCLUSION".bold(),
        "BRANCH".bold(),
        "CREATED".bold(),
        "URL".bold()
    );
    println!("{}", "─".repeat(80).dimmed());

    for run in &runs.workflow_runs {
        let status_icon = match run.status.as_str() {
            "completed" => "✅",
            "in_progress" => "🔄",
            "queued" => "⏳",
            _ => "❓",
        };

        let conclusion_icon = match run.conclusion.as_deref() {
            Some("success") => "✅".green(),
            Some("failure") => "❌".red(),
            Some("cancelled") => "⏹️".yellow(),
            _ => "─".dimmed(),
        };

        // Extract just the date and time part of the RFC3339 string
        let created = run
            .created_at
            .split('T')
            .next()
            .and_then(|date| {
                run.created_at.split('T').nth(1).map(|time| {
                    let time_part = time.split('.').next().unwrap_or(time);
                    format!("{} {}", date, time_part)
                })
            })
            .unwrap_or_else(|| run.created_at.clone());

        println!(
            "{:<8} {:<12} {:<12} {:<15} {:<25} {}",
            format!("#{}", run.run_number),
            format!("{} {}", status_icon, run.status),
            format!(
                "{} {}",
                conclusion_icon,
                run.conclusion.as_deref().unwrap_or("-")
            ),
            run.head_branch.yellow(),
            created.dimmed(),
            run.html_url.cyan()
        );
    }

    println!();
    println!("Total runs: {}", runs.total_count);
}

/// Get repository from argument or git remote
fn get_repository(repo_arg: &Option<String>) -> Result<String> {
    if let Some(repo) = repo_arg {
        return Ok(repo.clone());
    }

    // Try to get from git remote
    let output = std::process::Command::new("git")
        .args(["remote", "get-url", "origin"])
        .output()?;

    if !output.status.success() {
        anyhow::bail!(
            "Could not determine repository. Provide --repo or run from a git repository."
        );
    }

    let remote_url = String::from_utf8(output.stdout)?.trim().to_string();

    // Parse GitHub URL (supports both HTTPS and SSH)
    // HTTPS: https://github.com/owner/repo.git
    // SSH: git@github.com:owner/repo.git
    let repo = if remote_url.contains("github.com") {
        let parts: Vec<&str> = if remote_url.contains("https://") {
            remote_url
                .trim_end_matches(".git")
                .split("github.com/")
                .collect()
        } else {
            remote_url
                .trim_end_matches(".git")
                .split("github.com:")
                .collect()
        };

        if parts.len() == 2 {
            parts[1].to_string()
        } else {
            anyhow::bail!(
                "Could not parse GitHub repository from remote URL: {}",
                remote_url
            );
        }
    } else {
        anyhow::bail!("Remote URL is not a GitHub repository: {}", remote_url);
    };

    Ok(repo)
}
