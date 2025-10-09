use clap::Args;
use colored::*;
use ggen_core::RegistryClient;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct CategoriesArgs {
    /// Show popular keywords instead of categories
    #[arg(long)]
    pub keywords: bool,

    /// Show detailed statistics
    #[arg(short, long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,
}

pub async fn run(args: &CategoriesArgs) -> Result<()> {
    let registry_client = RegistryClient::new()?;

    if args.keywords {
        let keywords = registry_client.get_popular_keywords().await?;
        display_keywords(&keywords, args.detailed, args.json)?;
    } else {
        let categories = registry_client.get_popular_categories().await?;
        display_categories(&categories, args.detailed, args.json)?;
    }

    Ok(())
}

fn display_categories(categories: &[(String, u64)], detailed: bool, json: bool) -> Result<()> {
    if json {
        let json_output = serde_json::to_string_pretty(categories)?;
        println!("{}", json_output);
        return Ok(());
    }

    println!("📂 Popular Categories");
    println!("===================");

    if detailed {
        for (i, (category, count)) in categories.iter().enumerate() {
            println!(
                "{}. {} ({})",
                i + 1,
                category.bold().cyan(),
                format!("{} templates", count).dimmed()
            );
        }
    } else {
        let categories_str = categories
            .iter()
            .take(10)
            .map(|(cat, _)| cat.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        println!("{}", categories_str.cyan());
    }

    Ok(())
}

fn display_keywords(keywords: &[(String, u64)], detailed: bool, json: bool) -> Result<()> {
    if json {
        let json_output = serde_json::to_string_pretty(keywords)?;
        println!("{}", json_output);
        return Ok(());
    }

    println!("🏷️  Popular Keywords");
    println!("===================");

    if detailed {
        for (i, (keyword, count)) in keywords.iter().enumerate() {
            println!(
                "{}. {} ({})",
                i + 1,
                keyword.bold().green(),
                format!("{} templates", count).dimmed()
            );
        }
    } else {
        let keywords_str = keywords
            .iter()
            .take(15)
            .map(|(kw, _)| kw.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        println!("{}", keywords_str.green());
    }

    Ok(())
}
