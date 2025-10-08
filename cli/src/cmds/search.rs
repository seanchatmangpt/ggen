use anyhow::Result;
use clap::Args;
use core::RegistryClient;
use serde_json;

#[derive(Args, Debug)]
pub struct SearchArgs {
    /// Search query
    pub query: String,
    
    /// Output results as JSON
    #[arg(long)]
    pub json: bool,
}

pub async fn run(args: &SearchArgs) -> Result<()> {
    let registry_client = RegistryClient::new()?;
    
    let results = registry_client.search(&args.query).await?;
    
    if args.json {
        // Output JSON format
        let json_output = serde_json::to_string_pretty(&results)?;
        println!("{}", json_output);
    } else {
        // Output human-readable format
        if results.is_empty() {
            println!("No rpacks found matching '{}'", args.query);
            return Ok(());
        }
        
        println!("Found {} rpack(s) matching '{}':", results.len(), args.query);
        println!();
        
        // Header
        println!("{:<40} {:<12} {:<20} {}", "ID", "LATEST", "TAGS", "DESCRIPTION");
        println!("{}", "-".repeat(80));
        
        // Results
        for result in results {
            let tags = result.tags.join(", ");
            let description = if result.description.len() > 30 {
                format!("{}...", &result.description[..27])
            } else {
                result.description
            };
            
            println!("{:<40} {:<12} {:<20} {}", 
                result.id, result.latest_version, tags, description);
        }
    }
    
    Ok(())
}
