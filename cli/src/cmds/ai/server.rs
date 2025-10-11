//! Start the MCP server

use clap::Args;
use ggen_utils::error::Result;

#[derive(Debug, Args)]
pub struct ServerArgs {
    /// Port to run the server on
    #[arg(short, long, default_value = "3000")]
    pub port: u16,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,
}

pub async fn run(args: &ServerArgs) -> Result<()> {
    println!("Starting ggen-ai MCP server...");
    println!("Port: {}", args.port);

    if args.openai {
        println!("Using OpenAI client");
    } else if args.anthropic {
        println!("Using Anthropic client");
    } else if args.ollama {
        println!("Using Ollama client");
    } else {
        println!("No client specified, using default");
    }

    // For now, just print instructions
    println!("\nTo start the actual MCP server, run:");
    println!("cargo run --bin ggen-ai-mcp");

    if args.openai {
        println!("OPENAI_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.anthropic {
        println!("ANTHROPIC_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.ollama {
        println!("USE_OLLAMA=true cargo run --bin ggen-ai-mcp");
    }

    Ok(())
}

