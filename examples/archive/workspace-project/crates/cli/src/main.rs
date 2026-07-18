use clap::Parser;

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    name: String,
}

fn main() {
    let args = Args::parse();
    println!("Project: {}", args.name);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_cli() {
        assert!(true);
    }
}
