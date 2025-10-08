use anyhow::Result;

mod marketplace_demo;

fn main() -> Result<()> {
    marketplace_demo::run_marketplace_demo()
}
