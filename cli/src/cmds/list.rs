use std::{env, path::PathBuf};

pub fn run() -> utils::error::Result<()> {
    let root: PathBuf = env::current_dir().unwrap();
    let items = core::engine::list(&root)?;
    for it in items {
        // it.scope, it.action, it.count
        println!("{}/{}  ({})", it.scope, it.action, it.count);
    }
    Ok(())
}
