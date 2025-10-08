use std::{collections::HashMap, env, path::PathBuf};

pub fn run(scope: &str, action: &str, vars: &Vec<String>, dry_run: bool) -> utils::error::Result<()> {
    let root: PathBuf = env::current_dir().unwrap();
    let mut map = HashMap::new();
    for kv in vars {
        if let Some((k,v)) = kv.split_once('=') { map.insert(k.to_string(), v.to_string()); }
    }
    let report = core::engine::project(&root, scope, action, map, dry_run)?;
    for a in report.artifacts {
        println!("{}  {}", a.key, a.out_path);
    }
    Ok(())
}
