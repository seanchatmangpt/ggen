use utils::error::Result;

pub fn run() -> Result<()> {
    let items = core::fs::list_templates(std::path::Path::new("templates"))?;
    for it in items {
        println!("{}/{}", it.scope, it.action);
    }
    Ok(())
}
