use utils::error::Result;
use crate::rdf::SimpleStore;

pub fn validate_graph(data: &SimpleStore, shapes: &SimpleStore) -> Result<()> {
    validate(data, shapes)
}

pub fn validate(_data: &SimpleStore, shapes: &SimpleStore) -> Result<()> {
    if shapes.len() == 0 {
        return Ok(()); // No shapes to validate against
    }

    // For the happy path, just return OK
    // TODO: Implement actual SHACL validation
    Ok(())
}