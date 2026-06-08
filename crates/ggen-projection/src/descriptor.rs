use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackTemplateDescriptor {
    pub path: PathBuf,
    pub description: String,
    #[serde(default)]
    pub variables: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackDescriptor {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub license: String,
    pub dependencies: BTreeMap<String, String>, // Pack ID -> Semver constraint
    pub templates: Vec<PackTemplateDescriptor>,
    pub query_aliases: BTreeMap<String, String>,
}

impl PackDescriptor {
    pub fn from_toml(content: &str) -> Result<Self, anyhow::Error> {
        let descriptor: Self = toml::from_str(content)?;
        descriptor.validate()?;
        Ok(descriptor)
    }

    pub fn validate(&self) -> Result<(), anyhow::Error> {
        if self.id.trim().is_empty() {
            return Err(anyhow::anyhow!("Required metadata field 'id' is empty"));
        }
        if self.name.trim().is_empty() {
            return Err(anyhow::anyhow!("Required metadata field 'name' is empty"));
        }
        if self.version.trim().is_empty() {
            return Err(anyhow::anyhow!(
                "Required metadata field 'version' is empty"
            ));
        }
        if self.templates.is_empty() {
            return Err(anyhow::anyhow!(
                "Required metadata field 'templates' is empty"
            ));
        }
        for (i, t) in self.templates.iter().enumerate() {
            if t.path.as_os_str().is_empty() {
                return Err(anyhow::anyhow!("Template {} path is empty", i));
            }
        }
        Ok(())
    }
}
