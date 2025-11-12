//! Source module - Data source connectors

use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SourceType {
    Csv,
    Json,
    Rdf,
    Sql,
    Api,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceConfig {
    pub source_type: SourceType,
    pub location: String,
    pub options: std::collections::HashMap<String, String>,
}

pub struct Source {
    config: SourceConfig,
}

impl Source {
    pub fn csv(path: &str) -> Result<Self> {
        Ok(Self {
            config: SourceConfig {
                source_type: SourceType::Csv,
                location: path.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn json(path: &str) -> Result<Self> {
        Ok(Self {
            config: SourceConfig {
                source_type: SourceType::Json,
                location: path.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn rdf(uri: &str) -> Result<Self> {
        Ok(Self {
            config: SourceConfig {
                source_type: SourceType::Rdf,
                location: uri.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn sql(connection: &str) -> Result<Self> {
        Ok(Self {
            config: SourceConfig {
                source_type: SourceType::Sql,
                location: connection.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn api(url: &str) -> Result<Self> {
        Ok(Self {
            config: SourceConfig {
                source_type: SourceType::Api,
                location: url.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn with_name(self, _name: &str) -> Self {
        self
    }

    pub fn with_query(self, _query: &str) -> Self {
        self
    }

    pub fn with_pagination(self, _param: &str, _start: usize, _pages: usize) -> Self {
        self
    }

    pub fn with_rate_limit(self, _limit: usize, _duration: std::time::Duration) -> Self {
        self
    }

    pub fn with_auth_token(self, _token: &str) -> Result<Self> {
        Ok(self)
    }
}
