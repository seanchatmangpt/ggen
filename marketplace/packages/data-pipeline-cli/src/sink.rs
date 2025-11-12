//! Sink module - Data sink connectors

use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SinkType {
    Csv,
    Json,
    Rdf,
    Sql,
    Api,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SinkConfig {
    pub sink_type: SinkType,
    pub location: String,
    pub options: std::collections::HashMap<String, String>,
}

pub struct Sink {
    config: SinkConfig,
}

impl Sink {
    pub fn csv(path: &str) -> Result<Self> {
        Ok(Self {
            config: SinkConfig {
                sink_type: SinkType::Csv,
                location: path.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn json(path: &str) -> Result<Self> {
        Ok(Self {
            config: SinkConfig {
                sink_type: SinkType::Json,
                location: path.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn rdf(uri: &str) -> Result<Self> {
        Ok(Self {
            config: SinkConfig {
                sink_type: SinkType::Rdf,
                location: uri.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn sql(connection: &str) -> Result<Self> {
        Ok(Self {
            config: SinkConfig {
                sink_type: SinkType::Sql,
                location: connection.to_string(),
                options: Default::default(),
            },
        })
    }

    pub fn api(url: &str) -> Result<Self> {
        Ok(Self {
            config: SinkConfig {
                sink_type: SinkType::Api,
                location: url.to_string(),
                options: Default::default(),
            },
        })
    }
}
