use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Endpoint {
    pub path: String,
    pub method: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Api {
    pub endpoints: Vec<Endpoint>,
}

impl Default for Api {
    fn default() -> Self {
        Self::new()
    }
}

impl Api {
    pub fn new() -> Self {
        Self {
            endpoints: Vec::new(),
        }
    }

    pub fn add_endpoint(&mut self, endpoint: Endpoint) {
        self.endpoints.push(endpoint);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_api_creation() {
        let api = Api::new();
        assert_eq!(api.endpoints.len(), 0);
    }

    #[test]
    fn test_add_endpoint() {
        let mut api = Api::new();
        api.add_endpoint(Endpoint {
            path: "/users".to_string(),
            method: "GET".to_string(),
            name: "list_users".to_string(),
        });
        assert_eq!(api.endpoints.len(), 1);
    }
}
