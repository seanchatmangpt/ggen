//! Mock implementations for testing

use std::sync::{Arc, Mutex};

/// Mock HTTP client for testing marketplace integration
#[derive(Clone, Default)]
pub struct MockHttpClient {
    responses: Arc<Mutex<Vec<MockResponse>>>,
}

pub struct MockResponse {
    pub url_pattern: String,
    pub status: u16,
    pub body: String,
}

impl MockHttpClient {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_response(&mut self, url_pattern: String, status: u16, body: String) {
        self.responses.lock().unwrap().push(MockResponse {
            url_pattern,
            status,
            body,
        });
    }

    pub fn get_response(&self, url: &str) -> Option<MockResponse> {
        let responses = self.responses.lock().unwrap();
        responses.iter()
            .find(|r| url.contains(&r.url_pattern))
            .map(|r| MockResponse {
                url_pattern: r.url_pattern.clone(),
                status: r.status,
                body: r.body.clone(),
            })
    }
}

/// Mock filesystem operations
pub struct MockFs {
    files: Arc<Mutex<std::collections::HashMap<String, Vec<u8>>>>,
}

impl MockFs {
    pub fn new() -> Self {
        Self {
            files: Arc::new(Mutex::new(std::collections::HashMap::new())),
        }
    }

    pub fn add_file(&mut self, path: String, content: Vec<u8>) {
        self.files.lock().unwrap().insert(path, content);
    }

    pub fn read_file(&self, path: &str) -> Option<Vec<u8>> {
        self.files.lock().unwrap().get(path).cloned()
    }

    pub fn exists(&self, path: &str) -> bool {
        self.files.lock().unwrap().contains_key(path)
    }
}
