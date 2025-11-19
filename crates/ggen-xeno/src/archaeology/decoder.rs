//! Decoding implementations for alien data formats.

use super::{DecodingScheme, PatternAnalysis, Pattern, PatternType};
use crate::ontology::OntologyError;

/// Binary XOR decoding scheme
pub struct XorDecoder {
    key: Vec<u8>,
}

impl XorDecoder {
    /// Create a new XOR decoder with a key
    #[must_use]
    pub fn new(key: Vec<u8>) -> Self {
        Self { key }
    }
}

impl DecodingScheme for XorDecoder {
    fn decode(&self, data: &[u8]) -> Result<Vec<u8>, OntologyError> {
        if self.key.is_empty() {
            return Err(OntologyError::Unknown("Empty XOR key".to_string()));
        }

        let decoded: Vec<u8> = data
            .iter()
            .enumerate()
            .map(|(i, &byte)| byte ^ self.key[i % self.key.len()])
            .collect();

        Ok(decoded)
    }

    fn can_decode(&self, _data: &[u8]) -> bool {
        !self.key.is_empty()
    }
}

/// Pattern analyzer for alien artifacts
pub struct PatternAnalyzer;

impl PatternAnalyzer {
    /// Create a new pattern analyzer
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Analyze an artifact for patterns
    #[must_use]
    pub fn analyze(&self, data: &[u8]) -> PatternAnalysis {
        let mut patterns = Vec::new();

        // Detect repetitions
        patterns.extend(self.detect_repetitions(data));

        // Detect potential headers (first 256 bytes)
        if data.len() > 256 {
            patterns.push(Pattern {
                pattern_type: PatternType::Header,
                offset: 0,
                length: 256,
                description: "Potential header section".to_string(),
                confidence: 0.5,
            });
        }

        // Calculate overall confidence
        let confidence = if patterns.is_empty() {
            0.0
        } else {
            patterns.iter().map(|p| p.confidence).sum::<f64>() / patterns.len() as f64
        };

        PatternAnalysis {
            patterns,
            confidence,
            method: "Statistical pattern recognition".to_string(),
        }
    }

    fn detect_repetitions(&self, data: &[u8]) -> Vec<Pattern> {
        let mut patterns = Vec::new();
        let window_size = 8;

        if data.len() < window_size * 2 {
            return patterns;
        }

        for i in 0..data.len() - window_size {
            let window = &data[i..i + window_size];

            // Look for repetitions
            for j in (i + window_size)..data.len() - window_size {
                if &data[j..j + window_size] == window {
                    patterns.push(Pattern {
                        pattern_type: PatternType::Repetition,
                        offset: i,
                        length: window_size,
                        description: format!("Repeated pattern at offsets {} and {}", i, j),
                        confidence: 0.7,
                    });
                    break;
                }
            }
        }

        patterns
    }
}

impl Default for PatternAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
