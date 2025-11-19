//! Artifact reconstruction tools for damaged or incomplete data.

use super::Artifact;
use crate::ontology::OntologyError;

/// Reconstructor for damaged artifacts
pub struct Reconstructor;

impl Reconstructor {
    /// Create a new reconstructor
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Attempt to repair damaged data
    ///
    /// # Errors
    ///
    /// Returns an error if reconstruction is not possible
    pub fn reconstruct(&self, artifact: &Artifact, corruption_map: &[bool]) -> Result<Vec<u8>, OntologyError> {
        if artifact.raw_data.len() != corruption_map.len() {
            return Err(OntologyError::Unknown(
                "Corruption map length mismatch".to_string(),
            ));
        }

        let mut reconstructed = artifact.raw_data.clone();

        // Simple interpolation for corrupted bytes
        for i in 0..corruption_map.len() {
            if corruption_map[i] {
                // Find nearest uncorrupted neighbors
                let prev = self.find_previous_uncorrupted(&artifact.raw_data, corruption_map, i);
                let next = self.find_next_uncorrupted(&artifact.raw_data, corruption_map, i);

                reconstructed[i] = match (prev, next) {
                    (Some(p), Some(n)) => {
                        // Interpolate
                        ((artifact.raw_data[p] as u16 + artifact.raw_data[n] as u16) / 2) as u8
                    }
                    (Some(p), None) => artifact.raw_data[p],
                    (None, Some(n)) => artifact.raw_data[n],
                    (None, None) => 0, // No reference, use zero
                };
            }
        }

        Ok(reconstructed)
    }

    fn find_previous_uncorrupted(&self, _data: &[u8], corruption_map: &[bool], from: usize) -> Option<usize> {
        for i in (0..from).rev() {
            if !corruption_map[i] {
                return Some(i);
            }
        }
        None
    }

    fn find_next_uncorrupted(&self, _data: &[u8], corruption_map: &[bool], from: usize) -> Option<usize> {
        for i in (from + 1)..corruption_map.len() {
            if !corruption_map[i] {
                return Some(i);
            }
        }
        None
    }
}

impl Default for Reconstructor {
    fn default() -> Self {
        Self::new()
    }
}
