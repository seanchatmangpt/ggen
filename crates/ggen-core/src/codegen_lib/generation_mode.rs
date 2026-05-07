//! Generation mode for output file handling

/// Generation mode for output files
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenerationMode {
    /// Overwrite existing file
    Overwrite,
    /// Append to existing file
    Append,
    /// Skip if file exists
    SkipIfExists,
}
