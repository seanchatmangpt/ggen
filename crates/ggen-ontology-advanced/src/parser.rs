//! Zero-copy streaming parser for RDF data
//!
//! This module provides high-performance parsers that minimize memory allocations
//! through strategic use of lifetimes and borrowing.

use crate::error::{OntologyError, Result};
use crate::traits::{StreamingParse, ZeroCopyParse};
use std::borrow::Cow;

/// A zero-copy reference to an RDF triple
///
/// All fields borrow from the original input data, enabling zero-copy parsing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TripleRef<'a> {
    /// Subject of the triple (borrows from input)
    pub subject: Cow<'a, str>,
    /// Predicate of the triple (borrows from input)
    pub predicate: Cow<'a, str>,
    /// Object of the triple (borrows from input)
    pub object: Cow<'a, str>,
}

impl<'a> TripleRef<'a> {
    /// Create a new triple reference
    pub const fn new(subject: Cow<'a, str>, predicate: Cow<'a, str>, object: Cow<'a, str>) -> Self {
        Self {
            subject,
            predicate,
            object,
        }
    }

    /// Convert to an owned triple
    pub fn into_owned(self) -> Triple {
        Triple {
            subject: self.subject.into_owned(),
            predicate: self.predicate.into_owned(),
            object: self.object.into_owned(),
        }
    }
}

/// An owned RDF triple
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Triple {
    /// Subject of the triple
    pub subject: String,
    /// Predicate of the triple
    pub predicate: String,
    /// Object of the triple
    pub object: String,
}

impl Triple {
    /// Create a new triple
    pub fn new(subject: String, predicate: String, object: String) -> Self {
        Self {
            subject,
            predicate,
            object,
        }
    }

    /// Borrow as a triple reference
    pub fn as_ref(&self) -> TripleRef<'_> {
        TripleRef {
            subject: Cow::Borrowed(&self.subject),
            predicate: Cow::Borrowed(&self.predicate),
            object: Cow::Borrowed(&self.object),
        }
    }
}

/// Zero-copy streaming parser for Turtle/N-Triples format
///
/// This parser uses lifetime annotations to avoid copying data,
/// yielding references to the original input buffer.
pub struct StreamingParser {
    /// Parser configuration
    config: ParserConfig,
}

/// Configuration for the streaming parser
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// Buffer size for streaming
    pub buffer_size: usize,
    /// Whether to validate IRIs
    pub validate_iris: bool,
    /// Whether to parse in strict mode
    pub strict_mode: bool,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            buffer_size: 8192,
            validate_iris: true,
            strict_mode: false,
        }
    }
}

impl StreamingParser {
    /// Create a new streaming parser with default configuration
    pub const fn new() -> Self {
        Self {
            config: ParserConfig {
                buffer_size: 8192,
                validate_iris: true,
                strict_mode: false,
            },
        }
    }

    /// Create a parser with custom configuration
    pub const fn with_config(config: ParserConfig) -> Self {
        Self { config }
    }

    /// Parse a string into a streaming iterator of triples
    pub fn parse_str(&self, input: &str) -> Result<TripleIterator<'_>> {
        TripleIterator::new(input, &self.config)
    }

    /// Parse bytes into a streaming iterator of triples
    pub fn parse_bytes<'a>(&self, input: &'a [u8]) -> Result<TripleIterator<'a>> {
        let input_str = std::str::from_utf8(input)
            .map_err(|e| OntologyError::ParseError(format!("Invalid UTF-8: {}", e)))?;
        TripleIterator::new(input_str, &self.config)
    }
}

impl Default for StreamingParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator that yields zero-copy triple references
pub struct TripleIterator<'a> {
    /// The input data being parsed
    input: &'a str,
    /// Current position in the input
    position: usize,
    /// Parser configuration
    config: &'a ParserConfig,
    /// Line number for error reporting
    line: usize,
}

impl<'a> TripleIterator<'a> {
    /// Create a new triple iterator
    fn new(input: &'a str, config: &'a ParserConfig) -> Result<Self> {
        Ok(Self {
            input,
            position: 0,
            config,
            line: 1,
        })
    }

    /// Parse a single triple from the current position
    fn parse_triple(&mut self) -> Result<Option<TripleRef<'a>>> {
        // Skip whitespace and comments
        self.skip_whitespace();

        if self.position >= self.input.len() {
            return Ok(None);
        }

        // Simple N-Triples parser (for demonstration)
        // In production, this would use a proper parser like rio_turtle
        let remaining = &self.input[self.position..];

        // Skip comments
        if remaining.starts_with('#') {
            if let Some(newline_pos) = remaining.find('\n') {
                self.position += newline_pos + 1;
                self.line += 1;
                return self.parse_triple();
            } else {
                self.position = self.input.len();
                return Ok(None);
            }
        }

        // Skip prefix declarations
        if remaining.starts_with("@prefix") || remaining.starts_with("@base") {
            if let Some(dot_pos) = remaining.find('.') {
                self.position += dot_pos + 1;
                return self.parse_triple();
            }
        }

        // Parse subject, predicate, object
        let (subject, new_pos) = self.parse_term(self.position)?;
        self.position = new_pos;
        self.skip_whitespace();

        let (predicate, new_pos) = self.parse_term(self.position)?;
        self.position = new_pos;
        self.skip_whitespace();

        let (object, new_pos) = self.parse_term(self.position)?;
        self.position = new_pos;
        self.skip_whitespace();

        // Expect '.' at end of triple
        if self.position < self.input.len() && self.input.as_bytes()[self.position] == b'.' {
            self.position += 1;
        }

        Ok(Some(TripleRef::new(subject, predicate, object)))
    }

    /// Parse a single RDF term (IRI, blank node, or literal)
    fn parse_term(&self, start: usize) -> Result<(Cow<'a, str>, usize)> {
        let bytes = self.input.as_bytes();
        if start >= bytes.len() {
            return Err(OntologyError::ParseError(
                "Unexpected end of input".to_string(),
            ));
        }

        match bytes[start] {
            b'<' => self.parse_iri(start),
            b'_' => self.parse_blank_node(start),
            b'"' => self.parse_literal(start),
            _ => Err(OntologyError::ParseError(format!(
                "Unexpected character at line {}: {}",
                self.line,
                bytes[start] as char
            ))),
        }
    }

    /// Parse an IRI
    fn parse_iri(&self, start: usize) -> Result<(Cow<'a, str>, usize)> {
        let bytes = self.input.as_bytes();
        let mut pos = start + 1; // Skip '<'

        while pos < bytes.len() && bytes[pos] != b'>' {
            pos += 1;
        }

        if pos >= bytes.len() {
            return Err(OntologyError::ParseError("Unterminated IRI".to_string()));
        }

        let iri = &self.input[start..=pos];
        Ok((Cow::Borrowed(iri), pos + 1))
    }

    /// Parse a blank node
    fn parse_blank_node(&self, start: usize) -> Result<(Cow<'a, str>, usize)> {
        let bytes = self.input.as_bytes();
        let mut pos = start;

        while pos < bytes.len()
            && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_' || bytes[pos] == b':')
        {
            pos += 1;
        }

        let blank = &self.input[start..pos];
        Ok((Cow::Borrowed(blank), pos))
    }

    /// Parse a literal
    fn parse_literal(&self, start: usize) -> Result<(Cow<'a, str>, usize)> {
        let bytes = self.input.as_bytes();
        let mut pos = start + 1; // Skip opening '"'
        let mut escaped = false;

        while pos < bytes.len() {
            if escaped {
                escaped = false;
                pos += 1;
                continue;
            }

            match bytes[pos] {
                b'\\' => escaped = true,
                b'"' => {
                    // Check for language tag or datatype
                    let mut end = pos + 1;
                    if end < bytes.len() && bytes[end] == b'@' {
                        // Language tag
                        while end < bytes.len()
                            && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'-')
                        {
                            end += 1;
                        }
                    } else if end + 1 < bytes.len()
                        && bytes[end] == b'^'
                        && bytes[end + 1] == b'^'
                    {
                        // Datatype
                        end += 2;
                        if end < bytes.len() && bytes[end] == b'<' {
                            while end < bytes.len() && bytes[end] != b'>' {
                                end += 1;
                            }
                            if end < bytes.len() {
                                end += 1;
                            }
                        }
                    }
                    let literal = &self.input[start..end];
                    return Ok((Cow::Borrowed(literal), end));
                }
                _ => {}
            }
            pos += 1;
        }

        Err(OntologyError::ParseError(
            "Unterminated literal".to_string(),
        ))
    }

    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        let bytes = self.input.as_bytes();
        while self.position < bytes.len() && bytes[self.position].is_ascii_whitespace() {
            if bytes[self.position] == b'\n' {
                self.line += 1;
            }
            self.position += 1;
        }
    }
}

impl<'a> Iterator for TripleIterator<'a> {
    type Item = Result<TripleRef<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parse_triple() {
            Ok(Some(triple)) => Some(Ok(triple)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

impl StreamingParse for StreamingParser {
    type Item = Triple;
    type Stream = std::vec::IntoIter<Result<Triple>>;

    fn parse_stream(&self, input: &str) -> Result<Self::Stream> {
        let triples: Vec<Result<Triple>> = self
            .parse_str(input)?
            .map(|r| r.map(|t| t.into_owned()))
            .collect();
        Ok(triples.into_iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_triple() {
        let parser = StreamingParser::new();
        let input = "<http://example.org/subject> <http://example.org/predicate> <http://example.org/object> .";

        let mut iter = parser.parse_str(input).expect("Failed to parse");
        let triple = iter.next().expect("Expected triple").expect("Parse error");

        assert_eq!(triple.subject, "<http://example.org/subject>");
        assert_eq!(triple.predicate, "<http://example.org/predicate>");
        assert_eq!(triple.object, "<http://example.org/object>");
    }

    #[test]
    fn test_multiple_triples() {
        let parser = StreamingParser::new();
        let input = r#"
            <http://example.org/s1> <http://example.org/p1> <http://example.org/o1> .
            <http://example.org/s2> <http://example.org/p2> "literal" .
        "#;

        let triples: Vec<_> = parser
            .parse_str(input)
            .expect("Failed to parse")
            .collect::<Result<Vec<_>>>()
            .expect("Parse errors");

        assert_eq!(triples.len(), 2);
    }
}
