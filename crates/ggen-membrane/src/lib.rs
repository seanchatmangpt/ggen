use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;

use genesis_core::primitives::{Pair2, Refusal, RelationPage};
use knhk_construct8::models::{Construct8Packet, SymbolTable};
use rio_api::model::{NamedNode, Subject, Term};
use rio_api::parser::TriplesParser;
use rio_turtle::TurtleParser;

#[derive(Debug, Error)]
pub enum MembraneError {
    #[error(
        "Symbol table/byte table overflow: at most 256 unique symbols allowed per relation side"
    )]
    ByteTableOverflow,

    #[error("Refusal from core: {0:?}")]
    Refusal(Refusal),

    #[error("Index out of bounds on {side} side: tried to look up index {index} but table size is {max}")]
    IndexOutOfBounds {
        side: &'static str,
        index: u8,
        max: usize,
    },

    #[error("Serialization or parsing error: {0}")]
    ParseError(String),

    #[error("Predicate not found: {0}")]
    PredicateNotFound(String),
}

impl From<Refusal> for MembraneError {
    fn from(err: Refusal) -> Self {
        MembraneError::Refusal(err)
    }
}

/// A page that maps byte values (0..255) to actual global symbol IDs.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq, Default)]
pub struct SymbolPage {
    pub symbols: Vec<u32>,
}

/// Builder for constructing left and right byte symbol tables.
#[derive(Debug, Clone, Default)]
pub struct SymbolPageBuilder {
    pub symbols: Vec<u32>,
    pub map: HashMap<u32, u8>,
}

impl SymbolPageBuilder {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Register a symbol ID, returning the mapped u8 byte or error if full.
    pub fn register(&mut self, symbol_id: u32) -> Result<u8, MembraneError> {
        if let Some(&byte) = self.map.get(&symbol_id) {
            Ok(byte)
        } else {
            if self.symbols.len() >= 256 {
                return Err(MembraneError::ByteTableOverflow);
            }
            let byte = self.symbols.len() as u8;
            self.symbols.push(symbol_id);
            self.map.insert(symbol_id, byte);
            Ok(byte)
        }
    }

    /// Build the immutable `SymbolPage` mapping.
    pub fn build(self) -> SymbolPage {
        SymbolPage {
            symbols: self.symbols,
        }
    }
}

/// Binds assumed relation middles to public expansion laws.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct PublicExpansionLaw {
    pub predicate: u32,
    pub left_page: SymbolPage,
    pub right_page: SymbolPage,
}

impl PublicExpansionLaw {
    pub fn new(predicate: u32, left_page: SymbolPage, right_page: SymbolPage) -> Self {
        Self {
            predicate,
            left_page,
            right_page,
        }
    }

    /// Expands the byte-packed `RelationPage` back to full triples of `(subject, predicate, object)` IDs.
    pub fn expand<const CAP: usize>(
        &self,
        page: &RelationPage<CAP>,
    ) -> Result<Vec<(u32, u32, u32)>, MembraneError> {
        let mut triples = Vec::new();
        for i in 0..page.len {
            let pair = page.pairs[i as usize];
            let left_symbol = self
                .left_page
                .symbols
                .get(pair.left as usize)
                .copied()
                .ok_or(MembraneError::IndexOutOfBounds {
                    side: "left",
                    index: pair.left,
                    max: self.left_page.symbols.len(),
                })?;
            let right_symbol = self
                .right_page
                .symbols
                .get(pair.right as usize)
                .copied()
                .ok_or(MembraneError::IndexOutOfBounds {
                    side: "right",
                    index: pair.right,
                    max: self.right_page.symbols.len(),
                })?;
            triples.push((left_symbol, self.predicate, right_symbol));
        }
        Ok(triples)
    }

    /// Expands a `RelationPage` into a vector of Construct8 packets.
    pub fn expand_to_packets<const CAP: usize>(
        &self,
        page: &RelationPage<CAP>,
        epoch: u64,
    ) -> Result<Vec<Construct8Packet>, MembraneError> {
        let triples = self.expand(page)?;
        let mut packets = Vec::new();
        let mut current_packet = Construct8Packet::new();
        current_packet.epoch = epoch;
        current_packet.law_ref = self.predicate as u64;

        for (s, p, o) in triples {
            if let Err(_) = current_packet.push(s, p, o) {
                packets.push(current_packet);
                current_packet = Construct8Packet::new();
                current_packet.epoch = epoch;
                current_packet.law_ref = self.predicate as u64;
                current_packet.push(s, p, o).unwrap();
            }
        }

        if !current_packet.is_empty() {
            packets.push(current_packet);
        }

        Ok(packets)
    }
}

/// High-level claim ingested from external shells.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct ExternalClaim {
    pub subject: String,
    pub predicate: String,
    pub object: String,
}

/// The core Membrane Foundry that adapts external, messy Claims into Construct8-compatible RelationPages and SymbolPages.
pub struct MembraneFoundry {
    pub symbol_table: Arc<SymbolTable>,
    pub relations: HashMap<u32, RelationPage<256>>,
    pub left_builders: HashMap<u32, SymbolPageBuilder>,
    pub right_builders: HashMap<u32, SymbolPageBuilder>,
}

/// Helper to parse a CSV line, respecting double-quoted fields with nested commas.
fn parse_csv_line(line: &str) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    let mut chars = line.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '"' => {
                if in_quotes {
                    if chars.peek() == Some(&'"') {
                        chars.next();
                        current.push('"');
                    } else {
                        in_quotes = false;
                    }
                } else {
                    in_quotes = true;
                }
            }
            ',' => {
                if in_quotes {
                    current.push(',');
                } else {
                    parts.push(current.trim().to_string());
                    current.clear();
                }
            }
            _ => {
                current.push(c);
            }
        }
    }
    parts.push(current.trim().to_string());
    if in_quotes {
        return None;
    }
    Some(parts)
}

impl MembraneFoundry {
    /// Create a new Membrane Foundry instance.
    pub fn new(symbol_table: Arc<SymbolTable>) -> Self {
        Self {
            symbol_table,
            relations: HashMap::new(),
            left_builders: HashMap::new(),
            right_builders: HashMap::new(),
        }
    }

    /// Ingest a single external claim.
    pub fn ingest_claim(&mut self, claim: ExternalClaim, epoch: u64) -> Result<(), MembraneError> {
        let subject = claim.subject.trim();
        let predicate = claim.predicate.trim();
        let object = claim.object.trim();

        if subject.is_empty() || predicate.is_empty() || object.is_empty() {
            return Err(MembraneError::ParseError(
                "Empty subject, predicate, or object claim field".to_string(),
            ));
        }

        let s_id = self.symbol_table.get_or_insert(subject);
        let p_id = self.symbol_table.get_or_insert(predicate);
        let o_id = self.symbol_table.get_or_insert(object);

        let left_builder = self
            .left_builders
            .entry(p_id)
            .or_insert_with(SymbolPageBuilder::new);
        let right_builder = self
            .right_builders
            .entry(p_id)
            .or_insert_with(SymbolPageBuilder::new);

        let left_byte = left_builder.register(s_id)?;
        let right_byte = right_builder.register(o_id)?;

        let relation_page = self
            .relations
            .entry(p_id)
            .or_insert_with(|| RelationPage::new(p_id));

        let pair = Pair2::new(left_byte, right_byte);
        if !relation_page.contains(pair) {
            relation_page.insert(pair, epoch)?;
        }

        Ok(())
    }

    /// Ingest multiple external claims.
    pub fn ingest_claims(
        &mut self,
        claims: Vec<ExternalClaim>,
        epoch: u64,
    ) -> Result<(), MembraneError> {
        for claim in claims {
            self.ingest_claim(claim, epoch)?;
        }
        Ok(())
    }

    /// Ingest claims parsed from a JSON array string.
    pub fn ingest_json_claims(&mut self, json_str: &str, epoch: u64) -> Result<(), MembraneError> {
        let claims: Vec<ExternalClaim> =
            serde_json::from_str(json_str).map_err(|e| MembraneError::ParseError(e.to_string()))?;
        self.ingest_claims(claims, epoch)
    }

    /// Ingest claims parsed from a CSV string.
    pub fn ingest_csv_claims(&mut self, csv_str: &str, epoch: u64) -> Result<(), MembraneError> {
        let mut claims = Vec::new();
        for (idx, line) in csv_str.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let parts = parse_csv_line(line).ok_or_else(|| {
                MembraneError::ParseError(format!(
                    "CSV line {} has mismatched or invalid double quotes: {}",
                    idx + 1,
                    line
                ))
            })?;
            if parts.len() != 3 {
                return Err(MembraneError::ParseError(format!(
                    "CSV line {} does not contain exactly 3 columns (found {}): {}",
                    idx + 1,
                    parts.len(),
                    line
                )));
            }
            claims.push(ExternalClaim {
                subject: parts[0].clone(),
                predicate: parts[1].clone(),
                object: parts[2].clone(),
            });
        }
        self.ingest_claims(claims, epoch)
    }

    /// Ingest claims parsed from an RDF/Turtle string.
    pub fn ingest_turtle_claims(
        &mut self,
        turtle_str: &str,
        epoch: u64,
    ) -> Result<(), MembraneError> {
        let mut claims = Vec::new();
        let mut parser = TurtleParser::new(turtle_str.as_bytes(), None);

        let format_named_node = |n: NamedNode<'_>| format!("<{}>", n.iri);

        let format_subject = |sub: Subject<'_>| match sub {
            Subject::NamedNode(n) => format_named_node(n),
            Subject::BlankNode(b) => format!("_:{}", b.id),
            Subject::Triple(_) => "<<nested_triple>>".to_string(),
        };

        let format_term = |term: Term<'_>| match term {
            Term::NamedNode(n) => format_named_node(n),
            Term::BlankNode(b) => format!("_:{}", b.id),
            Term::Literal(l) => match l {
                rio_api::model::Literal::Simple { value } => {
                    format!("\"{}\"", value.escape_debug())
                }
                rio_api::model::Literal::LanguageTaggedString { value, language } => {
                    format!("\"{}\"@{}", value.escape_debug(), language)
                }
                rio_api::model::Literal::Typed { value, datatype } => {
                    format!("\"{}\"^^<{}>", value.escape_debug(), datatype.iri)
                }
            },
            Term::Triple(_) => "<<nested_triple>>".to_string(),
        };

        let res: Result<(), rio_turtle::TurtleError> = parser.parse_all(&mut |t| {
            claims.push(ExternalClaim {
                subject: format_subject(t.subject),
                predicate: format_named_node(t.predicate),
                object: format_term(t.object),
            });
            Ok(())
        });

        res.map_err(|e| MembraneError::ParseError(e.to_string()))?;
        self.ingest_claims(claims, epoch)
    }

    /// Compile a `PublicExpansionLaw` for a given predicate path.
    pub fn build_expansion_law(
        &self,
        predicate: &str,
    ) -> Result<PublicExpansionLaw, MembraneError> {
        let p_id = self
            .symbol_table
            .lookup(self.symbol_table.get_or_insert(predicate))
            .and_then(|_| self.symbol_table.get_or_insert(predicate).into())
            .ok_or_else(|| MembraneError::PredicateNotFound(predicate.to_string()))?;

        let left_builder = self
            .left_builders
            .get(&p_id)
            .ok_or_else(|| MembraneError::PredicateNotFound(predicate.to_string()))?;
        let right_builder = self
            .right_builders
            .get(&p_id)
            .ok_or_else(|| MembraneError::PredicateNotFound(predicate.to_string()))?;

        Ok(PublicExpansionLaw::new(
            p_id,
            left_builder.clone().build(),
            right_builder.clone().build(),
        ))
    }

    /// Fetch a compiled `RelationPage` for a given predicate.
    pub fn get_relation_page(&self, predicate: &str) -> Result<RelationPage<256>, MembraneError> {
        let p_id = self.symbol_table.get_or_insert(predicate);

        self.relations
            .get(&p_id)
            .cloned()
            .ok_or_else(|| MembraneError::PredicateNotFound(predicate.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ingest_and_expand() {
        let symbol_table = Arc::new(SymbolTable::new());
        let mut foundry = MembraneFoundry::new(symbol_table.clone());

        let claims = vec![
            ExternalClaim {
                subject: "Alice".to_string(),
                predicate: "knows".to_string(),
                object: "Bob".to_string(),
            },
            ExternalClaim {
                subject: "Bob".to_string(),
                predicate: "knows".to_string(),
                object: "Charlie".to_string(),
            },
        ];

        foundry.ingest_claims(claims, 1).unwrap();

        let law = foundry.build_expansion_law("knows").unwrap();
        let page = foundry.get_relation_page("knows").unwrap();

        assert_eq!(page.len, 2);

        let expanded = law.expand(&page).unwrap();
        assert_eq!(expanded.len(), 2);

        // Verify expansion matches symbol IDs
        let alice_id = symbol_table.get_or_insert("Alice");
        let knows_id = symbol_table.get_or_insert("knows");
        let bob_id = symbol_table.get_or_insert("Bob");
        let charlie_id = symbol_table.get_or_insert("Charlie");

        assert_eq!(expanded[0], (alice_id, knows_id, bob_id));
        assert_eq!(expanded[1], (bob_id, knows_id, charlie_id));
    }

    #[test]
    fn test_ingest_json_and_csv() {
        let symbol_table = Arc::new(SymbolTable::new());
        let mut foundry = MembraneFoundry::new(symbol_table);

        let json_str = r#"[
            {"subject": "A", "predicate": "rel", "object": "B"},
            {"subject": "B", "predicate": "rel", "object": "C"}
        ]"#;
        foundry.ingest_json_claims(json_str, 1).unwrap();

        let csv_str = "C, rel, D\nD, rel, E";
        foundry.ingest_csv_claims(csv_str, 1).unwrap();

        let page = foundry.get_relation_page("rel").unwrap();
        assert_eq!(page.len, 4);
    }

    #[test]
    fn test_ingest_turtle() {
        let symbol_table = Arc::new(SymbolTable::new());
        let mut foundry = MembraneFoundry::new(symbol_table);

        let turtle_str = r#"
            <http://example.org/Alice> <http://example.org/knows> <http://example.org/Bob> .
            <http://example.org/Bob> <http://example.org/knows> <http://example.org/Charlie> .
        "#;

        foundry.ingest_turtle_claims(turtle_str, 2).unwrap();

        let page = foundry
            .get_relation_page("<http://example.org/knows>")
            .unwrap();
        assert_eq!(page.len, 2);
    }

    #[test]
    fn test_expand_to_packets() {
        let symbol_table = Arc::new(SymbolTable::new());
        let mut foundry = MembraneFoundry::new(symbol_table);

        // Push 10 claims to force packet segmentation (since Construct8 holds max 8)
        let mut claims = Vec::new();
        for i in 0..10 {
            claims.push(ExternalClaim {
                subject: format!("Sub{}", i),
                predicate: "p".to_string(),
                object: format!("Obj{}", i),
            });
        }

        foundry.ingest_claims(claims, 42).unwrap();

        let law = foundry.build_expansion_law("p").unwrap();
        let page = foundry.get_relation_page("p").unwrap();

        let packets = law.expand_to_packets(&page, 42).unwrap();
        assert_eq!(packets.len(), 2);
        assert_eq!(packets[0].len(), 8);
        assert_eq!(packets[1].len(), 2);
    }

    #[test]
    fn test_ingest_edge_cases() {
        let symbol_table = Arc::new(SymbolTable::new());
        let mut foundry = MembraneFoundry::new(symbol_table);

        // 1. Test CSV parsing with nested commas and quotes
        let csv_str = r#"
            "Alice, Jr.", knows, "Bob, Sr."
            "Charlie", "hates, or loves", "Dave"
        "#;
        foundry.ingest_csv_claims(csv_str, 1).unwrap();

        let knows_page = foundry.get_relation_page("knows").unwrap();
        assert_eq!(knows_page.len, 1);

        let hates_page = foundry.get_relation_page("hates, or loves").unwrap();
        assert_eq!(hates_page.len, 1);

        // 2. Test duplicate claim deduplication (should not fail with Refusal)
        let dup_claims = vec![
            ExternalClaim {
                subject: "Alice, Jr.".to_string(),
                predicate: "knows".to_string(),
                object: "Bob, Sr.".to_string(),
            },
            ExternalClaim {
                subject: "Alice, Jr.".to_string(),
                predicate: "knows".to_string(),
                object: "Bob, Sr.".to_string(),
            },
        ];
        foundry.ingest_claims(dup_claims, 1).unwrap();
        let knows_page_after = foundry.get_relation_page("knows").unwrap();
        assert_eq!(knows_page_after.len, 1); // still 1

        // 3. Test empty claim validation
        let invalid_claim = ExternalClaim {
            subject: "   ".to_string(),
            predicate: "knows".to_string(),
            object: "Bob".to_string(),
        };
        let res = foundry.ingest_claim(invalid_claim, 1);
        assert!(res.is_err());
        if let Err(MembraneError::ParseError(msg)) = res {
            assert!(msg.contains("Empty subject"));
        } else {
            panic!("Expected ParseError");
        }
    }
}
