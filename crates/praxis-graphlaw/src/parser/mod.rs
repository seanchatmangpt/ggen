use crate::{BodyLiteral, Rule, Triple, VarOrTerm};
use rio_api::parser::{QuadsParser, TriplesParser};
use rio_turtle::{NQuadsParser, NTriplesParser, TriGParser, TurtleError, TurtleParser};
use rio_xml::RdfXmlParser;

mod iri_resolve;
mod n3_terms;
mod n3rule_parser;

pub struct Parser;
#[derive(PartialEq, Default)]
pub enum Syntax {
    #[default]
    NTriples,
    Turtle,
    TriG,
    NQuads,
    RdfXml,
}

impl Parser {
    fn preprocess_turtle(data: &str) -> String {
        crate::preprocess_turtle(data)
    }

    pub fn parse_triples(data: &str, syntax: Syntax) -> Result<Vec<Triple>, String> {
        let preprocessed = if syntax == Syntax::Turtle {
            Self::preprocess_turtle(data)
        } else {
            data.to_string()
        };
        if syntax == Syntax::Turtle || syntax == Syntax::NTriples || syntax == Syntax::RdfXml {
            Self::parse_triples_helper(&preprocessed, syntax)
        } else {
            Self::parse_quads_helper(&preprocessed, syntax)
        }
    }
    fn parse_quads_helper(data: &str, syntax: Syntax) -> Result<Vec<Triple>, String> {
        let preprocessed = if syntax == Syntax::TriG {
            Self::preprocess_turtle(data)
        } else {
            data.to_string()
        };
        let mut triples = Vec::new();
        let closure_quad = &mut |t: rio_api::model::Quad| {
            let s = VarOrTerm::new_term(t.subject.to_string());
            let p = VarOrTerm::new_term(t.predicate.to_string());
            let o = VarOrTerm::new_term(t.object.to_string());
            let g = t.graph_name.map(|g| VarOrTerm::new_term(g.to_string()));
            triples.push(Triple { s, p, o, g });
            Ok(()) as Result<(), TurtleError>
        };

        let result = match syntax {
            Syntax::TriG => TriGParser::new(preprocessed.as_ref(), None).parse_all(closure_quad),
            Syntax::NQuads => NQuadsParser::new(preprocessed.as_ref()).parse_all(closure_quad),
            _ => NQuadsParser::new(preprocessed.as_ref()).parse_all(closure_quad),
        };
        match result {
            Ok(_) => Ok(triples),
            Err(parsing_error) => Err(format!("Parsing error! {:?}", parsing_error.to_string())),
        }
    }
    fn parse_triples_helper(data: &str, syntax: Syntax) -> Result<Vec<Triple>, String> {
        let mut triples = Vec::new();
        let closure_triple = &mut |t: rio_api::model::Triple| {
            let s = VarOrTerm::new_term(t.subject.to_string());
            let p = VarOrTerm::new_term(t.predicate.to_string());
            let o = VarOrTerm::new_term(t.object.to_string());
            triples.push(Triple { s, p, o, g: None });
            Ok(()) as Result<(), TurtleError>
        };
        match syntax {
            Syntax::NTriples => {
                let result = NTriplesParser::new(data.as_ref()).parse_all(closure_triple);
                result
                    .map(|_| triples)
                    .map_err(|e| format!("Parsing error! {}", e))
            }
            Syntax::Turtle => {
                let result = TurtleParser::new(data.as_ref(), None).parse_all(closure_triple);
                result
                    .map(|_| triples)
                    .map_err(|e| format!("Parsing error! {}", e))
            }
            Syntax::RdfXml => {
                let mut local_triples = Vec::new();
                let result = RdfXmlParser::new(data.as_ref(), None).parse_all(
                    &mut |t: rio_api::model::Triple| {
                        let s = VarOrTerm::new_term(t.subject.to_string());
                        let p = VarOrTerm::new_term(t.predicate.to_string());
                        let o = VarOrTerm::new_term(t.object.to_string());
                        local_triples.push(Triple { s, p, o, g: None });
                        Ok::<(), rio_xml::RdfXmlError>(())
                    },
                );
                result
                    .map(|_| local_triples)
                    .map_err(|e| format!("Parsing error! {}", e))
            }
            _ => {
                let result = NTriplesParser::new(data.as_ref()).parse_all(closure_triple);
                result
                    .map(|_| triples)
                    .map_err(|e| format!("Parsing error! {}", e))
            }
        }
    }
    fn parse_triple(data: &str) -> Result<Triple, String> {
        let items: Vec<&str> = data.split(" ").collect();
        if items.len() < 3 {
            return Err(format!(
                "Malformed triple: expected at least 3 tokens, got {}",
                items.len()
            ));
        }
        let s = items.first().ok_or_else(|| "Missing subject".to_string())?;
        let p = items
            .get(1)
            .ok_or_else(|| "Missing predicate".to_string())?;

        let o = if items.get(2).unwrap().ends_with(".") {
            let mut o_chars = items.get(2).unwrap().chars();
            o_chars.next_back();
            o_chars.as_str()
        } else {
            items.get(2).unwrap()
        };
        let convert_item = |item: &&str| {
            if item.starts_with("?") {
                VarOrTerm::new_var(item.to_string())
            } else {
                VarOrTerm::new_term(item.to_string())
            }
        };
        let s = convert_item(s);
        let p = convert_item(p);
        let o = convert_item(&o);
        Ok(Triple { s, p, o, g: None })
    }
    fn rem_first_and_last(value: &str) -> &str {
        let mut chars = value.chars();
        chars.next();
        chars.next_back();
        chars.as_str()
    }
    pub fn parse(data: String) -> (Vec<Triple>, Vec<Rule>) {
        let mut rules = Vec::new();
        let mut content = Vec::new();
        //line by line
        for line in data.split("\n") {
            if line.contains("=>") {
                //process rule
                let rule: Vec<&str> = line.split("=>").collect();
                let body = Self::rem_first_and_last(rule.first().unwrap());
                let head = Self::rem_first_and_last(rule.get(1).unwrap());
                if let Ok(head_triple) = Self::parse_triple(head) {
                    let mut body_triples = Vec::new();
                    for body_triple in body.split(".") {
                        let body_triple_trimmed = body_triple.trim();
                        if !body_triple_trimmed.is_empty() {
                            let is_negated = body_triple_trimmed.starts_with("not");
                            let triple_str = if is_negated {
                                let open_curly = body_triple_trimmed.find('{').unwrap_or(0);
                                let close_curly = body_triple_trimmed
                                    .rfind('}')
                                    .unwrap_or(body_triple_trimmed.len());
                                // Bounds-checked: a body literal whose braces are missing,
                                // reversed, or otherwise malformed (e.g. a '}' appearing before
                                // a '{', which a bare open_curly+1..close_curly slice would panic
                                // on with "slice index starts at X but ends at Y") falls back to
                                // the untouched trimmed text instead of crashing. parse_triple
                                // below then correctly fails to parse that text as a well-formed
                                // triple, and this body literal is silently skipped -- the same
                                // "malformed sub-part is skipped, not a hard failure" convention
                                // this function already applies to head/pattern parse failures a
                                // few lines above and below.
                                if open_curly < close_curly {
                                    &body_triple_trimmed[open_curly + 1..close_curly]
                                } else {
                                    body_triple_trimmed
                                }
                            } else {
                                body_triple_trimmed
                            };
                            if let Ok(pattern) = Self::parse_triple(triple_str.trim()) {
                                body_triples.push(BodyLiteral {
                                    negated: is_negated,
                                    pattern,
                                });
                            }
                        }
                    }
                    rules.push(Rule {
                        head: head_triple,
                        body: body_triples,
                    })
                }
            } else {
                //process triple
                let trimmed = line.trim();
                if !trimmed.is_empty()
                    && !trimmed.starts_with('@')
                    && !trimmed.starts_with("PREFIX")
                    && !trimmed.starts_with("BASE")
                    && !trimmed.starts_with("#")
                {
                    if let Ok(triple) = Self::parse_triple(line) {
                        content.push(triple);
                    }
                }
            }
        }
        (content, rules)
    }
    pub fn parse_rules(parse_string: &str) -> Result<Vec<Rule>, String> {
        n3rule_parser::parse(parse_string)
    }

    /// Parse a complete N3 document (facts and rules together) using the
    /// pest-based grammar. Unlike `parse_rules`, this also returns plain
    /// top-level fact triples, and understands `@prefix` declarations, RDF
    /// lists ("(...)"), and quoted graphs ("{...}" used as a term) -- none of
    /// which the legacy line-based `parse` can handle.
    pub fn parse_n3_document(parse_string: &str) -> Result<(Vec<Triple>, Vec<Rule>), String> {
        n3rule_parser::parse_document(parse_string)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_parsing() {
        let ntriples_file = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/foo> <http://schema.org/name> \"Foo\" .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/bar> <http://schema.org/name> \"Bar\" .";
        let triples = Parser::parse_triples(ntriples_file, Syntax::NTriples).unwrap();
        assert_eq!(4, triples.len());

        let trig_file = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>, <http://schema.org/Student> .
<http://example.com/foo> <http://schema.org/name> \"Foo\" .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>; <http://schema.org/name> \"Bar\" .";
        let triples = Parser::parse_triples(trig_file, Syntax::TriG).unwrap();
        assert_eq!(5, triples.len());

        let turtle = "@prefix schema: <http://schema.org/> .
<http://example.com/foo> a schema:Person ;
    schema:name  \"Foo\" .
<http://example.com/bar> a schema:Person ;
    schema:name  \"Bar\" .";
        let triples = Parser::parse_triples(turtle, Syntax::Turtle).unwrap();
        assert_eq!(4, triples.len());

        let nquads = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
<http://example.com/foo> <http://schema.org/name> \"Foo\" <http://example.com/> .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/bar> <http://schema.org/name> \"Bar\" .";
        let triples = Parser::parse_triples(nquads, Syntax::NQuads).unwrap();
        assert_eq!(4, triples.len());

        let parsing_error = "<http://example.com/foo> http://www.w3.org/1999/02/22-rdf-syntax-ns#typehema.org/Person";
        let triples = Parser::parse_triples(parsing_error, Syntax::NQuads);
        assert!(triples.is_err());
    }
    #[test]
    fn test_empty_abox_parsing() {
        let ntriples_file = "";
        let triples = Parser::parse_triples(ntriples_file, Syntax::NTriples).unwrap();
        assert_eq!(0, triples.len());
    }

    #[test]
    fn test_error_abox_parsing() {
        let ntriples_file = "asdfadsf";
        match Parser::parse_triples(ntriples_file, Syntax::NTriples) {
            Ok(_result) => assert_eq!(0, 1),
            Err(_err) => assert_eq!(0, 0),
        }
    }
    #[test]
    fn test_syntactic_sugar_rdf_type() {
        let ntriples_file = "<http://example2.com/a> a <http://www.test.be/test#SubClass> .";
        match Parser::parse_triples(ntriples_file, Syntax::Turtle) {
            Ok(result) => assert_eq!(1, result.len()),
            Err(_err) => assert_eq!(0, 1),
        }
    }
    #[test]
    fn test_white_space_in_rules() {
        let rules = "{?source a test:Source. }=>{?source a test:NeededInput.}";
        match Parser::parse_rules(rules) {
            Ok(result) => assert_eq!(1, result.len()),
            Err(_err) => assert_eq!(0, 1),
        }
    }

    /// The legacy line-based `Parser::parse` (unlike the pest-based `parse_rules`/
    /// `parse_n3_document`) treats ANY body clause starting with the literal substring
    /// "not" as a negated literal and extracts the text between its first `{` and last
    /// `}` with no bounds check. A clause whose `}` appears before its `{` -- here,
    /// `not }x{` -- makes `open_curly + 1 > close_curly`, which a bare
    /// `&s[open_curly + 1..close_curly]` slice previously panicked on ("slice index
    /// starts at 7 but ends at 4"). This is real, non-test-only reachable input: this
    /// exact `Parser::parse` is called from production code in
    /// crates/cng/src/bench/{roles.rs,decomp/rules.rs}, crates/praxis-graphlaw-wasm/
    /// src/core.rs (the WASM/browser-facing boundary -- fully untrusted external
    /// input), and this crate's own imars_reasoner.rs/lib.rs/pipeline.rs, none of
    /// which validate body-literal brace balance before calling it.
    ///
    /// Proves two things: (1) parsing this input does not panic, and (2) the
    /// well-formed parts of the SAME rule survive -- the valid `?x <p> <o>` body
    /// literal is still present, and the head still parses -- only the malformed
    /// negated literal is silently skipped, matching this function's own existing
    /// convention for other malformed sub-parts (see the `if let Ok(head_triple) = ...`
    /// / `if let Ok(pattern) = ...` guards above).
    #[test]
    fn test_parse_does_not_panic_on_malformed_negated_body_literal_braces() {
        // IRIs deliberately contain no `.` -- this legacy parser splits a rule body on
        // literal `.` characters with no IRI-escaping awareness, so a dotted host (e.g.
        // `example.org`) would itself fragment the body into more clauses than intended,
        // unrelated to the brace-panic this test targets.
        let malformed_rule =
            "{?x <http://example/p> <http://example/o>.not }x{ }=>{?y <http://example/p2> <http://example/o2>}";
        let (facts, rules) = Parser::parse(malformed_rule.to_string());

        assert_eq!(0, facts.len(), "this line is a rule, not a top-level fact");
        assert_eq!(
            1,
            rules.len(),
            "the rule itself (head + surviving body) must still parse"
        );

        let rule = &rules[0];
        assert_eq!(
            1,
            rule.body.len(),
            "only the well-formed body literal survives; the malformed negated one is \
             silently skipped, not fabricated into a bogus triple"
        );
        assert!(
            !rule.body[0].negated,
            "the surviving body literal is the well-formed, non-negated ?x <p> <o> clause"
        );
    }
}
