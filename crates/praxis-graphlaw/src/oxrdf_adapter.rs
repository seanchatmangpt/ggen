use crate::encoding::Encoder;
use crate::tripleindex::TripleIndex;
use crate::triples::Term;
use oxrdf::{BlankNode, Graph, Literal, NamedNode, NamedOrBlankNode, Term as OxTerm};

/// Helper to strip leading `<` and trailing `>` from IRI string representation if present.
fn clean_iri(s: &str) -> &str {
    if s.starts_with('<') && s.ends_with('>') {
        &s[1..s.len() - 1]
    } else {
        s
    }
}

/// Helper to strip leading `_:` from BlankNode string representation if present.
fn clean_blank_node(s: &str) -> &str {
    s.strip_prefix("_:").unwrap_or(s)
}

/// Converts a `TripleIndex` to an `oxrdf::Graph`.
/// Only ground terms (IRIs, Blank Nodes, and Literals) are processed; any invalid terms are skipped.
pub fn triple_index_to_oxrdf_graph(index: &TripleIndex) -> Graph {
    let mut graph = Graph::new();
    for triple in &index.triples {
        if triple.s.is_term() && triple.p.is_term() && triple.o.is_term() {
            let s_term = triple.s.as_term();
            let p_term = triple.p.as_term();
            let o_term = triple.o.as_term();

            let subject = match s_term {
                Term::Iri(iri) => {
                    let s_str = Encoder::decode(&iri.iri).unwrap();
                    let clean = clean_iri(&s_str);
                    NamedOrBlankNode::NamedNode(NamedNode::new_unchecked(clean))
                }
                Term::BlankNode(bnode) => {
                    let s_str = Encoder::decode(&bnode.id).unwrap();
                    let clean = clean_blank_node(&s_str);
                    NamedOrBlankNode::BlankNode(BlankNode::new_unchecked(clean))
                }
                Term::Literal(_) => continue, // RDF subjects cannot be literals
            };

            let predicate = match p_term {
                Term::Iri(iri) => {
                    let p_str = Encoder::decode(&iri.iri).unwrap();
                    let clean = clean_iri(&p_str);
                    NamedNode::new_unchecked(clean)
                }
                _ => continue, // RDF predicates must be IRIs
            };

            let object = match o_term {
                Term::Iri(iri) => {
                    let o_str = Encoder::decode(&iri.iri).unwrap();
                    let clean = clean_iri(&o_str);
                    OxTerm::NamedNode(NamedNode::new_unchecked(clean))
                }
                Term::BlankNode(bnode) => {
                    let o_str = Encoder::decode(&bnode.id).unwrap();
                    let clean = clean_blank_node(&o_str);
                    OxTerm::BlankNode(BlankNode::new_unchecked(clean))
                }
                Term::Literal(lit) => {
                    let val_str = Encoder::decode(&lit.value).unwrap();
                    let dt_str = lit.datatype.map(|d| Encoder::decode(&d).unwrap());
                    let lang_str = lit.lang.map(|l| Encoder::decode(&l).unwrap());

                    let ox_literal = if let Some(lang) = lang_str {
                        Literal::new_language_tagged_literal_unchecked(val_str, lang)
                    } else if let Some(dt) = dt_str {
                        let clean_dt = clean_iri(&dt);
                        Literal::new_typed_literal(val_str, NamedNode::new_unchecked(clean_dt))
                    } else {
                        Literal::new_simple_literal(val_str)
                    };
                    OxTerm::Literal(ox_literal)
                }
            };

            graph.insert(&oxrdf::Triple::new(subject, predicate, object));
        }
    }
    graph
}

/// Converts an `oxrdf::Term` back to a `praxis_graphlaw::Term`.
pub fn oxrdf_term_to_roxi_term(term: &oxrdf::Term) -> Term {
    match term {
        oxrdf::Term::NamedNode(node) => {
            let iri_str = format!("<{}>", node.as_str());
            let id = Encoder::add(iri_str);
            Encoder::decode_to_term(id).expect("Successfully decoded just-added NamedNode term")
        }
        oxrdf::Term::BlankNode(node) => {
            let bnode_str = format!("_:{}", node.as_str());
            let id = Encoder::add(bnode_str);
            Encoder::decode_to_term(id).expect("Successfully decoded just-added BlankNode term")
        }
        oxrdf::Term::Literal(literal) => {
            let lexical = literal.value().to_string();
            let lang = literal.language().map(|l| l.to_string());
            let dt = literal.datatype().as_str();
            let datatype = if dt == "http://www.w3.org/2001/XMLSchema#string"
                || dt == "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
            {
                None
            } else {
                Some(format!("<{}>", dt))
            };
            let id = Encoder::add_literal(lexical, datatype, lang);
            Encoder::decode_to_term(id).expect("Successfully decoded just-added Literal term")
        }
        // RDF-star / RDF 1.2 triple terms (oxrdf's `rdf-12` feature, which
        // this crate no longer enables -- it broke oxigraph 0.5.9's oxttl
        // via feature unification). Wildcard keeps the match exhaustive
        // whether or not another crate in the build enables that feature.
        #[allow(unreachable_patterns)]
        _ => panic!("RDF-star Triple terms are not supported by roxi"),
    }
}

/// Converts an `oxrdf::NamedOrBlankNode` back to a `praxis_graphlaw::Term`.
pub fn oxrdf_named_or_blank_node_to_roxi_term(node: &oxrdf::NamedOrBlankNode) -> Term {
    match node {
        oxrdf::NamedOrBlankNode::NamedNode(n) => {
            let iri_str = format!("<{}>", n.as_str());
            let id = Encoder::add(iri_str);
            Encoder::decode_to_term(id).expect("Successfully decoded just-added NamedNode")
        }
        oxrdf::NamedOrBlankNode::BlankNode(n) => {
            let bnode_str = format!("_:{}", n.as_str());
            let id = Encoder::add(bnode_str);
            Encoder::decode_to_term(id).expect("Successfully decoded just-added BlankNode")
        }
    }
}
