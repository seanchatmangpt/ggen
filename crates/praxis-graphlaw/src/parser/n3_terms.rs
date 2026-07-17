use crate::registry::SYNTHETIC_COUNTER;
use crate::{Triple, VarOrTerm};
use std::sync::atomic::Ordering;

use pest::iterators::Pair;

use super::iri_resolve::PrefixMapper;
use super::n3rule_parser::{parse_tp, resolve_var, Rule};

// ---------------------------------------------------------------------------
// Term building helpers
// ---------------------------------------------------------------------------

/// Convert a raw string value (already expanded) into a VarOrTerm.
///
/// NOTE: the fallback branch deliberately calls `VarOrTerm::new_term` directly
/// rather than `VarOrTerm::convert`. `convert` wraps any string that isn't
/// already `<...>`/`"..."`/`_:...` in angle brackets -- which is exactly right
/// for a *bare* prefixed name with no matching `@prefix` (e.g. "test:Foo" with
/// no `@prefix test:` declared, where `PrefixMapper::expand` returns the text
/// unchanged as a fallback). Using `new_term` here keeps that fallback case
/// encoded identically to the legacy line-based parser (`Parser::parse`),
/// which also interns such tokens raw/unwrapped. This matters because
/// `TripleStore::from` tries this pest-based parser first and falls back to
/// the legacy parser on failure -- if both parsers can succeed on the same
/// kind of document, they must agree on term encoding or a test comparing
/// pre-existing (legacy-encoded) terms against freshly-parsed (pest-encoded)
/// ones would silently break.
pub fn make_term(raw: &str) -> VarOrTerm {
    let trimmed = raw.trim();
    if let Some(name) = trimmed.strip_prefix('?') {
        // Strip the leading '?' to match VarOrTerm::convert("?x") behaviour which stores "x"
        resolve_var(name)
    } else if let Some(label) = trimmed.strip_prefix("_:") {
        VarOrTerm::new_blank_node(label.to_string())
    } else {
        VarOrTerm::new_term(trimmed.to_string())
    }
}

/// Parse a literal pest pair into a VarOrTerm literal.
///
/// NOTE: numeric/boolean literals are encoded via `VarOrTerm::new_literal` with
/// a proper xsd datatype (mirroring the string-literal handling just below),
/// **not** via `VarOrTerm::convert`. `convert` would wrap a bare lexical form
/// like "42" in angle brackets ("<42>"), i.e. treat it as an opaque IRI-like
/// token instead of a numeric value -- which would make it impossible for the
/// math:* built-ins (queryengine.rs) to recover a numeric value from it.
pub fn parse_literal_pair(pair: Pair<Rule>, prefixes: &PrefixMapper) -> VarOrTerm {
    // The outer Literal rule may contain StringValue + optional LangTag / DatatypeAnnotation,
    // or a numeric / boolean literal.
    let raw = pair.as_str().trim().to_string();
    let mut inner = pair.into_inner().peekable();

    if let Some(first) = inner.peek() {
        match first.as_rule() {
            Rule::StringValue => {
                if let Some(string_pair) = inner.next() {
                    let lex = unescape_string(string_pair.as_str());

                    // Check for lang tag or datatype
                    if let Some(annotation) = inner.next() {
                        match annotation.as_rule() {
                            Rule::LangTag => {
                                // @en → strip the @
                                let lang = &annotation.as_str()[1..];
                                return VarOrTerm::new_literal(lex, None, Some(lang.to_string()));
                            }
                            Rule::DatatypeAnnotation => {
                                // "^^" has been consumed by pest; the child is either a
                                // bare "<IRI>" (Rule::Iri) or a "prefix:local" name
                                // (Rule::Prefixed) that must be expanded via `prefixes`.
                                let dt_str = match annotation.into_inner().next() {
                                    Some(p) if p.as_rule() == Rule::BracketedIri => {
                                        let iri =
                                            p.into_inner().next().map(|q| q.as_str()).unwrap_or("");
                                        format!("<{}>", prefixes.resolve(iri))
                                    }
                                    Some(p) if p.as_rule() == Rule::Prefixed => {
                                        prefixes.expand(p.as_str())
                                    }
                                    _ => String::new(),
                                };
                                return VarOrTerm::new_literal(lex, Some(dt_str), None);
                            }
                            _ => {}
                        }
                    }
                    // Plain string literal → xsd:string
                    let xsd_string = "<http://www.w3.org/2001/XMLSchema#string>".to_string();
                    return VarOrTerm::new_literal(lex, Some(xsd_string), None);
                }
                return VarOrTerm::new_literal(String::new(), None, None);
            }
            Rule::IntegerLiteral => {
                return VarOrTerm::new_literal(
                    raw,
                    Some("<http://www.w3.org/2001/XMLSchema#integer>".to_string()),
                    None,
                );
            }
            Rule::DecimalLiteral => {
                return VarOrTerm::new_literal(
                    raw,
                    Some("<http://www.w3.org/2001/XMLSchema#decimal>".to_string()),
                    None,
                );
            }
            Rule::DoubleLiteral => {
                return VarOrTerm::new_literal(
                    raw,
                    Some("<http://www.w3.org/2001/XMLSchema#double>".to_string()),
                    None,
                );
            }
            Rule::BoolLiteral => {
                return VarOrTerm::new_literal(
                    raw,
                    Some("<http://www.w3.org/2001/XMLSchema#boolean>".to_string()),
                    None,
                );
            }
            _ => {}
        }
    }

    // Should not normally be reached given the Literal grammar's alternatives.
    VarOrTerm::convert(raw)
}

/// Parse an RDF list ("(" ListItem* ")") into a single VarOrTerm list term.
/// Any bnode-property-list members (`[ ... ]`) nested inside the list
/// contribute their desugared property triples into `extra`, exactly as
/// they would in Subject/Object position.
pub fn parse_list(pair: Pair<Rule>, prefixes: &PrefixMapper, extra: &mut Vec<Triple>) -> VarOrTerm {
    let mut members = Vec::new();
    for list_item in pair.into_inner() {
        if let Some(child) = list_item.into_inner().next() {
            members.push(term_from_pair(child, prefixes, extra));
        }
    }
    VarOrTerm::new_list(members)
}

/// Mint a fresh synthetic blank node, reusing the process-wide
/// `SYNTHETIC_COUNTER` (same mechanism `VarOrTerm::new_list`/`new_formula`
/// use) so the label can never collide with a user-written `_:label` or
/// another synthetic term.
pub fn fresh_bnode() -> VarOrTerm {
    let tag = SYNTHETIC_COUNTER.fetch_add(1, Ordering::SeqCst);
    VarOrTerm::new_blank_node(format!("__n3bnodeprops_{}", tag))
}

/// Parse an anonymous blank-node property list ("[" PredicateObjectList? "]")
/// into a fresh synthetic blank node, pushing its desugared property triples
/// into `extra`. An empty "[]" (no PredicateObjectList child) yields just the
/// fresh blank node with no additional triples -- matching Turtle/N3
/// semantics exactly.
pub fn parse_bnode_props(
    pair: Pair<Rule>,
    prefixes: &PrefixMapper,
    extra: &mut Vec<Triple>,
) -> VarOrTerm {
    let bnode = fresh_bnode();
    for child in pair.into_inner() {
        if child.as_rule() == Rule::PredicateObjectList {
            parse_predicate_object_list(bnode.clone(), child, prefixes, extra);
        }
    }
    bnode
}

/// Shared logic for a "property object[, object]* [; property object[, object]*]*"
/// production attached to `subject_vot` (used both for a TP's top-level
/// PredicateObjectList and for a `[ ... ]` bnode property list's inner one).
/// Pushes one triple per (property, object) pair into `extra`.
pub fn parse_predicate_object_list(
    subject_vot: VarOrTerm,
    pair: Pair<Rule>,
    prefixes: &PrefixMapper,
    extra: &mut Vec<Triple>,
) {
    let mut property_vot = VarOrTerm::new_var("p".to_string());
    let mut objects_vot: Vec<VarOrTerm> = Vec::new();
    let flush =
        |property_vot: &VarOrTerm, objects_vot: &mut Vec<VarOrTerm>, triples: &mut Vec<Triple>| {
            if objects_vot.is_empty() {
                objects_vot.push(VarOrTerm::new_var("o".to_string()));
            }
            for o in objects_vot.drain(..) {
                triples.push(Triple {
                    s: subject_vot.clone(),
                    p: property_vot.clone(),
                    o,
                    g: None,
                });
            }
        };
    for pol_part in pair.into_inner() {
        match pol_part.as_rule() {
            Rule::Property => {
                if !objects_vot.is_empty() {
                    flush(&property_vot, &mut objects_vot, extra);
                }
                let expanded = expand_property(pol_part, prefixes);
                property_vot = make_term(&expanded);
            }
            Rule::ObjectList => {
                for obj_pair in pol_part.into_inner() {
                    if obj_pair.as_rule() == Rule::Object {
                        objects_vot.push(parse_object(obj_pair, prefixes, extra));
                    }
                }
            }
            _ => {}
        }
    }
    flush(&property_vot, &mut objects_vot, extra);
}

/// Parse a quoted graph ("{" (ForAll | ForSome | TP)* "}") into a single
/// VarOrTerm formula term. Pushes a fresh formula-scope so any `@forAll`/
/// `@forSome` declared directly inside this formula scopes those variables
/// to it (see the `ScopeStack` module doc comment in n3rule_parser) rather than leaking into
/// an enclosing or sibling formula.
pub fn parse_formula(pair: Pair<Rule>, prefixes: &PrefixMapper) -> VarOrTerm {
    use super::n3rule_parser::register_quantifier_declarations;
    use super::n3rule_parser::with_new_scope;

    with_new_scope(|| {
        let children: Vec<Pair<Rule>> = pair.into_inner().collect();
        // Pre-pass: register this formula's own quantifier declarations
        // before parsing its triples, so order-within-the-formula doesn't
        // matter (a declaration after its first use still applies).
        for child in &children {
            if child.as_rule() == Rule::ForAll || child.as_rule() == Rule::ForSome {
                register_quantifier_declarations(child);
            }
        }
        let mut triples = Vec::new();
        for tp_pair in children {
            if tp_pair.as_rule() == Rule::TP {
                triples.extend(parse_tp(tp_pair.into_inner(), prefixes));
            }
        }
        VarOrTerm::new_formula(triples)
    })
}

/// Extract the raw (unbracketed) IRI-reference text from an `IriRef`
/// (`"<" ~ IriReference ~ ">"`, wrapped in the atomic `BracketedIri`) pair.
pub fn bracketed_iri_text(iri_ref_pair: Pair<'_, Rule>) -> &str {
    iri_ref_pair
        .into_inner()
        .next()
        .and_then(|bracketed| bracketed.into_inner().next())
        .map(|iri_reference| iri_reference.as_str())
        .unwrap_or("")
}

/// Shared term-building logic for anything that can appear in a Subject or
/// Object position (IRI, prefixed name, variable, blank node, literal, list,
/// or quoted graph).
pub fn term_from_pair(
    child: Pair<Rule>,
    prefixes: &PrefixMapper,
    extra: &mut Vec<Triple>,
) -> VarOrTerm {
    match child.as_rule() {
        Rule::IriRef => {
            let iri = bracketed_iri_text(child);
            make_term(&format!("<{}>", prefixes.resolve(iri)))
        }
        Rule::Prefixed => make_term(&prefixes.expand(child.as_str())),
        Rule::Var => make_term(child.as_str()),
        Rule::BlankNode => make_term(child.as_str()),
        Rule::Literal => parse_literal_pair(child, prefixes),
        Rule::List => parse_list(child, prefixes, extra),
        Rule::Formula => parse_formula(child, prefixes),
        Rule::BNodeProps => parse_bnode_props(child, prefixes, extra),
        Rule::PathExpr => parse_path_expr(child, prefixes, extra),
        _ => make_term(child.as_str()),
    }
}

/// Mint a fresh synthetic blank node for a path-syntax existential (`x!p` /
/// `x^p`), reusing the process-wide `SYNTHETIC_COUNTER` so its label can
/// never collide with a user-written `_:label` or any other synthetic term.
pub fn fresh_path_bnode() -> VarOrTerm {
    let tag = SYNTHETIC_COUNTER.fetch_add(1, Ordering::SeqCst);
    VarOrTerm::new_blank_node(format!("__n3path_{}", tag))
}

/// Parse a `PathPredicate` (`IriRef | Prefixed | Var`) into the term used as
/// a path segment's predicate.
pub fn parse_path_predicate(pair: Pair<Rule>, prefixes: &PrefixMapper) -> VarOrTerm {
    match pair.into_inner().next() {
        Some(child) => match child.as_rule() {
            Rule::IriRef => {
                let iri = bracketed_iri_text(child);
                make_term(&format!("<{}>", prefixes.resolve(iri)))
            }
            Rule::Prefixed => make_term(&prefixes.expand(child.as_str())),
            Rule::Var => make_term(child.as_str()),
            _ => make_term(child.as_str()),
        },
        None => make_term(""),
    }
}

/// Parse a `PathExpr` (`PathHead ~ PathSegment+`) into the term the whole
/// path expression evaluates to, pushing one desugared triple per segment
/// into `extra`. `x!p` desugars to a fresh existential `_:v` plus the triple
/// `(x, p, _:v)`; `x^p` (inverse path) desugars to a fresh existential `_:v`
/// plus the triple `(_:v, p, x)`. Chained segments (mixing `!`/`^` freely,
/// e.g. `x!p^q`) fold left-to-right: each segment's fresh existential
/// becomes the base term for the next segment.
pub fn parse_path_expr(
    pair: Pair<Rule>,
    prefixes: &PrefixMapper,
    extra: &mut Vec<Triple>,
) -> VarOrTerm {
    let mut inner = pair.into_inner();
    let Some(head_pair) = inner.next() else {
        return VarOrTerm::new_var("parse_error".to_string());
    };
    let Some(head_child) = head_pair.into_inner().next() else {
        return VarOrTerm::new_var("parse_error".to_string());
    };
    let mut current = term_from_pair(head_child, prefixes, extra);
    for segment in inner {
        let mut seg_inner = segment.into_inner();
        let Some(dir_pair) = seg_inner.next() else {
            continue;
        };
        let Some(pred_pair) = seg_inner.next() else {
            continue;
        };
        let predicate = parse_path_predicate(pred_pair, prefixes);
        let fresh = fresh_path_bnode();
        match dir_pair.as_rule() {
            Rule::PathInverse => extra.push(Triple {
                s: fresh.clone(),
                p: predicate,
                o: current,
                g: None,
            }),
            // PathForward, and any other case (shouldn't occur per grammar).
            _ => extra.push(Triple {
                s: current,
                p: predicate,
                o: fresh.clone(),
                g: None,
            }),
        }
        current = fresh;
    }
    current
}

/// Strip surrounding quotes from a string literal and decode N3/Turtle
/// string escape sequences (`\n \r \t \b \f \" \' \\ \uXXXX \UXXXXXXXX`).
pub fn unescape_string(raw: &str) -> String {
    let s = raw.trim();
    let inner = if (s.starts_with("\"\"\"") && s.ends_with("\"\"\"") && s.len() >= 6)
        || (s.starts_with("'''") && s.ends_with("'''") && s.len() >= 6)
    {
        &s[3..s.len() - 3]
    } else if (s.starts_with('"') && s.ends_with('"') && s.len() >= 2)
        || (s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2)
    {
        &s[1..s.len() - 1]
    } else {
        s
    };
    decode_escapes(inner)
}

/// Decode N3/Turtle string escape sequences in an already-unquoted string.
///
/// Supported escapes: `\t \n \r \b \f \" \' \\ \uXXXX \UXXXXXXXX`. Any other
/// `\x` sequence is left as-is (backslash + char preserved) since it is not a
/// recognized escape.
pub fn decode_escapes(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('r') => out.push('\r'),
            Some('t') => out.push('\t'),
            Some('b') => out.push('\u{0008}'),
            Some('f') => out.push('\u{000C}'),
            Some('"') => out.push('"'),
            Some('\'') => out.push('\''),
            Some('\\') => out.push('\\'),
            Some('u') => {
                if let Some(ch) = take_hex_escape(&mut chars, 4) {
                    out.push(ch);
                } else {
                    out.push('\\');
                    out.push('u');
                }
            }
            Some('U') => {
                if let Some(ch) = take_hex_escape(&mut chars, 8) {
                    out.push(ch);
                } else {
                    out.push('\\');
                    out.push('U');
                }
            }
            Some(other) => {
                out.push('\\');
                out.push(other);
            }
            None => out.push('\\'),
        }
    }
    out
}

/// Consume exactly `n` hex digits from `chars` and decode them as a Unicode
/// code point. Returns `None` (consuming nothing further) if fewer than `n`
/// hex digits are available or the resulting code point is invalid, in which
/// case the caller falls back to emitting the escape literally.
pub fn take_hex_escape(chars: &mut std::iter::Peekable<std::str::Chars>, n: usize) -> Option<char> {
    let mut digits = String::with_capacity(n);
    let mut lookahead = chars.clone();
    for _ in 0..n {
        match lookahead.next() {
            Some(c) if c.is_ascii_hexdigit() => digits.push(c),
            _ => return None,
        }
    }
    let code = match u32::from_str_radix(&digits, 16) {
        Ok(c) => c,
        Err(_) => return None,
    };
    let ch = char::from_u32(code)?;
    // Only consume from the real iterator once we know decoding succeeded.
    for _ in 0..n {
        chars.next();
    }
    Some(ch)
}

/// Parse a Property rule into its expanded IRI form.
pub fn expand_property(pair: Pair<Rule>, prefixes: &PrefixMapper) -> String {
    let inner = pair.into_inner().next();
    if let Some(child) = inner {
        match child.as_rule() {
            Rule::RdfType => "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".to_string(),
            // "=" sugar for owl:sameAs in Property position (real N3 idiom).
            Rule::SameAs => "<http://www.w3.org/2002/07/owl#sameAs>".to_string(),
            Rule::IriRef => {
                let iri = bracketed_iri_text(child);
                format!("<{}>", prefixes.resolve(iri))
            }
            Rule::Prefixed => prefixes.expand(child.as_str()),
            Rule::Var => child.as_str().to_string(),
            _ => child.as_str().to_string(),
        }
    } else {
        String::new()
    }
}

/// Parse an Object rule into a VarOrTerm.
pub fn parse_object(
    pair: Pair<Rule>,
    prefixes: &PrefixMapper,
    extra: &mut Vec<Triple>,
) -> VarOrTerm {
    match pair.into_inner().next() {
        Some(child) => term_from_pair(child, prefixes, extra),
        None => VarOrTerm::new_var("o".to_string()),
    }
}
