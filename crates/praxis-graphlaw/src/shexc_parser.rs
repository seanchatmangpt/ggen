//! An 80/20 ShExC (ShEx Compact syntax) parser, producing the exact same
//! `Schema`/`ShapeDecl`/`ShapeExprOrRef`/`ShapeExpr`/`TripleExpr`/
//! `ValueSetValue` structs `shex_native.rs` already defines for ShExJ --
//! this module is purely an alternate *front end* onto that AST, not a new
//! validator (the validator in `shex_native.rs` is untouched and unaware
//! this parser exists).
//!
//! Motivation: most real-world ShEx schemas are hand-authored in ShExC, not
//! ShExJ (the JSON serialization) -- until this module, `praxis_graphlaw::shex` could
//! only consume the latter. This closes the single largest real-world gap
//! flagged in this session's ShEx coverage assessment.
//!
//! ## Scope (honest, deliberate 80/20 limits)
//!
//! Covers: `PREFIX`/`BASE` declarations and prefixed-name resolution, shape
//! declarations (`<label> { ... }`), `CLOSED`/`EXTRA`, triple constraints
//! with cardinality (`*`/`+`/`?`/`{m,n}`), `EachOf` (`;`) and `OneOf` (`|`)
//! grouping (including parenthesized nesting), `ShapeAnd`/`ShapeOr`/
//! `ShapeNot` (`AND`/`OR`/`NOT`), shape references (`@<label>`), `.` (any
//! node), NodeKind keywords (`IRI`/`BNODE`/`LITERAL`/`NONLITERAL`), a bare
//! datatype IRI as a node constraint, numeric/string facets
//! (`LENGTH`/`MINLENGTH`/`MAXLENGTH`/`MININCLUSIVE`/`MAXINCLUSIVE`/
//! `MINEXCLUSIVE`/`MAXEXCLUSIVE`, regex `pattern/flags`), and value sets
//! (`[...]` with plain IRIs, IRI stems `~`, language-tagged literals, and
//! `LANGUAGE "tag"`).
//!
//! Explicitly NOT implemented (parses to a clear `Err`, never a silent
//! wrong parse) -- matching this project's established "unsupported
//! constructs get a clear error" convention: semantic actions (`%...%`),
//! `IMPORT`, triple-expression labels `$label` and inclusion `&label`,
//! ShapeMap compact syntax (shape maps are still passed as a
//! `&[(String,String)]` parameter, not parsed from text), value-set stem
//! *exclusions* (`[<iri>~ - <excl>]`), annotations, and `EXTERNAL` shape
//! expressions. None of these appear anywhere in this crate's vendored
//! shexTest corpus.

use crate::shex_native::{Schema, ShapeDecl, ShapeExpr, ShapeExprOrRef, TripleExpr, ValueSetValue};
use pest::iterators::Pair;
use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "parser/shexc.pest"]
struct ShexCParser;

/// Parse a ShExC schema string into the same `Schema` AST `shex_native.rs`
/// builds from ShExJ. `start = ...` declarations are accepted syntactically
/// (so schemas that use `start` don't fail to parse) but are not recorded --
/// this crate's `shex_native::validate_shex_native` always takes its target
/// shape explicitly from the caller's `shape_map`, never from a schema's
/// `start` shape, so there is nowhere to plug a recorded start shape into.
pub fn parse_shexc(src: &str) -> Result<Schema, String> {
    let mut pairs =
        ShexCParser::parse(Rule::Schema, src).map_err(|e| format!("ShExC parse error: {e}"))?;
    let schema_pair = pairs.next().ok_or("ShExC parse error: empty input")?;

    let mut prefixes: HashMap<String, String> = HashMap::new();
    let mut base: Option<String> = None;
    let mut shapes = Vec::new();

    for pair in schema_pair.into_inner() {
        match pair.as_rule() {
            Rule::Directive => {
                let inner = pair.into_inner().next().ok_or("malformed directive")?;
                match inner.as_rule() {
                    Rule::PrefixDecl => {
                        let mut parts = inner.into_inner();
                        // `PnPrefix?` is optional -- when absent (the default
                        // `PREFIX : <iri>` form), the next pair is the IriRef.
                        let first = parts.next().ok_or("malformed PREFIX")?;
                        let (prefix_name, iri_pair) = if first.as_rule() == Rule::PnPrefix {
                            let iri = parts.next().ok_or("malformed PREFIX: missing IRI")?;
                            (first.as_str().to_string(), iri)
                        } else {
                            (String::new(), first)
                        };
                        let raw_iri = strip_brackets(iri_pair.as_str());
                        let resolved = resolve_iri(&base, raw_iri);
                        prefixes.insert(prefix_name, resolved);
                    }
                    Rule::BaseDecl => {
                        let iri_pair = inner.into_inner().next().ok_or("malformed BASE")?;
                        let raw_iri = strip_brackets(iri_pair.as_str());
                        base = Some(resolve_iri(&base, raw_iri));
                    }
                    _ => return Err(format!("unexpected directive: {:?}", inner.as_rule())),
                }
            }
            Rule::StartDecl => {
                // Accepted, intentionally not recorded -- see this function's
                // doc comment.
            }
            Rule::ShapeExprDecl => {
                let mut parts = pair.into_inner();
                let label_pair = parts
                    .next()
                    .ok_or("malformed shape declaration: missing label")?;
                let label = resolve_iri_pair(&prefixes, &base, label_pair)?;
                let shape_or = parts
                    .next()
                    .ok_or("malformed shape declaration: missing body")?;
                let expr = convert_shape_or(&prefixes, &base, shape_or)?;
                shapes.push(ShapeDecl {
                    id: label,
                    shape_expr: expr,
                });
            }
            Rule::EOI => {}
            other => return Err(format!("unexpected top-level construct: {other:?}")),
        }
    }

    Ok(Schema { shapes })
}

// ---------------------------------------------------------------------
// IRI / prefixed-name resolution
// ---------------------------------------------------------------------

fn strip_brackets(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

/// Minimal RFC-3986-flavored relative resolution: an iri containing "://" (or
/// starting with "#"/"/" for a same-document/absolute-path reference against
/// an absolute base) is left alone if it already has a scheme; otherwise it
/// is joined onto `base`. Every IRI in this crate's vendored ShExC corpus is
/// already absolute, so this is intentionally simple, not a full RFC 3986
/// implementation.
fn resolve_iri(base: &Option<String>, raw: &str) -> String {
    if raw.contains("://") {
        return raw.to_string();
    }
    match base {
        Some(b) if !raw.is_empty() => {
            if let Some(hash_pos) = b.find('#') {
                format!("{}{}", &b[..hash_pos], raw)
            } else {
                format!("{b}{raw}")
            }
        }
        _ => raw.to_string(),
    }
}

fn resolve_iri_pair(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<String, String> {
    // `pair` is an `Iri` (IriRef | PrefixedName), or occasionally the raw
    // IriRef/PrefixedName pair directly when called on an already-unwrapped
    // child.
    let inner = match pair.as_rule() {
        Rule::Iri => pair.into_inner().next().ok_or("empty Iri")?,
        _ => pair,
    };
    match inner.as_rule() {
        Rule::IriRef => Ok(resolve_iri(base, strip_brackets(inner.as_str()))),
        Rule::PrefixedName => {
            let text = inner.as_str();
            let colon = text.find(':').ok_or("malformed prefixed name")?;
            let prefix = &text[..colon];
            let local = &text[colon + 1..];
            let ns = prefixes
                .get(prefix)
                .ok_or_else(|| format!("undeclared prefix: {prefix}"))?;
            Ok(format!("{ns}{local}"))
        }
        other => Err(format!("expected an IRI, got {other:?}")),
    }
}

// ---------------------------------------------------------------------
// Shape expressions
// ---------------------------------------------------------------------

fn convert_shape_or(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExpr, String> {
    Ok(unwrap_or_ref(convert_shape_or_ref(prefixes, base, pair)?))
}

/// Same as `convert_shape_or` but preserves a bare shape reference as
/// `ShapeExprOrRef::Ref` rather than eagerly wrapping it in a synthetic
/// single-alternative `ShapeAnd`/`ShapeOr` -- used everywhere a
/// `ShapeExprOrRef` (not a bare `ShapeExpr`) is the right AST shape (e.g.
/// inside another `ShapeAnd`/`ShapeOr`'s alternatives list, or a triple
/// constraint's `value_expr`).
fn convert_shape_or_ref(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExprOrRef, String> {
    let mut ands: Vec<ShapeExprOrRef> = pair
        .into_inner()
        .map(|p| convert_shape_and_ref(prefixes, base, p))
        .collect::<Result<_, _>>()?;
    if ands.len() == 1 {
        Ok(ands.remove(0))
    } else {
        Ok(ShapeExprOrRef::Expr(ShapeExpr::ShapeOr {
            shape_exprs: ands,
        }))
    }
}

fn convert_shape_and_ref(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExprOrRef, String> {
    let mut nots: Vec<ShapeExprOrRef> = pair
        .into_inner()
        .map(|p| convert_shape_not_ref(prefixes, base, p))
        .collect::<Result<_, _>>()?;
    if nots.len() == 1 {
        Ok(nots.remove(0))
    } else {
        Ok(ShapeExprOrRef::Expr(ShapeExpr::ShapeAnd {
            shape_exprs: nots,
        }))
    }
}

fn convert_shape_not_ref(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExprOrRef, String> {
    let mut inner = pair.into_inner();
    let first = inner.next().ok_or("empty ShapeNot")?;
    if first.as_rule() == Rule::NotKeyword {
        let atom = inner.next().ok_or("NOT with no shape expression")?;
        let inner_ref = convert_shape_atom_ref(prefixes, base, atom)?;
        Ok(ShapeExprOrRef::Expr(ShapeExpr::ShapeNot {
            shape_expr: Box::new(inner_ref),
        }))
    } else {
        convert_shape_atom_ref(prefixes, base, first)
    }
}

fn convert_shape_atom_ref(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExprOrRef, String> {
    let atom = pair.into_inner().next().ok_or("empty ShapeAtom")?;
    match atom.as_rule() {
        Rule::AnyNode => Ok(ShapeExprOrRef::Expr(ShapeExpr::Shape {
            closed: false,
            extra: vec![],
            expression: None,
        })),
        Rule::ShapeRef => {
            let label_pair = atom.into_inner().next().ok_or("empty ShapeRef")?;
            let label = resolve_iri_pair(prefixes, base, label_pair)?;
            Ok(ShapeExprOrRef::Ref(label))
        }
        Rule::NodeConstraint => Ok(ShapeExprOrRef::Expr(convert_node_constraint(
            prefixes, base, atom,
        )?)),
        Rule::ShapeDefinition => Ok(ShapeExprOrRef::Expr(convert_shape_definition(
            prefixes, base, atom,
        )?)),
        Rule::ShapeOr => convert_shape_or_ref(prefixes, base, atom),
        other => Err(format!("unexpected shape atom: {other:?}")),
    }
}

fn unwrap_or_ref(r: ShapeExprOrRef) -> ShapeExpr {
    match r {
        ShapeExprOrRef::Expr(e) => e,
        // A bare `@<label>` used where only a `ShapeExpr` (not a
        // `ShapeExprOrRef`) is syntactically valid (a top-level
        // `ShapeExprDecl`'s body) -- represent it as a trivial `ShapeAnd` of
        // one reference, which validates identically to the reference
        // itself (see `shex_native.rs::validate_node`'s `ShapeAnd` case).
        ShapeExprOrRef::Ref(label) => ShapeExpr::ShapeAnd {
            shape_exprs: vec![ShapeExprOrRef::Ref(label)],
        },
    }
}

fn convert_shape_definition(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExpr, String> {
    let mut closed = false;
    let mut extra = Vec::new();
    let mut expression = None;

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::Qualifier => {
                let q = p.into_inner().next().ok_or("empty Qualifier")?;
                match q.as_rule() {
                    Rule::Closed => closed = true,
                    Rule::Extra => {
                        for iri_pair in q.into_inner() {
                            extra.push(resolve_iri_pair(prefixes, base, iri_pair)?);
                        }
                    }
                    other => return Err(format!("unexpected qualifier: {other:?}")),
                }
            }
            Rule::TripleExpr => {
                expression = Some(convert_triple_expr(prefixes, base, p)?);
            }
            other => return Err(format!("unexpected shape-definition member: {other:?}")),
        }
    }

    Ok(ShapeExpr::Shape {
        closed,
        extra,
        expression,
    })
}

fn convert_node_constraint(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<ShapeExpr, String> {
    let mut datatype = None;
    let mut node_kind = None;
    let mut length = None;
    let mut minlength = None;
    let mut maxlength = None;
    let mut mininclusive = None;
    let mut maxinclusive = None;
    let mut minexclusive = None;
    let mut maxexclusive = None;
    let mut pattern = None;
    let mut flags = String::new();
    let mut values = None;

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::NodeKindWord => {
                node_kind = Some(p.as_str().to_lowercase());
            }
            Rule::DatatypeSpec => {
                let iri_pair = p.into_inner().next().ok_or("empty DatatypeSpec")?;
                datatype = Some(resolve_iri_pair(prefixes, base, iri_pair)?);
            }
            Rule::ValueSet => {
                values = Some(convert_value_set(prefixes, base, p)?);
            }
            Rule::Facet => {
                let f = p.into_inner().next().ok_or("empty Facet")?;
                match f.as_rule() {
                    Rule::LengthFacet => length = Some(parse_int(&f)?),
                    Rule::MinLengthFacet => minlength = Some(parse_int(&f)?),
                    Rule::MaxLengthFacet => maxlength = Some(parse_int(&f)?),
                    Rule::MinInclusiveFacet => mininclusive = Some(parse_float(&f)?),
                    Rule::MaxInclusiveFacet => maxinclusive = Some(parse_float(&f)?),
                    Rule::MinExclusiveFacet => minexclusive = Some(parse_float(&f)?),
                    Rule::MaxExclusiveFacet => maxexclusive = Some(parse_float(&f)?),
                    Rule::PatternFacet => {
                        let (pat, fl) = parse_regex_literal(f.as_str())?;
                        pattern = Some(pat);
                        flags = fl;
                    }
                    other => return Err(format!("unexpected facet: {other:?}")),
                }
            }
            other => return Err(format!("unexpected node-constraint member: {other:?}")),
        }
    }

    Ok(ShapeExpr::NodeConstraint {
        datatype,
        node_kind,
        length,
        minlength,
        maxlength,
        mininclusive,
        maxinclusive,
        minexclusive,
        maxexclusive,
        pattern,
        flags,
        values,
    })
}

fn parse_int(facet_pair: &Pair<Rule>) -> Result<i64, String> {
    let text = facet_pair.as_str();
    let digits: String = text
        .chars()
        .filter(|c| c.is_ascii_digit() || *c == '-' || *c == '+')
        .collect();
    digits
        .parse::<i64>()
        .map_err(|e| format!("invalid integer facet {text:?}: {e}"))
}

fn parse_float(facet_pair: &Pair<Rule>) -> Result<f64, String> {
    let inner = facet_pair.clone().into_inner().next();
    let text = match inner {
        Some(p) => p.as_str().to_string(),
        None => facet_pair.as_str().to_string(),
    };
    // Strip the leading keyword, keep the trailing numeric literal.
    let numeric = text
        .split_whitespace()
        .last()
        .ok_or_else(|| format!("malformed numeric facet: {text:?}"))?;
    numeric
        .parse::<f64>()
        .map_err(|e| format!("invalid numeric facet {numeric:?}: {e}"))
}

fn parse_regex_literal(raw: &str) -> Result<(String, String), String> {
    // `/pattern/flags`, with `\/` inside the pattern as an escaped literal
    // slash (per the grammar's `RegexChar`).
    let rest = raw.strip_prefix('/').ok_or("malformed regex literal")?;
    let end = find_unescaped_slash(rest).ok_or("malformed regex literal: missing closing '/'")?;
    let pattern = rest[..end].replace("\\/", "/");
    let flags = rest[end + 1..].to_string();
    Ok((pattern, flags))
}

fn find_unescaped_slash(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            i += 2;
            continue;
        }
        if bytes[i] == b'/' {
            return Some(i);
        }
        i += 1;
    }
    None
}

fn convert_value_set(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<Vec<ValueSetValue>, String> {
    let mut out = Vec::new();
    for p in pair.into_inner() {
        let v = p.into_inner().next().ok_or("empty ValueSetValue")?;
        out.push(match v.as_rule() {
            Rule::LanguageValue => {
                let str_pair = v.into_inner().next().ok_or("empty LANGUAGE value")?;
                ValueSetValue::Language {
                    ty: "Language".to_string(),
                    language_tag: unescape_string(str_pair.as_str()),
                }
            }
            Rule::IriStemValue => {
                let iri_pair = v.into_inner().next().ok_or("empty IriStemValue")?;
                let iri = resolve_iri_pair(prefixes, base, iri_pair)?;
                ValueSetValue::IriStem {
                    ty: "IriStem".to_string(),
                    stem: iri,
                }
            }
            Rule::IriValue => {
                let iri_pair = v.into_inner().next().ok_or("empty IriValue")?;
                ValueSetValue::Iri(resolve_iri_pair(prefixes, base, iri_pair)?)
            }
            Rule::LiteralValue => {
                let mut parts = v.into_inner();
                let str_pair = parts.next().ok_or("empty LiteralValue")?;
                let lang = parts
                    .next()
                    .map(|l| l.as_str().trim_start_matches('@').to_string());
                ValueSetValue::LangLiteral {
                    value: unescape_string(str_pair.as_str()),
                    language: lang,
                }
            }
            other => return Err(format!("unexpected value-set entry: {other:?}")),
        });
    }
    Ok(out)
}

fn unescape_string(quoted: &str) -> String {
    let inner = &quoted[1..quoted.len() - 1];
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.next() {
                out.push(next);
            }
        } else {
            out.push(c);
        }
    }
    out
}

// ---------------------------------------------------------------------
// Triple expressions
// ---------------------------------------------------------------------

fn convert_triple_expr(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<TripleExpr, String> {
    let one_of = pair.into_inner().next().ok_or("empty TripleExpr")?;
    convert_one_of(prefixes, base, one_of)
}

fn convert_one_of(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<TripleExpr, String> {
    let mut each_ofs: Vec<TripleExpr> = pair
        .into_inner()
        .map(|p| convert_each_of(prefixes, base, p))
        .collect::<Result<_, _>>()?;
    if each_ofs.len() == 1 {
        Ok(each_ofs.remove(0))
    } else {
        Ok(TripleExpr::OneOf {
            expressions: each_ofs,
        })
    }
}

fn convert_each_of(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<TripleExpr, String> {
    let mut unaries: Vec<TripleExpr> = pair
        .into_inner()
        .map(|p| convert_unary(prefixes, base, p))
        .collect::<Result<_, _>>()?;
    if unaries.len() == 1 {
        Ok(unaries.remove(0))
    } else {
        Ok(TripleExpr::EachOf {
            expressions: unaries,
        })
    }
}

fn convert_unary(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<TripleExpr, String> {
    let inner = pair.into_inner().next().ok_or("empty UnaryTripleExpr")?;
    match inner.as_rule() {
        Rule::TripleConstraint => convert_triple_constraint(prefixes, base, inner),
        Rule::OneOfTripleExpr => convert_one_of(prefixes, base, inner),
        other => Err(format!("unexpected unary triple expression: {other:?}")),
    }
}

fn convert_triple_constraint(
    prefixes: &HashMap<String, String>, base: &Option<String>, pair: Pair<Rule>,
) -> Result<TripleExpr, String> {
    let mut parts = pair.into_inner();
    let predicate_pair = parts
        .next()
        .ok_or("malformed triple constraint: missing predicate")?;
    let predicate = match predicate_pair.into_inner().next() {
        Some(iri_pair) => resolve_iri_pair(prefixes, base, iri_pair)?,
        // Bare `a` shorthand.
        None => "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
    };

    let value_or_pair = parts
        .next()
        .ok_or("malformed triple constraint: missing value expression")?;
    let value_ref = convert_shape_or_ref(prefixes, base, value_or_pair)?;
    // `.` (AnyNode) means "no value constraint at all" -- represented as
    // `value_expr: None`, matching how `shex_native.rs` treats an absent
    // `valueExpr` in ShExJ. Detected structurally: `convert_shape_or_ref`
    // for a lone `AnyNode` atom returns `Expr(Shape{ expression: None, .. })`
    // with no closed/extra/expression set -- the canonical "always
    // conforms" shape -- which we special-case back to `None` here so the
    // AST matches ShExJ byte-for-byte for the common `.`-as-any-node idiom.
    let value_expr = if is_any_node_shape(&value_ref) {
        None
    } else {
        Some(Box::new(value_ref))
    };

    let cardinality = parts.next();
    let (min, max) = match cardinality {
        Some(c) => convert_cardinality(c)?,
        None => (None, None),
    };

    Ok(TripleExpr::TripleConstraint {
        predicate,
        value_expr,
        min,
        max,
    })
}

fn is_any_node_shape(r: &ShapeExprOrRef) -> bool {
    matches!(
        r,
        ShapeExprOrRef::Expr(ShapeExpr::Shape { closed: false, extra, expression: None }) if extra.is_empty()
    )
}

fn convert_cardinality(pair: Pair<Rule>) -> Result<(Option<i64>, Option<i64>), String> {
    let inner = pair.into_inner().next().ok_or("empty Cardinality")?;
    match inner.as_rule() {
        Rule::Star => Ok((Some(0), Some(-1))),
        Rule::Plus => Ok((Some(1), Some(-1))),
        Rule::Question => Ok((Some(0), Some(1))),
        Rule::RepeatRange => {
            let mut nums = inner.into_inner();
            let min = nums
                .next()
                .ok_or("empty repeat range")?
                .as_str()
                .parse::<i64>()
                .map_err(|e| e.to_string())?;
            let max = match nums.next() {
                Some(m) => m.as_str().parse::<i64>().map_err(|e| e.to_string())?,
                None => -1,
            };
            Ok((Some(min), Some(max)))
        }
        other => Err(format!("unexpected cardinality: {other:?}")),
    }
}
