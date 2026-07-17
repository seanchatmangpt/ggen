/// Value extraction and manipulation utilities
///
/// Functions for extracting lexical forms, numeric values, date/times,
/// and string representations from RDF terms.
use crate::encoding::Encoder;
use crate::triples::Term;

/// Extract the lexical form of an IRI, literal, or blank node
pub fn get_lexical_form(x: usize) -> Option<String> {
    let term = Encoder::decode_to_term(x)?;
    match term {
        Term::Iri(_) => {
            let s = Encoder::decode(&x)?;
            if s.starts_with('<') && s.ends_with('>') {
                Some(s[1..s.len() - 1].to_string())
            } else {
                Some(s)
            }
        }
        Term::Literal(lit) => Encoder::decode(&lit.value),
        Term::BlankNode(_) => {
            let s = Encoder::decode(&x)?;
            Some(s.strip_prefix("_:").map(str::to_string).unwrap_or(s))
        }
    }
}

/// Like `get_lexical_form`, but for constraints where the SHACL spec means
/// an actual RDF string representation (IRIs and literals), NOT a blank
/// node's internal label -- which is an implementation detail, not a value
/// the graph author wrote, and must never be treated as satisfying a
/// string-shaped constraint. Used by sh:minLength/sh:maxLength: a real W3C
/// test case (minLength-001) targets exactly this, expecting a blank node
/// (reached via sh:targetClass) to always violate sh:minLength/maxLength.
pub(crate) fn get_string_representation(x: usize) -> Option<String> {
    match Encoder::decode_to_term(x)? {
        Term::BlankNode(_) => None,
        _ => get_lexical_form(x),
    }
}

/// Get the language tag of a language-tagged string literal
pub fn get_lang_tag(x: usize) -> Option<String> {
    if let Some(Term::Literal(lit)) = Encoder::decode_to_term(x) {
        lit.lang.as_ref().and_then(Encoder::decode)
    } else {
        None
    }
}

/// Return the numeric value of a literal as f64, handling xsd:integer,
/// xsd:decimal, xsd:double.
pub(crate) fn get_numeric_value(term_id: usize) -> Result<f64, String> {
    let lex = get_lexical_form(term_id).ok_or_else(|| "Not a lexical form".to_string())?;
    lex.trim()
        .parse::<f64>()
        .map_err(|e| format!("parse error: {}", e))
}

/// Parse an `xsd:dateTime` lexical form
/// (`YYYY-MM-DDTHH:MM:SS[.fff][(Z|+HH:MM|-HH:MM)]`)
/// into (seconds-since-a-fixed-epoch, has_explicit_timezone). The epoch choice is
/// arbitrary (proleptic Gregorian day count is not needed here) since only
/// relative comparisons between two parsed values are ever made.
pub(crate) fn parse_datetime(lex: &str) -> Result<(i64, bool), String> {
    let lex = lex.trim();
    let t_pos = lex
        .find('T')
        .ok_or_else(|| "Missing T in datetime".to_string())?;
    let (date_part, rest) = lex.split_at(t_pos);
    let rest = &rest[1..]; // skip 'T'

    let date_fields: Vec<&str> = date_part.splitn(3, '-').collect();
    // A leading '-' (BCE year) would produce an empty first field; not handled here.
    if date_fields.len() != 3 || date_fields.iter().any(|f| f.is_empty()) {
        return Err("Invalid date fields".to_string());
    }
    let year: i64 = date_fields[0]
        .parse()
        .map_err(|e| format!("year parse error: {}", e))?;
    let month: i64 = date_fields[1]
        .parse()
        .map_err(|e| format!("month parse error: {}", e))?;
    let day: i64 = date_fields[2]
        .parse()
        .map_err(|e| format!("day parse error: {}", e))?;

    // Split off an explicit timezone: 'Z', or a trailing "+HH:MM"/"-HH:MM"
    // (search from index 1 to skip a leading '-' that might belong to... time
    // fields never start with '-', so any '+' or '-' found is the tz offset).
    let (time_part, tz_offset_secs, has_tz) = if let Some(z_pos) = rest.find('Z') {
        (&rest[..z_pos], 0i64, true)
    } else if let Some(sign_pos) = rest.rfind(['+', '-']) {
        let (t, off) = rest.split_at(sign_pos);
        let off_fields: Vec<&str> = off[1..].splitn(2, ':').collect();
        let off_h: i64 = off_fields
            .first()
            .ok_or_else(|| "Missing tz hour".to_string())?
            .parse()
            .map_err(|e| format!("tz hour parse error: {}", e))?;
        let off_m: i64 = match off_fields.get(1) {
            Some(s) => s
                .parse()
                .map_err(|e| format!("tz minute parse error: {}", e))?,
            None => 0,
        };
        let sign = if off.starts_with('-') { -1 } else { 1 };
        (t, sign * (off_h * 3600 + off_m * 60), true)
    } else {
        (rest, 0i64, false)
    };

    let time_fields: Vec<&str> = time_part.splitn(3, ':').collect();
    if time_fields.len() != 3 {
        return Err("Invalid time fields".to_string());
    }
    let hour: i64 = time_fields[0]
        .parse()
        .map_err(|e| format!("hour parse error: {}", e))?;
    let minute: i64 = time_fields[1]
        .parse()
        .map_err(|e| format!("minute parse error: {}", e))?;
    let second: f64 = time_fields[2]
        .parse()
        .map_err(|e| format!("second parse error: {}", e))?;

    // Days since an arbitrary fixed epoch via a standard civil-to-days
    // algorithm (Howard Hinnant's `days_from_civil`), valid for the
    // proleptic Gregorian calendar.
    let y = if month <= 2 { year - 1 } else { year };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = (month + 9) % 12;
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146097 + doe - 719468;

    let total_secs = days * 86400 + hour * 3600 + minute * 60 + (second as i64) - tz_offset_secs;
    Ok((total_secs, has_tz))
}

/// Compare two terms numerically (for sh:lessThan etc.), falling back to
/// `xsd:dateTime`-aware comparison when numeric parsing fails. Returns
/// `None` if not comparable at all, OR if comparing a timezone-qualified
/// dateTime against a timezone-less one -- per XSD's dateTime partial order,
/// such pairs have an "indeterminate" ordering (the comparison result
/// depends on which of the 14-hour timezone extremes is assumed), which
/// this crate treats the same as any other non-comparable pair: a
/// range-facet result of `None` (constraint not satisfied). This matches
/// the real W3C SHACL test suite's expectation (minInclusive-002/003).
pub fn compare_numeric(a: usize, b: usize) -> Result<Option<std::cmp::Ordering>, String> {
    if let (Ok(av), Ok(bv)) = (get_numeric_value(a), get_numeric_value(b)) {
        return Ok(av.partial_cmp(&bv));
    }
    let (a_lex, b_lex) = (
        get_lexical_form(a).ok_or_else(|| "a not lexical".to_string())?,
        get_lexical_form(b).ok_or_else(|| "b not lexical".to_string())?,
    );
    let (a_secs, a_tz) = parse_datetime(&a_lex)?;
    let (b_secs, b_tz) = parse_datetime(&b_lex)?;
    if a_tz != b_tz {
        return Ok(None);
    }
    Ok(a_secs.partial_cmp(&b_secs))
}

/// Test if text matches a regex pattern with the given flags
pub fn match_regex(pattern: &str, text: &str, flags: &str) -> bool {
    let mut builder = regex::RegexBuilder::new(pattern);
    for c in flags.chars() {
        match c {
            'i' => {
                builder.case_insensitive(true);
            }
            'm' => {
                builder.multi_line(true);
            }
            's' => {
                builder.dot_matches_new_line(true);
            }
            'x' => {
                builder.ignore_whitespace(true);
            }
            _ => {}
        }
    }
    builder.build().is_ok_and(|re| re.is_match(text))
}

/// Parse an xsd:integer literal as i64 (allowing negative values)
pub(crate) fn get_integer_value(term_id: usize) -> Result<i64, String> {
    let lex = get_lexical_form(term_id).ok_or_else(|| "Not a lexical form".to_string())?;
    lex.trim()
        .parse::<i64>()
        .map_err(|e| format!("parse error: {}", e))
}

/// Decode a term ID to a Term
pub fn decode_to_term(id: usize) -> Term {
    match Encoder::decode_to_term(id) {
        Some(t) => t,
        None => Term::Iri(crate::triples::TermImpl { iri: id }),
    }
}
