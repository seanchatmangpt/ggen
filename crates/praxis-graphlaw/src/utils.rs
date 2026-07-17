use crate::{Encoder, Rule, Triple};

pub struct Utils;

impl Utils {
    pub fn decode_triple(triple: &Triple) -> String {
        let s = Encoder::decode(&triple.s.to_encoded()).unwrap();
        let p = Encoder::decode(&triple.p.to_encoded()).unwrap();
        let o = Encoder::decode(&triple.o.to_encoded()).unwrap();
        format!("{} {} {}", s, p, o)
    }
    pub fn decode_rule(rule: &Rule) -> String {
        let body = rule
            .body
            .iter()
            .map(|lit| {
                let s = Self::decode_triple(&lit.pattern);
                if lit.negated {
                    format!("not {{{}}},", s)
                } else {
                    s + ","
                }
            })
            .collect::<String>();
        let head = Self::decode_triple(&rule.head);
        format!("{{{}}}=>{{{}}}", body, head)
    }
    /// Strip a `^^<datatype>` suffix (if present) and the surrounding quotes from a
    /// decoded literal's lexical form -- `"10"^^<http://.../integer>` and `"10"` both
    /// become `10`.
    ///
    /// Previously only stripped quotes when a `^^` suffix was present; an untagged
    /// literal (RDF's own default for a bare quoted value, e.g. `"10"` with no
    /// datatype) fell through the `else` branch unchanged, still wrapped in quote
    /// characters. `Encoder::decode` (encoding.rs) always renders a literal's value as
    /// `"value"`, quoted, regardless of whether it carries a datatype -- so every
    /// caller of this function that immediately does `.parse::<f64>()` on the result
    /// (this crate's SUM/MIN/MAX/AVG accumulators, sparql/accumulators.rs) received a
    /// quote-wrapped string for any untyped numeric literal, which `f64::from_str`
    /// rejects outright, silently falling back to 0 via each caller's own
    /// `.unwrap_or(0.0)`. Confirmed via direct tracing this session: a SUM over three
    /// untagged literal triples computed 0 instead of 6 before this fix.
    pub fn remove_literal_tags(literal: &str) -> String {
        let lexical = literal.split("^^").next().unwrap_or(literal);
        if lexical.len() >= 2 && lexical.starts_with('"') && lexical.ends_with('"') {
            lexical[1..lexical.len() - 1].to_string()
        } else {
            lexical.to_string()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::utils::Utils;

    #[test]
    fn test_remove_literal_tages() {
        let literal = "\"10\"^^<http://www.w3.org/2001/XMLSchema#integer>";
        let expected = "10".to_string();
        assert_eq!(expected, Utils::remove_literal_tags(literal));
    }

    // Regression: an untagged (no `^^datatype`) literal previously passed through
    // unchanged, still wrapped in quotes -- silently unparseable as a number by any
    // caller that immediately does `.parse::<f64>()` (this crate's SUM/MIN/MAX/AVG
    // accumulators), which is exactly the shape `Encoder::decode` produces for a plain
    // literal with no datatype.
    #[test]
    fn remove_literal_tags_strips_quotes_from_untagged_literal() {
        assert_eq!(Utils::remove_literal_tags("\"10\""), "10");
        assert_eq!(Utils::remove_literal_tags("\"3.5\""), "3.5");
    }

    #[test]
    fn remove_literal_tags_leaves_non_quoted_input_unchanged() {
        // Not every caller passes a quoted literal (e.g. an already-bare IRI string);
        // this function must not strip characters from input it doesn't recognize as
        // a quoted lexical form.
        assert_eq!(
            Utils::remove_literal_tags("http://example.org/foo"),
            "http://example.org/foo"
        );
    }
}
