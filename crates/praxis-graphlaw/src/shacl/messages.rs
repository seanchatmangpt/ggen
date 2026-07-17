use super::index_utils::get_objects;
use super::report::ValidationResult;
use super::values::{get_lang_tag, get_lexical_form};
use super::Vocab;
/// Message and severity handling
///
/// Functions for extracting and processing SHACL validation messages
/// and severity levels.
use crate::tripleindex::TripleIndex;

/// Get the severity level of a shape (defaults to sh:Violation)
pub(crate) fn get_severity(shapes: &TripleIndex, shape_node: usize, vocab: &Vocab) -> usize {
    let sevs = get_objects(shapes, shape_node, vocab.sh_severity);
    if sevs.is_empty() {
        vocab.sh_violation
    } else {
        sevs[0]
    }
}

/// Returns each sh:message value on `shape_node` paired with its language tag (if any)
pub(crate) fn get_shape_messages(
    shapes: &TripleIndex,
    shape_node: usize,
    vocab: &Vocab,
) -> Vec<(Option<String>, String)> {
    get_objects(shapes, shape_node, vocab.sh_message)
        .into_iter()
        .filter_map(|m| get_lexical_form(m).map(|text| (get_lang_tag(m), text)))
        .collect()
}

/// Select a single message to report when a shape has zero or more sh:message
/// values, possibly in different languages.
///
/// NOTE: there is no locale parameter threaded through the validator (the
/// public API has no way for a caller to request a specific language), so this
/// implements a fixed, spec-informed default policy rather than full RFC 4647
/// language-range negotiation: prefer a plain (no language tag) literal first
/// -- matching the SHACL/RDF convention that a language-less sh:message is the
/// implementation-neutral fallback -- then prefer one tagged "en", and
/// otherwise fall back to whichever value came first.
pub(crate) fn pick_preferred_message(messages: &[(Option<String>, String)]) -> Option<String> {
    if let Some((_, text)) = messages.iter().find(|(lang, _)| lang.is_none()) {
        return Some(text.clone());
    }
    if let Some((_, text)) = messages.iter().find(|(lang, _)| {
        lang.as_deref()
            .map(|l| l.eq_ignore_ascii_case("en") || l.to_lowercase().starts_with("en-"))
            .unwrap_or(false)
    }) {
        return Some(text.clone());
    }
    messages.first().map(|(_, text)| text.clone())
}

/// Create a ValidationResult from individual components
pub(crate) fn make_result(
    focus_node: usize,
    result_path: Option<usize>,
    value: Option<usize>,
    component: usize,
    shape_node: usize,
    severity: usize,
    message: Option<String>,
) -> ValidationResult {
    ValidationResult {
        focus_node: super::values::decode_to_term(focus_node),
        result_path: result_path.map(super::values::decode_to_term),
        value: value.map(super::values::decode_to_term),
        source_constraint_component: super::values::decode_to_term(component),
        source_shape: super::values::decode_to_term(shape_node),
        severity: super::values::decode_to_term(severity),
        message,
    }
}
