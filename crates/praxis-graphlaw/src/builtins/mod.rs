//! N3 core built-in predicates (log:, math:, string:, list:, func:).
//!
//! `log:implies`/`log:collectAllIn`/`log:notIncludes` are deliberately **not**
//! handled here: they require dynamically reifying formulas and feeding the
//! result back into the fixpoint loop, which only makes sense at the
//! `Reasoner::materialize` level (see `reasoner/log_implies.rs`,
//! `reasoner/log_collect_all_in.rs`, `reasoner/log_not_includes.rs`). Their
//! IRIs are still registered in `classify` below so that "is this predicate a
//! builtin the engine supports somewhere" is a single, centralized check --
//! `classify` never returns `Unknown` for a predicate the engine actually
//! supports, even if `evaluate` isn't the module that ultimately handles it.
//!
//! This module is the single dispatch point for procedural (one-shot, not
//! fixpoint-level) builtin evaluation, called from both
//! `queryengine::SimpleQueryEngine::query` (forward chaining) and
//! `backwardchaining::BackwardChainer` (indirectly, via
//! `SimpleQueryEngine::query`).

pub mod crypto;
pub mod func;
pub mod list;
pub mod log;
pub mod math;
pub mod string;
pub mod time;

#[cfg(test)]
mod builtins_test;

use crate::{Binding, Encoder, Term, Triple, VarOrTerm};

// -- reasoner-level builtin IRIs (handled in reasoner/, not here) -----------
//
// Registered in `classify` so unknown-builtin detection stays centralized,
// per the plan: "register their IRIs in builtins::classify so classify never
// returns Unknown for a predicate the engine actually supports somewhere."
const LOG_IMPLIES: &str = "<http://www.w3.org/2000/10/swap/log#implies>";
const LOG_COLLECT_ALL_IN: &str = "<http://www.w3.org/2000/10/swap/log#collectAllIn>";
const LOG_NOT_INCLUDES: &str = "<http://www.w3.org/2000/10/swap/log#notIncludes>";
// `log:includes`, `log:forAllIn`, `log:ifThenElseIn`, `log:conclusion`: also
// reasoner-level (need a live `TripleIndex`/nested materialize run), same
// reasoning as the three above -- see `reasoner/log_includes.rs`,
// `reasoner/log_for_all_in.rs`, `reasoner/log_if_then_else_in.rs`,
// `reasoner/log_conclusion.rs`.
const LOG_INCLUDES: &str = "<http://www.w3.org/2000/10/swap/log#includes>";
const LOG_FOR_ALL_IN: &str = "<http://www.w3.org/2000/10/swap/log#forAllIn>";
const LOG_IF_THEN_ELSE_IN: &str = "<http://www.w3.org/2000/10/swap/log#ifThenElseIn>";
const LOG_CONCLUSION: &str = "<http://www.w3.org/2000/10/swap/log#conclusion>";
// `log:semantics` -- deliberately UNSUPPORTED: proper semantics would fetch
// and parse a document from a URI (network I/O), which this engine never
// performs. Not registered in `classify` at all, so it correctly falls
// through as an ordinary (unbound) predicate rather than silently claiming
// support -- see `lib/tests/n3_conformance/manifest.json`'s
// `builtin_log_semantics` fixture (status: blocked) for the documented
// rationale.
#[allow(dead_code)]
const LOG_SEMANTICS: &str = "<http://www.w3.org/2000/10/swap/log#semantics>";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinKind {
    EqualTo,
    GreaterThan,
    Sum,
    StringLength,
    StringConcat,
    ListLength,
    ListIn,
    Difference,
    Product,
    Quotient,
    Remainder,
    NotLessThan,
    NotGreaterThan,
    LessThan,
    MathEqualTo,
    MathNotEqualTo,
    NotEqualTo,
    ListAppend,
    StringLessThan,
    LangFromPlainLiteral,
    AbsoluteValue,
    Negation,
    Rounded,
    Ceiling,
    Floor,
    Exponentiation,
    IntegerQuotient,
    Min,
    Max,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Atan2,
    Logarithm,
    MemberCount,
    StringContains,
    StringContainsIgnoringCase,
    StringStartsWith,
    StringEndsWith,
    StringMatches,
    StringNotMatches,
    StringReplace,
    StringSubstring,
    StringToUpperCase,
    StringToLowerCase,
    StringSplit,
    StringEqualIgnoringCase,
    StringNotEqualIgnoringCase,
    StringGreaterThan,
    StringFormat,
    StringScrape,
    ListFirst,
    ListRest,
    ListLast,
    ListMember,
    ListMemberAt,
    ListNotMember,
    ListFirstRest,
    ListRemove,
    ListSort,
    ListUnique,
    ListReverse,
    ListIterate,
    LogDtlit,
    LogRawType,
    LogUri,
    LogLocalName,
    LogBound,
    LogN3String,
    LogParsedAsN3,
    LogConjunction,
    FuncNumericAdd,
    FuncNumericSubtract,
    FuncNumericMultiply,
    FuncNumericDivide,
    FuncNumericIntegerDivide,
    FuncNumericMod,
    FuncNumericAbs,
    FuncNumericNegate,
    FuncNumericEqual,
    FuncNumericLessThan,
    FuncNumericGreaterThan,
    FuncStringLength,
    FuncSubstring,
    TimeLocalTime,
    TimeYear,
    TimeMonth,
    TimeDay,
    TimeHour,
    TimeMinute,
    TimeSecond,
    TimeDayOfWeek,
    TimeTimeZone,
    TimeInSeconds,
    CryptoSha,
    CryptoSha256,
    CryptoSha512,
    CryptoMd5,
    /// Reasoner-level (fixpoint) builtins: classified here so `classify`
    /// never returns `Unknown` for them, but `evaluate` does not handle
    /// them -- see the reasoner/log_* modules.
    ReasonerLevel,
}

/// IRI prefixes of namespaces this engine recognizes as N3/EYE *builtin*
/// vocabularies -- as opposed to ordinary user/application predicates. Used
/// by `reject_if_unsupported_builtin` to distinguish "this predicate is
/// trying to invoke a known builtin family this engine doesn't implement"
/// (which should fail loudly) from "this is just an ordinary predicate with
/// no matching facts yet" (which is completely normal and must NOT fail).
const KNOWN_BUILTIN_NAMESPACES: &[&str] = &[
    "http://www.w3.org/2000/10/swap/log#",
    "http://www.w3.org/2000/10/swap/math#",
    "http://www.w3.org/2000/10/swap/string#",
    "http://www.w3.org/2000/10/swap/list#",
    "http://www.w3.org/2000/10/swap/time#",
    "http://www.w3.org/2000/10/swap/crypto#",
    "http://www.w3.org/2007/rif-builtin-function#",
    "http://www.w3.org/2007/rif-builtin-predicate#",
    "http://eulersharp.sourceforge.net/2003/03swap/log-rules#",
];

/// If `p` names a predicate from a *known* N3/EYE builtin namespace that
/// this engine does not (or not yet) implement -- e.g. `log:callWithCut`,
/// `log:repeat`, `e:findall` -- panic with a clear, actionable message
/// instead of silently falling through to an ordinary (always-empty) EDB
/// triple lookup. That silent-fallthrough was a real, disclosed gap: rules
/// depending on an unregistered builtin used to just never fire, with
/// nothing to distinguish "this fact doesn't exist yet" from "this engine
/// doesn't understand this predicate at all" (see `lib/tests/n3_conformance/
/// SKIPPED.md`'s `path-discovery`/`medic`/`ackermann`/etc. entries, each
/// diagnosed only by manual inspection). Ordinary user predicates (any IRI
/// outside `KNOWN_BUILTIN_NAMESPACES`) are completely unaffected -- an
/// unmatched `:hasOwner` pattern is normal Datalog/N3 behavior, not an error.
pub fn reject_if_unsupported_builtin(p: &VarOrTerm) {
    if !p.is_term() || classify(p).is_some() {
        return;
    }
    let Some(decoded) = Encoder::decode(&p.to_encoded()) else {
        return;
    };
    let iri = decoded.trim_start_matches('<').trim_end_matches('>');
    if KNOWN_BUILTIN_NAMESPACES
        .iter()
        .any(|ns| iri.starts_with(ns))
    {
        panic!(
            "Unsupported N3 builtin predicate: <{}> -- this is a recognized builtin \
             namespace, but this specific predicate is not implemented by this engine \
             (see lib/tests/n3_conformance/SKIPPED.md for known examples like \
             log:callWithCut, log:repeat, e:findall). Refusing to silently treat it as \
             an ordinary (always-unmatched) fact pattern.",
            iri
        );
    }
}

/// Identify whether `p` (a body literal's predicate position) names one of
/// the built-ins handled procedurally by `evaluate`, or a reasoner-level
/// (fixpoint) builtin handled elsewhere.
pub fn classify(p: &VarOrTerm) -> Option<BuiltinKind> {
    if !p.is_term() {
        return None;
    }
    let decoded = Encoder::decode(&p.to_encoded())?;
    Some(match decoded.as_str() {
        log::LOG_EQUAL_TO => BuiltinKind::EqualTo,
        log::LOG_NOT_EQUAL_TO => BuiltinKind::NotEqualTo,
        math::MATH_GREATER_THAN => BuiltinKind::GreaterThan,
        math::MATH_SUM => BuiltinKind::Sum,
        string::STRING_LENGTH => BuiltinKind::StringLength,
        string::STRING_CONCAT => BuiltinKind::StringConcat,
        list::LIST_LENGTH => BuiltinKind::ListLength,
        list::LIST_IN => BuiltinKind::ListIn,
        math::MATH_DIFFERENCE => BuiltinKind::Difference,
        math::MATH_PRODUCT => BuiltinKind::Product,
        math::MATH_QUOTIENT => BuiltinKind::Quotient,
        math::MATH_REMAINDER => BuiltinKind::Remainder,
        math::MATH_NOT_LESS_THAN => BuiltinKind::NotLessThan,
        math::MATH_NOT_GREATER_THAN => BuiltinKind::NotGreaterThan,
        math::MATH_LESS_THAN => BuiltinKind::LessThan,
        math::MATH_EQUAL_TO => BuiltinKind::MathEqualTo,
        math::MATH_NOT_EQUAL_TO => BuiltinKind::MathNotEqualTo,
        list::LIST_APPEND => BuiltinKind::ListAppend,
        string::STRING_LESS_THAN => BuiltinKind::StringLessThan,
        func::FUNC_LANG_FROM_PLAIN_LITERAL => BuiltinKind::LangFromPlainLiteral,
        math::MATH_ABSOLUTE_VALUE => BuiltinKind::AbsoluteValue,
        math::MATH_NEGATION => BuiltinKind::Negation,
        math::MATH_ROUNDED => BuiltinKind::Rounded,
        math::MATH_CEILING => BuiltinKind::Ceiling,
        math::MATH_FLOOR => BuiltinKind::Floor,
        math::MATH_EXPONENTIATION => BuiltinKind::Exponentiation,
        math::MATH_INTEGER_QUOTIENT => BuiltinKind::IntegerQuotient,
        math::MATH_MIN => BuiltinKind::Min,
        math::MATH_MAX => BuiltinKind::Max,
        math::MATH_SIN => BuiltinKind::Sin,
        math::MATH_COS => BuiltinKind::Cos,
        math::MATH_TAN => BuiltinKind::Tan,
        math::MATH_ASIN => BuiltinKind::Asin,
        math::MATH_ACOS => BuiltinKind::Acos,
        math::MATH_ATAN => BuiltinKind::Atan,
        math::MATH_ATAN2 => BuiltinKind::Atan2,
        math::MATH_LOGARITHM => BuiltinKind::Logarithm,
        math::MATH_MEMBER_COUNT => BuiltinKind::MemberCount,
        string::STRING_CONTAINS => BuiltinKind::StringContains,
        string::STRING_CONTAINS_IGNORING_CASE => BuiltinKind::StringContainsIgnoringCase,
        string::STRING_STARTS_WITH => BuiltinKind::StringStartsWith,
        string::STRING_ENDS_WITH => BuiltinKind::StringEndsWith,
        string::STRING_MATCHES => BuiltinKind::StringMatches,
        string::STRING_NOT_MATCHES => BuiltinKind::StringNotMatches,
        string::STRING_REPLACE => BuiltinKind::StringReplace,
        string::STRING_SUBSTRING => BuiltinKind::StringSubstring,
        string::STRING_TO_UPPER_CASE => BuiltinKind::StringToUpperCase,
        string::STRING_TO_LOWER_CASE => BuiltinKind::StringToLowerCase,
        string::STRING_SPLIT => BuiltinKind::StringSplit,
        string::STRING_EQUAL_IGNORING_CASE => BuiltinKind::StringEqualIgnoringCase,
        string::STRING_NOT_EQUAL_IGNORING_CASE => BuiltinKind::StringNotEqualIgnoringCase,
        string::STRING_GREATER_THAN => BuiltinKind::StringGreaterThan,
        string::STRING_FORMAT => BuiltinKind::StringFormat,
        string::STRING_SCRAPE => BuiltinKind::StringScrape,
        list::LIST_FIRST => BuiltinKind::ListFirst,
        list::LIST_REST => BuiltinKind::ListRest,
        list::LIST_LAST => BuiltinKind::ListLast,
        list::LIST_MEMBER => BuiltinKind::ListMember,
        list::LIST_MEMBER_AT => BuiltinKind::ListMemberAt,
        list::LIST_NOT_MEMBER => BuiltinKind::ListNotMember,
        list::LIST_FIRST_REST => BuiltinKind::ListFirstRest,
        list::LIST_REMOVE => BuiltinKind::ListRemove,
        list::LIST_SORT => BuiltinKind::ListSort,
        list::LIST_UNIQUE => BuiltinKind::ListUnique,
        list::LIST_REVERSE => BuiltinKind::ListReverse,
        list::LIST_ITERATE => BuiltinKind::ListIterate,
        log::LOG_DTLIT => BuiltinKind::LogDtlit,
        log::LOG_RAW_TYPE => BuiltinKind::LogRawType,
        log::LOG_URI => BuiltinKind::LogUri,
        log::LOG_LOCAL_NAME => BuiltinKind::LogLocalName,
        log::LOG_BOUND => BuiltinKind::LogBound,
        log::LOG_N3_STRING => BuiltinKind::LogN3String,
        log::LOG_PARSED_AS_N3 => BuiltinKind::LogParsedAsN3,
        log::LOG_CONJUNCTION => BuiltinKind::LogConjunction,
        func::FUNC_NUMERIC_ADD => BuiltinKind::FuncNumericAdd,
        func::FUNC_NUMERIC_SUBTRACT => BuiltinKind::FuncNumericSubtract,
        func::FUNC_NUMERIC_MULTIPLY => BuiltinKind::FuncNumericMultiply,
        func::FUNC_NUMERIC_DIVIDE => BuiltinKind::FuncNumericDivide,
        func::FUNC_NUMERIC_INTEGER_DIVIDE => BuiltinKind::FuncNumericIntegerDivide,
        func::FUNC_NUMERIC_MOD => BuiltinKind::FuncNumericMod,
        func::FUNC_NUMERIC_ABS => BuiltinKind::FuncNumericAbs,
        func::FUNC_NUMERIC_NEGATE => BuiltinKind::FuncNumericNegate,
        func::FUNC_NUMERIC_EQUAL => BuiltinKind::FuncNumericEqual,
        func::FUNC_NUMERIC_LESS_THAN => BuiltinKind::FuncNumericLessThan,
        func::FUNC_NUMERIC_GREATER_THAN => BuiltinKind::FuncNumericGreaterThan,
        func::FUNC_STRING_LENGTH => BuiltinKind::FuncStringLength,
        func::FUNC_SUBSTRING => BuiltinKind::FuncSubstring,
        time::TIME_LOCAL_TIME => BuiltinKind::TimeLocalTime,
        time::TIME_YEAR => BuiltinKind::TimeYear,
        time::TIME_MONTH => BuiltinKind::TimeMonth,
        time::TIME_DAY => BuiltinKind::TimeDay,
        time::TIME_HOUR => BuiltinKind::TimeHour,
        time::TIME_MINUTE => BuiltinKind::TimeMinute,
        time::TIME_SECOND => BuiltinKind::TimeSecond,
        time::TIME_DAY_OF_WEEK => BuiltinKind::TimeDayOfWeek,
        time::TIME_TIME_ZONE => BuiltinKind::TimeTimeZone,
        time::TIME_IN_SECONDS => BuiltinKind::TimeInSeconds,
        crypto::CRYPTO_SHA => BuiltinKind::CryptoSha,
        crypto::CRYPTO_SHA256 => BuiltinKind::CryptoSha256,
        crypto::CRYPTO_SHA512 => BuiltinKind::CryptoSha512,
        crypto::CRYPTO_MD5 => BuiltinKind::CryptoMd5,
        LOG_IMPLIES | LOG_COLLECT_ALL_IN | LOG_NOT_INCLUDES | LOG_INCLUDES | LOG_FOR_ALL_IN
        | LOG_IF_THEN_ELSE_IN | LOG_CONCLUSION => BuiltinKind::ReasonerLevel,
        _ => return None,
    })
}

/// Evaluate a single built-in body literal against the bindings
/// accumulated so far in the enclosing conjunction. Returns `None` if the
/// builtin cannot fire at all (constraint failed / operands unresolved
/// for every row), mirroring a failed `TripleIndex::query` lookup.
pub fn evaluate(kind: BuiltinKind, pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    match kind {
        BuiltinKind::EqualTo => log::eval_equal_to(pattern, bindings),
        BuiltinKind::NotEqualTo => log::eval_not_equal_to(pattern, bindings),
        BuiltinKind::GreaterThan => math::eval_greater_than(pattern, bindings),
        BuiltinKind::Sum => math::eval_sum(pattern, bindings),
        BuiltinKind::Difference => math::eval_difference(pattern, bindings),
        BuiltinKind::Product => math::eval_product(pattern, bindings),
        BuiltinKind::Quotient => math::eval_quotient(pattern, bindings),
        BuiltinKind::Remainder => math::eval_remainder(pattern, bindings),
        BuiltinKind::NotLessThan => math::eval_not_less_than(pattern, bindings),
        BuiltinKind::NotGreaterThan => math::eval_not_greater_than(pattern, bindings),
        BuiltinKind::LessThan => math::eval_less_than(pattern, bindings),
        BuiltinKind::MathEqualTo => math::eval_math_equal_to(pattern, bindings),
        BuiltinKind::MathNotEqualTo => math::eval_math_not_equal_to(pattern, bindings),
        BuiltinKind::StringLength => string::eval_string_length(pattern, bindings),
        BuiltinKind::StringConcat => string::eval_string_concat(pattern, bindings),
        BuiltinKind::StringLessThan => string::eval_string_less_than(pattern, bindings),
        BuiltinKind::ListLength => list::eval_list_length(pattern, bindings),
        BuiltinKind::ListIn => list::eval_list_in(pattern, bindings),
        BuiltinKind::ListAppend => list::eval_list_append(pattern, bindings),
        BuiltinKind::LangFromPlainLiteral => func::eval_lang_from_plain_literal(pattern, bindings),
        BuiltinKind::AbsoluteValue => math::eval_absolute_value(pattern, bindings),
        BuiltinKind::Negation => math::eval_negation(pattern, bindings),
        BuiltinKind::Rounded => math::eval_rounded(pattern, bindings),
        BuiltinKind::Ceiling => math::eval_ceiling(pattern, bindings),
        BuiltinKind::Floor => math::eval_floor(pattern, bindings),
        BuiltinKind::Exponentiation => math::eval_exponentiation(pattern, bindings),
        BuiltinKind::IntegerQuotient => math::eval_integer_quotient(pattern, bindings),
        BuiltinKind::Min => math::eval_min(pattern, bindings),
        BuiltinKind::Max => math::eval_max(pattern, bindings),
        BuiltinKind::Sin => math::eval_sin(pattern, bindings),
        BuiltinKind::Cos => math::eval_cos(pattern, bindings),
        BuiltinKind::Tan => math::eval_tan(pattern, bindings),
        BuiltinKind::Asin => math::eval_asin(pattern, bindings),
        BuiltinKind::Acos => math::eval_acos(pattern, bindings),
        BuiltinKind::Atan => math::eval_atan(pattern, bindings),
        BuiltinKind::Atan2 => math::eval_atan2(pattern, bindings),
        BuiltinKind::Logarithm => math::eval_logarithm(pattern, bindings),
        BuiltinKind::MemberCount => math::eval_member_count(pattern, bindings),
        BuiltinKind::StringContains => string::eval_string_contains(pattern, bindings),
        BuiltinKind::StringContainsIgnoringCase => {
            string::eval_string_contains_ignoring_case(pattern, bindings)
        }
        BuiltinKind::StringStartsWith => string::eval_string_starts_with(pattern, bindings),
        BuiltinKind::StringEndsWith => string::eval_string_ends_with(pattern, bindings),
        BuiltinKind::StringMatches => string::eval_string_matches(pattern, bindings),
        BuiltinKind::StringNotMatches => string::eval_string_not_matches(pattern, bindings),
        BuiltinKind::StringReplace => string::eval_string_replace(pattern, bindings),
        BuiltinKind::StringSubstring => string::eval_string_substring(pattern, bindings),
        BuiltinKind::StringToUpperCase => string::eval_string_to_upper_case(pattern, bindings),
        BuiltinKind::StringToLowerCase => string::eval_string_to_lower_case(pattern, bindings),
        BuiltinKind::StringSplit => string::eval_string_split(pattern, bindings),
        BuiltinKind::StringEqualIgnoringCase => {
            string::eval_string_equal_ignoring_case(pattern, bindings)
        }
        BuiltinKind::StringNotEqualIgnoringCase => {
            string::eval_string_not_equal_ignoring_case(pattern, bindings)
        }
        BuiltinKind::StringGreaterThan => string::eval_string_greater_than(pattern, bindings),
        BuiltinKind::StringFormat => string::eval_string_format(pattern, bindings),
        BuiltinKind::StringScrape => string::eval_string_scrape(pattern, bindings),
        BuiltinKind::ListFirst => list::eval_list_first(pattern, bindings),
        BuiltinKind::ListRest => list::eval_list_rest(pattern, bindings),
        BuiltinKind::ListLast => list::eval_list_last(pattern, bindings),
        BuiltinKind::ListMember => list::eval_list_member(pattern, bindings),
        BuiltinKind::ListMemberAt => list::eval_list_member_at(pattern, bindings),
        BuiltinKind::ListNotMember => list::eval_list_not_member(pattern, bindings),
        BuiltinKind::ListFirstRest => list::eval_list_first_rest(pattern, bindings),
        BuiltinKind::ListRemove => list::eval_list_remove(pattern, bindings),
        BuiltinKind::ListSort => list::eval_list_sort(pattern, bindings),
        BuiltinKind::ListUnique => list::eval_list_unique(pattern, bindings),
        BuiltinKind::ListReverse => list::eval_list_reverse(pattern, bindings),
        BuiltinKind::ListIterate => list::eval_list_iterate(pattern, bindings),
        BuiltinKind::LogDtlit => log::eval_dtlit(pattern, bindings),
        BuiltinKind::LogRawType => log::eval_raw_type(pattern, bindings),
        BuiltinKind::LogUri => log::eval_uri(pattern, bindings),
        BuiltinKind::LogLocalName => log::eval_local_name(pattern, bindings),
        BuiltinKind::LogBound => log::eval_bound(pattern, bindings),
        BuiltinKind::LogN3String => log::eval_n3_string(pattern, bindings),
        BuiltinKind::LogParsedAsN3 => log::eval_parsed_as_n3(pattern, bindings),
        BuiltinKind::LogConjunction => log::eval_conjunction(pattern, bindings),
        BuiltinKind::FuncNumericAdd => func::eval_numeric_add(pattern, bindings),
        BuiltinKind::FuncNumericSubtract => func::eval_numeric_subtract(pattern, bindings),
        BuiltinKind::FuncNumericMultiply => func::eval_numeric_multiply(pattern, bindings),
        BuiltinKind::FuncNumericDivide => func::eval_numeric_divide(pattern, bindings),
        BuiltinKind::FuncNumericIntegerDivide => {
            func::eval_numeric_integer_divide(pattern, bindings)
        }
        BuiltinKind::FuncNumericMod => func::eval_numeric_mod(pattern, bindings),
        BuiltinKind::FuncNumericAbs => func::eval_numeric_abs(pattern, bindings),
        BuiltinKind::FuncNumericNegate => func::eval_numeric_negate(pattern, bindings),
        BuiltinKind::FuncNumericEqual => func::eval_numeric_equal(pattern, bindings),
        BuiltinKind::FuncNumericLessThan => func::eval_numeric_less_than(pattern, bindings),
        BuiltinKind::FuncNumericGreaterThan => func::eval_numeric_greater_than(pattern, bindings),
        BuiltinKind::FuncStringLength => func::eval_string_length(pattern, bindings),
        BuiltinKind::FuncSubstring => func::eval_substring(pattern, bindings),
        BuiltinKind::TimeLocalTime => time::eval_local_time(pattern, bindings),
        BuiltinKind::TimeYear => time::eval_year(pattern, bindings),
        BuiltinKind::TimeMonth => time::eval_month(pattern, bindings),
        BuiltinKind::TimeDay => time::eval_day(pattern, bindings),
        BuiltinKind::TimeHour => time::eval_hour(pattern, bindings),
        BuiltinKind::TimeMinute => time::eval_minute(pattern, bindings),
        BuiltinKind::TimeSecond => time::eval_second(pattern, bindings),
        BuiltinKind::TimeDayOfWeek => time::eval_day_of_week(pattern, bindings),
        BuiltinKind::TimeTimeZone => time::eval_time_zone(pattern, bindings),
        BuiltinKind::TimeInSeconds => time::eval_in_seconds(pattern, bindings),
        BuiltinKind::CryptoSha => crypto::eval_sha(pattern, bindings),
        BuiltinKind::CryptoSha256 => crypto::eval_sha256(pattern, bindings),
        BuiltinKind::CryptoSha512 => crypto::eval_sha512(pattern, bindings),
        BuiltinKind::CryptoMd5 => crypto::eval_md5(pattern, bindings),
        // Reasoner-level builtins are never dispatched from here -- the
        // fixpoint loop in reasoner/mod.rs intercepts them before falling
        // through to `SimpleQueryEngine`/`builtins::evaluate`.
        BuiltinKind::ReasonerLevel => None,
    }
}

// -- shared operand-resolution helpers, used by every namespace submodule --

/// Resolve a pattern operand (subject/object) to a concrete encoded value
/// id for the given row: variables are looked up in `bindings`, ground
/// terms (including list/formula handles) are already resolved.
pub(crate) fn resolve_operand(term: &VarOrTerm, bindings: &Binding, row: usize) -> Option<usize> {
    if term.is_var() {
        bindings.get(&term.to_encoded())?.get(row).copied()
    } else {
        Some(term.to_encoded())
    }
}

/// Resolve a list-term operand (subject of math:sum/string:concat/list:length)
/// to its ordered, row-resolved member value ids. Members that are
/// themselves variables (e.g. `( ?p1 ?p2 )`) are looked up against `row`;
/// members that are ground terms (e.g. the literal " " in
/// `( ?fn " " ?ln )`) are used as-is.
pub(crate) fn subject_list_members(
    term: &VarOrTerm, bindings: &Binding, row: usize,
) -> Option<Vec<usize>> {
    let list_id = resolve_operand(term, bindings, row)?;
    let member_ids = VarOrTerm::list_members(list_id)?;
    let mut resolved = Vec::with_capacity(member_ids.len());
    for m in member_ids {
        if let Some(vals) = bindings.get(&m) {
            resolved.push(*vals.get(row)?);
        } else {
            resolved.push(m);
        }
    }
    Some(resolved)
}

pub(crate) fn numeric_value(id: usize) -> Option<f64> {
    let lex = match Encoder::decode_to_term(id) {
        Some(Term::Literal(lit)) => Encoder::decode(&lit.value)?,
        _ => Encoder::decode(&id)?,
    };
    lex.trim()
        .trim_matches(|c| c == '<' || c == '>')
        .parse::<f64>()
        .ok()
}

pub(crate) fn lexical_value(id: usize) -> Option<String> {
    match Encoder::decode_to_term(id) {
        Some(Term::Literal(lit)) => Encoder::decode(&lit.value),
        _ => Encoder::decode(&id),
    }
}

/// A literal's language tag, or `""` if it has none (per RIF's
/// `func:lang-from-PlainLiteral` semantics) -- `None` only if `id`
/// isn't a literal at all.
pub(crate) fn lang_value(id: usize) -> Option<String> {
    match Encoder::decode_to_term(id) {
        Some(Term::Literal(lit)) => match lit.lang {
            Some(lang_id) => Encoder::decode(&lang_id),
            None => Some(String::new()),
        },
        _ => None,
    }
}

pub(crate) fn intern_number(value: f64) -> usize {
    let is_whole = value.fract() == 0.0 && value.abs() < 1e15;
    let lexical = if is_whole {
        format!("{}", value as i64)
    } else {
        format!("{}", value)
    };
    let datatype = if is_whole {
        "<http://www.w3.org/2001/XMLSchema#integer>"
    } else {
        "<http://www.w3.org/2001/XMLSchema#decimal>"
    };
    VarOrTerm::new_literal(lexical, Some(datatype.to_string()), None).to_encoded()
}

pub(crate) fn intern_string(value: String) -> usize {
    // `datatype: None` (an RDF 1.1 simple literal), not an explicit
    // xsd:string annotation -- matches every other literal-producing path in
    // this engine (parsed Turtle/N3 data, N3/Datalog rule literals, SPARQL
    // CONSTRUCT template instantiation). A builtin's computed string result
    // (string:concat, crypto:sha256, log:uri, ...) must encode identically to
    // a hand-written literal with the same lexical value, or a rule/query
    // comparing against it silently fails to unify. Found via the N3
    // conformance corpus (`builtin_string_concat` et al.), which encodes its
    // expected-triple fixtures as bare strings.
    VarOrTerm::new_literal(value, None, None).to_encoded()
}

/// Copy every column's value at `row` from `bindings` into `out` -- used
/// to carry forward already-bound variables when a builtin filters rows
/// (constraints) or expands them (list:in).
pub(crate) fn copy_row(bindings: &Binding, row: usize, out: &mut Binding) {
    for (&k, v) in bindings.iter() {
        if let Some(&val) = v.get(row) {
            out.add(&k, val);
        }
    }
}

// -- constraint builtins (log:equalTo, math:greaterThan) ----------------

pub(crate) fn eval_row_constraint(
    pattern: &Triple, bindings: &Binding, check: impl Fn(usize, usize) -> bool,
) -> Option<Binding> {
    if bindings.is_empty() {
        // No prior rows: treat as a single ground/implicit row. A
        // successful check yields "1 matched row with 0 columns" (same
        // convention TripleIndex::query uses for a matched ground triple).
        let s = resolve_operand(&pattern.s, bindings, 0)?;
        let o = resolve_operand(&pattern.o, bindings, 0)?;
        return if check(s, o) {
            Some(Binding::new())
        } else {
            None
        };
    }
    let mut result = Binding::new();
    for row in 0..bindings.len() {
        if let (Some(s), Some(o)) = (
            resolve_operand(&pattern.s, bindings, row),
            resolve_operand(&pattern.o, bindings, row),
        ) {
            if check(s, o) {
                copy_row(bindings, row, &mut result);
            }
        }
    }
    if !result.is_empty() {
        Some(result)
    } else {
        None
    }
}

// -- functional builtins (math:sum, string:length/concat, list:length) --

pub(crate) fn eval_functional(
    pattern: &Triple, bindings: &Binding,
    compute: impl Fn(&Triple, &Binding, usize) -> Option<usize>,
) -> Option<Binding> {
    if !pattern.o.is_var() {
        return None;
    }
    let obj_var = pattern.o.to_encoded();
    if bindings.is_empty() {
        let value = compute(pattern, bindings, 0)?;
        let mut result = Binding::new();
        result.add(&obj_var, value);
        return Some(result);
    }
    let mut result = Binding::new();
    for row in 0..bindings.len() {
        if let Some(value) = compute(pattern, bindings, row) {
            copy_row(bindings, row, &mut result);
            result.add(&obj_var, value);
        }
    }
    if !result.is_empty() {
        Some(result)
    } else {
        None
    }
}

// -- generator builtin (list:in) -----------------------------------------

pub(crate) fn eval_generator(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    if !pattern.s.is_var() {
        return None;
    }
    let subj_var = pattern.s.to_encoded();
    if bindings.is_empty() {
        let list_id = resolve_operand(&pattern.o, bindings, 0)?;
        let members = VarOrTerm::list_members(list_id)?;
        let mut result = Binding::new();
        for m in members {
            result.add(&subj_var, m);
        }
        return if !result.is_empty() {
            Some(result)
        } else {
            None
        };
    }
    let mut result = Binding::new();
    for row in 0..bindings.len() {
        if let Some(list_id) = resolve_operand(&pattern.o, bindings, row) {
            if let Some(members) = VarOrTerm::list_members(list_id) {
                for m in members {
                    copy_row(bindings, row, &mut result);
                    result.add(&subj_var, m);
                }
            }
        }
    }
    if !result.is_empty() {
        Some(result)
    } else {
        None
    }
}
