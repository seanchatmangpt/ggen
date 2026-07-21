//! `time:` namespace builtins (`http://www.w3.org/2000/10/swap/time#`).
//!
//! All predicates here are unary functional builtins: the subject is an
//! `xsd:dateTime` literal (or plain string in that lexical form), the
//! object is the extracted/derived value. Parsing is self-contained (no
//! external date/time crate) following the same lexical-form parsing
//! already used for `xsd:dateTime` comparison in `shacl.rs`.

use super::{eval_functional, intern_number, intern_string, lexical_value, resolve_operand};
use crate::{Binding, Triple};

pub(crate) const TIME_LOCAL_TIME: &str = "<http://www.w3.org/2000/10/swap/time#localTime>";
pub(crate) const TIME_YEAR: &str = "<http://www.w3.org/2000/10/swap/time#year>";
pub(crate) const TIME_MONTH: &str = "<http://www.w3.org/2000/10/swap/time#month>";
pub(crate) const TIME_DAY: &str = "<http://www.w3.org/2000/10/swap/time#day>";
pub(crate) const TIME_HOUR: &str = "<http://www.w3.org/2000/10/swap/time#hour>";
pub(crate) const TIME_MINUTE: &str = "<http://www.w3.org/2000/10/swap/time#minute>";
pub(crate) const TIME_SECOND: &str = "<http://www.w3.org/2000/10/swap/time#second>";
pub(crate) const TIME_DAY_OF_WEEK: &str = "<http://www.w3.org/2000/10/swap/time#dayOfWeek>";
pub(crate) const TIME_TIME_ZONE: &str = "<http://www.w3.org/2000/10/swap/time#timeZone>";
pub(crate) const TIME_IN_SECONDS: &str = "<http://www.w3.org/2000/10/swap/time#inSeconds>";

/// Parsed components of an `xsd:dateTime` lexical form:
/// `YYYY-MM-DDTHH:MM:SS[.fff][(Z|+HH:MM|-HH:MM)]`.
struct DateTimeParts {
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
    /// Timezone suffix as written (`"Z"`, `"+05:00"`, `"-08:00"`), or
    /// `""` if the lexical form had no explicit timezone.
    tz_str: String,
    /// Timezone offset in seconds from UTC (0 for `Z` or no timezone).
    tz_offset_secs: i64,
}

fn parse_datetime(lex: &str) -> Option<DateTimeParts> {
    let lex = lex.trim();
    let t_pos = lex.find('T')?;
    let (date_part, rest) = lex.split_at(t_pos);
    let rest = &rest[1..]; // skip 'T'

    let date_fields: Vec<&str> = date_part.splitn(3, '-').collect();
    if date_fields.len() != 3 || date_fields.iter().any(|f| f.is_empty()) {
        return None;
    }
    let year: i64 = date_fields[0].parse().ok()?;
    let month: i64 = date_fields[1].parse().ok()?;
    let day: i64 = date_fields[2].parse().ok()?;

    let (time_part, tz_str, tz_offset_secs) = if let Some(z_pos) = rest.find('Z') {
        (&rest[..z_pos], "Z".to_string(), 0i64)
    } else if let Some(sign_pos) = rest.rfind(['+', '-']) {
        let (t, off) = rest.split_at(sign_pos);
        let off_fields: Vec<&str> = off[1..].splitn(2, ':').collect();
        let off_h: i64 = off_fields.first()?.parse().ok()?;
        let off_m: i64 = off_fields.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
        let sign = if off.starts_with('-') { -1 } else { 1 };
        (t, off.to_string(), sign * (off_h * 3600 + off_m * 60))
    } else {
        (rest, String::new(), 0i64)
    };

    let time_fields: Vec<&str> = time_part.splitn(3, ':').collect();
    if time_fields.len() != 3 {
        return None;
    }
    let hour: i64 = time_fields[0].parse().ok()?;
    let minute: i64 = time_fields[1].parse().ok()?;
    let second: f64 = time_fields[2].parse().ok()?;

    Some(DateTimeParts {
        year,
        month,
        day,
        hour,
        minute,
        second,
        tz_str,
        tz_offset_secs,
    })
}

/// Days since the Unix epoch (1970-01-01) for a proleptic-Gregorian
/// civil date, via Howard Hinnant's `days_from_civil` algorithm.
fn days_from_civil(year: i64, month: i64, day: i64) -> i64 {
    let y = if month <= 2 { year - 1 } else { year };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = (month + 9) % 12;
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146097 + doe - 719468
}

fn epoch_seconds(p: &DateTimeParts) -> i64 {
    let days = days_from_civil(p.year, p.month, p.day);
    days * 86400 + p.hour * 3600 + p.minute * 60 + (p.second as i64) - p.tz_offset_secs
}

/// ISO 8601 day of week as a 1 (Monday) .. 7 (Sunday) integer, computed
/// from the days-since-epoch (1970-01-01 was a Thursday, ISO day 4).
fn day_of_week(p: &DateTimeParts) -> i64 {
    let days = days_from_civil(p.year, p.month, p.day);
    let rem = (days + 3).rem_euclid(7); // shift so 1970-01-01 (Thu) lands on 3
    rem + 1
}

fn eval_component(
    pattern: &Triple, bindings: &Binding, extract: impl Fn(&DateTimeParts) -> f64,
) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let lex = lexical_value(s)?;
        let parts = parse_datetime(&lex)?;
        Some(intern_number(extract(&parts)))
    })
}

pub(crate) fn eval_year(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| p.year as f64)
}

pub(crate) fn eval_month(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| p.month as f64)
}

pub(crate) fn eval_day(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| p.day as f64)
}

pub(crate) fn eval_hour(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| p.hour as f64)
}

pub(crate) fn eval_minute(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| p.minute as f64)
}

pub(crate) fn eval_second(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| p.second)
}

pub(crate) fn eval_day_of_week(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| day_of_week(p) as f64)
}

pub(crate) fn eval_in_seconds(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_component(pattern, bindings, |p| epoch_seconds(p) as f64)
}

pub(crate) fn eval_time_zone(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let lex = lexical_value(s)?;
        let parts = parse_datetime(&lex)?;
        Some(intern_string(parts.tz_str))
    })
}

/// `time:localTime` -- no timezone database is available, so this is
/// documented as an identity pass-through of the dateTime lexical form
/// (a validated round-trip: parse then re-serialize), rather than a real
/// UTC-to-local conversion. This keeps the builtin deterministic and
/// testable without depending on the host's local timezone or wall clock.
pub(crate) fn eval_local_time(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let lex = lexical_value(s)?;
        let parts = parse_datetime(&lex)?;
        let out = format!(
            "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}{}",
            parts.year,
            parts.month,
            parts.day,
            parts.hour,
            parts.minute,
            parts.second as i64,
            parts.tz_str
        );
        Some(intern_string(out))
    })
}
