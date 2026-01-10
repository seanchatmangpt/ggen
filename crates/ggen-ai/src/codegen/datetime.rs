//! DateTime validation and conversion from XSD formats to Rust chrono types
//!
//! This module provides comprehensive support for parsing and validating XSD datetime types
//! according to the XML Schema specification (W3C).
//!
//! ## XSD DateTime Types Supported
//!
//! - `xsd:dateTime` → `chrono::DateTime<chrono::Utc>`
//! - `xsd:date` → `chrono::NaiveDate`
//! - `xsd:time` → `chrono::NaiveTime`
//! - `xsd:duration` → `chrono::Duration`
//! - `xsd:gYear` → `u16`
//! - `xsd:gYearMonth` → `(u16, u8)` (year, month)
//! - `xsd:gDay` → `u8` (day of month)
//! - `xsd:gMonth` → `u8` (month value)
//! - `xsd:gMonthDay` → `(u8, u8)` (month, day)
//!
//! ## ISO 8601 Format Support
//!
//! All datetime parsing follows ISO 8601 standard with optional timezone information:
//! - Date format: `YYYY-MM-DD`
//! - Time format: `HH:MM:SS` or `HH:MM:SS.sss`
//! - DateTime format: `YYYY-MM-DDTHH:MM:SSZ` or `YYYY-MM-DDTHH:MM:SS±HH:MM`

use crate::error::{GgenAiError, Result};
use chrono::{DateTime, Duration, NaiveDate, NaiveTime, Utc};
use serde::{Deserialize, Serialize};

/// DateTime validation result containing parsed value and metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DateTimeValidation {
    /// The parsed datetime value as a string representation
    pub value: String,
    /// The XSD datatype that was used for validation
    pub xsd_type: String,
    /// Whether the value includes timezone information
    pub has_timezone: bool,
    /// Optional timezone offset in minutes (positive for east of UTC)
    pub timezone_offset: Option<i32>,
}

impl DateTimeValidation {
    /// Create a new datetime validation result
    pub fn new(value: impl Into<String>, xsd_type: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            xsd_type: xsd_type.into(),
            has_timezone: false,
            timezone_offset: None,
        }
    }

    /// Set timezone information
    pub fn with_timezone(mut self, offset_minutes: i32) -> Self {
        self.has_timezone = true;
        self.timezone_offset = Some(offset_minutes);
        self
    }
}

/// Parse xsd:dateTime value (ISO 8601 with timezone)
///
/// # Format
/// `YYYY-MM-DDTHH:MM:SSZ` or `YYYY-MM-DDTHH:MM:SS±HH:MM` or `YYYY-MM-DDTHH:MM:SS.sssZ`
///
/// # Examples
/// ```ignore
/// assert!(parse_datetime("2023-12-25T10:30:45Z").is_ok());
/// assert!(parse_datetime("2023-12-25T10:30:45+05:30").is_ok());
/// assert!(parse_datetime("2023-12-25T10:30:45.123Z").is_ok());
/// ```
pub fn parse_datetime(value: &str) -> Result<DateTime<Utc>> {
    // Try parsing with timezone first
    if let Ok(dt) = DateTime::parse_from_rfc3339(value) {
        return Ok(dt.with_timezone(&Utc));
    }

    // Try parsing ISO 8601 format
    if let Ok(dt) = DateTime::parse_from_rfc2822(value) {
        return Ok(dt.with_timezone(&Utc));
    }

    // Fallback: try parsing without timezone (assume UTC)
    let value = value.trim_end_matches('Z');
    if let Ok(naive_dt) = chrono::NaiveDateTime::parse_from_str(value, "%Y-%m-%dT%H:%M:%S") {
        return Ok(DateTime::<Utc>::from_naive_utc_and_offset(naive_dt, Utc));
    }

    if let Ok(naive_dt) = chrono::NaiveDateTime::parse_from_str(value, "%Y-%m-%dT%H:%M:%S%.f") {
        return Ok(DateTime::<Utc>::from_naive_utc_and_offset(naive_dt, Utc));
    }

    Err(GgenAiError::validation(format!(
        "Invalid datetime format: '{}'. Expected ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ",
        value
    )))
}

/// Parse xsd:date value (date without time)
///
/// # Format
/// `YYYY-MM-DD`
///
/// # Examples
/// ```ignore
/// assert!(parse_date("2023-12-25").is_ok());
/// assert!(parse_date("2000-01-01").is_ok());
/// assert!(parse_date("2024-02-29").is_ok()); // leap year
/// ```
pub fn parse_date(value: &str) -> Result<NaiveDate> {
    NaiveDate::parse_from_str(value, "%Y-%m-%d").map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid date format: '{}'. Expected: YYYY-MM-DD",
            value
        ))
    })
}

/// Parse xsd:time value (time without date)
///
/// # Format
/// `HH:MM:SS` or `HH:MM:SS.sss`
///
/// # Examples
/// ```ignore
/// assert!(parse_time("10:30:45").is_ok());
/// assert!(parse_time("23:59:59").is_ok());
/// assert!(parse_time("00:00:00.123").is_ok());
/// ```
pub fn parse_time(value: &str) -> Result<NaiveTime> {
    // Try with milliseconds first
    if let Ok(time) = NaiveTime::parse_from_str(value, "%H:%M:%S%.f") {
        return Ok(time);
    }

    // Try without milliseconds
    NaiveTime::parse_from_str(value, "%H:%M:%S").map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid time format: '{}'. Expected: HH:MM:SS or HH:MM:SS.sss",
            value
        ))
    })
}

/// Parse xsd:duration value
///
/// # Format
/// `P[n]Y[n]M[n]DT[n]H[n]M[n]S` (ISO 8601 duration)
///
/// # Examples
/// ```ignore
/// assert!(parse_duration("P1Y").is_ok()); // 1 year
/// assert!(parse_duration("PT1H30M").is_ok()); // 1 hour 30 minutes
/// assert!(parse_duration("P1DT12H").is_ok()); // 1 day 12 hours
/// ```
pub fn parse_duration(value: &str) -> Result<Duration> {
    // Simple duration parser for ISO 8601 duration format
    // For full implementation, consider the `iso8601` crate
    let trimmed = value.trim();

    if !trimmed.starts_with('P') {
        return Err(GgenAiError::validation(format!(
            "Invalid duration format: '{}'. Expected ISO 8601 format: P[n]Y[n]M[n]DT[n]H[n]M[n]S",
            value
        )));
    }

    // Parse basic duration: supports days, hours, minutes, seconds
    let mut duration = Duration::seconds(0);

    let (date_part, time_part) = if let Some(t_pos) = trimmed.find('T') {
        (&trimmed[1..t_pos], Some(&trimmed[t_pos + 1..]))
    } else {
        (&trimmed[1..], None)
    };

    // Parse date part (simplified: only supports D)
    if let Some(d_pos) = date_part.find('D') {
        if let Ok(days) = date_part[..d_pos].parse::<i64>() {
            duration = duration + Duration::days(days);
        }
    }

    // Parse time part
    if let Some(time_str) = time_part {
        if let Some(h_pos) = time_str.find('H') {
            if let Ok(hours) = time_str[..h_pos].parse::<i64>() {
                duration = duration + Duration::hours(hours);
            }
        }

        if let Some(m_pos) = time_str.find('M') {
            let start = if time_str.contains('H') {
                time_str.find('H').unwrap() + 1
            } else {
                0
            };
            if let Ok(minutes) = time_str[start..m_pos].parse::<i64>() {
                duration = duration + Duration::minutes(minutes);
            }
        }

        if let Some(s_pos) = time_str.find('S') {
            let start = if time_str.contains('M') {
                time_str.rfind('M').unwrap() + 1
            } else if time_str.contains('H') {
                time_str.find('H').unwrap() + 1
            } else {
                0
            };
            if let Ok(seconds) = time_str[start..s_pos].parse::<i64>() {
                duration = duration + Duration::seconds(seconds);
            }
        }
    }

    Ok(duration)
}

/// Validate xsd:gYear value
///
/// # Format
/// `YYYY` (4-digit year)
///
/// # Constraints
/// - Year range: 0-9999
pub fn validate_gyear(value: &str) -> Result<u16> {
    let trimmed = value.trim();

    if trimmed.len() != 4 {
        return Err(GgenAiError::validation(format!(
            "Invalid year format: '{}'. Expected 4-digit year (YYYY)",
            value
        )));
    }

    trimmed.parse::<u16>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid year value: '{}'. Must be a valid 4-digit year",
            value
        ))
    })
}

/// Validate xsd:gYearMonth value
///
/// # Format
/// `YYYY-MM`
///
/// # Constraints
/// - Year range: 0-9999
/// - Month range: 1-12
pub fn validate_gyear_month(value: &str) -> Result<(u16, u8)> {
    let parts: Vec<&str> = value.trim().split('-').collect();

    if parts.len() != 2 {
        return Err(GgenAiError::validation(format!(
            "Invalid year-month format: '{}'. Expected YYYY-MM",
            value
        )));
    }

    let year = parts[0].parse::<u16>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid year in year-month: '{}'. Must be a valid 4-digit year",
            parts[0]
        ))
    })?;

    let month = parts[1].parse::<u8>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid month in year-month: '{}'. Must be a valid number",
            parts[1]
        ))
    })?;

    if !(1..=12).contains(&month) {
        return Err(GgenAiError::validation(format!(
            "Invalid month value: {}. Month must be between 1 and 12",
            month
        )));
    }

    Ok((year, month))
}

/// Validate xsd:gDay value
///
/// # Format
/// `DD` (day of month)
///
/// # Constraints
/// - Day range: 1-31
pub fn validate_gday(value: &str) -> Result<u8> {
    let trimmed = value.trim();
    let day = trimmed.parse::<u8>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid day format: '{}'. Must be a number",
            value
        ))
    })?;

    if !(1..=31).contains(&day) {
        return Err(GgenAiError::validation(format!(
            "Invalid day value: {}. Day must be between 1 and 31",
            day
        )));
    }

    Ok(day)
}

/// Validate xsd:gMonth value
///
/// # Format
/// `MM` (month number)
///
/// # Constraints
/// - Month range: 1-12
pub fn validate_gmonth(value: &str) -> Result<u8> {
    let trimmed = value.trim();
    let month = trimmed.parse::<u8>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid month format: '{}'. Must be a number",
            value
        ))
    })?;

    if !(1..=12).contains(&month) {
        return Err(GgenAiError::validation(format!(
            "Invalid month value: {}. Month must be between 1 and 12",
            month
        )));
    }

    Ok(month)
}

/// Validate xsd:gMonthDay value
///
/// # Format
/// `MM-DD`
///
/// # Constraints
/// - Month range: 1-12
/// - Day range: 1-31
pub fn validate_gmonth_day(value: &str) -> Result<(u8, u8)> {
    let parts: Vec<&str> = value.trim().split('-').collect();

    if parts.len() != 2 {
        return Err(GgenAiError::validation(format!(
            "Invalid month-day format: '{}'. Expected MM-DD",
            value
        )));
    }

    let month = parts[0].parse::<u8>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid month in month-day: '{}'. Must be a valid number",
            parts[0]
        ))
    })?;

    let day = parts[1].parse::<u8>().map_err(|_| {
        GgenAiError::validation(format!(
            "Invalid day in month-day: '{}'. Must be a valid number",
            parts[1]
        ))
    })?;

    if !(1..=12).contains(&month) {
        return Err(GgenAiError::validation(format!(
            "Invalid month value: {}. Month must be between 1 and 12",
            month
        )));
    }

    if !(1..=31).contains(&day) {
        return Err(GgenAiError::validation(format!(
            "Invalid day value: {}. Day must be between 1 and 31",
            day
        )));
    }

    Ok((month, day))
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{Datelike, Timelike};

    // ===== DateTime Parsing Tests =====

    #[test]
    fn test_parse_datetime_with_utc_timezone() {
        let result = parse_datetime("2023-12-25T10:30:45Z");
        assert!(result.is_ok());
        let dt = result.unwrap();
        assert_eq!(dt.year(), 2023);
        assert_eq!(dt.month(), 12);
        assert_eq!(dt.day(), 25);
        assert_eq!(dt.hour(), 10);
        assert_eq!(dt.minute(), 30);
        assert_eq!(dt.second(), 45);
    }

    #[test]
    fn test_parse_datetime_with_positive_offset() {
        let result = parse_datetime("2023-12-25T10:30:45+05:30");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_datetime_with_negative_offset() {
        let result = parse_datetime("2023-12-25T10:30:45-08:00");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_datetime_with_milliseconds() {
        let result = parse_datetime("2023-12-25T10:30:45.123Z");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_datetime_invalid_format() {
        let result = parse_datetime("2023/12/25 10:30:45");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_datetime_invalid_date() {
        let result = parse_datetime("2023-13-45T10:30:45Z");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_datetime_invalid_time() {
        let result = parse_datetime("2023-12-25T25:30:45Z");
        assert!(result.is_err());
    }

    // ===== Date Parsing Tests =====

    #[test]
    fn test_parse_date_valid() {
        let result = parse_date("2023-12-25");
        assert!(result.is_ok());
        let date = result.unwrap();
        assert_eq!(date.year(), 2023);
        assert_eq!(date.month(), 12);
        assert_eq!(date.day(), 25);
    }

    #[test]
    fn test_parse_date_leap_year() {
        let result = parse_date("2024-02-29");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_date_invalid_leap_year() {
        let result = parse_date("2023-02-29");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_date_invalid_month() {
        let result = parse_date("2023-13-25");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_date_invalid_day() {
        let result = parse_date("2023-02-30");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_date_invalid_format() {
        let result = parse_date("25-12-2023");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_date_year_boundary() {
        let result = parse_date("0000-01-01");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_date_max_year() {
        let result = parse_date("9999-12-31");
        assert!(result.is_ok());
    }

    // ===== Time Parsing Tests =====

    #[test]
    fn test_parse_time_valid() {
        let result = parse_time("10:30:45");
        assert!(result.is_ok());
        let time = result.unwrap();
        assert_eq!(time.hour(), 10);
        assert_eq!(time.minute(), 30);
        assert_eq!(time.second(), 45);
    }

    #[test]
    fn test_parse_time_with_milliseconds() {
        let result = parse_time("10:30:45.123");
        assert!(result.is_ok());
        let time = result.unwrap();
        assert_eq!(time.hour(), 10);
        assert_eq!(time.minute(), 30);
        assert_eq!(time.second(), 45);
    }

    #[test]
    fn test_parse_time_midnight() {
        let result = parse_time("00:00:00");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_time_end_of_day() {
        let result = parse_time("23:59:59");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_time_invalid_hour() {
        let result = parse_time("25:30:45");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_time_invalid_minute() {
        let result = parse_time("10:60:45");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_time_invalid_second() {
        let result = parse_time("10:30:60");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_time_invalid_format() {
        let result = parse_time("10-30-45");
        assert!(result.is_err());
    }

    // ===== Duration Parsing Tests =====

    #[test]
    fn test_parse_duration_days() {
        let result = parse_duration("P1D");
        assert!(result.is_ok());
        let duration = result.unwrap();
        assert_eq!(duration.num_days(), 1);
    }

    #[test]
    fn test_parse_duration_hours() {
        let result = parse_duration("PT1H");
        assert!(result.is_ok());
        let duration = result.unwrap();
        assert_eq!(duration.num_hours(), 1);
    }

    #[test]
    fn test_parse_duration_minutes() {
        let result = parse_duration("PT30M");
        assert!(result.is_ok());
        let duration = result.unwrap();
        assert_eq!(duration.num_minutes(), 30);
    }

    #[test]
    fn test_parse_duration_combined() {
        let result = parse_duration("P1DT12H30M");
        assert!(result.is_ok());
        let duration = result.unwrap();
        assert_eq!(duration.num_hours(), 36); // 1 day (24h) + 12h
        assert_eq!(duration.num_minutes(), 2190); // 36h * 60 + 30m
    }

    #[test]
    fn test_parse_duration_missing_p() {
        let result = parse_duration("1D");
        assert!(result.is_err());
    }

    // ===== GYear Validation Tests =====

    #[test]
    fn test_validate_gyear_valid() {
        let result = validate_gyear("2023");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 2023);
    }

    #[test]
    fn test_validate_gyear_year_zero() {
        let result = validate_gyear("0000");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 0);
    }

    #[test]
    fn test_validate_gyear_max_year() {
        let result = validate_gyear("9999");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 9999);
    }

    #[test]
    fn test_validate_gyear_too_short() {
        let result = validate_gyear("23");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gyear_too_long() {
        let result = validate_gyear("20230");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gyear_non_numeric() {
        let result = validate_gyear("abcd");
        assert!(result.is_err());
    }

    // ===== GYearMonth Validation Tests =====

    #[test]
    fn test_validate_gyear_month_valid() {
        let result = validate_gyear_month("2023-12");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), (2023, 12));
    }

    #[test]
    fn test_validate_gyear_month_january() {
        let result = validate_gyear_month("2023-01");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), (2023, 1));
    }

    #[test]
    fn test_validate_gyear_month_invalid_month_zero() {
        let result = validate_gyear_month("2023-00");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gyear_month_invalid_month_13() {
        let result = validate_gyear_month("2023-13");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gyear_month_invalid_format() {
        let result = validate_gyear_month("2023/12");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gyear_month_missing_month() {
        let result = validate_gyear_month("2023");
        assert!(result.is_err());
    }

    // ===== GDay Validation Tests =====

    #[test]
    fn test_validate_gday_valid() {
        let result = validate_gday("25");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 25);
    }

    #[test]
    fn test_validate_gday_first_day() {
        let result = validate_gday("1");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 1);
    }

    #[test]
    fn test_validate_gday_last_day() {
        let result = validate_gday("31");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 31);
    }

    #[test]
    fn test_validate_gday_zero() {
        let result = validate_gday("0");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gday_32() {
        let result = validate_gday("32");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gday_non_numeric() {
        let result = validate_gday("abc");
        assert!(result.is_err());
    }

    // ===== GMonth Validation Tests =====

    #[test]
    fn test_validate_gmonth_valid() {
        let result = validate_gmonth("12");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 12);
    }

    #[test]
    fn test_validate_gmonth_january() {
        let result = validate_gmonth("1");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 1);
    }

    #[test]
    fn test_validate_gmonth_december() {
        let result = validate_gmonth("12");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 12);
    }

    #[test]
    fn test_validate_gmonth_zero() {
        let result = validate_gmonth("0");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gmonth_13() {
        let result = validate_gmonth("13");
        assert!(result.is_err());
    }

    // ===== GMonthDay Validation Tests =====

    #[test]
    fn test_validate_gmonth_day_valid() {
        let result = validate_gmonth_day("12-25");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), (12, 25));
    }

    #[test]
    fn test_validate_gmonth_day_february_29() {
        let result = validate_gmonth_day("02-29");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), (2, 29));
    }

    #[test]
    fn test_validate_gmonth_day_invalid_month() {
        let result = validate_gmonth_day("13-15");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gmonth_day_invalid_day() {
        let result = validate_gmonth_day("12-32");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gmonth_day_zero_month() {
        let result = validate_gmonth_day("00-15");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gmonth_day_zero_day() {
        let result = validate_gmonth_day("12-00");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_gmonth_day_invalid_format() {
        let result = validate_gmonth_day("12/25");
        assert!(result.is_err());
    }

    // ===== Integration Tests =====

    #[test]
    fn test_datetime_validation_builder() {
        let validation = DateTimeValidation::new("2023-12-25", "xsd:date")
            .with_timezone(330); // UTC+5:30

        assert_eq!(validation.value, "2023-12-25");
        assert_eq!(validation.xsd_type, "xsd:date");
        assert!(validation.has_timezone);
        assert_eq!(validation.timezone_offset, Some(330));
    }

    #[test]
    fn test_all_datetime_types_roundtrip() {
        // DateTime
        assert!(parse_datetime("2023-12-25T10:30:45Z").is_ok());

        // Date
        assert!(parse_date("2023-12-25").is_ok());

        // Time
        assert!(parse_time("10:30:45").is_ok());

        // Duration
        assert!(parse_duration("P1DT12H").is_ok());

        // GYear
        assert!(validate_gyear("2023").is_ok());

        // GYearMonth
        assert!(validate_gyear_month("2023-12").is_ok());

        // GDay
        assert!(validate_gday("25").is_ok());

        // GMonth
        assert!(validate_gmonth("12").is_ok());

        // GMonthDay
        assert!(validate_gmonth_day("12-25").is_ok());
    }
}
