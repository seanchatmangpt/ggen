//! Clean fixture: a Result-returning test using `?` fails on Err — it is
//! failure-capable even with zero assert macros.
use std::num::ParseIntError;

#[test]
fn parses_number() -> Result<(), ParseIntError> {
    let n: u32 = "42".parse()?;
    let _ = n;
    Ok(())
}
