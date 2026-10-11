// NEGATIVE (historical FP class): comments and doc text mention the
// forbidden words (mockall, automock, assert!(true), is_ok() || is_err())
// but no actual violation exists. Comments are not in the AST.
// Expected verdict: [] (clean).
/// Docs warn against `use mockall::...` and `#[automock]`; see also the
/// anti-pattern `assert!(x.is_ok() || x.is_err())`.
#[test]
fn words_in_comments_are_not_violations() {
    // Never write: assert!(true);
    // Never write: result.is_ok() || result.is_err()
    let parsed: Result<u8, String> = "3".parse::<u8>().map_err(|e: std::num::ParseIntError| e.to_string());
    assert_eq!(parsed.unwrap(), 3);
}
