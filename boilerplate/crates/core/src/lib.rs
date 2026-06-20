pub mod error;
pub mod id;
pub mod pagination;

pub use error::CoreError;
pub type Result<T, E = CoreError> = std::result::Result<T, E>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::Id;
    use crate::pagination::{Page, PagedResult};

    // --- Id<T> tests ---

    #[test]
    fn id_new_produces_non_zero_uuid() {
        let id = Id::<()>::new();
        assert_ne!(id.inner(), uuid::Uuid::nil(), "new Id must not be the nil UUID");
    }

    #[test]
    fn two_fresh_ids_are_distinct() {
        let a = Id::<()>::new();
        let b = Id::<()>::new();
        assert_ne!(a, b, "two independently created Ids must be distinct");
    }

    // --- Name value object tests (via CoreError::validation path) ---

    #[test]
    fn core_error_not_found_formats_correctly() {
        let err = CoreError::not_found("Item", "abc");
        let msg = err.to_string();
        assert!(msg.contains("Item"), "error message must contain entity name");
        assert!(msg.contains("abc"), "error message must contain id");
    }

    #[test]
    fn core_error_validation_round_trips_message() {
        let err = CoreError::validation("must not be empty");
        assert!(err.to_string().contains("must not be empty"));
    }

    // --- Page / PagedResult tests ---

    #[test]
    fn page_new_caps_limit_at_100() {
        let p = Page::new(0, 200);
        assert_eq!(p.limit, 100, "limit above 100 must be capped to 100");
        assert_eq!(p.offset, 0);
    }

    #[test]
    fn page_new_preserves_limit_below_cap() {
        let p = Page::new(5, 50);
        assert_eq!(p.limit, 50);
        assert_eq!(p.offset, 5);
    }

    #[test]
    fn paged_result_has_next_true_when_more_items_remain() {
        let result: PagedResult<i32> = PagedResult::new(
            vec![1, 2, 3],
            10,
            Page::new(0, 3),
        );
        assert!(result.has_next(), "has_next must be true when offset+limit < total");
    }

    #[test]
    fn paged_result_has_next_false_on_last_page() {
        let result: PagedResult<i32> = PagedResult::new(
            vec![1, 2, 3],
            3,
            Page::new(0, 3),
        );
        assert!(!result.has_next(), "has_next must be false when offset+limit >= total");
    }

    #[test]
    fn paged_result_has_next_false_when_empty() {
        let result: PagedResult<i32> = PagedResult::new(vec![], 0, Page::new(0, 20));
        assert!(!result.has_next());
    }
}
