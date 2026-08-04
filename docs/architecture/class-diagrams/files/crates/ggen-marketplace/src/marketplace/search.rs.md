# `crates/ggen-marketplace/src/marketplace/search.rs`

Source SHA-256: `e0ea12fbd2f2e050b321cd2d797966a0e3771724d31040c30993a913c71b536d`

```mermaid
classDiagram
    class struct_SearchQuery {
      <<struct>>
      +"text: String"
      +"category_filter: Option~String~"
      +"min_quality_score: Option~QualityScore~"
      +"author_filter: Option~String~"
      +"license_filter: Option~String~"
      +"sort_by: SortBy"
      +"limit: usize"
      +"offset: usize"
    }
    class enum_SortBy {
      <<enum>>
    }
    class struct_SearchEngine {
      <<struct>>
      +"ranker: Arc~Box~dyn Ranker + Send + Sync~~"
    }
    class fn_levenshtein_distance {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for SearchEngine"
    note "SearchEngine"
    note "SearchQuery"
```

## Dependencies

- `crate::marketplace::error::Result`
- `crate::marketplace::models::{Package, QualityScore, SearchResult}`
- `crate::marketplace::traits::{DefaultRanker, Ranker}`
- `std::sync::Arc`
- `super::*`
- `tracing::debug`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
