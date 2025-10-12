# features/15_api_stability.feature
@api @doc
Feature: Public API stability and docs build
  Scenario: Docs build without private panics
    When I build documentation with --document-private-items
    Then build succeeds
    And crate root has forbid(unsafe_code)
    And clippy passes with -D warnings
