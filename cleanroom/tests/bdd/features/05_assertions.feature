# features/05_assertions.feature
@assert
Feature: Fluent assertions on output, JSON, and timing
  Background:
    Given a fixture project "cli_json"

  Scenario: JSON assertion with predicate
    When I run ["json","--emit","{\"ok\":true,\"n\":3}"] using "auto"
    Then stdout is valid json
    And stdout.json at "$.ok" is true
    And stdout.json at "$.n" equals 3
    And duration <= 1000 ms

  Scenario: Regex assertions
    When I run ["--help"] using "local"
    Then stdout matches /USAGE(?s).*OPTIONS/
