# features/11_determinism.feature
@determinism
Feature: Deterministic outputs with seeded RNG and stable mounts
  Background:
    Given a fixture project "cli_rng"

  Scenario Outline: Same seed yields identical outputs
    Given seed "<seed>"
    When I run ["rand","--count","5"] using "auto"
    Then stdout equals the previous stdout for the same seed
    And artifact hashes match

    Examples:
      | seed |
      | 42   |
      | 1337 |

  Scenario: Different seeds yield different outputs
    Given seed "1"
    When I run ["rand","--count","5"] using "auto"
    And I run ["rand","--count","5"] using "auto" with seed "2"
    Then stdout differs
