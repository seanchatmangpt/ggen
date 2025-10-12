# features/23_skip_logic.feature
@skip
Feature: Skip semantics for missing engines or capabilities
  Scenario: Missing podman skips podman scenarios only
    Given podman is unavailable
    When I run the full feature suite
    Then podman-tagged scenarios are skipped
    And other scenarios run
