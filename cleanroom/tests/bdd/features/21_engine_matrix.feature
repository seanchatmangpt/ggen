# features/21_engine_matrix.feature
@matrix
Feature: Matrix behavior across docker and podman
  Background:
    Given docker is "maybe" available
    And podman is "maybe" available

  Scenario: Same scenario passes on both engines
    When I execute scenario "help" on engines ["docker","podman"]
    Then results are equivalent modulo engine metadata
