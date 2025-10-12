# features/07_coverage.feature
@coverage
Feature: Coverage artifacts collection and host merge
  Background:
    Given a fixture project "cli_cov" instrumented for llvm-cov

  Scenario: Container-produced profraw is merged on host
    When I run ["cover","--work"] using "docker"
    Then a .profraw file exists in artifacts
    And cleanroom merges coverage into "target/coverage/coverage.json"
    And remapped paths point to host sources
