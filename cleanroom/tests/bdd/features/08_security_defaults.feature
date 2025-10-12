# features/08_security_defaults.feature
@security
Feature: Secure-by-default policy
  Background:
    Given a fixture project "cli_net"

  Scenario: Network is disabled by default
    When I run ["fetch","http://example.org"] using "docker"
    Then the exit code is nonzero
    And stderr contains "network disabled"
    And capabilities dropped include ALL
    And process runs as non-root

  Scenario: Opt-in permissive policy
    Given I enable policy "permissive" explicitly
    When I run ["fetch","http://example.org"] using "docker"
    Then the exit code is 0
