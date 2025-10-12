# features/24_io_limits.feature
@limits
Feature: Output and resource limits
  Background:
    Given a fixture project "cli_spam"

  Scenario: Output is truncated with indicator
    Given max_output is 4KB
    When I run ["spam","--bytes","16384"] using "auto"
    Then captured stdout size is 4096 bytes
    And stdout ends with "[TRUNCATED]"
