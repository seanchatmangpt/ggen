# features/10_redaction.feature
@redact
Feature: Secret redaction in logs and artifacts
  Background:
    Given a fixture project "cli_secret"
    And environment SECRET_TOKEN="s3cr3t-ABC"

  Scenario: Secrets do not leak
    When I run ["echo-env","SECRET_TOKEN"] using "auto"
    Then stdout does not contain "s3cr3t-ABC"
    And stdout contains "[REDACTED]"
    And artifacts contain no unredacted secrets
