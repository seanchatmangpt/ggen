# features/02_backend_autodetect.feature
@core @detect
Feature: Backend auto-detection and override
  Background:
    Given a fixture project "cli_smoke"
    And docker is "maybe" available
    And podman is "maybe" available

  Scenario: Auto chooses docker if available
    Given environment CLEANROOM_BACKEND is unset
    When I request backend "auto"
    Then the resolved backend is one of ["docker","podman","local"]
    And preference order is docker > podman > local given availability

  Scenario: Env override forces local
    Given environment CLEANROOM_BACKEND="local"
    When I request backend "auto"
    Then the resolved backend is "local"
