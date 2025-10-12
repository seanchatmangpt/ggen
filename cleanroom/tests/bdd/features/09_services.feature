# features/09_services.feature
@services
Feature: Side services with health gates
  Background:
    Given a fixture project "cli_pg"

  Scenario: Postgres service with ready gate
    Given services:
      | name     | image              | port | health               |
      | postgres | postgres:16-alpine | 5432 | pg_isready -q        |
    When I run ["migrate"] using "docker"
    Then exit code is 0
    And service "postgres" logs contain "database system is ready"
