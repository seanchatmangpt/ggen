# features/25_workdir_isolation.feature
@isolation
Feature: Ephemeral workspace and read-only rootfs
  Background:
    Given a fixture project "cli_fs"

  Scenario: Writes land in tmpfs workdir
    When I run ["write","/tmp/testfile"] using "docker"
    Then file "/tmp/testfile" exists inside container
    And no writes occurred on host source directory
