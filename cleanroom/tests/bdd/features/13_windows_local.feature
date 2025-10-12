# features/13_windows_local.feature
@windows @local
Feature: Windows support via local backend
  Background:
    Given the OS is Windows

  Scenario: Local backend works without container engine
    Given docker is unavailable
    When I run ["--help"] using "local"
    Then exit code is 0
