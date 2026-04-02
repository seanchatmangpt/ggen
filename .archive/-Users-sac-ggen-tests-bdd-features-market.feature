Feature: Marketplace Operations
  As a developer
  I want to discover and manage reusable gpacks
  So that I can leverage community templates and accelerate development

  Background:
    Given I am in a clean project directory
    And the marketplace registry is available

  Scenario: Search for gpacks by keyword
    When I run "ggen market search rust"
    Then the command should succeed
    And I should see results for rust gpacks
    And I should see "io.ggen.rust" in output

  Scenario: Search with category filter
    When I run "ggen market search cli --category rust"
    Then the command should succeed
    And results should only show rust category gpacks

  Scenario: Search with detailed output
    When I run "ggen market search rust --detailed"
    Then the command should succeed
    And I should see "Name:" in output
    And I should see "Description:" in output
    And I should see "Version:" in output

  Scenario: Search with JSON output
    When I run "ggen market search rust --json"
    Then the command should succeed
    And the output should be valid JSON
    And the JSON should contain a "results" array

  Scenario: Add a gpack from marketplace
    When I run "ggen market add io.ggen.rust.cli-subcommand"
    Then the command should succeed
    And I should see "Successfully added gpack" in output
    And the gpack should be listed in "ggen.lock"
    And the gpack should be cached locally

  Scenario: Add a specific version of a gpack
    When I run "ggen market add io.ggen.rust.cli-subcommand@0.2.0"
    Then the command should succeed
    And the lockfile should show version "0.2.0"

  Scenario: Add gpack that is already installed
    Given I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen market add io.ggen.rust.cli-subcommand"
    Then the command should succeed
    And I should see "already installed" in output

  Scenario: Remove an installed gpack
    Given I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen market remove io.ggen.rust.cli-subcommand"
    Then the command should succeed
    And I should see "Successfully removed" in output
    And the gpack should not be in "ggen.lock"

  Scenario: Remove gpack that is not installed
    When I run "ggen market remove io.ggen.nonexistent.pack"
    Then the command should fail
    And I should see "not installed" in stderr

  Scenario: List all installed gpacks
    Given I have installed the gpack "io.ggen.rust.cli-subcommand"
    And I have installed the gpack "io.ggen.python.web-api"
    When I run "ggen market list"
    Then the command should succeed
    And I should see "io.ggen.rust.cli-subcommand" in output
    And I should see "io.ggen.python.web-api" in output

  Scenario: List with no gpacks installed
    When I run "ggen market list"
    Then the command should succeed
    And I should see "No gpacks installed" in output

  Scenario: List with detailed information
    Given I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen market list --detailed"
    Then the command should succeed
    And I should see version information
    And I should see source URLs

  Scenario: Update all gpacks to latest versions
    Given I have installed the gpack "io.ggen.rust.cli-subcommand@0.1.0"
    And a newer version "0.2.0" is available
    When I run "ggen market update"
    Then the command should succeed
    And I should see "Updated io.ggen.rust.cli-subcommand" in output
    And the lockfile should show version "0.2.0"

  Scenario: Update specific gpack
    Given I have installed the gpack "io.ggen.rust.cli-subcommand@0.1.0"
    When I run "ggen market update io.ggen.rust.cli-subcommand"
    Then the command should succeed
    And the gpack should be updated to latest version

  Scenario: Show popular categories
    When I run "ggen market categories"
    Then the command should succeed
    And I should see popular categories
    And I should see "rust" or "python" or "typescript" in output

  Scenario: Show detailed gpack information
    When I run "ggen market info io.ggen.rust.cli-subcommand"
    Then the command should succeed
    And I should see package metadata
    And I should see description
    And I should see version information

  Scenario: Search with no results
    When I run "ggen market search nonexistentpackage12345"
    Then the command should succeed
    And I should see "No gpacks found" in output

  Scenario: Add gpack that doesn't exist
    When I run "ggen market add io.ggen.nonexistent.pack"
    Then the command should fail
    And I should see "Failed to resolve gpack" in stderr

  Scenario: Update with no gpacks installed
    When I run "ggen market update"
    Then the command should succeed
    And I should see "No gpacks to update" in output

  Scenario: List gpacks shows SHA256 hashes
    Given I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen market list --detailed"
    Then I should see SHA256 hash for each gpack
    And the SHA256 should be 64 hex characters

  Scenario: Add gpack with PQC signature verification
    Given the gpack "io.ggen.rust.cli-subcommand" has a PQC signature
    When I run "ggen market add io.ggen.rust.cli-subcommand"
    Then the command should succeed
    And the lockfile should contain the PQC signature
    And the lockfile should contain the PQC public key
