# features/27_json_report.feature
@report
Feature: Machine-readable run reports
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Report contains normalized schema
    When I run ["--help"] using "auto" with report enabled
    Then "artifacts/<run_id>/report.json" exists
    And $.backend in report is one of ["local","docker","podman"]
    And $.steps[0].name == "run --help"
    And $.steps[0].exit_code == 0
