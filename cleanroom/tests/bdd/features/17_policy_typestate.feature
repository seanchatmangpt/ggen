# features/17_policy_typestate.feature
@policy @compile
Feature: Typestate-enforced policy elevation
  Scenario: Elevation requires explicit permissive type
    Given a test that calls "enable_network()" under Restricted policy
    When I compile the test
    Then compilation fails with a type error mentioning "Permissive"
    And when I change policy to Permissive
    Then compilation succeeds
