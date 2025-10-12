Feature: Attestation and Signing
  As a developer
  I want to sign and verify test artifacts
  So that I can ensure integrity and authenticity

  Background:
    Given I have a clean test environment

  Scenario: Basic artifact signing
    Given I have attestation enabled
    And I have a signing key "signing_key.pem"
    And I have a file "test_artifact.txt" with content:
      """
      Test artifact content
      """
    When I sign the artifact "test_artifact.txt"
    Then the artifact should be signed

  Scenario: Signature verification
    Given I have attestation enabled
    And I have a verification key "verification_key.pem"
    And I have a signed artifact "signed_artifact.txt"
    When I verify the signature of "signed_artifact.txt"
    Then the signature should be valid

  Scenario: Invalid signature detection
    Given I have attestation enabled
    And I have a verification key "verification_key.pem"
    And I have a file "corrupted_artifact.txt" with content:
      """
      Corrupted content
      """
    When I verify the signature of "corrupted_artifact.txt"
    Then the signature should be invalid

  Scenario: Attestation creation
    Given I have attestation enabled
    And I have a signing key "signing_key.pem"
    And I have a file "target_artifact.bin" with content:
      """
      Binary artifact content
      """
    When I create an attestation for "target_artifact.bin"
    Then the attestation should be created
    And the attestation should include metadata
    And the attestation should include artifacts

  Scenario: Attestation verification
    Given I have attestation enabled
    And I have a verification key "verification_key.pem"
    And I have a file "attested_artifact.json" with content:
      """
      {"data": "test"}
      """
    When I create an attestation for "attested_artifact.json"
    And I verify the attestation for "attested_artifact.json"
    Then the attestation should be verified

  Scenario: Invalid attestation detection
    Given I have attestation enabled
    And I have a verification key "verification_key.pem"
    And I have a file "invalid_artifact.txt" with content:
      """
      Invalid content
      """
    When I create an attestation for "invalid_artifact.txt"
    And I verify the attestation for "invalid_artifact.txt"
    Then the attestation should be invalid

  Scenario: Verification report generation
    Given I have attestation enabled
    And I have a signed artifact "artifact1.txt"
    And I have a signed artifact "artifact2.bin"
    And I have a signed artifact "artifact3.json"
    When I generate a verification report
    Then the verification report should be generated
    And the verification should pass

  Scenario: Failed verification report
    Given I have attestation enabled
    And I have a file "failed_artifact.txt" with content:
      """
      Failed verification content
      """
    When I generate a verification report
    Then the verification report should be generated
    And the verification should fail

  Scenario: Trust level assessment
    Given I have attestation enabled
    And I have a signed artifact "trusted_artifact.txt"
    When I generate a verification report
    Then the verification report should be generated
    And the trust level should be "MEDIUM"

  Scenario: Signing key protection
    Given I have attestation enabled
    And I have a signing key "protected_key.pem"
    When I sign the artifact "test_artifact.txt"
    Then the artifact should be signed
    And the signing key should be protected

  Scenario: Attestation policy enforcement
    Given I have attestation enabled
    And I have an attestation policy "attestation_policy.yaml"
    And I have a file "policy_artifact.txt" with content:
      """
      Policy enforced content
      """
    When I create an attestation for "policy_artifact.txt"
    Then the attestation should be created
    And the attestation should include metadata
