// tests/chicago_v04_test.c
// Main test runner for KNHK v0.4.0 Chicago TDD test suite
// Orchestrates all v0.4.0 tests

#include <stdio.h>
#include <stdlib.h>

// External test suite functions
extern int chicago_test_e2e_integration(void);
extern int chicago_test_network_integration(void);
extern int chicago_test_cli_integration(void);
extern int chicago_test_configuration(void);
extern int chicago_test_lockchain_integration(void);
extern int chicago_test_performance_v04(void);

int main(void)
{
  printf("========================================\n");
  printf("KNHK v0.4.0 Chicago TDD Test Suite\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  // E2E Integration Tests
  printf("--- E2E Integration Tests ---\n");
  if (chicago_test_e2e_integration()) {
    passed += 6;
  }
  total += 6;
  printf("\n");
  
  // Network Integration Tests
  printf("--- Network Integration Tests ---\n");
  if (chicago_test_network_integration()) {
    passed += 9;
  }
  total += 9;
  printf("\n");
  
  // CLI Integration Tests
  printf("--- CLI Integration Tests ---\n");
  if (chicago_test_cli_integration()) {
    passed += 8;
  }
  total += 8;
  printf("\n");
  
  // Configuration Tests
  printf("--- Configuration Tests ---\n");
  if (chicago_test_configuration()) {
    passed += 7;
  }
  total += 7;
  printf("\n");
  
  // Lockchain Integration Tests
  printf("--- Lockchain Integration Tests ---\n");
  if (chicago_test_lockchain_integration()) {
    passed += 7;
  }
  total += 7;
  printf("\n");
  
  // Performance Tests
  printf("--- Performance Tests v0.4.0 ---\n");
  if (chicago_test_performance_v04()) {
    passed += 6;
  }
  total += 6;
  printf("\n");
  
  printf("========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

