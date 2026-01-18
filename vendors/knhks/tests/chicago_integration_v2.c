// chicago_integration_v2.c
// Main test runner - orchestrates all integration v2 test suites

#include <stdio.h>
#include <stdlib.h>

// External test suite functions
extern int chicago_test_integration_core(void);
extern int chicago_test_integration_systems(void);
extern int chicago_test_integration_advanced(void);

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: Integration v2 (80/20 Critical Path)\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  // Core integration tests
  if (chicago_test_integration_core())
    passed += 2;
  total += 2;
  printf("\n");
  
  // System integration tests
  if (chicago_test_integration_systems())
    passed += 2;
  total += 2;
  printf("\n");
  
  // Advanced integration tests
  if (chicago_test_integration_advanced())
    passed += 3;
  total += 3;
  printf("\n");
  
  printf("========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  if (passed == total) {
    printf("✓ All integration tests passed!\n");
    printf("  Critical path validated: Connectors → ETL → Hot Path → Lockchain → OTEL\n");
  }
  
  return (passed == total) ? 0 : 1;
}
