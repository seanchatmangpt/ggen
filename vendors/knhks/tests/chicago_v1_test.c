// chicago_v1_test.c
// Main test runner - orchestrates all v1.0 test suites

#include <stdio.h>
#include <stdlib.h>

// External test suite functions
extern int chicago_test_v1_receipts(void);
extern int chicago_test_v1_operations(void);
extern int chicago_test_v1_validation(void);

int main(void)
{
  printf("========================================\n");
  printf("KNHKS v1.0 Chicago TDD Test Suite\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  // Validation tests (constants, guards, timing)
  if (chicago_test_v1_validation())
    passed += 3;
  total += 3;
  printf("\n");
  
  // Receipt tests
  if (chicago_test_v1_receipts())
    passed += 2;
  total += 2;
  printf("\n");
  
  // Operations tests
  if (chicago_test_v1_operations())
    passed += 3;
  total += 3;
  printf("\n");
  
  printf("========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}
