// chicago_enterprise_use_cases.c
// Main test runner - orchestrates all test suites

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "chicago_test_helpers.h"

// External test suite functions
extern int chicago_test_basic_operations(void);
extern int chicago_test_cardinality(void);
extern int chicago_test_object_operations(void);
extern int chicago_test_advanced(void);

int main(void)
{
  printf("========================================\n");
  printf("KNKHS Enterprise Use Cases Test Suite\n");
  printf("Chicago TDD - 8-Tick Performance Goal\n");
  printf("========================================\n\n");

  int passed = 0;
  int total = 19;

  // Basic operations (tests 1, 2, 5)
  if (chicago_test_basic_operations())
    passed += 3;

  // Cardinality tests (tests 3, 6, 7, 9)
  if (chicago_test_cardinality())
    passed += 4;

  // Object operations (tests 8, 10, 11, 12)
  if (chicago_test_object_operations())
    passed += 4;

  // Advanced tests (tests 4, 13-19)
  if (chicago_test_advanced())
    passed += 8;

  // Summary
  printf("=========================\n");
  printf("All tests passed: %d/%d\n", passed, total);
  if (passed == total)
  {
    printf("Performance goal achieved: 100%% of queries ≤8 ticks\n");
    return 0;
  }
  else
  {
    printf("Some tests failed\n");
    return 1;
  }
}
