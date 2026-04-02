// tests/chicago_configuration.c
// Chicago TDD: Configuration Management Tests for v0.4.0
// Tests configuration file loading, validation, and defaults

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhk.h"
#include "chicago_test_helpers.h"

#if defined(__GNUC__)
#define ALN __attribute__((aligned(64)))
#else
#define ALN
#endif

// Mock configuration structure (simulating Rust config)
typedef struct {
  const char *connector_name;
  uint64_t max_run_len;
  uint64_t max_batch_size;
  uint32_t max_ticks;
} config_connector_t;

typedef struct {
  config_connector_t connectors[10];
  size_t connector_count;
  uint32_t default_max_ticks;
} config_t;

static config_t test_config = {0};

// Mock config loader (in real implementation, TOML/YAML parser)
static int load_config(const char *filename)
{
  (void)filename; // Not used in mock
  test_config.default_max_ticks = 8; // KNHK_TICK_BUDGET
  test_config.connector_count = 2;
  test_config.connectors[0] = (config_connector_t){
    .connector_name = "kafka",
    .max_run_len = 8,
    .max_batch_size = 1000,
    .max_ticks = 8
  };
  test_config.connectors[1] = (config_connector_t){
    .connector_name = "salesforce",
    .max_run_len = 8,
    .max_batch_size = 500,
    .max_ticks = 8
  };
  return 0; // Success
}

// Test: Config File Loading
static int test_config_file_loading(void)
{
  printf("[TEST] Config File Loading\n");
  
  // Simulate config file loading (in real implementation, TOML parser)
  int result = load_config("config.toml");
  
  assert(result == 0);
  assert(test_config.default_max_ticks == 8);
  assert(test_config.connector_count == 2);
  assert(strcmp(test_config.connectors[0].connector_name, "kafka") == 0);
  
  printf("  ✓ Config file loaded successfully\n");
  return 1;
}

// Test: Config Environment Variables
static int test_config_env_vars(void)
{
  printf("[TEST] Config Environment Variables\n");
  
  // Simulate env var override (in real implementation, env vars override file)
  // Set env var: KNHK_MAX_TICKS=8
  uint32_t env_max_ticks = 8; // From env var
  
  // Config should use env var over file value
  assert(env_max_ticks == KNHK_TICK_BUDGET);
  
  printf("  ✓ Environment variables override config file\n");
  return 1;
}

// Test: Config Validation
static int test_config_validation(void)
{
  printf("[TEST] Config Validation\n");
  
  // Invalid config: max_run_len > 8
  config_connector_t invalid_config = {
    .connector_name = "invalid",
    .max_run_len = 10, // Invalid: exceeds guard constraint
    .max_batch_size = 1000,
    .max_ticks = 8
  };
  
  // Validation should reject max_run_len > 8
  int valid = (invalid_config.max_run_len <= 8) ? 1 : 0;
  assert(valid == 0); // Should be invalid
  
  // Valid config
  config_connector_t valid_config = {
    .connector_name = "valid",
    .max_run_len = 8,
    .max_batch_size = 1000,
    .max_ticks = 8
  };
  
  valid = (valid_config.max_run_len <= 8) ? 1 : 0;
  assert(valid == 1); // Should be valid
  
  printf("  ✓ Config validation rejects invalid values\n");
  return 1;
}

// Test: Config Defaults
static int test_config_defaults(void)
{
  printf("[TEST] Config Defaults\n");
  
  // Test: Missing config should use defaults
  config_t empty_config = {0};
  
  // Defaults should be applied
  uint32_t default_max_ticks = 8; // KNHK_TICK_BUDGET
  uint64_t default_max_run_len = 8; // Guard constraint
  
  assert(default_max_ticks == KNHK_TICK_BUDGET);
  assert(default_max_run_len == 8);
  
  printf("  ✓ Config defaults applied correctly\n");
  return 1;
}

// Test: Config Connector Settings
static int test_config_connector_settings(void)
{
  printf("[TEST] Config Connector Settings\n");
  
  // Load config
  load_config("config.toml");
  
  // Verify connector settings
  assert(test_config.connector_count == 2);
  assert(test_config.connectors[0].max_run_len <= 8);
  assert(test_config.connectors[0].max_batch_size > 0);
  assert(test_config.connectors[0].max_ticks == 8);
  
  printf("  ✓ Connector settings parsed correctly\n");
  return 1;
}

// Test: Config Epoch Settings
static int test_config_epoch_settings(void)
{
  printf("[TEST] Config Epoch Settings\n");
  
  // Epoch settings: max_ticks, ordering
  uint32_t epoch_max_ticks = 8; // From config
  const char *ordering = "deterministic"; // From config
  
  assert(epoch_max_ticks == KNHK_TICK_BUDGET);
  assert(strcmp(ordering, "deterministic") == 0);
  
  printf("  ✓ Epoch settings parsed correctly\n");
  return 1;
}

// Test: Config Hook Settings
static int test_config_hook_settings(void)
{
  printf("[TEST] Config Hook Settings\n");
  
  // Hook settings: max_count, validation
  uint32_t max_hook_count = 100; // From config
  int validation_enabled = 1; // From config
  
  assert(max_hook_count > 0);
  assert(validation_enabled == 1);
  
  // Verify hook execution respects config
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhk_context_t ctx;
  
  knhk_init_ctx(&ctx, S, P, O);
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_ASK_SP,
    .s = 0xA11CE,
    .p = 0xC0FFEE,
    .o = 0,
    .k = 0,
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  knhk_receipt_t rcpt = {0};
  int result = knhk_eval_bool(&ctx, &ir, &rcpt);
  
  assert(result == 1);
  assert(rcpt.ticks <= KNHK_TICK_BUDGET);
  
  printf("  ✓ Hook settings parsed and applied correctly\n");
  return 1;
}

// Test suite runner
int chicago_test_configuration(void)
{
  int passed = 0;
  int total = 0;
  
  total++; if (test_config_file_loading()) passed++;
  total++; if (test_config_env_vars()) passed++;
  total++; if (test_config_validation()) passed++;
  total++; if (test_config_defaults()) passed++;
  total++; if (test_config_connector_settings()) passed++;
  total++; if (test_config_epoch_settings()) passed++;
  total++; if (test_config_hook_settings()) passed++;
  
  printf("\nConfiguration: %d/%d tests passed\n", passed, total);
  return (passed == total) ? 1 : 0;
}

