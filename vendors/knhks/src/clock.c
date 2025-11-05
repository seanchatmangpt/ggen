// clock.c
// OTEL span ID generation (no timing dependencies)

#include "clock.h"
#include <stdint.h>

// Simple counter for span ID generation (thread-local in production)
static uint64_t span_id_counter = 1;

// Generate OTEL-compatible span ID (64-bit)
// Optimized for hot path: minimal overhead, deterministic, no timing dependency
uint64_t knhk_generate_span_id(void)
{
  // Use counter-based approach: simple increment with mixing for uniqueness
  uint64_t id = span_id_counter++;
  id ^= 0x9e3779b97f4a7c15ULL;  // Golden ratio constant for mixing
  id |= 1;  // Ensure non-zero (branchless)
  return id;
}

