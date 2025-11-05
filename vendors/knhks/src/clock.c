// clock.c
// Platform-specific clock/timing utilities and OTEL span ID generation

#include "clock.h"
#include <stdlib.h>
#include <stdint.h>

#if defined(__aarch64__)
uint64_t knhks_rd_ticks(void)
{
  uint64_t c;
  __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(c));
  return c;
}

double knhks_ticks_hz(void)
{
  uint64_t f;
  __asm__ __volatile__("mrs %0, cntfrq_el0" : "=r"(f));
  return (double)f;
}
#elif defined(__x86_64__)
uint64_t knhks_rd_ticks(void)
{
  unsigned hi, lo;
  __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t)hi << 32) | lo;
}

double knhks_ticks_hz(void)
{
  // x86: no easy invariant freq; ask user to pass CPU_GHZ env or assume 4.0 GHz for ballpark.
  const char *e = getenv("CPU_GHZ");
  return e ? atof(e) * 1e9 : 4.0e9;
}
#else
uint64_t knhks_rd_ticks(void)
{
  return 0;
}

double knhks_ticks_hz(void)
{
  return 1.0;
}
#endif

// Generate OTEL-compatible span ID (64-bit)
// Ultra-optimized for hot path: minimal operations, branchless
// Uses provided ticks value to avoid extra clock reads
uint64_t knhks_generate_span_id_from_ticks(uint64_t ticks)
{
  // Minimal overhead: just XOR with constant and ensure non-zero
  // This is the fastest possible while still providing uniqueness
  uint64_t id = ticks;
  id ^= 0x9e3779b97f4a7c15ULL;  // Golden ratio constant for mixing
  id |= 1;  // Ensure non-zero (branchless)
  return id;
}

// Generate OTEL-compatible span ID (64-bit)
// Optimized for hot path: minimal overhead, deterministic
// Uses ticks with simple mixing for uniqueness
uint64_t knhks_generate_span_id(void)
{
  // Fast path: use ticks directly with simple mixing
  uint64_t ticks = knhks_rd_ticks();
  return knhks_generate_span_id_from_ticks(ticks);
}

