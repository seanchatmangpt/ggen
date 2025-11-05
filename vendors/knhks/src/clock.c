// clock.c
// Platform-specific clock/timing utilities

#include "clock.h"
#include <stdlib.h>

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

