// clock.h
// Platform-specific clock/timing utilities and OTEL span ID generation

#ifndef KNHKS_CLOCK_H
#define KNHKS_CLOCK_H

#include <stdint.h>

uint64_t knhks_rd_ticks(void);
double knhks_ticks_hz(void);
uint64_t knhks_generate_span_id(void); // Generate OTEL-compatible span ID
uint64_t knhks_generate_span_id_from_ticks(uint64_t ticks); // Generate from existing ticks (faster)

#endif // KNHKS_CLOCK_H

