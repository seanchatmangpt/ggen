// clock.h
// OTEL span ID generation (no timing dependencies)

#ifndef KNHK_CLOCK_H
#define KNHK_CLOCK_H

#include <stdint.h>

uint64_t knhk_generate_span_id(void); // Generate OTEL-compatible span ID

#endif // KNHK_CLOCK_H

