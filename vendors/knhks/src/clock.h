// clock.h
// Platform-specific clock/timing utilities

#ifndef KNHKS_CLOCK_H
#define KNHKS_CLOCK_H

#include <stdint.h>

uint64_t knhks_rd_ticks(void);
double knhks_ticks_hz(void);

#endif // KNHKS_CLOCK_H

