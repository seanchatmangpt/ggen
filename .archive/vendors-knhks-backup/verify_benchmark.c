#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(__aarch64__)
static inline uint64_t rd_ticks(void) {
    uint64_t c;
    __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(c));
    return c;
}
static inline double ticks_hz(void) {
    uint64_t f;
    __asm__ __volatile__("mrs %0, cntfrq_el0" : "=r"(f));
    return (double)f;
}
#else
static inline uint64_t rd_ticks(void) { return 0; }
static inline double ticks_hz(void) { return 1.0; }
#endif

int main(void) {
    // Measure overhead of rd_ticks itself
    uint64_t t0 = rd_ticks();
    uint64_t t1 = rd_ticks();
    double hz = ticks_hz();
    double overhead_ns = ((double)(t1 - t0) / hz) * 1e9;
    printf("rd_ticks() overhead: %.3f ns\n", overhead_ns);
    
    // Measure empty loop overhead
    const int N = 200000;
    t0 = rd_ticks();
    volatile int sink = 0;
    for (int i = 0; i < N; i++) {
        sink ^= 1;
    }
    t1 = rd_ticks();
    double loop_ns = (((double)(t1 - t0) / hz) * 1e9) / N;
    printf("Empty loop overhead: %.3f ns/iter\n", loop_ns);
    
    return 0;
}
