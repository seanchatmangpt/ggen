#include <stdint.h>
#include <stdio.h>
#include "tcps.h"

int main(void) {
    if (tcps_version_v1() != 0x1A0713u) {
        fputs("version mismatch\n", stderr);
        return 1;
    }
    const tcps_request_v1 request = {
        .authority_mask = 1,
        .ready_mask = 3,
        .maximum_time = 999,
        .deterministic_required = 1,
        .receipt_required = 1,
        .reserved = {0, 0},
        .timestamp = 1000,
    };
    const tcps_outcome_v1 outcome = tcps_select_v1(request);
    if (outcome.kind != 1 || outcome.tool != 1 || outcome.eligible_mask != 3 || outcome.ready_mask != 3) {
        fprintf(stderr, "unexpected outcome kind=%u tool=%u eligible=%llu ready=%llu\n",
            (unsigned)outcome.kind,
            (unsigned)outcome.tool,
            (unsigned long long)outcome.eligible_mask,
            (unsigned long long)outcome.ready_mask);
        return 2;
    }
    puts("c-abi-smoke: accepted");
    return 0;
}

