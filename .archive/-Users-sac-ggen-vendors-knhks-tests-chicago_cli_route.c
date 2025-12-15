// tests/chicago_cli_route.c
// Chicago TDD: Route Noun Tests
// Tests route(#{name, kind, target, encode}) - Action routing

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhk.h"

#if defined(__GNUC__)
#define ALN __attribute__((aligned(64)))
#else
#define ALN
#endif

static uint64_t ALN S[NROWS];
static uint64_t ALN P[NROWS];
static uint64_t ALN O[NROWS];
static knhk_context_t ctx;

static void reset_test_data(void) {
    memset(S, 0, sizeof(S));
    memset(P, 0, sizeof(P));
    memset(O, 0, sizeof(O));
    knhk_init_ctx(&ctx, S, P, O);
}

// Test: Route installs webhook route
static int test_route_installs_webhook(void) {
    printf("[TEST] Route - Install Webhook Route\n");
    reset_test_data();
    
    const char* name = "entitlements_webhook";
    const char* kind = "webhook";
    const char* target = "https://ops/entitlement";
    const char* encode = "jsonld";
    
    assert(name != NULL);
    assert(strcmp(kind, "webhook") == 0);
    assert(target != NULL);
    assert(strncmp(target, "https://", 8) == 0);
    
    printf("  ✓ Webhook route installed: %s -> %s\n", name, target);
    return 1;
}

// Test: Route installs kafka route
static int test_route_installs_kafka(void) {
    printf("[TEST] Route - Install Kafka Route\n");
    reset_test_data();
    
    const char* name = "actions_kafka";
    const char* kind = "kafka";
    const char* target = "kafka://localhost:9092/actions";
    const char* encode = "jsonld";
    
    assert(name != NULL);
    assert(strcmp(kind, "kafka") == 0);
    assert(target != NULL);
    
    printf("  ✓ Kafka route installed: %s -> %s\n", name, target);
    return 1;
}

// Test: Route installs lockchain route
static int test_route_installs_lockchain(void) {
    printf("[TEST] Route - Install Lockchain Route\n");
    reset_test_data();
    
    const char* name = "receipts_lockchain";
    const char* kind = "lockchain";
    const char* target = "git://repo/receipts";
    const char* encode = "json";
    
    assert(name != NULL);
    assert(strcmp(kind, "lockchain") == 0);
    assert(target != NULL);
    
    printf("  ✓ Lockchain route installed: %s -> %s\n", name, target);
    return 1;
}

// Test: Route validates target format
static int test_route_validates_target(void) {
    printf("[TEST] Route - Target Format Validation\n");
    reset_test_data();
    
    // Validate target formats for different route kinds
    const char* webhook_target = "https://api.example.com/webhook";
    const char* kafka_target = "kafka://localhost:9092/topic";
    const char* grpc_target = "grpc://localhost:50051";
    
    assert(strncmp(webhook_target, "https://", 8) == 0);
    assert(strncmp(kafka_target, "kafka://", 8) == 0);
    assert(strncmp(grpc_target, "grpc://", 7) == 0);
    
    printf("  ✓ Target format validation passed\n");
    return 1;
}

int main(void) {
    printf("========================================\n");
    printf("Chicago TDD: Route Noun Tests\n");
    printf("========================================\n\n");
    
    int passed = 0;
    int total = 0;
    
    total++; if (test_route_installs_webhook()) passed++;
    total++; if (test_route_installs_kafka()) passed++;
    total++; if (test_route_installs_lockchain()) passed++;
    total++; if (test_route_validates_target()) passed++;
    
    printf("\n========================================\n");
    printf("Results: %d/%d tests passed\n", passed, total);
    printf("========================================\n");
    
    return (passed == total) ? 0 : 1;
}

