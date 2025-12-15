// tests/integration/test_etl_integration.c
// ETL pipeline integration test

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static int test_all_services_available(void)
{
    printf("[TEST] ETL Pipeline Service Availability\n");
    
    int all_available = 1;
    
    // Check Kafka
    int kafka_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (kafka_sock >= 0) {
        struct sockaddr_in addr;
        addr.sin_family = AF_INET;
        addr.sin_port = htons(9092);
        inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
        if (connect(kafka_sock, (struct sockaddr *)&addr, sizeof(addr)) == 0) {
            printf("  ✓ Kafka available\n");
        } else {
            printf("  ✗ Kafka not available\n");
            all_available = 0;
        }
        close(kafka_sock);
    }
    
    // Check PostgreSQL
    int pg_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (pg_sock >= 0) {
        struct sockaddr_in addr;
        addr.sin_family = AF_INET;
        addr.sin_port = htons(5432);
        inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
        if (connect(pg_sock, (struct sockaddr *)&addr, sizeof(addr)) == 0) {
            printf("  ✓ PostgreSQL available\n");
        } else {
            printf("  ✗ PostgreSQL not available\n");
            all_available = 0;
        }
        close(pg_sock);
    }
    
    // Check OTEL Collector
    int otel_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (otel_sock >= 0) {
        struct sockaddr_in addr;
        addr.sin_family = AF_INET;
        addr.sin_port = htons(4317);
        inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
        if (connect(otel_sock, (struct sockaddr *)&addr, sizeof(addr)) == 0) {
            printf("  ✓ OTEL Collector available\n");
        } else {
            printf("  ✗ OTEL Collector not available\n");
            all_available = 0;
        }
        close(otel_sock);
    }
    
    return all_available;
}

int main(void)
{
    printf("========================================\n");
    printf("ETL Pipeline Integration Test\n");
    printf("========================================\n\n");
    
    int passed = test_all_services_available();
    
    printf("\n========================================\n");
    printf("Results: %s\n", passed ? "PASSED" : "FAILED");
    printf("========================================\n");
    
    return passed ? 0 : 1;
}

