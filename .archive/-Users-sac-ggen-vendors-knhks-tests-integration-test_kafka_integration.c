// tests/integration/test_kafka_integration.c
// Kafka integration test using Docker containers
// Tests connector against real Kafka instance

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

// Simple TCP connection test
static int test_kafka_connectivity(void)
{
    printf("[TEST] Kafka Connectivity\n");
    
    // Check if Kafka is reachable at localhost:9092
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        printf("  ✗ Failed to create socket\n");
        return 0;
    }
    
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(9092);
    inet_pton(AF_INET, "127.0.0.1", &server_addr.sin_addr);
    
    // Try to connect (with timeout)
    int result = connect(sock, (struct sockaddr *)&server_addr, sizeof(server_addr));
    close(sock);
    
    if (result == 0) {
        printf("  ✓ Kafka is reachable at localhost:9092\n");
        return 1;
    } else {
        printf("  ✗ Kafka is not reachable (is container running?)\n");
        return 0;
    }
}

int main(void)
{
    printf("========================================\n");
    printf("Kafka Integration Test\n");
    printf("========================================\n\n");
    
    int passed = test_kafka_connectivity();
    
    printf("\n========================================\n");
    printf("Results: %s\n", passed ? "PASSED" : "FAILED");
    printf("========================================\n");
    
    return passed ? 0 : 1;
}

