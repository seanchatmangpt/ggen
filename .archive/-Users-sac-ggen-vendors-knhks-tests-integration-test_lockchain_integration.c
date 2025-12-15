// tests/integration/test_lockchain_integration.c
// Lockchain integration test using PostgreSQL container

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static int test_postgres_connectivity(void)
{
    printf("[TEST] PostgreSQL Connectivity\n");
    
    // Check if PostgreSQL is reachable at localhost:5432
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        printf("  ✗ Failed to create socket\n");
        return 0;
    }
    
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(5432);
    inet_pton(AF_INET, "127.0.0.1", &server_addr.sin_addr);
    
    int result = connect(sock, (struct sockaddr *)&server_addr, sizeof(server_addr));
    close(sock);
    
    if (result == 0) {
        printf("  ✓ PostgreSQL is reachable at localhost:5432\n");
        return 1;
    } else {
        printf("  ✗ PostgreSQL is not reachable (is container running?)\n");
        return 0;
    }
}

int main(void)
{
    printf("========================================\n");
    printf("Lockchain Integration Test\n");
    printf("========================================\n\n");
    
    int passed = test_postgres_connectivity();
    
    printf("\n========================================\n");
    printf("Results: %s\n", passed ? "PASSED" : "FAILED");
    printf("========================================\n");
    
    return passed ? 0 : 1;
}

