#include "kupyna.h"
#include "sha256.h"
#include "stdio.h"
#include <time.h>


int main() {
    clock_t start, end;
    double cpu_time_used;

    FILE *test_data  = fopen("test_data.txt", "r");
    fseek(test_data, 0, SEEK_END);
    long filelen = ftell(test_data);
    rewind(test_data);
    char* buffer = (char *)malloc(filelen * sizeof(char));
    fread(buffer, filelen, 1, test_data);

    SHA256_CTX ctx2;
    BYTE* text1 = buffer;
    BYTE buf[32];
    start = clock();
	sha256_init(&ctx2);
	sha256_update(&ctx2, text1, strlen(text1));
	sha256_final(&ctx2, buf);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

    printf("SHA256 time: %f sec.\n", cpu_time_used);


    kupyna_t ctx;
    uint8_t* test = buffer;
    uint8_t* hash_code[32];
    start = clock();
    KupynaInit(256, &ctx);
    KupynaHash(&ctx, test, 512, hash_code);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

    printf("Kupyna time: %f sec.\n", cpu_time_used);

    fclose(test_data);
}