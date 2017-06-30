#include <stdio.h>
#include <omp.h>

int main(int argc, char *argv[])
{
    printf("Hello world! %d Threads\n", omp_get_num_threads());
#pragma omp parallel
    {
			printf("Hello world! %d Threads\n", omp_get_num_threads());
        printf("%d\n", omp_get_thread_num());
    }
    printf("Hello world! %d Threads\n", omp_get_num_threads());

    return 0;
}
