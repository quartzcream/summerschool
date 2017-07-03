#include <stdio.h>

#define NX 102400

int main(void)
{
    long vecA[NX];
    long sum, psum, sumex;

    /* Initialization of the vectors */
    for (int i = 0; i < NX; i++) {
        vecA[i] = (long) i+1;
    }

    sum = 0.0;
#pragma omp parallel for reduction(+:sum)
    for (int i = 0; i < NX; i++) {
        sum += vecA[i];
    }
    printf("Sum: %ld\n",sum);

    return 0;
}
