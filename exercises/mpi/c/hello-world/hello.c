#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	int rank, cnt;
	MPI_Init(&argc, &argv);

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	printf("Hello from %d!\n", rank);
	if(!rank){
		MPI_Comm_size(MPI_COMM_WORLD, &cnt);
		printf("%d MPI threads\n", cnt);
	}
	MPI_Finalize();

	return 0;
}
