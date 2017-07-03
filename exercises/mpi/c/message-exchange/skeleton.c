#include<stdio.h>
#include<stdlib.h>
#include<mpi.h>


int main(int argc, char *argv[])
{
	int i, myid, ntasks;
	int size = 10;
	int *message;
	int *receiveBuffer;
	MPI_Status status;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &ntasks);
	MPI_Comm_rank(MPI_COMM_WORLD, &myid);

	/* Allocate message */
	message = malloc(sizeof(int) * size);
	receiveBuffer = malloc(sizeof(int) * size);
	/* Initialize message */
	for (i = 0; i < size; i++) {
		message[i] = myid;
	}
	double t1, t2; 
	t1 = MPI_Wtime(); 
	if (myid == 0) {
		MPI_Send(message, size, MPI_INT, ntasks-1, 0, MPI_COMM_WORLD);
		MPI_Recv(receiveBuffer, size, MPI_INT, ntasks-1, 0, MPI_COMM_WORLD, &status);

	} else if (myid == ntasks - 1) {
		MPI_Recv(receiveBuffer, size, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
		MPI_Send(message, size, MPI_INT, 0, 0, MPI_COMM_WORLD);
	}
	t2 = MPI_Wtime(); 
	printf( "Elapsed time for rank=%d is %f\n", myid, t2 - t1 ); 
	if (myid == 0) {
		printf("Rank %i received %i\n", myid, receiveBuffer[0]);
	} else if (myid == ntasks-1) {
		printf("Rank %i received %i\n", myid, receiveBuffer[0]);
	}


	free(message);
	free(receiveBuffer);
	MPI_Finalize();
	return 0;
}
