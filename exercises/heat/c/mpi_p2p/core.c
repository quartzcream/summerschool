/* Main solver routines for heat equation solver */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <malloc.h>
#include <mpi.h>

#include "heat.h"

/* Exchange the boundary values */
void exchange(field *temperature, parallel_data *parallel)
{
	MPI_Request request[4];
	MPI_Status status[4];
	int nx = temperature->nx;
	int ny = temperature->ny;
	/* TODO start: implement halo exchange */
	// Send to the up, receive from down

	MPI_Isend(temperature->data[1], ny+2, MPI_DOUBLE, parallel->nup, 0, MPI_COMM_WORLD, request);
	MPI_Irecv(temperature->data[nx+1], ny+2, MPI_DOUBLE, parallel->ndown, 0, MPI_COMM_WORLD, request+1);

	// Send to the down, receive from up
	MPI_Isend(temperature->data[nx], ny+2, MPI_DOUBLE, parallel->ndown, 1, MPI_COMM_WORLD, request+2);
	MPI_Irecv(temperature->data[0], ny+2, MPI_DOUBLE, parallel->nup, 1, MPI_COMM_WORLD, request+3);


	MPI_Waitall(4, request, status);
	/* TODO end */
}


/* Update the temperature values using five-point stencil */
void evolve(field *curr, field *prev, double a, double dt)
{
	int i, j;
	double dx2, dy2;

	/* Determine the temperature field at next time step
	 * As we have fixed boundary conditions, the outermost gridpoints
	 * are not updated. */
	dx2 = prev->dx * prev->dx;
	dy2 = prev->dy * prev->dy;
	for (i = 1; i < curr->nx + 1; i++) {
		for (j = 1; j < curr->ny + 1; j++) {
			curr->data[i][j] = prev->data[i][j] + a * dt *
				((prev->data[i + 1][j] -
					2.0 * prev->data[i][j] +
					prev->data[i - 1][j]) / dx2 +
				 (prev->data[i][j + 1] -
					2.0 * prev->data[i][j] +
					prev->data[i][j - 1]) / dy2);
		}
	}
}


