/*
* This program computes the steady state of a matrix with update given by the following equation.
* w_new[i, j] = ( w[i+1, j] + w[i−1, j] + w[i, j+1] + w[i, j−1])/4
* for i = 0...100, j = 0...100
* Serial, and parallelised versions
*/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

typedef struct Matrix {
	int nrows;
	int ncolumns;
	float* data;
} matrix;

int getIndex(matrix m, int i, int j);

int main(void){
	//Struct to hold information about matrix
	matrix w;
	w.nrows = 20;
	w.ncolumns = 20;
	//iterators 
	int i, j, k;
	//index of matrix
	int index;
	//break condition
	float tol = 0.0001;
	//allocate storage for matrix
	w.data = malloc(sizeof(float)*w.nrows*w.ncolumns);
	//begin timing routine
	double time = omp_get_wtime(); //Start timer

	/* Initialize matrix */
#pragma omp parallel for shared(w) private(i, j, index) //parallelize outer loop (i.e rows)
	for (i = 0; i < w.nrows; i++)
	{
		for (j = 0; j < w.ncolumns; j++)
		{
			index = getIndex(w, i, j);
			if (j == 0 || j == w.ncolumns -1 ) 		// first and last column set 100
				w.data[index] = 100.0f;
			else if (i == 0) 						// first row set 0
				w.data[index] = 0.0f;
			else if (i == w.nrows-1) 				// last row set 100
				w.data[index] = 100.0f;
			else
				w.data[index] = 75.0f;				// everything else set 75
		}
	}
	//variables to store diffences between successive iterations
	float globaldiff;
	float localdiff;

	float before;
	/* Start Main Update Loop */
	//OMP does not support parallel break clauses
	while(1)
	{
		globaldiff = 0.0f;
		localdiff = 0.0f;
    /* Update Matrix */
#pragma omp parallel shared(w, globaldiff) private(i, j, before, index) firstprivate(localdiff)
{
#pragma omp for nowait //use can use a nowait clause as it doesn't matter which thread updates globaldiff 
			for (i = 1; i < w.nrows; i++) //update only interior points (i=1, j=1)
			{
				for (j = 1; j < w.nrows; j++)
				{
					index = getIndex(w, i, j);
					before = w.data[index];
					w.data[index] = (w.data[getIndex(w, i+1, j)] + w.data[getIndex(w, i-1, j)] + w.data[getIndex(w, i, j+1)] + w.data[getIndex(w, i, j-1)])/4.0f;
					if (before - w.data[index] > localdiff){ 
						//here we update the thread-wise difference
						// We only care about the largest difference between two iterations
						// Anytime the difference between two values is > localdiff we update localdiff.
						localdiff = before - w.data[index]; 
					}
				}
			}
			if (localdiff > globaldiff) 
				//Here we update the matrix-wise difference
				//If a thread's localdiff > matrix-wise globaldiff we update
				//This means we always store the largest difference
#pragma omp critical
				globaldiff = localdiff;
}
 		//We break only when the largest difference is less than the tolerance
		if (globaldiff < tol)
			break;
	}	
	//print time taken to execute code
	time = omp_get_wtime() - time;
	printf("Time taken: %f\n", time);
	free(w.data);
}

int getIndex(matrix m, int i, int j){
	return m.ncolumns*i + j;
}
