/*
	This program compute the determinant of a 5x5 matrix by 
	parallelizing the cofactor expansion along the first row.

	This program requires 5 MPI processes to run successfully.
	Each process allocates the full matrix in memory but only 
		computes the determinant of a submatrix.
	The subdeterminants are combined on rank 0 using reduction.
*/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/*
	Auxiliary function to allocate 2d matrix of double.
	Adapted from Week 10 practical alloc2d.h
*/
//allocates 2d matrix
double **alloc2dDouble( size_t n1, size_t n2 ) {
/*
 * Function:  alloc2dDouble 
 * --------------------
 * allocate memory for a 2d matrix of doubles
 *
 *  n1: number of rows
 *  n2: number of columns
 *
 *  returns: allocated matrix
 */
    if ( n1*n2 == 0 ) {
        n1 = n2 = 1;
    }
    size_t i;
    //Array of pointers of size n1 (int* dummy[n1])
    double **dummy = (double **) calloc( n1, sizeof( double * ) );
    if ( dummy == NULL ) {
        printf( " Could not allocate memory 1 2d float\n" );
        MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    //Dynamically allocate memory of size n1*n2 and let dummy* point to it
    *dummy = (double *) calloc( n1*n2, sizeof( double ) );
    //Position allocated memory across n1 pointers
    for ( i = 1; i < n1; i++ ) {
        dummy[i] = (*dummy + n2*i);
    }
    return dummy;
}

double determinant(double*** matrix, int size, int* rows, int* columns)
{
/*
 * Function:  alloc2dDouble 
 * --------------------
 * computes determinant of a matrix by recursive cofactor expansion along top row.
 *
 * matrix: pointer to matrix 
 * size: matrix size (n)
 * rows: vector of row indices that represent submatrix
 * columns: vector of column indices that represent submatrix
 *
 *  returns: determinant
 */
	//base case
	if (size == 1)
		return (*matrix)[ rows[0] ][ columns[0] ];
	//Compute 2x2 matrix instead of descending down to base case
	if (size == 2){
		return (*matrix)[ rows[0] ][ columns[0] ] * (*matrix)[ rows[1] ][ columns[1] ] - (*matrix)[ rows[1] ][ columns[0] ] * (*matrix)[ rows[0] ][ columns[1] ];
	}
	/*
		If size > 2 we recursively compute determinant
		Expansion always along first row
	*/
	int i,j,k;            // iterators
	int sign = 1;         // sign of cofactor in expansion
	double det = 0.0f;    // stores resulting determinant
	double subdet = 0.0f; // stores result of subdeterminant
	int* new_rows;        // array of row indices that represent submatrix
	int* new_columns;     // array of column indices that represent submatrix
	//alloc memory for rows/columns indices
	new_rows = malloc(sizeof(int)* (size-1));
	new_columns = malloc(sizeof(int)* (size-1));
	//initialize row indices
	for (k=1; k < size; ++k)
		new_rows[k-1] = rows[k];
	//cofactor expand along row 
	for (i = 0; i < size; ++i)
	{
		//initialize column indices
		for (k=0; k < i; ++k) 
			new_columns[k] = columns[k];
		for (k=k+1; k < size; ++k) 
			new_columns[k-1] = columns[k];
		//recursion: compute submatrix determinant
		subdet = determinant(matrix, size-1, new_rows, new_columns);
		//sum cofactor expansion of submatrix
		det += sign * (*matrix)[rows[0]][columns[i]] * subdet;
		//sign alternates between 1 and -1
		sign = -sign;
	}
	//free row/column indices
	free(new_rows);
	free(new_columns);
	return det;
}

int main(int argc, char** argv) {
	int rank, nProcs, ierror; // define MPI related variables
	int i, j;									// iterators
	int ROW = 5; 							// number of rows
	int COLUMN = 5; 					// number of columns
	ierror = MPI_Init(&argc, &argv); // initialize MPI
	ierror = MPI_Comm_size(MPI_COMM_WORLD, &nProcs); // get number of processes
	ierror = MPI_Comm_rank(MPI_COMM_WORLD, &rank);	 // get process rank
	
	if (ierror != MPI_SUCCESS)
	{
		printf("An error occured when initializing MPI. Error code: %d\n",ierror);
		MPI_Abort(MPI_COMM_WORLD, ierror);
		return EXIT_FAILURE;
	}
	if (nProcs != 5 )
	{
		printf("Error. This program must be run with 5 MPI nodes!\n");
		MPI_Abort(MPI_COMM_WORLD, 101);
		return EXIT_FAILURE;
	}

	double** matrix;  // 2d array to hold matrix
	int* rows;        // array to hold row indices for submatrix
	int* columns;			// array to hold column indices for submatrix

	double local_det;  // current processes determinant 
	double global_det; // master rank's determinant

	matrix = alloc2dDouble(ROW, COLUMN); // allocate memory for matrix
	for (i = 0; i < ROW; ++i)						 // initialize matrix
	{
		for (j = 0; j < COLUMN; ++j)
		{
			if (i == j)
				matrix[i][j] = 1.0f/(double)(1+i+j);
			else
				matrix[i][j] = -1.0f/(double)(1+i+j);
		}
	}

	//compute row/column indices to specify which submatrix to find determinant for
	rows = malloc(sizeof(int)*4);    // allocate memory for row/column indices
	columns = malloc(sizeof(int)*4);
	for (i = 1; i < 5; ++i)					 // compute row indices 
		rows[i-1] = i;
	for (i = 0, j=0; i < 5; ++i) {	 // compute column indices
		if (rank == i)
			continue;
		columns[j] = i;
		j++;
	}
	
	//compute determinant
	local_det = determinant(&matrix, 4, rows, columns);
	local_det = (rank % 2 == 0 ? 1 : -1)*matrix[0][rank]*local_det;
	printf("Hello from rank %d my determinant is %f\n", rank, local_det);
	//use reduce to sum all subdeterminants on rank 0
	ierror = MPI_Reduce(&local_det,&global_det, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);	
	if (rank == 0)
		printf("Hello from master rank %d. My determinant is %f.\n",rank,global_det);
	// free resources
	free(matrix[0]);
	free(matrix);
	free(rows);
	free(columns);
	// finalize MPI
	MPI_Finalize();

	return 0;
}
