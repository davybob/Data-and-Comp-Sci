#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>
#include "ran2.c"

/*
	This function computes PI using Monte Carlo Methods by an OpenMP parallel process 
*/
double calculatePi(int iters, long seed){
/*
 * Function:  calculatePi 
 * --------------------
 *
 * computes an approximation of pi by generating points in the range [0,1] and counting the number of points within a distance d of the origin.
 * Thus, let n_d be the number of points a distance d = 1 from the origin, and n_t be the total number of points generated. A value of pi can be 
 * compute by 
 *    pi = 4 x (n_d/n_t)
 *
 *  iters: Number of points to generate. Larger values yield more accurate values of Pi at the expense of computational cost.
 *  seed: A random number to instantiate the rng.
 *  returns: the approximate value of pi obtained by 4 times the ratio
 *           of the number of points generated
 *           to the points that lie within the circle (< distance d from origin).
 */
  
	int nd = 0;			// number of points in quadrant 
	double x, y, d; // variables to store x,y coords and distance

	int tid;
	int i;

	//parallel region
	//seed is private for each threads
	//We apply an atomic clause to update nd
	/*
		If we change the number of threads in the parallel region but keep the number of 
     iterations the same we will get a different result for pi.
		Each thread has its own copy of ran2 and its own unique seed. 
    Each thread generates a different set of random numbers.
		By changing the number of threads we change which random numbers are generated. 
		For example, say we have 2 threads t1 and t2 and they use the unique seeds s1 and s2 respectively. 
    Suppose also we loop for 100 iterations and use a static worksharing construct. Each thread will do 50 
      iterations each and hence will generate 50 random numbers from their random number generators. If we increase 
      the number of threads to 4, say t1, t2, t3, t4, with unique seeds s1, s2, s3, and s4, then each thread
      will generate 25 random numbers from their random number generators. In both cases we generate 100 random numbers.
      With 2 threads we generate 50 from t1 and 50 from t2. With 4 threads we generate 25 from t1, 25 from t2, 25 from t3, and 25 from t4. 
      This means that 50 numbers are the same (25 from t1, 25 from t2) and 50 numbers are different (25 from t3, 25 from t4). 
      Hence the two sets, having the same cardinality, will have different numbers.
	*/
#pragma omp parallel shared(iters) private(x, y, d, i, tid) firstprivate(s) //firstprivate sets seed s for all threads
	{
    // Here we change the random number generator seed 
    // Each thread has it's own unique rn generator
    // That way threads don't generate the same random numbers.
		tid = omp_get_thread_num(); 
		s += tid; // set seperate seed for each thread
		s = -s; 	// seed must be negative for a different rn state, I don't know why this has to be but it must be.
#pragma omp for schedule(dynamic)
		for (i = 0; i < iters; i++)
		{
			x = ran2(&s);		 // generate x coord in [0,1]
			y = ran2(&s);		 // generate y coord in [0,1]
			d = sqrt(x*x + y*y); // compute distance
			if (d <= 1.0)		 // update result
#pragma omp atomic
				nd = nd + 1; 	 // atomic clause to prevent race conditions
		}		
	}
	return 4.0 * (double)nd / (double)nt; //return approximation of pi
}

int main(void){
	int iterations = 10000;	// number of iterations to perform (higher -> better accuracy for PI).
	long s = 12345678;      // set random seed

	double p;
	/* Calculate Pi Here */
	p = calculatePi(iterations, s);
	/* Print the result. */
	printf("Computing Pi using Monte Carlo Methods\nPi = %f\n", p);
	return 0;
}
