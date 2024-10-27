#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/*
	Synchronos communication in a ring.

	This function does two things:
		Node sends a message to its right neighbour. If the message is sent for the first time,
		the message sent is the current process' rank. 
    Node receives a message from its left neighbour. It adds the node's rank to the message
    and transmittes the message its right neighbour.
*/

int pass_around_ring_blocking(int msg, int rank, int recnode, int destnode){
/*
 * Function:  pass_around_ring_blocking 
 * --------------------
 * Passes a message (rank) through a ring of nodes
 *
 * rank: rank of current node
 * recnode: receiver's node ID
 * destnode: destination's node ID
 *
 * returns: received message
 */
  
	MPI_Status status; //   variable to store message status information
	int ierror;		     //   Error code
	int sum = 0;       //   running sum of received messages
	int tag = rank;    //   message tag
	//each process does two things
	//relays any message it receives to next node (+rank to message, defined by different tag)
	//sends its own message to the next node
	//once it receives the message with its tag (i.e. tag=rank) it stops sending
	//The message should have went around the loop!
	while (1){
		//send blocking message to right neighbour
		ierror = MPI_Send(&msg, 1, MPI_INT, destnode, tag, MPI_COMM_WORLD);
		//receive message from left neighnour
		ierror = MPI_Recv(&msg, 1, MPI_INT, recnode, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
		//check received message's tag
		tag = status.MPI_TAG;
		//if tag corresponds to first message sent, break
		if (tag == rank){
			sum = msg;
			break;
		}
		//otherwise add process' rank to received message
		msg = msg + rank;
	}
	//return received message (i.e. sum)
	return sum;
}

int pass_around_ring_nonblocking(int msg, int rank, int recnode, int destnode){
/*
 * Function:  pass_around_ring_nonblocking 
 * --------------------
 * Passes a message (rank) through a ring of nodes by nonblocking communication
 *
 * rank: rank of current node
 * recnode: receiver's node ID
 * destnode: destination's node ID
 *
 * returns: received message
 */
	MPI_Status status;   //   variable to store message status information
	MPI_Request request; //   variable to store message request information
	int ierror;			     //   error code
	int send_msg = msg;  //   buffer to store sending message (initialized to process' rank)
	int recv_msg;        //   buffer to store receiving message
	int sum = 0;         //   sum of ranks
	int tag = rank;      //   message tag
	while(1){
		//send asynchronos message to right neighbour
		ierror = MPI_Isend(&send_msg, 1, MPI_INT, destnode, tag, MPI_COMM_WORLD, &request);
		//wait for confirmation
		ierror = MPI_Wait(&request, &status);
		//receive asynchronos message from left neighbour
		ierror = MPI_Irecv(&recv_msg, 1, MPI_INT, recnode, MPI_ANY_TAG, MPI_COMM_WORLD, &request);
		//wait for confirmation
		ierror = MPI_Wait(&request, &status);
		tag = status.MPI_TAG;
       	if (tag == rank){
       		sum = send_msg;
       		break;
       	}
		send_msg = recv_msg + rank;
	}
	return sum;
}

int pass_around_by_reduce(int rank){
/*
 * Function:  pass_around_by_reduce 
 * --------------------
 * Computes the sum of MPI process ranks.
 * The sum is then sent to every MPI process.
 *
 * rank: rank of current node
 *
 * returns: sum
 */
	MPI_Request request;
	MPI_Status status;
	int ierror;
	int sum;
	int msg = rank;
	//use non-blocking reduction
	ierror = MPI_Ireduce(&msg, &sum, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD, &request);
	//wait for request to complete
	ierror = MPI_Wait(&request, &status);
	//send sum to all processes
	ierror = MPI_Bcast(&sum, 1, MPI_INT, 0, MPI_COMM_WORLD);
	//return result
	return sum;
}

int main(int argc, char** argv){

	int i, ierror, rank, nprocs;
	int sum;

	int dest, recv; 						      // destination/receiving ranks
	ierror = MPI_Init(&argc, &argv);  // initialize MPI

	if (ierror != MPI_SUCCESS)
	{
		printf("An error occured when initializing MPI.\nError code: %d\n", ierror);
		MPI_Abort(MPI_COMM_WORLD, 101);
		return EXIT_FAILURE;
	}

	ierror = MPI_Comm_rank(MPI_COMM_WORLD, &rank);   // get processes rank
	ierror = MPI_Comm_size(MPI_COMM_WORLD, &nprocs); // get number of processes

	if (ierror != MPI_SUCCESS)
	{
		printf("An error occured when checking process rank/ number of processes. Error code: %d\n", ierror);
		MPI_Abort(MPI_COMM_WORLD, 101);
		return EXIT_FAILURE;
	}
	//check that 5 mpi processes were created
	if (nprocs != 5){
		printf("There must be 5 MPI processes for this program to run.\nYou specified %d processes.\n", nprocs);
		MPI_Abort(MPI_COMM_WORLD, 101);
		return EXIT_FAILURE;
	}	
	//compute right neighbour
	dest = (rank+1) % nprocs;
	//compute left neighbour
	recv = (rank-1);
	if (recv < 0) 
		recv = nprocs - 1;

	//run communication functions

	/* QUESTION 1  */
	sum = pass_around_ring_blocking(rank, rank, recv, dest);
	/* QUESTION 2  */
	//sum = pass_around_ring_nonblocking(rank, rank, recv, dest);
	/* QUESTION 3  */
	//sum = pass_around_by_reduce(rank);

	printf("Rank %d has sum: %d\n", rank, sum);

	ierror = MPI_Finalize(); 					 		// end MPI
	return EXIT_SUCCESS;
}
