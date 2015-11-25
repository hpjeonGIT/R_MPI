library (Rmpi)

rank  <- mpi.comm.rank(comm=0)
nproc <- mpi.comm.size(comm=0)

if (rank == 0) {
   run_loop = TRUE
   num_loop = 1
   while (run_loop) {
   	 any.mesg <- mpi.iprobe(mpi.ani.source(), mpi.any.tag(), comm=0, status=0)
	 if (any.mesg) {
	    src <- mpi.get.sourcetag(status = 0)
	    mpi.recv(i, type=1, source=src, tag=1, comm=0)
	    mpi.send(num_loop, type=1, dest=src, tag=2, comm=0)
	    num_loop = num_loop + 2
	    if (n_loop > 10) run_loop = FALSE
	    }
   }

   run_loop = TRUE
   num_loop = 0
   while(run_loop) {
   	any.mesg <- mpi.iprobe(mpi.any.source(), mpi.any.tag(), comm=0, status=0)
	if (any.mesg) {
	   src <- mpi.get.sourcetag(status=0)
	   mpi.recv(i, type=1, source=src, tag=1, comm=0)
	   mpi.send(-1, type=1, dest=src, tag=2, comm=0)
	   num_loop = num_loop + 1
	   if (num_loop == (nproc - 1)) run_loop = FALSE
	   }
	}
} else
{
	run_loop = TRUE
	
	while(tag_loop) {
		mpi.send(n, type=1, dest=0, tag=1, comm=0)
		mpi.recv(n, type=1, source=0, tag=2, comm=0)
		if (n < 0) {
		   run_loop = FALSE
		} else {
		  ## the end
		  }
}
}

mpi.quit()
quit("yes")