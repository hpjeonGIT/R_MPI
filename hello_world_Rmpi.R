set.seed(1234)
library(Rmpi)

myid <- mpi.comm.rank(comm=0)
ncpu <- mpi.comm.size(comm=0)
cat("hello world from ", myid, "and ", mpi.get.processor.name(), " out of ", ncpu ,  "\n")

mpi.barrier(comm=0)

mpi.quit()

quit("yes")
# run command: mpirun -n 2 Rscript hello_world_Rmpi.R
