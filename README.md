# Sample R script using Rmpi
- For distributed computing using MPI
- Requires Rmpi package with MPI environment
    - In windows, using MS-MPI
    - in Linux, openmpi is recommended while intel mpi or mvapich works too

## exhaustive_search
- Comparison of regular exhaustive search of original FSelector package with doSNOW/doMC/Rmpi implementation
- doSNOW and doMC works in multicore environment within a single node
- Rmpi version will work in a single or multiple nodes
- The current implementation uses Iris test from FSelector documentation, and may not work in very large number of MPI ranks

## greedy_search
- Comparison of regular greedy search of original FSelector package with Rmpi implementation
- The current implementation uses Iris test from FSelector documentation, and may not work in very large number of MPI ranks
- In addition to splitting loops over multiple ranks, polling version is provided, as a sample work of dynamic load balancing

