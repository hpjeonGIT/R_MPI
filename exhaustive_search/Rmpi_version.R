Rmpi_exhaustive.search <- function(attributes, eval.fun) {
	len = length(attributes)
	if(len == 0)
		stop("Attributes not specified")
	
	eval.fun = match.fun(eval.fun)
	best = list(
		result = -Inf,
		attrs = rep(0, len)
	)
	best.index = as.integer(0)
	rs.vec = rep(0,NCPUS)
	rs.ind = rep(as.integer(0), NCPUS)
	
	#
	for(size in 1:len) {
		child_comb = combn(1:len, size)
		Nloop = dim(child_comb)[2]
		update <- FALSE
		if (Nloop > NCPUS) {
			i_ini = myrank * Nloop/NCPUS + 1
			i_fin = (myrank + 1)*Nloop/NCPUS
			best.index = as.integer(0)
			for(i in i_ini:i_fin) {
				subset = rep(0, len)
				subset[child_comb[, i]] = 1
				result = eval.fun(attributes[as.logical(subset)])
				if(result > best$result) {
					best$result <- result
					best.index <- as.integer(i)
				}
			}
			if (best.index > 0) {
				subset = rep(0, len)
				subset[child_comb[, best.index]] = 1
				best$attrs <- subset
			}
			mpi.allgather(best$result, type=2, rs.vec, comm=0)
			mpi.allgather(best.index,  type=1, rs.ind, comm=0)
			for (i in 1:NCPUS) {
				if (rs.vec[i] > best$result) {
					best$result <- rs.vec[i]
					best.index  <- rs.ind[i]
					update <- TRUE
				}
			}
			if (update) {
				subset = rep(0, len)
				subset[child_comb[, best.index]] = 1
				best$attrs <- subset
			}
		}
		else {
			# when Nloop is small, no communication. Only local computing results
			for (i in 1:Nloop) {
				subset = rep(0,len)
				subset[child_comb[,i]] = 1
				result = eval.fun(attributes[as.logical(subset)])
				if(result > best$result) {
					best$result = result
					best$attrs = subset
				}
			}
		}
	}
	return(attributes[as.logical(best$attrs)])
}
