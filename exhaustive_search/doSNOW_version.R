doSNOW_exhaustive.search <- function(attributes, eval.fun) {
    len = length(attributes)
    if(len == 0)
        stop("Attributes not specified")
    
    eval.fun = match.fun(eval.fun)
    best = list(
        result = -Inf,
        attrs = rep(0, len)
    )
    ## DO SNOW loop
    N_CPUS = 2
    cat("Start cluster with N_CPUS=",N_CPUS, " len =", len)
    mycluster <- makeCluster(N_CPUS, outfile="", type="SOCK")
    ## type="MPI" may hang
    ##clusterExport(mycluster, "eval.set")
    ##clusterExport(mycluster, "train.set")
    registerDoSNOW(mycluster)
    myloop <- foreach(size=1:len,
                      .packages=c("FSelector","rpart")) %dopar% {
                          child_comb <- combn(1:len, size)
                                        # for each child
                          for(i in 1:dim(child_comb)[2]) {
			      subset <- rep(0, len)
			      subset[child_comb[, i]] <- 1
			      result <- eval.fun(attributes[as.logical(subset)])
			      if(result > best$result) {
				  best$result <- result
				  best$attrs <- subset
                              }
                          }
                          list(best$result, best$attrs) # this stores into myloop
                      }
    stopCluster(mycluster)
    cat("Stop cluster now")
    best$result = as.numeric(myloop[[1]][1])
    best$attrs  = as.vector(unlist(myloop[[1]][2]))
    for (i in 2:len) {
        result <- as.numeric(myloop[[i]][1])
        if (result > best$result) {
            best$result = result
            best$attrs = as.vector(unlist(myloop[[i]][2]))
        }
    }
    return(attributes[as.logical(best$attrs)])
}
