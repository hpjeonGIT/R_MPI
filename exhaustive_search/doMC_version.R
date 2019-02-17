doMC_exhaustive.search <- function(attributes, eval.fun) {
        len = length(attributes)
	if(len == 0)
		stop("Attributes not specified")
	
	eval.fun = match.fun(eval.fun)
	best = list(
		result = -Inf,
		attrs = rep(0, len)
	)
	
  	N_CPUS = 2
  	Ncriterion = N_CPUS*1
  	registerDoMC(N_CPUS)
	#
  	for(size in 1:len) {
		child_comb = combn(1:len, size)
      		Nloop <- dim(child_comb)[2]
      		if (Nloop > Ncriterion) {
        		myloop <- foreach(i=1:Nloop,
			       .packages=c("rpart","FSelector")) %dopar%
                  	{
                    		subset = rep(0,len)
                    		subset[child_comb[, i]] = 1
                    		result = eval.fun(attributes[as.logical(subset)])
                    	}
        		best.index = which.max(unlist(myloop))
        		result = myloop[[best.index]]
        		if (result > best$result)
			{
          			best$result = result
          			subset = rep(0,len)
          			subset[child_comb[, best.index]] = 1
          			best$attrs = subset
          		}
      		}
      		else {
           		for (i in 1:Nloop)
			    {
                		subset = rep(0,len)
                		subset[child_comb[,i]]=1
                		result = eval.fun(attributes[as.logical(subset)])
                		if (result > best$result)
				{
                    			best$result <- result
                    			best$attrs <- subset
                 		}
            		    }
      		       }      
  	}
	return(attributes[as.logical(best$attrs)])
}
