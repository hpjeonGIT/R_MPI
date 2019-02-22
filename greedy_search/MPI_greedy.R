Rmpi_greedy.search <- function(attributes, eval.fun, forward = TRUE) {
    if(length(attributes) == 0)
        stop("Attributes not specified")
    
    eval.fun = match.fun(eval.fun)
    best = list(
        result = -Inf,
        attrs = rep(as.numeric(!forward), length(attributes))
	)
    
    ## initial evaluation for full set when backward
    if(!forward) {
        best$result = eval.fun(attributes[as.logical(best$attrs)])
    }
    
    forward_text = ifelse(forward, "forward", "backward")
    tag_break  = 0L
    
    ## main loop here
    repeat {
        ## create new matrix of children to evaluate
        children = create.children(best$attrs, forward_text)
        if(is.null(children))
            break()
        
        ## initial configuration per processor
        nrow <- dim(children)[1]; children_results=double(length=nrow)
        ncol  <- dim(children)[2]
        children_results[1:nrow] <- -Inf
        i_ini <- as.integer(myid*nrow/NCPUS + 1)
        i_fin <- as.integer((myid+1)*nrow/NCPUS)
        ## loop distribution
        for (i in i_ini:i_fin) {
            children_results[i] = eval.fun(attributes[as.logical(children[i,])])
        }
        local_best = find.best(children_results)
        ## Find world maximum
        allred <- mpi.allreduce(local_best$result, type=2, op="maxloc", comm=0)
        world_best <- allred[1]
        rank_id <- as.integer(allred[2])
        world_id <- mpi.bcast(local_best$idx, type=1, rank=rank_id, comm=0)
        if(world_best > best$result & myid == 0 ) {
            best$result = world_best
            best$attrs = children[world_id,]
            tag_break = world_id
        } else {
            tag_break = -1L
        }
        ## Broadcast the found id
        world_id <- mpi.bcast(tag_break,  type=1, rank=0, comm=0)
        if (world_id < 1) {
            break()
        } else {
            best$result = world_best
            best$attrs = children[world_id,]
        }
    }
    return(attributes[as.logical(best$attrs)])
}
