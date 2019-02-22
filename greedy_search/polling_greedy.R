polling_greedy.search <- function(attributes, eval.fun, forward = TRUE) {
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
    
    ## main loop
    repeat {
        ## create new matrix of children to evaluate
        children = create.children(best$attrs, forward_text)
        if(is.null(children))
            break()
        
        ## Initial configuration of the matrix
        nrow <- dim(children)[1]; children_results=double(length=nrow)
        ncol <- dim(children)[2]
        children_results[1:nrow] <- -Inf

        nband <- 2L; ninc <- 1L
        # polling here
        if (myrank == 0) { # Master rank
            run_loop <- TRUE
            n_count = NCPUS*nband - ninc
            i_dummy = 0L
            while(run_loop) {
                any.msg <- mpi.iprobe(mpi.any.source(),
                                       mpi.any.tag(), comm=0, status=0)
                if (any.msg) {
                    slave <- mpi.get.sourcetag(status=0)
                    mpi.recv(i_dummy, type=1, source=slave, tag=1, comm=0)
                    mpi.send(n_count, type=1, dest=slave,   tag=2, comm=0)
                    n_count = n_count + 1
                    if (n_count > nrow) run_loop <- FALSE
                }
            }
            # Now sends termination signal
            run_loop <- TRUE
            n_count = 0
            while(run_loop) {
                any.msg <- mpi.iprobe(mpi.any.source(),
                                       mpi.any.tag(), comm=0, status=0)
                if (any.msg) {
                    slave <- mpi.get.sourcetag(status=0)
                    mpi.recv(i_dummy, type=1, source=slave, tag=1, comm=0)
                    mpi.send(-1L, type=1, dest=slave, tag=2, comm=0)
                    n_count = n_count + 1
                    if (n_count == (NCPUS-1)) run_loop <- FALSE
                }
            }
        } else ## slave ranks
            {
                # early calc without communication
                i_ini <- myrank*nband - ninc
                i_fin <- min(i_ini+nband-1,nrow)
                for (i in i_ini:i_fin) {
                    if (i <= nrow) children_results[i] =
                        eval.fun(attributes[as.logical(children[i,])])
                }
                # Polling here
                run_loop <- TRUE
                i_loop = 0L
                while(run_loop) {
                    mpi.send(i_loop, type=1, dest=0,   tag=1, comm=0)
                    mpi.recv(i_loop, type=1, source=0, tag=2, comm=0)
                    if (i_loop < 0) {
                        run_loop <- FALSE
                    } else if (i_loop <= nrow) {
                        children_results[i_loop] =
                            eval.fun(attributes[as.logical(children[i_loop,])])
                    }
                }
            }
        ## polling is done here
        mpi.barrier(comm=0)
        
        local_best = find.best(children_results)
        allred <- mpi.allreduce(local_best$result, type=2, op="maxloc", comm=0)
        world_best <- allred[1]
        id_rank <- as.integer(allred[2])
        world_id <- mpi.bcast(local_best$idx, type=1, rank=id_rank, comm=0)
       
        ## compare to the best ones collected
        if(world_best > best$result & myrank == 0 ) {
            best$result = world_best
            best$attrs = children[world_id,]
            tag_break = world_id
        } else {
            tag_break = -1L
        }
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

