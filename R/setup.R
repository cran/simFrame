# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

# group sampling: for methods with the argument 'x', the observation 
#                 with the first occurrence of the group will be used 
#                 as prototype (how to aggregate the data?)

setMethod("setup",
    signature(x = "data.frame", control = "SampleControl"), 
    function(x, control) {
        
        # initializations
        cnam <- names(x)
        design <- getCharacter(control@design, cnam)
        group <- getCharacter(control@group, cnam)
        if(length(group) > 1) {
            stop("'group' must not specify more than one variable")
        }
        method <- control@method
        nam <- argNames(method)  # argument names of 'method'
        size <- control@size
        prob <- control@prob
        dots <- control@dots
        k <- control@k
        
        # For now, the branches of the 'if' clauses are based on the 
        # corresponding previously defined methods of 'simSample'. It 
        # might be possible to increase the performance (C code?).
        
        if(length(design)) {
            spl <- getStrataSplit(x, design)
            ca <- call("mapply", FUN=method, MoreArgs=dots, 
                SIMPLIFY=FALSE, USE.NAMES=FALSE)  # initialize call
            if(length(group)) {
                # stratification, group sampling
                grSpl <- lapply(spl, function(s) x[s, group])
                igrSpl <- lapply(grSpl, function(x) !duplicated(x))
                ugrSpl <- mapply(function(x, i) x[i], grSpl, igrSpl, 
                    SIMPLIFY=FALSE, USE.NAMES=FALSE)  # unique groups in strata
                N <- sapply(ugrSpl, length)  # number of groups in strata
                if("x" %in% nam) {
                    tmp <- mapply(function(x, i) x[i], spl, igrSpl, 
                        SIMPLIFY=FALSE, USE.NAMES=FALSE)  # list of indices
                    xSpl <- lapply(tmp, function(s) x[s, , drop=FALSE])
                    ca$x <- xSpl
                } else if("N" %in% nam) ca$N <- N
                if(!is.null(size)) {
                    if(length(size) > 1) size <- rep(size, length.out=length(N))
                    ca$size <- size
                }
                if(!is.null(prob)) {
                    Ngr <- sum(N)  # number of groups
                    if(length(prob) != Ngr) {
                        stop(gettextf("'prob' must be a vector of length %i", Ngr))
                    }
                    sval <- unsplit(1:length(spl), x[, design])
                    igr <- unsplit(igrSpl, sval)
                    probSpl <- split(prob, sval[igr])
                    names(probSpl) <- NULL
                    ca$prob <- probSpl
                    if(!is.null(size)) {
                        probSpl <- mapply(getProb, probSpl, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                    }
                    # reconstruct inclustion probabilities for individuals
                    prob <- mapply(unsplit, probSpl, grSpl,  # from groups 
                        SIMPLIFY=FALSE, USE.NAMES=FALSE)     # in each stratum
                    prob <- unsplit(prob, sval)  # from strata
                } else prob <- unsplit(size/N, x[, design])
                indices <- replicate(k, try(simEval(ca, spl, grSpl, ugrSpl)), 
                    simplify=FALSE)  # repeated call
            } else {
                # stratification, no group sampling
                N <- getStratumSizes(spl, USE.NAMES=FALSE)  # stratum sizes
                if("x" %in% nam) {
                    xSpl <- lapply(spl, function(s) x[s, , drop=FALSE])
                    ca$x <- xSpl
                } else if("N" %in% nam) ca$N <- N
                if(!is.null(size)) {
                    if(length(size) > 1) size <- rep(size, length.out=length(N))
                    ca$size <- size
                }
                if(!is.null(prob) && "prob" %in% nam) {
                    Nobj <- nrow(x)  # size of population
                    if(length(prob) != Nobj) {
                        stop(gettextf("'prob' must be a vector of length %i", Nobj))
                    }
                    probSpl <- lapply(spl, function(s) prob[s])
                    ca$prob <- probSpl
                    if(!is.null(size)) {
                        probSpl <- mapply(getProb, probSpl, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                        prob <- unsplit(probSpl, x[, design])
                    }
                } else prob <- unsplit(size/N, x[, design])
                # repeated call
                indices <- replicate(k, try(simEval(ca, spl)), simplify=FALSE)
            }
        } else {
            ca <- as.call(c(method, dots))  # initialize call to 'method'
            if(length(group)) {
                # no stratification, group sampling
                gr <- x[, group]  # group of each observation
                igr <- !duplicated(gr)  # logicals to extract unique groups
                ugr <- gr[igr]  # unique groups
                N <- length(ugr)  # number of groups
                if("x" %in% nam) ca$x <- x[igr, , drop=FALSE]
                else if("N" %in% nam) ca$N <- N
                if(!is.null(size)) {
                    if(length(size) > 1) size <- size[1]
                    ca$size <- size
                }
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    ca$prob <- prob
                    if(!is.null(size)) getProb(prob, size)
                    prob <- unsplit(prob, gr)
                } else prob <- rep.int(size/N, length(gr))
                indices <- replicate(k, try(simEval(ca, group=gr, unique=ugr)), 
                    simplify=FALSE)  # repeated call
            } else {
                # no stratification, no group sampling
                N <- nrow(x)  # size of population
                if("x" %in% nam) ca$x <- x
                else if("N" %in% nam) ca$N <- N
                if(!is.null(size)) {  # sample size
                    if(length(size) > 1) size <- size[1]
                    ca$size <- size
                }
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    ca$prob <- prob
                    if(!is.null(size)) prob <- getProb(prob, size)
                } else prob <- rep.int(size/N, N)
                # repeated call to method
                indices <- replicate(k, try(eval(ca)), simplify=FALSE)
            }
        }
        
        # check for try errors and return 'SampleSetup' object
        # replace errors with empty index vectors: let 'runSimulation' handle 
        # the problems, this is important to debug model-based sampling
        ok <- checkError(indices)
        indices[!ok] <- integer()
        SampleSetup(indices=indices, prob=prob, 
            design=design, group=group, method=method)
    })


# name of control class can be supplied as character string
setMethod("setup",
    signature(x = "data.frame", control = "character"), 
    function(x, control, ...) {
        if(length(control) != 1) {
            stop("'control' must specify exactly one class ", 
                "inheriting from \"VirtualSampleControl\"")
        }
        if(!extends(control, "VirtualSampleControl")) {
            stop(gettextf("\"%s\" does not extend class \"VirtualSampleControl\"", 
                    control))
        }
        setup(x, control=new(control, ...))
    })


# default control class
setMethod("setup",
    signature(x = "data.frame", control = "missing"),
    function(x, control, ...) {
        setup(x, SampleControl(...))
    })


### utilities

# check for errors in a list (for sample setup)
checkError <- function(x) {
    # x ... list
    if(length(x)) sapply(x, function(x) class(x) != "try-error")
    else logical()
}

# evaluate calls to sample methods with stratification and group sampling
simEval <- function(call, split, group, unique) {
    if(!missing(split)) {
        if(missing(group)) {
            # 'call' returns list of within-strata indices.
            tmp <- eval(call)
        } else {
            # 'group' and 'unique' are lists
            # 'call' returns list of within-strata indices of groups. these 
            # are used to obtain the within-strata indices of individuals.
            tmp <- mapply(function(i, g, u) which(g %in% u[i]), 
                eval(call), group, unique, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        }
        # within-strata indices are turned into global indices 
        # and the resulting list is converted to a vector.
        unlist(mapply("[", split, tmp, SIMPLIFY=FALSE, USE.NAMES=FALSE))
    } else if(!missing(group)) {  # only 'group' is not missing
        # 'group' and 'unique' are vectors
        # 'call' returns list of groups. these are
        # used to obtain the indices of individuals.
        which(group %in% unique[eval(call)])
    } else eval(call)
}
