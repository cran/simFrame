# --------------------------------------
# Author: Andreas Alfons
#         Vienna University of Techology
# --------------------------------------

## use 'snow' cluster for parallel computing (what about multicore?)

## wrapper

# name of control class can be supplied as character string

setMethod("clusterSetup",
    signature(cl = "ANY", x = "data.frame", control = "character"),
    function(cl, x, control, ...) {
        if(length(control) != 1) {
            stop("'control' must specify exactly one class ", 
                "inheriting from \"VirtualSampleControl\"")
        }
        if(!extends(control, "VirtualSampleControl")) {
            stop(gettextf("\"%s\" does not extend class \"VirtualSampleControl\"", 
                    control))
        }
        control <- new(control, ...)
        clusterAssign(cl, "control", control)
        clusterSetup(cl, x, control=control)
    })

# default control class
setMethod("clusterSetup",
    signature(cl = "ANY", x = "data.frame", control = "missing"),
    function(cl, x, control, ...) {
        clusterSetup(cl, x, SampleControl(...))
    })

# ---------------------------------------

## get sample setup

setMethod("clusterSetup",
    signature(cl = "ANY", x = "data.frame", control = "SampleControl"),
    function(cl, x, control) {
        # initializations
        nam <- names(x)
        design <- getCharacter(control@design, nam)
        group <- getCharacter(control@group, nam)
        if(length(group) > 1) {
            stop("'group' must not specify more than one variable")
        }
        # parallel computing
        seqList <- clusterSplit(cl, 1:control@k)
        kList <- lapply(seqList, length)
        indices <- clusterApply(cl, kList, 
            function(k, x, control) {
                control@k <- k
                getSampleIndices(x, control)
            }, x, control)
        indices <- do.call("c", indices)
        prob <- getSampleProb(x, control)
        # return 'SampleSetup' object
        SampleSetup(indices=indices, prob=prob, 
            design=design, group=group, method=control@method)
    })


## utilities
# this is currently an ugly solution, it would 
# be nice to share code with 'setup' instead

# get indices of the sampled observations
setMethod("getSampleIndices",
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
        
        # it might be possible to increase the performance (C code?)
        
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
                if(!is.null(prob) && "prob" %in% nam) {
                    Ngr <- sum(N)  # number of groups
                    if(length(prob) != Ngr) {
                        stop(gettextf("'prob' must be a vector of length %i", Ngr))
                    }
                    sval <- unsplit(1:length(spl), x[, design])
                    igr <- unsplit(igrSpl, sval)
                    probSpl <- split(prob, sval[igr])
                    ca$prob <- probSpl
                }
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
                if(!is.null(prob)) {
                    Nobj <- nrow(x)  # size of population
                    if(length(prob) != Nobj) {
                        stop(gettextf("'prob' must be a vector of length %i", Nobj))
                    }
                    probSpl <- lapply(spl, function(s) prob[s])
                    ca$prob <- probSpl
                }
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
                }
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
                }
                # repeated call to method
                indices <- replicate(k, try(eval(ca)), simplify=FALSE)
            }
        }
        
        # check for try errors and return 'SampleSetup' object
        # replace errors with empty index vectors: let 'runSimulation' handle 
        # the problems, this is important to debug model-based sampling
        ok <- checkError(indices)
        indices[!ok] <- integer()
        indices
    })

# get inclustion probabilities
setMethod("getSampleProb",
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
        
        # it might be possible to increase the performance (C code?)
        
        if(length(design)) {
            if(length(group)) {
                # stratification, group sampling
                spl <- getStrataSplit(x, design)
                grSpl <- lapply(spl, function(s) x[s, group])
                igrSpl <- lapply(grSpl, function(x) !duplicated(x))
                ugrSpl <- mapply(function(x, i) x[i], grSpl, igrSpl, 
                    SIMPLIFY=FALSE, USE.NAMES=FALSE)  # unique groups in strata
                N <- sapply(ugrSpl, length)  # number of groups in strata
                if(length(size) > 1) size <- rep(size, length.out=length(N))
                if(!is.null(prob)) {
                    Ngr <- sum(N)  # number of groups
                    if(length(prob) != Ngr) {
                        stop(gettextf("'prob' must be a vector of length %i", Ngr))
                    }
                    sval <- unsplit(1:length(spl), x[, design])
                    igr <- unsplit(igrSpl, sval)
                    probSpl <- split(prob, sval[igr])
                    names(probSpl) <- NULL
                    if(!is.null(size)) {
                        probSpl <- mapply(getProb, probSpl, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                    } 
                    # reconstruct inclustion probabilities for individuals
                    prob <- mapply(unsplit, probSpl, grSpl,  # from groups 
                        SIMPLIFY=FALSE, USE.NAMES=FALSE)     # in each stratum
                    prob <- unsplit(prob, sval)  # from strata
                } else prob <- unsplit(size/N, x[, design])
            } else {
                # stratification, no group sampling
                if(!is.null(size)) {
                    spl <- getStrataSplit(x, design)
                    N <- getStratumSizes(spl, USE.NAMES=FALSE)  # stratum sizes
                    if(length(size) > 1) size <- rep(size, length.out=length(N))
                }
                if(!is.null(prob)) {
                    Nobj <- nrow(x)  # size of population
                    if(length(prob) != Nobj) {
                        stop(gettextf("'prob' must be a vector of length %i", Nobj))
                    }
                    if(!is.null(size)) {
                        probSpl <- lapply(spl, function(s) prob[s])
                        probSpl <- mapply(getProb, probSpl, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                        prob <- unsplit(probSpl, x[, design])
                    }
                } else prob <- unsplit(size/N, x[, design])
            }
        } else {
            if(length(group)) {
                # no stratification, group sampling
                gr <- x[, group]  # group of each observation
                N <- length(unique(gr))  # number of groups
                if(length(size) > 1) size <- size[1]
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    if(!is.null(size)) prob <- getProb(prob, size)
                    prob <- unsplit(prob, gr)
                } else prob <- rep.int(size/N, length(gr))
            } else {
                # no stratification, no group sampling
                N <- nrow(x)  # size of population
                if(length(size) > 1) size <- size[1]
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    if(!is.null(size)) prob <- getProb(prob, size)
                } else prob <- rep.int(size/N, N)
            }
        }
        
        # return final inclustion probabilities
        prob
    })
