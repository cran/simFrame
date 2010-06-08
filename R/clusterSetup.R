# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Techonlogy
# ---------------------------------------

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
        design <- getCharacter(getDesign(control), nam)
        grouping <- getCharacter(getGrouping(control), nam)
        if(length(grouping) > 1) {
            stop("'grouping' must not specify more than one variable")
        }
        collectGroups <- (length(grouping) > 0) && isTRUE(getCollect(control))
        # parallel computing
        seqList <- clusterSplit(cl, 1:getK(control))
        kList <- lapply(seqList, length)
        indices <- clusterApply(cl, kList, 
            function(k, x, control) {
                setK(control, k)
                getSampleIndices(x, control)
            }, x, control)
        indices <- do.call("c", indices)
        prob <- getSampleProb(x, control)
        # return 'SampleSetup' object
        SampleSetup(indices=indices, prob=prob, design=design, 
            grouping=grouping, collect=collectGroups, fun=getFun(control))
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
        design <- getCharacter(getDesign(control), cnam)
        grouping <- getCharacter(getGrouping(control), cnam)
        if(length(grouping) > 1) {
            stop("'grouping' must not specify more than one variable")
        }
        collect <- isTRUE(getCollect(control))
        groupSampling <- (length(grouping) > 0) && !collect
        collectGroups <- (length(grouping) > 0) && collect
        fun <- getFun(control)
        nam <- argNames(fun)  # argument names of 'fun'
        size <- getSize(control)
        prob <- getProb(control)
        dots <- getDots(control)
        k <- getK(control)
        
        # it might be possible to increase the performance (C code?)
        
        if(length(design)) {
            # -------------------
            # stratified sampling
            # -------------------
            split <- getStrataSplit(x, design)
            call <- call("mapply", FUN=fun, MoreArgs=dots, 
                SIMPLIFY=FALSE, USE.NAMES=FALSE)  # initialize call
            
            if(groupSampling) {
                # --------------
                # group sampling
                # --------------
                groupSplit <- lapply(split, function(s) x[s, grouping])
                iGroupSplit <- lapply(groupSplit, function(x) !duplicated(x))
                uniqueGroupSplit <- mapply(function(x, i) x[i], groupSplit, iGroupSplit, 
                    SIMPLIFY=FALSE, USE.NAMES=FALSE)  # unique groups in strata
                N <- sapply(uniqueGroupSplit, length)  # number of groups in strata
                if("x" %in% nam) {
                    tmp <- mapply(function(x, i) x[i], split, iGroupSplit, 
                        SIMPLIFY=FALSE, USE.NAMES=FALSE)  # list of indices
                    xSplit <- lapply(tmp, function(s) x[s, , drop=FALSE])
                    call$x <- xSplit
                } else if("N" %in% nam) call$N <- N
                if(!is.null(size)) {
                    if(length(size) > 1) size <- rep(size, length.out=length(N))
                    call$size <- size
                }
                if(!is.null(prob) && "prob" %in% nam) {
                    Ngroup <- sum(N)  # number of groups
                    if(length(prob) != Ngroup) {
                        stop(gettextf("'prob' must be a vector of length %i", Ngroup))
                    }
                    strataValues <- unsplit(1:length(split), x[, design])
                    iGroup <- unsplit(iGroupSplit, strataValues)
                    probSplit <- split(prob, strataValues[iGroup])
                    call$prob <- probSplit
                }
                indices <- replicate(k, 
                    try(simEval(call, split, groupSplit, uniqueGroupSplit)), 
                    simplify=FALSE)  # repeated call
            } else {
                # -----------------------
                # sampling of individuals
                # groups may be collected
                # -----------------------
                N <- getStratumSizes(split, USE.NAMES=FALSE)  # stratum sizes
                if("x" %in% nam) {
                    xSplit <- lapply(split, function(s) x[s, , drop=FALSE])
                    call$x <- xSplit
                } else if("N" %in% nam) call$N <- N
                if(!is.null(size)) {
                    if(length(size) > 1) size <- rep(size, length.out=length(N))
                    call$size <- size
                }
                if(!is.null(prob)) {
                    Ntotal <- nrow(x)  # size of population
                    if(length(prob) != Ntotal) {
                        stop(gettextf("'prob' must be a vector of length %i", Ntotal))
                    }
                    probSplit <- lapply(split, function(s) prob[s])
                    call$prob <- probSplit
                }
                # repeated call
                if(collectGroups) {
                    groupSplit <- lapply(split, function(s) x[s, grouping])
                    indices <- replicate(k, 
                        try(simEval(call, split, groupSplit, groupSplit)), 
                        simplify=FALSE)
                } else indices <- replicate(k, try(simEval(call, split)), simplify=FALSE)
            }
        } else {
            # -----------------
            # no stratification
            # -----------------
            call <- as.call(c(fun, dots))  # initialize call to 'fun'
            
            if(groupSampling) {
                # --------------
                # group sampling
                # --------------
                groups <- x[, grouping]  # group of each observation
                iGroup <- !duplicated(groups)  # logicals to extract unique groups
                uniqueGroup <- groups[iGroup]  # unique groups
                N <- length(uniqueGroup)  # number of groups
                if("x" %in% nam) call$x <- x[iGroup, , drop=FALSE]
                else if("N" %in% nam) call$N <- N
                if(!is.null(size)) {
                    if(length(size) > 1) size <- size[1]
                    call$size <- size
                }
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    call$prob <- prob
                }
                indices <- replicate(k, 
                    try(simEval(call, groups=groups, unique=uniqueGroup)), 
                    simplify=FALSE)  # repeated call
            } else {
                # -----------------------
                # sampling of individuals
                # groups may be collected
                # -----------------------
                N <- nrow(x)  # size of population
                if("x" %in% nam) call$x <- x
                else if("N" %in% nam) call$N <- N
                if(!is.null(size)) {  # sample size
                    if(length(size) > 1) size <- size[1]
                    call$size <- size
                }
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    call$prob <- prob
                }
                # repeated call to fun
                if(collectGroups) {
                    groups <- x[, grouping]  # group of each observation
                    indices <- replicate(k, 
                        try(simEval(call, groups=groups, unique=groups)), 
                        simplify=FALSE)
                } else indices <- replicate(k, try(eval(call)), simplify=FALSE)
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
        design <- getCharacter(getDesign(control), cnam)
        grouping <- getCharacter(getGrouping(control), cnam)
        if(length(grouping) > 1) {
            stop("'grouping' must not specify more than one variable")
        }
        collect <- isTRUE(getCollect(control))
        groupSampling <- (length(grouping) > 0) && !collect
        collectGroups <- (length(grouping) > 0) && collect
        size <- getSize(control)
        prob <- getProb(control)
        
        # it might be possible to increase the performance (C code?)
        
        if(length(design) > 0) {
            if(groupSampling) {
                # -------------------------
                # stratified group sampling
                # -------------------------
                split <- getStrataSplit(x, design)
                groupSplit <- lapply(split, function(s) x[s, grouping])
                iGroupSplit <- lapply(groupSplit, function(x) !duplicated(x))
                uniqueGroupSplit <- mapply(function(x, i) x[i], groupSplit, iGroupSplit, 
                    SIMPLIFY=FALSE, USE.NAMES=FALSE)  # unique groups in strata
                N <- sapply(uniqueGroupSplit, length)  # number of groups in strata
                if(length(size) > 1) size <- rep(size, length.out=length(N))
                if(!is.null(prob)) {
                    Ngroup <- sum(N)  # number of groups
                    if(length(prob) != Ngroup) {
                        stop(gettextf("'prob' must be a vector of length %i", Ngroup))
                    }
                    strataValues <- unsplit(1:length(split), x[, design])
                    iGroup <- unsplit(iGroupSplit, strataValues)
                    probSplit <- split(prob, strataValues[iGroup])
                    names(probSplit) <- NULL
                    if(!is.null(size)) {
                        probSplit <- mapply(inclusionProb, probSplit, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                    } 
                    # reconstruct inclustion probabilities for individuals
                    prob <- mapply(unsplit, probSplit, groupSplit,  # from groups 
                        SIMPLIFY=FALSE, USE.NAMES=FALSE)            # in each stratum
                    prob <- unsplit(prob, strataValues)  # from strata
                } else prob <- unsplit(size/N, x[, design])
            } else {
                # ----------------------------------
                # stratified sampling of individuals
                # groups may have been collected
                # ----------------------------------
                if(!is.null(size)) {
                    split <- getStrataSplit(x, design)
                    N <- getStratumSizes(split, USE.NAMES=FALSE)  # stratum sizes
                    if(length(size) > 1) size <- rep(size, length.out=length(N))
                }
                if(!is.null(prob)) {
                    Ntotal <- nrow(x)  # size of population
                    if(length(prob) != Ntotal) {
                        stop(gettextf("'prob' must be a vector of length %i", Ntotal))
                    }
                    if(!is.null(size)) {
                        probSplit <- lapply(split, function(s) prob[s])
                        probSplit <- mapply(inclusionProb, probSplit, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                        prob <- unsplit(probSplit, x[, design])
                    }
                } else prob <- unsplit(size/N, x[, design])
                if(collectGroups) groups <- x[, grouping]  # group of each observation
            }
        } else {
            if(groupSampling) {
                # ---------------------------------
                # no stratification, group sampling
                # ---------------------------------
                groups <- x[, grouping]  # group of each observation
                N <- length(unique(groups))  # number of groups
                if(length(size) > 1) size <- size[1]
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    if(!is.null(size)) prob <- inclusionProb(prob, size)
                    prob <- unsplit(prob, groups)
                } else prob <- rep.int(size/N, length(groups))
            } else {
                # ------------------------------------------
                # no stratification, sampling of individuals
                # groups may have been collected
                # ------------------------------------------
                N <- nrow(x)  # size of population
                if(length(size) > 1) size <- size[1]
                if(!is.null(prob)) {
                    if(length(prob) != N) {
                        stop(gettextf("'prob' must be a vector of length %i", N))
                    }
                    if(!is.null(size)) prob <- inclusionProb(prob, size)
                } else prob <- rep.int(size/N, N)
                if(collectGroups) groups <- x[, grouping]  # group of each observation
            }
        }
        
        if(collectGroups) {
            # aggregate inclusion probabilities
            prob <- tapply(prob, groups, sum, simplify=FALSE)  # aggregate
            prob <- unsplit(prob, groups)  # blow up again
        }
        
        # return final inclustion probabilities
        prob
    })
