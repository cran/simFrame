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
        
        # For now, the branches of the 'if' clauses are based on the 
        # corresponding previously defined methods of 'simSample'. It 
        # might be possible to increase the performance (C code?).
        
        if(length(design) > 0) {
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
                if(!is.null(prob)) {
                    Ngroups <- sum(N)  # number of groups
                    if(length(prob) != Ngroups) {
                        stop(gettextf("'prob' must be a vector of length %i", Ngroups))
                    }
                    strataValues <- unsplit(1:length(split), x[, design])
                    iGroups <- unsplit(iGroupSplit, strataValues)
                    probSplit <- split(prob, strataValues[iGroups])
                    names(probSplit) <- NULL
                    call$prob <- probSplit
                    if(!is.null(size)) {
                        probSplit <- mapply(inclusionProb, probSplit, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                    }
                    # reconstruct inclustion probabilities for individuals
                    prob <- mapply(unsplit, probSplit, groupSplit,  # from groups 
                        SIMPLIFY=FALSE, USE.NAMES=FALSE)            # in each stratum
                    prob <- unsplit(prob, strataValues)  # from strata
                } else prob <- unsplit(size/N, x[, design])
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
                if(!is.null(prob) && "prob" %in% nam) {
                    Ntotal <- nrow(x)  # size of population
                    if(length(prob) != Ntotal) {
                        stop(gettextf("'prob' must be a vector of length %i", Ntotal))
                    }
                    probSplit <- lapply(split, function(s) prob[s])
                    call$prob <- probSplit
                    if(!is.null(size)) {
                        probSplit <- mapply(inclusionProb, probSplit, size, 
                            SIMPLIFY=FALSE, USE.NAMES=FALSE)
                        prob <- unsplit(probSplit, x[, design])
                    }
                } else prob <- unsplit(size/N, x[, design])
                # repeated call
                if(collectGroups) {
                    groups <- x[, grouping]  # group of each observation
                    groupSplit <- lapply(split, function(s) groups[s])
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
                iGroups <- !duplicated(groups)  # logicals to extract unique groups
                uniqueGroups <- groups[iGroups]  # unique groups
                N <- length(uniqueGroups)  # number of groups
                if("x" %in% nam) call$x <- x[iGroups, , drop=FALSE]
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
                    if(!is.null(size)) prob <- inclusionProb(prob, size)
                    prob <- unsplit(prob, groups)
                } else prob <- rep.int(size/N, length(groups))
                indices <- replicate(k, 
                    try(simEval(call, groups=groups, unique=uniqueGroups)), 
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
                    if(!is.null(size)) prob <- inclusionProb(prob, size)
                } else prob <- rep.int(size/N, N)
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
        
        if(collectGroups) {
            # aggregate inclusion probabilities
            prob <- tapply(prob, groups, sum, simplify=FALSE)  # aggregate
            prob <- unsplit(prob, groups)  # blow up again
        }
        
        SampleSetup(indices=indices, prob=prob, design=design, 
            grouping=grouping, collect=collectGroups, fun=fun)
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
simEval <- function(call, split, groups, unique) {
    if(!missing(split)) {
        if(missing(groups)) {
            # 'call' returns list of within-strata indices.
            tmp <- eval(call)
        } else {
            # 'groups' and 'unique' are lists
            # 'call' returns list of within-strata indices of groups. these 
            # are used to obtain the within-strata indices of individuals.
            tmp <- mapply(function(i, g, u) which(g %in% u[i]), 
                eval(call), groups, unique, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        }
        # within-strata indices are turned into global indices 
        # and the resulting list is converted to a vector.
        unlist(mapply("[", split, tmp, SIMPLIFY=FALSE, USE.NAMES=FALSE))
    } else if(!missing(groups)) {  # only 'groups' is not missing
        # 'groups' and 'unique' are vectors
        # 'call' returns list of groups. these are
        # used to obtain the indices of individuals.
        which(groups %in% unique[eval(call)])
    } else eval(call)
}
