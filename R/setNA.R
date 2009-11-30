# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setMethod("setNA",
    signature(x = "data.frame", control = "NAControl"),
    function(x, control, i = 1) {
        # initializations
        target <- control@target
        if(is.null(target)) target <- getNames(x)
        tl <- length(target)
        if(is(control@NArate, "numeric")) NArate <- control@NArate[i]
        else NArate <- rep(control@NArate[i,], length.out=tl)
        if(all(NArate == 0) || any(dim(x) == 0)) return(x)  # nothing to do
        group <- control@group
        if(length(group) > 1) {
            stop("'group' must not specify more than one variable")
        }
        useGroup <- as.logical(length(group))  # 'group' supplied
        aux <- control@aux
        if(length(aux) > 1) {
            stop("'aux' must not specify more than one variable")
        }
        useAux <- as.logical(length(aux))  # 'aux' supplied
        if(control@intoContamination) contaminated <- NULL
        else contaminated <- x$.contaminated
        isContaminated <- !control@intoContamination && !is.null(contaminated)
        # get population size and number of observations/groups to be set NA
        if(useGroup) {
            gr <- x[, group]  # group of each individual
            if(useAux || isContaminated) {
                Nobj <- nrow(x)
                spl <- split(1:Nobj, getFactor(gr))
                N <- length(spl)
            } else {
                ugr <- unique(gr)  # unique groups
                N <- length(ugr)  # number of groups
            }
            if(isContaminated) {
                # don't set to NA if any in the group is contaminated
                contaminated <- sapply(spl, function(i) any(contaminated[i]))
            }
        } else N <- nrow(x)
        n <- ceiling(NArate * N)
        # prepare auxiliary variable, if supplied
        if(useAux) {
            aux <- x[, aux]
            if(!is.numeric(aux)) {
                stop("slot 'aux' in 'control' must specify a numeric variable.")
            }
            if(!all(is.finite(aux))) {
                stop("variable definted by slot 'aux' in 'control'", 
                    "must not contain missing or infinite values.")
            }
            if(any(aux < 0)) aux <- aux - min(aux)  # add small value?
            if(useGroup) {
                # use the group means (much faster than medians)
                #aux <- sapply(spl, function(i) median(aux[i]))
                aux <- sapply(spl, function(i) mean(aux[i]))
            }
        } else aux <- NULL
        # get indices
        if(length(n) == 1) {
            ind <- replicate(tl, getIndicesSetNA(N, n, aux, contaminated))
        } else {
            ind <- mapply(getIndicesSetNA, N, n, 
                MoreArgs=list(aux=aux, contaminated=contaminated))
        }
        if(useGroup) {
            if(useAux || isContaminated) {
                ind <- apply(ind, 2, 
                    function(i) {
                        ans <- logical(Nobj)
                        ans[unlist(spl[i])] <- TRUE
                        ans
                    })
            } else ind <- apply(ind, 2, function(i) gr %in% ugr[i])
        }
        ## do not append logical variables indicating the values set to NA
        ## using 'is.na' in the function for the simulation runs is much faster
        #colnames(ind) <- paste(".", target, sep="")
        # set selected values to NA and return x
        x[, target][ind] <- NA
        #existing <- intersect(names(x), colnames(ind))
        #if(length(existing)) {
        #    x[, existing] <- x[, existing] | ind[, existing]
        #    new <- setdiff(colnames(ind), existing)
        #    if(length(new)) x <- cbind(x, ind[, new, drop=FALSE])
        #    x
        #} else cbind(x, ind)
        x
    })

setMethod("setNA", 
    signature(x = "data.frame", control = "character"), 
    function(x, control, ...) {
        if(length(control) != 1) {
            stop("'control' must specify exactly one ", 
                "class extending \"VirtualNAControl\"")
        }
        if(!extends(control, "VirtualNAControl")) {
            stop(gettextf("\"%s\" does not extend \"VirtualNAControl\"", 
                    control))
        }
        setNA(x, new(control, ...))
    })

setMethod("setNA",
    signature(x = "data.frame", control = "missing"),
    function(x, control, ...) {
        setNA(x, NAControl(...))
    })


## utilities
# this is an internal function, otherwise there should be some error checking
getIndicesSetNA <- function(N, size, aux = NULL, contaminated = logical()) {
    x <- 1:N
    if(length(contaminated)) {
        nc <- !contaminated
        x <- x[nc]
        aux <- aux[nc]
    }
    ans <- logical(N)
    ans[samplex(x, size, aux)] <- TRUE
    ans
}

