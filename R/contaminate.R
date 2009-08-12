# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setMethod("contaminate",
    signature(x = "data.frame", control = "ContControl"),
    function(x, control, i = 1) {
        # initializations
        epsilon <- control@epsilon[i]
        if(epsilon == 0 || any(dim(x) == 0)) return(x)  # nothing to do
        target <- control@target
        if(is.null(target)) target <- getNames(x)
        group <- control@group
        if(length(group) > 1) {
            stop("'group' must not specify more than one variable")
        }
        aux <- control@aux
        if(length(aux) > 1) {
            stop("'aux' must not specify more than one variable")
        }
        useGroup <- as.logical(length(group))  # 'group' supplied
        useAux <- as.logical(length(aux))  # 'aux' supplied
        # get population size and number of 
        # observations or groups to be contaminated
        if(useGroup) {
            gr <- x[, group]  # group of each individual
            if(useAux) {
                Nobj <- nrow(x)
                spl <- split(1:Nobj, getFactor(gr))
                N <- length(spl)
            } else {
                ugr <- unique(gr)  # unique groups
                N <- length(ugr)  # number of groups
            }
        } else N <- nrow(x)
        n <- ceiling(epsilon * N)
        if(useAux) {  # prepare auxiliary variable
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
            ind <- ups(N, n, prob=aux)  # get indices (unequal prob. sampling)
        } else ind <- srs(N, n)  # get indices (simple random sampling)
        if(useGroup) {
            if(useAux) ind <- unlist(spl[ind])
            else ind <- which(gr %in% ugr[ind])  # row indices
        }
        if(is(control, "DCARContControl")) {
            values <- do.call(control@distribution, c(n, control@dots))
            if(useGroup) {
                rep <- unsplit(1:n, getFactor(gr[ind]))  # replication indices
                values <- if(is.null(dim(values))) values[rep] else values[rep,]
            }
        } else if(is(control, "DARContControl")) {
            dots <- c(list(x[ind, target]), control@dots)
            values <- do.call(control@fun, dots)
        } else {
            stop("for user defined contamination control classes, a ", 
                "method 'contaminate(x, control, i)' needs to be defined")
        }
        values <- as.data.frame(values)
        # set contaminated values and return x
        x[ind, target] <- values
        if(is.null(x$.contaminated)) {
            contaminated <- logical(nrow(x))
            contaminated[ind] <- TRUE
            x$.contaminated <- contaminated
        } else x[ind, ".contaminated"] <- TRUE
        x
    })

setMethod("contaminate", 
    signature(x = "data.frame", control = "character"), 
    function(x, control, ...) {
        if(length(control) != 1) {
            stop("'control' must specify exactly one ", 
                "class extending \"VirtualContControl\"")
        }
        if(!extends(control, "VirtualContControl")) {
            stop(gettextf("\"%s\" does not extend \"VirtualContControl\"", 
                    control))
        }
        contaminate(x, new(control, ...))
    })

setMethod("contaminate",
    signature(x = "data.frame", control = "missing"),
    function(x, control, ...) {
        contaminate(x, DCARContControl(...))
    })
