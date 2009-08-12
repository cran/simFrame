# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

# TODO: add argument specifying which columns to plot

setMethod("simBwplot", 
    signature(x = "SimResults"),
    function(x, true = NULL, epsilon, NArate, ...) {
        
        # initializations
        values <- x@values
        if(nrow(values) == 0) stop("slot 'values' does not contain any rows")
        if(ncol(values) == 0) stop("slot 'values' does not contain any columns")
        haveEpsilon <- length(x@epsilon) > 0
        supEpsilon <- !missing(epsilon)  # contamination level supplied
        haveNArate <- isTRUE(getLength(x@NArate) > 0)
        supNArate <- !missing(NArate)  # missing value rate supplied
        
        # check contamination level and missing value rate
        if(haveEpsilon) {
            if(supEpsilon) {
                if(!isTRUE(length(epsilon) == 1)) {
                    stop("'epsilon' must specify ", 
                        "exactly one contamination level")
                }
                eps <- values$Epsilon
            } else if(length(x@epsilon) > 1) {
                stop("this plot is not meaningful ", 
                    "for varying contamination levels")
            }
        }
        if(haveNArate) {
            if(supNArate) {
                if(!isTRUE(length(NArate) == 1)) {
                    stop("'NArate' must specify ", 
                        "exactly one missing value rate")
                }
                NAr <- values$NArate
            } else if(isTRUE(getLength(x@NArate) > 1)) {
                stop("this plot is not meaningful ", 
                    "for varying missing value rates")
            }
        }
        # get indices of selected results
        if(haveEpsilon && supEpsilon && haveNArate && supNArate) {
            values <- values[(eps == epsilon) & (NAr == NArate), , drop=FALSE]
            if(nrow(values) == 0) {
                stop("the contamination level specified by ",
                    "'epsilon' or the missing value rate specified by ", 
                    "'NArate' has not been used in the simulation")
            }
        } else if(haveEpsilon && supEpsilon) {
            values <- values[eps == epsilon, , drop=FALSE]
            if(nrow(values) == 0) {
                stop("the contamination level specified by ",
                    "'epsilon' has not been used in the simulation")
            }
        } else if(haveNArate && supNArate) {
            values <- values[NAr == NArate, , drop=FALSE]
            if(nrow(values) == 0) {
                stop("the missing value rate specified by ",
                    "'NArate' has not been used in the simulation")
            }
        }
        
        # call internal function
        internalSimBwplot(values, x@design, x@colnames, true=true, ...)
    })


## internal function
internalSimBwplot <- function(values, design, names, true = NULL, 
        horizontal = TRUE, xlab = NULL, ylab = NULL, ..., 
        # the following arguments are defined so that they aren't supplied twice
        x, data, panel, prepanel, groups) {
    # prepare data and formula for lattice graphics
    values <- getLatticeData(values, design, names)
    if(horizontal) {
        left <- ".Name"
        right <- ".Value"
    } else {
        left <- ".Value"
        right <- ".Name"
    }
    form <- getFormula(left, right, design)
    # call lattice function
    bwplot(form, data=values, panel=panelSimBwplot, prepanel=prepanelSimBwplot, 
        true=true, horizontal=horizontal, xlab=xlab, ylab=ylab, ...)
}

## panel function
panelSimBwplot <- function(x, y, true = NULL, horizontal = TRUE, ...) {
    if(length(true) > 0) {
        # if only one value is supplied for true, it is used for 
        # every panel, otherwise we select the one corresponding
        # to the panel
        if(length(true) > 1) {
            i <- packet.number()  # identifies current packet
            true <- true[i]
        }
        if(horizontal) panel.refline(v=true, ...)
        else panel.refline(h=true, ...)
    }
    panel.bwplot(x, y, horizontal=horizontal, ...)
}

## prepanel function
prepanelSimBwplot <- function(x, y, true = NULL, horizontal = TRUE, ...) {
    if(length(true) == 0) list()
    else {
# FIXME: default axis limits if relation is "sliced" or "free"
#        'packet.number' only works for panel function
#        can packet number be accessed from the prepanel function?
#        # if only one value is supplied for true, it is used for 
#        # every panel, otherwise we select the one corresponding
#        # to the panel
#        if(length(true) > 1) {
#            i <- packet.number()  # identifies current packet
#            true <- true[i]
#        }
        tmp <- c(if(horizontal) x else y, true)
        lim <- range(tmp, finite=TRUE)
        if(horizontal) list(xlim=lim) else list(ylim=lim)
    }
}
