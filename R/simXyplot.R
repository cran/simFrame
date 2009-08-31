# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setMethod("simXyplot", 
    signature(x = "SimResults"),
    function(x, true = NULL, epsilon, NArate, select, ...) {
        
        # initializations
        values <- x@values
        if(nrow(values) == 0) stop("slot 'values' does not contain any rows")
        if(ncol(values) == 0) stop("slot 'values' does not contain any columns")
        neps <- length(x@epsilon)
        haveEpsilon <- neps > 0
        supEpsilon <- !missing(epsilon)  # contamination level supplied
        nNA <- getLength(x@NArate)
        haveNArate <- isTRUE(nNA > 0)
        supNArate <- !missing(NArate)  # missing value rate supplied
        
        # check contamination levels and missing value rates
        if(neps <= 1 && isTRUE(nNA <= 1)) {
            stop("this plot is only meaningful with either varying ", 
                "contamination levels or varying missing value rates")
        } else if(haveEpsilon && haveNArate) {
            if(neps > 1 && isTRUE(nNA == 1)) {
                if(supEpsilon) warning("'epsilon' is ignored")
                if(supNArate) warning("'NArate' is ignored")
                xnam <- "Epsilon"
            } else if(neps == 1 && isTRUE(nNA > 1)) {
                if(supEpsilon) warning("'epsilon' is ignored")
                if(supNArate) warning("'NArate' is ignored")
                xnam <- "NArate"
            } else {
                if(supEpsilon && supNArate) {
                    stop("only one of 'epsilon' or 'NArate' may be supplied")
                } else if(supEpsilon) {
                    if(!isTRUE(length(epsilon) == 1)) {
                        stop("'epsilon' must specify ", 
                        "exactly one contamination level")
                    }
                    values <- values[values$Epsilon == epsilon, , drop=FALSE]
                    if(nrow(values) == 0) {
                        stop("the contamination level specified by ",
                            "'epsilon' has not been used in the simulation")
                    }
                    xnam <- "NArate"
                } else if(supNArate) {
                    if(!isTRUE(length(NArate) == 1)) {
                        stop("'NArate' must specify ", 
                            "exactly one missing value rate")
                    }
                    values <- values[values$NArate == NArate, , drop=FALSE]
                    if(nrow(values) == 0) {
                        stop("the missing value rate specified by ",
                            "'NArate' has not been used in the simulation")
                    }
                    xnam <- "Epsilon"
                } else {
                    stop("contamination levels and missing ", 
                        "value rates are both varying")
                }
            }
        } else if(haveEpsilon) {
            if(supEpsilon) warning("'epsilon' is ignored")
            xnam <- "Epsilon"
        } else if(haveNArate) {
            if(supNArate) warning("'NArate' is ignored")
            xnam <- "NArate"
        } else stop("unexpected problem with 'x'")  # just to be safe
        
        # check specified columns
        if(missing(select)) select <- x@colnames
        else {
            if(!is.character(select)) {
                stop("'select' must be a character vector")
            }
            if(!all(select %in% x@colnames)) stop("undefined columns selected")
        }
        
        # if missing value rates are plotted on x-axis and NArate is a matrix, 
        # the display on the x-axis should be more of a categorical nature 
        # (corresponding to the rows of NArate)
        at <- if(haveNArate && is(x@NArate, "matrix")) 1:nNA else NULL
        
        # call internal function
        internalSimXyplot(values, xnam, x@design, select, at=at, true=true, ...)
    })


## internal function
internalSimXyplot <- function(values, xnam = c("Epsilon","NArate"), 
        design, names, at = NULL, true = NULL, auto.key=TRUE, 
        scales=list(), type = "l", ylab = NULL, ..., 
        # the following arguments are defined so that they aren't supplied twice
        x, data, panel, prepanel, groups) {
    # prepare legend
    if(isTRUE(auto.key)) auto.key <- list(points=FALSE, lines=TRUE)
    else if(is.list(auto.key)) {
        if(is.null(auto.key$points)) auto.key$points <- FALSE
        if(is.null(auto.key$lines)) auto.key$lines <- TRUE
    }
    # prepare data and formula for lattice graphics
    xnam <- match.arg(xnam)
    if(xnam == "NArate") {
        if(!is.null(at)) {
            if(is.null(scales$x$at)) scales$x$at <- at
            if(is.null(scales$x$tck)) scales$x$tck <- 0
        }
    }
    cond <- c(xnam, design)
    tmp <- aggregate(values[, names, drop=FALSE], 
        values[, cond, drop=FALSE], function(x) mean(x, na.rm=TRUE))
    values <- getLatticeData(tmp, cond, names)
    form <- getFormula(".Value", xnam, design)
    # call lattice function
    xyplot(form, data=values, auto.key=auto.key, panel=panelSimXyplot, 
        prepanel=prepanelSimXyplot, scales=scales, true=true, groups=.Name, 
        type=type, ylab=ylab, ...)
}

## panel function
panelSimXyplot <- function(x, y, true = NULL, ...) {
    if(length(true) > 0) {
        # if only one value is supplied for true, it is used for 
        # every panel, otherwise we select the one corresponding
        # to the panel
        if(length(true) > 1) {
            i <- packet.number()  # identifies current packet
            true <- true[i]
        }
        panel.refline(h=true, ...)
    }
    panel.xyplot(x, y, ...)
}

## prepanel function
prepanelSimXyplot <- function(x, y, true = NULL, horizontal = TRUE, ...) {
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
        lim <- range(c(y, true), finite=TRUE)
        list(ylim=lim)
    }
}
