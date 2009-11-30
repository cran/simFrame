# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setMethod("aggregate", "SimResults", 
    function(x, select = NULL, FUN = mean, ...) {
        by <- c(if(length(x@epsilon)) "Epsilon", 
            if(length(x@NArate)) "NArate", x@design)
        if(is.null(select)) select <- x@colnames
        else {
            if(!is.character(select)) {
                stop("'select' must be a character vector")
            }
            if(!all(select %in% x@colnames)) stop("undefined columns selected")
        }
        x <- x@values
        if(length(by)) {
            aggregate(x[, select, drop=FALSE], 
                by=x[, by, drop=FALSE], FUN=FUN, ...)
        } else {
            apply(x[, select, drop=FALSE], 2, FUN=FUN, ...)
        }
    })
