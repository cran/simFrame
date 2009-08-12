# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setMethod("plot", 
    signature(x = "SimResults", y = "missing"), 
    function(x, y, ...) {
        neps <- length(x@epsilon)
        nNA <- getLength(x@NArate)
        if(neps <= 1 && isTRUE(nNA <= 1)) simBwplot(x, ...)
        else if(neps <= 1 || isTRUE(nNA <= 1)) simXyplot(x, ...)
        else {
            stop("no default print method available with varying ", 
                "contamination levels and varying missing value rates")
        }
    })

