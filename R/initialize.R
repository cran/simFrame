# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------


# model based data control
setMethod("initialize", "DataControl", 
    function(.Object, ...) {
        args <- list(...)
        # use 'rnorm' as default
        if(is.null(args$distribution)) .Object@distribution <- rnorm
        callNextMethod()  # call method for superclass (or default)
    })

# sample control
setMethod("initialize", "SampleControl", 
    function(.Object, ...) {
        args <- list(...)
        # use simple random sampling as default
        if(is.null(args$method)) .Object@method <- srs
        callNextMethod()  # call method for superclass (or default)
    })

# contamination distributed completely at random (DCAR)
setMethod("initialize", "DCARContControl", 
    function(.Object, ...) {
        args <- list(...)
        # use standard normal distribution as default for contamination data
        if(is.null(args$distribution)) .Object@distribution <- rnorm
        callNextMethod()  # call method for superclass (or default)
    })

# insertion of missing values
setMethod("initialize", "NAControl", 
    function(.Object, ...) {
        args <- list(...)
        # use standard normal distribution as default for contamination data
        .Object@intoContamination <- isTRUE(.Object@intoContamination)
        callNextMethod()  # call method for superclass (or default)
    })
