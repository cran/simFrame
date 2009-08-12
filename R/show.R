# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

# sample setup
setMethod("show", "SampleSetup", function(object) print(object@indices))

# contamination control
setMethod("show", "VirtualContControl", 
    function(object) {
        if(length(object@target == 1)) cat("Target variable:\n")
        else cat("Target variables:\n")
        print(object@target)
        cat("\nEpsilon:\n")
        print(object@epsilon)
    })

# NA control
setMethod("show", "VirtualNAControl", 
    function(object) {
        if(length(object@target == 1)) cat("Target variable:\n")
        else cat("Target variables:\n")
        print(object@target)
        cat("\nMissing value rates:\n")
        print(object@NArate)
    })

setMethod("show", "SimResults", function(object) print(object@values))

# strata information
setMethod("show", "Strata", 
    function(object) {
        cat("Strata sizes:\n")
        print(data.frame(object@legend, size=object@size))
    })
