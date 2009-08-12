# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setMethod("draw",  
    signature(x = "data.frame", setup = "SampleSetup"), 
    function(x, setup, i = 1) drawS3(x, setup, i))

setMethod("draw", 
    signature(x = "data.frame", setup = "VirtualSampleControl"), 
    function(x, setup) {
        setup@k <- 1
        draw(x, setup(x, setup), i=1)
    })

setMethod("draw", 
    signature(x = "data.frame", setup = "character"), 
    function(x, setup, ...) {
        if(length(setup) != 1) {
            stop("'setup' must specify exactly one ", 
                "class extending \"VirtualSampleControl\"")
        }
        if(!extends(setup, "VirtualSampleControl")) {
            stop(gettextf("\"%s\" does not extend \"VirtualSampleControl\"", 
                    setup))
        }
        draw(x, new(setup, ...))
    })

setMethod("draw", 
    signature(x = "data.frame", setup = "missing"), 
    function(x, setup, ...) {
        draw(x, SampleControl(...))
    })


## internal S3 function 
# this is used in 'runSimulation' and 'clusterRunSimulation': there the 
# objects are already checked for validity and this speeds things up slightly
drawS3 <- function(x, setup, i) {
    ind <- setup@indices[[i]]  # indices for i-th sample
    res <- x[ind, , drop=FALSE]
    if(length(setup@prob) > 0) res$.weight <- 1/(setup@prob[ind])
    res
}
