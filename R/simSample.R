# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

## convenience wrapper for 'setup'

simSample <- function(x, design = character(), group = character(), 
        method = srs, size = NULL, prob = NULL, ..., k = 1) {
    # define control object
    control <- SampleControl(design=design, group=group, 
        method=method, size=size, prob=prob, dots=list(...), k=k)
    # call 'setup'
    res <- setup(x, control)
    res@call <- match.call()
    res
}
