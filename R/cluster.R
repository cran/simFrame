# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

clusterAssign <- function(cl, x, value) {
    ans <- clusterCall(cl, 
        function(x, value) {
            assign(x, value, envir=.GlobalEnv)
            NULL
        }, 
        x=x, value=value)
    invisible(ans)
}
