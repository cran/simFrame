# --------------------------------------
# Author: Andreas Alfons
#         Vienna University of Techology
# --------------------------------------

clusterAssign <- function(cl, x, value) {
    ans <- clusterCall(cl, 
        function(x, value) {
            assign(x, value, env=.GlobalEnv)
            NULL
        }, 
        x=x, value=value)
    invisible(ans)
}
