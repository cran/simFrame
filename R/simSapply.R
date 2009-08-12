# --------------------------------------
# Author: Andreas Alfons
#         Vienna University of Techology
# --------------------------------------

## 'simSapply' uses 'sapply' to return vector, matrix or list

setMethod("simSapply", 
    signature(x = "data.frame", design = "BasicVector", fun = "function"), 
    function(x, design, fun, ..., simplify = TRUE) {
        s <- stratify(x, design)
        simSapply(x, s, fun, ..., simplify=simplify)
    })

setMethod("simSapply", 
    signature(x = "data.frame", design = "Strata", fun = "function"), 
    function(x, design, fun, ..., simplify = TRUE) {
        xSpl <- lapply(design@split, function(s, x) x[s,], x)
        ca <- as.call(list(sapply, ...))
        ca$X <- xSpl
        ca$FUN <- fun
        ca$simplify <- simplify
        ca$USE.NAMES <- NULL  # USE.NAMES is ignored
        eval(ca)
    })
