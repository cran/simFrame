# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

setGeneric("clusterRunSimulation",
#    function(cl, x, setup, nrep, control, ...) {
    function(cl, x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
        res <- standardGeneric("clusterRunSimulation")
        res@call <- match.call()
        res
    },
    valueClass = "SimResults")

setGeneric("clusterSetup",
    function(cl, x, control, ...) {
        res <- standardGeneric("clusterSetup")
        res@call <- match.call()
        res
    },
    valueClass = "SampleSetup")

setGeneric("contaminate",
    function(x, control, ...) standardGeneric("contaminate"),
    valueClass = "data.frame")

setGeneric("draw",
    function(x, setup, ...) standardGeneric("draw"), 
    valueClass = "data.frame")

setGeneric("generate",
    function(control, ...) standardGeneric("generate"), 
    valueClass = "data.frame")

setGeneric("getSampleIndices",
    function(x, control) standardGeneric("getSampleIndices"),
    valueClass = "list")

setGeneric("getSampleProb",
    function(x, control) standardGeneric("getSampleProb"),
    valueClass = "numeric")

setGeneric("getStrataLegend",
    function(x, design) standardGeneric("getStrataLegend"),
    valueClass = "data.frame")

setGeneric("getStrataSplit",
    function(x, design, USE.NAMES = TRUE) standardGeneric("getStrataSplit"),
    valueClass = "list")

setGeneric("getStrataTable",
    function(x, design) standardGeneric("getStrataTable"),
    valueClass = "data.frame")

setGeneric("getStratumSizes",
    function(x, design, USE.NAMES = TRUE) standardGeneric("getStratumSizes"),
    valueClass = "numeric")

setGeneric("getStratumValues",
    function(x, design, split) standardGeneric("getStratumValues"),
    valueClass = "numeric")

setGeneric("runSimulation",
    function(x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
        firstSeed <- .Random.seed
        res <- standardGeneric("runSimulation")
        lastSeed <- .Random.seed
        res@seed <- list(firstSeed, lastSeed)
        res@call <- match.call()
        res
    },
    valueClass = "SimResults")

setGeneric("setNA",
    function(x, control, ...) standardGeneric("setNA"),
    valueClass = "data.frame")

setGeneric("setup",
    function(x, control, ...) {
        res <- standardGeneric("setup")
        res@call <- match.call()
        res
    },
    valueClass = "SampleSetup")

setGeneric("simApply",
    function(x, design, fun, ...) standardGeneric("simApply"))

setGeneric("simBwplot",
    function(x, ...) standardGeneric("simBwplot"))

setGeneric("simDensityplot",
    function(x, ...) standardGeneric("simDensityplot"))

setGeneric("simSapply",
    function(x, design, fun, ..., simplify = TRUE) standardGeneric("simSapply"))

setGeneric("simXyplot",
    function(x, ...) standardGeneric("simXyplot"))

setGeneric("stratify", 
    function(x, design) {
        res <- standardGeneric("stratify")
        res@call <- match.call()
        res
    }, 
    valueClass = "Strata")


## S4 generics for existing S3 generics (just to be safe)
setGeneric("aggregate")
setGeneric("length")
setGeneric("plot")
setGeneric("summary")
