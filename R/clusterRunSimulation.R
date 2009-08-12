# --------------------------------------
# Author: Andreas Alfons
#         Vienna University of Techology
# --------------------------------------

## for convenience: construct "SimControl" object and re-call function
setMethod("clusterRunSimulation", 
    signature(cl = "ANY", x = "ANY", setup = "ANY", 
        nrep = "ANY", control = "missing"),
    function(cl, x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
        control <- SimControl(contControl=contControl, NAControl=NAControl, 
            design=design, fun=fun, dots=list(...), SAE=SAE)
        clusterAssign(cl, "control", control)
        if(missing(setup)) {
            if(missing(nrep)) clusterRunSimulation(cl, x, control=control)
            else clusterRunSimulation(cl, x, nrep=nrep, control=control)
        } else {
            if(missing(nrep)) clusterRunSimulation(cl, x, setup, control=control)
            else clusterRunSimulation(cl, x, setup, nrep, control)
        }
    })


## design-based simulation
setMethod("clusterRunSimulation", 
    signature(cl = "ANY", x = "data.frame", setup = "VirtualSampleControl", 
        nrep = "missing", control = "SimControl"),
#    function(cl, x, setup, nrep, control) {
    function(cl, x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
            setup <- clusterSetup(cl, x, setup)
        clusterAssign(cl, "setup", setup)
        clusterRunSimulation(cl, x, setup, control)
    })

setMethod("clusterRunSimulation", 
    signature(cl = "ANY", x = "data.frame", setup = "SampleSetup", 
        nrep = "missing", control = "SimControl"),
#    function(cl, x, setup, nrep, control) {
    function(cl, x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
            # initializations
        nsam <- length(setup)
        if(nsam == 0) return(SimResults(design=control@design))  # nothing to do
        contControl <- control@contControl
        epsilon <- if(is.null(contControl)) numeric() else contControl@epsilon
        NAControl <- control@NAControl
        NArate <- if(is.null(NAControl)) numeric() else NAControl@NArate
        # run the simulations
        s <- 1:nsam
        tmp <- parLapply(cl, s, designSimulation, x, setup, control)
        # construct results
        getSimResults(tmp, s, epsilon=epsilon, 
            NArate=NArate, design=control@design)
    })


## model-based simulation
setMethod("clusterRunSimulation", 
    signature(cl = "ANY", x = "VirtualDataControl", 
        setup = "missing", nrep = "numeric", control = "SimControl"),
#    function(cl, x, setup, nrep, control) {
    function(cl, x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
            # initializations
        if(length(nrep) == 0) stop("'nrep' must be a non-negative integer")
        else if(length(nrep) > 1) nrep <- nrep[1]
        if(nrep == 0) return(SimResults(design=control@design))  # nothing to do
        contControl <- control@contControl
        epsilon <- if(is.null(contControl)) numeric() else contControl@epsilon
        NAControl <- control@NAControl
        NArate <- if(is.null(NAControl)) numeric() else NAControl@NArate
        # run the simulations
        r <- 1:nrep
        tmp <- parLapply(cl, r, modelSimulation, x, control)
        # construct results
        getSimResults(tmp, reps=r, epsilon=epsilon, 
            NArate=NArate, design=control@design)
    })


## TODO: mixed simulation designs


## simulation with replications based on (possibly) real data
setMethod("clusterRunSimulation",
    signature(cl = "ANY", x = "data.frame", setup = "missing", 
        nrep = "numeric", control = "SimControl"),
#    function(cl, x, setup, nrep, control) {
    function(cl, x, setup, nrep, control, contControl = NULL, 
            NAControl = NULL, design = character(), fun, ..., 
            SAE = FALSE) {
            # initializations
        if(length(nrep) == 0) stop("'nrep' must be a non-negative integer")
        else if(length(nrep) > 1) nrep <- nrep[1]
        if(nrep == 0) return(SimResults(design=control@design))  # nothing to do
        contControl <- control@contControl
        epsilon <- if(is.null(contControl)) numeric() else contControl@epsilon
        NAControl <- control@NAControl
        NArate <- if(is.null(NAControl)) numeric() else NAControl@NArate
        SAE <- isTRUE(control@SAE)
        # get results
        r <- 1:nrep
        if(length(control@design)) {
            # necessary objects need to be constructed on workers
            seqList <- clusterSplit(cl, r)
            nrList <- lapply(seqList, length)
            if(SAE) {
                tmp <- clusterApply(cl, nrList, 
                    function(nr, x, control) {
                        spl <- getStrataSplit(x, control@design, USE.NAMES=FALSE)
                        leg <- getStrataLegend(x, control@design)
                        replicate(nr, manageSimulationSAE(x, spl, control, leg), simplify=FALSE)
                    }, x, control)
            } else {
                tmp <- clusterApply(cl, nrList, 
                    function(nr, x, control) {
                        spl <- getStrataSplit(x, control@design, USE.NAMES=FALSE)
                        xSpl <- lapply(spl, function(s, x) x[s, , drop=FALSE], x)
                        leg <- getStrataLegend(x, control@design)
                        replicate(nr, manageSimulationStrata(xSpl, spl, control, leg), simplify=FALSE)
                    }, x, control)
            }
            tmp <- do.call("c", tmp)
        } else {
            tmp <- parLapply(cl, r, 
                function(i) manageSimulation(x, control))
        }
        # construct results
        getSimResults(tmp, reps=r, epsilon=epsilon, 
            NArate=NArate, design=control@design)
    })
