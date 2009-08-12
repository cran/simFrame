# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

## class unions of elementary classes (for convenience)

setClassUnion("BasicVector", c("character", "logical", "numeric"))
setClassUnion("NumericMatrix", c("numeric", "matrix"))
setClassUnion("OptCall", c("NULL", "call"))
setClassUnion("OptCharacter", c("NULL", "character"))
setClassUnion("OptNumeric", c("NULL", "numeric"))

# ---------------------------------------

## control class for generating model based data

validDataControlObject <- function(object) {
    if(length(object@size) == 1 && object@size >= 0) TRUE
    else "'size' must be a single non-negative integer"
}

setClass("DataControl",
    representation(size = "numeric", distribution = "function", 
        dots = "list", colnames = "OptCharacter"),
    prototype(size = 0, colnames = NULL),
    validity = validDataControlObject)

DataControl <- function(...) new("DataControl", ...)

# class union (for extending the frameowrk)
setClassUnion("VirtualDataControl", "DataControl")

# ---------------------------------------

## sample control

# virtual class
validVirtualSampleControlObject <- function(object) {
    if(length(object@k) == 1 && object@k > 0) TRUE
    else "'k' must be a single positive integer"
}

setClass("VirtualSampleControl",
    representation(k = "numeric"),
    prototype(k = 1),
    contains = "VIRTUAL",
    validity = validVirtualSampleControlObject)

# sampling according to function 'simSample'
validSampleControlObject <- function(object) {
    l <- getSelectionLength(object@group)
    ok <- c(is.na(l) || l <= 1, 
        is.null(object@size) || length(object@size),
        is.null(object@prob) || length(object@prob))
    msg <- c("'group' must not specify more than one variable",
        "'size' must have positive length",
        "'prob' must have positive length")
    if(all(ok)) TRUE
    else msg[!ok]
}

setClass("SampleControl",
    representation(design = "BasicVector", group = "BasicVector", 
        method = "function", size = "OptNumeric", prob = "OptNumeric", 
        dots = "list"),
    prototype(design = character(), group = character(), 
        size = NULL, prob = NULL),
    contains = "VirtualSampleControl",
    validity = validSampleControlObject)

SampleControl <- function(...) new("SampleControl", ...)

# ---------------------------------------

## sample setup

validSampleSetupObject <- function(object) {
    if(length(object@group) <= 1) TRUE
    else "'group' must not specify more than one variable"
}

setClass("SampleSetup",
    representation(indices = "list", prob = "numeric", design = "character", 
        group = "character", method = "function", call = "OptCall"),
    prototype(call = NULL))

SampleSetup <- function(...) new("SampleSetup", ...)

# ---------------------------------------

## contamination control

# virtual class
validVirtualContControlObject <- function(object) {
    ok <- c(length(object@target) > 0 || is.null(object@target), 
        length(object@epsilon) > 0, 
        all(0 <= object@epsilon & object@epsilon <= 0.5))
    msg <- c("'target' must be specified", 
        "'epsilon' must be specified",  
        "values in 'epsilon' must be between 0 and 0.5")
    if(all(ok)) TRUE
    else msg[!ok]
}

setClass("VirtualContControl",
    representation(target = "OptCharacter", epsilon = "numeric"),
    prototype(target = NULL, epsilon = 0.05),
    contains = "VIRTUAL",
    validity = validVirtualContControlObject)

setClassUnion("OptContControl", c("NULL", "VirtualContControl"))

# internal control class (not expected to be extended by the user)
validContControlObject <- function(object) {
    ok <- c(length(object@group) <= 1, length(object@aux) <= 1)
    msg <- c("'group' must not specify more than one variable", 
        "'aux' must not specify more than one variable")
    if(all(ok)) TRUE
    else msg[!ok]
}

setClass("ContControl",
    representation(group = "character", aux = "character"),
    contains = c("VIRTUAL", "VirtualContControl"),
    validity = validContControlObject)

# contamination distributed completely at random (DCAR)
setClass("DCARContControl",
    representation(distribution = "function", dots = "list"),
    contains = "ContControl")

DCARContControl <- function(...) new("DCARContControl", ...)

# contamination distributed at random (DAR)
setClass("DARContControl",
    representation(fun = "function", dots = "list"),
    contains = "ContControl")

DARContControl <- function(...) new("DARContControl", ...)

# wrapper (mostly for compatibility)
ContControl <- function(..., type = c("DCAR", "DAR")) {
    type <- match.arg(type)
    class <- paste(type, "ContControl", sep="")
    new(class, ...)
}

# ---------------------------------------

## NA control

# virtual class
validVirtualNAControlObject <- function(object) {
    NArate <- object@NArate
    nl <- getLength(NArate)
    ok <- c(length(object@target) > 0 || is.null(object@target), 
        nl > 0 || is.na(nl), 
        checkNumericMatrix(NArate), 
        all(0 <= NArate & NArate <= 1))
    msg <- c("'target' must be specified", 
        "'NArate' must be specified", 
        "non-numeric values in 'NArate'", 
        "values in 'NArate' must be between 0 and 1")
    if(all(ok)) TRUE
    else msg[!ok]
}

setClass("VirtualNAControl",
    representation(target = "OptCharacter", NArate = "NumericMatrix"),
    prototype(target = NULL, NArate = 0.05),
    contains = "VIRTUAL",
    validity = validVirtualNAControlObject)

setClassUnion("OptNAControl", c("NULL", "VirtualNAControl"))


# select values randomly for each target variable
validNAControlObject <- function(object) {
    ok <- c(length(object@group) <= 1, length(object@aux) <= 1)
    msg <- c("'group' must not specify more than one variable", 
        "'aux' must not specify more than one variable")
    if(all(ok)) TRUE
    else msg[!ok]
}

setClass("NAControl",
    representation(group = "character", aux = "character"),
    contains = "VirtualNAControl", 
    validity = validNAControlObject)

NAControl <- function(...) new("NAControl", ...)

# ---------------------------------------

## strata information

setClass("Strata", 
    representation(values = "numeric", split = "list", 
        design = "character", nr = "numeric", legend = "data.frame", 
        size = "numeric", call = "OptCall"), 
    prototype(size = 0, call = NULL))

Strata <- function(...) new("Strata", ...)

# ---------------------------------------

## simulation control

setClass("SimControl",
    representation(contControl = "OptContControl", NAControl = "OptNAControl",
        design = "character", fun = "function", dots = "list", SAE = "logical"),
    prototype(contControl = NULL, NAControl = NULL, 
        design = character(), SAE = FALSE))

SimControl <- function(...) new("SimControl", ...)

# ---------------------------------------

## one simulation result

setClass("SimResult",
    representation(values = "numeric", add = "ANY"))

SimResult <- function(...) new("SimResult", ...)

# ---------------------------------------

## simulation results

setClass("SimResults",
    representation(values = "data.frame", add = "list", design = "character", 
        colnames = "character", epsilon = "numeric", NArate = "NumericMatrix", 
        seed = "list", call = "OptCall"),
    prototype(NArate = numeric(), call = NULL))

SimResults <- function(...) new("SimResults", ...)
