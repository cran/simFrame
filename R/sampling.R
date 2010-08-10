# ---------------------------------------
# Authors: Andreas Alfons
#          Vienna University of Technology
#          
#          Pablo Burgard
#          University of Trier
# ---------------------------------------

# simple random sampling
srs <- function(N, size, replace = FALSE) {
    if(N == 0) integer()
    else {
        if(length(N) > 1) N <- N[1]
        if(missing(size)) sample(N, replace=replace)
        else sample(N, size, replace)
    }
}

# unequal probability sampling
ups <- function(N, size, prob, replace = FALSE) {
    if(N == 0) integer()
    else {
        if(length(N) > 1) N <- N[1]
        sample(N, size, replace, prob)
    }
}

## Midzuno sampling
#midzuno <- function(prob, eps = 1e-06) {
#    prob <- 1 - tilleC(1 - prob, eps)  # call internal function for tille sampling
#    which(prob >= 1 - eps)  # indices of sampled observations
#}
#
## Tille sampling
#tille <- function(prob, eps = 1e-06) {
#    prob <- tilleC(prob, eps)  # call internal function
#    which(prob >= 1 - eps)  # indices of sampled observations
#}
#
## internal function for Tille sampling that calls the C function
#tilleC <- function(prob, eps = 1e-06) {
#    if(any(is.na(prob))) stop("there are missing values in 'prob'")
#    list <- (prob > eps) & (prob < 1 - eps)  # indices of probabilities to be used
#    probList <- prob[list]  # probabilities to be used
#    N <- length(probList)
#    if(N < 1) stop("all values in 'prob' outside the interval (eps, 1-eps)")
#    n <- sum(probList)  # number of observations to be sampled
#    prob[list] <- .Call("tille", prob=probList, n=n)  # call C function
#    prob
#}

# for internal use (in 'contaminate' and 'setNA')
samplex <- function(x, size, prob = NULL) {
    if(length(x) == 0) x
    else if(length(x) == 1) {
        if(!missing(size)) {
            if(isTRUE(size == 0)) x[FALSE]
            else if(isTRUE(size == 1)) x
            else stop("cannot take a sample larger than the population")
        } else x
    } else sample(x, size, prob = prob)
}
