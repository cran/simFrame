# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

# summary of data.frame is of class "table", 
# thus no extra class "summary.SimResults" necessary
setMethod("summary", "SimResults", 
    function(object, ...) summary(object@values, ...))
