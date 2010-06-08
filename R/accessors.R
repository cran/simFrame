# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------


## class "DataControl"

setMethod("getSize", "DataControl", function(x) slot(x, "size"))
setMethod("setSize", "DataControl", 
    function(x, size) eval.parent(substitute(slot(x, "size") <- size)))

setMethod("getDistribution", "DataControl", function(x) slot(x, "distribution"))
setMethod("setDistribution", "DataControl", 
    function(x, distribution) {
        eval.parent(substitute(slot(x, "distribution") <- distribution))
    })

setMethod("getDots", "DataControl", function(x) slot(x, "dots"))
setMethod("setDots", "DataControl", 
    function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))

setMethod("getColnames", "DataControl", function(x) slot(x, "colnames"))
setMethod("setColnames", "DataControl", 
    function(x, colnames) {
        eval.parent(substitute(slot(x, "colnames") <- colnames))
    })


## class "SampleControl"

setMethod("getK", "VirtualSampleControl", function(x) slot(x, "k"))
setMethod("setK", "VirtualSampleControl", 
    function(x, k) eval.parent(substitute(slot(x, "k") <- k)))

setMethod("getDesign", "SampleControl", function(x) slot(x, "design"))
setMethod("setDesign", "SampleControl", 
    function(x, design) eval.parent(substitute(slot(x, "design") <- design)))

setMethod("getGrouping", "SampleControl", function(x) slot(x, "grouping"))
setMethod("setGrouping", "SampleControl", 
    function(x, grouping) {
        eval.parent(substitute(slot(x, "grouping") <- grouping))
    })

setMethod("getCollect", "SampleControl", function(x) slot(x, "collect"))
setMethod("setCollect", "SampleControl", 
    function(x, collect) eval.parent(substitute(slot(x, "collect") <- collect)))

setMethod("getFun", "SampleControl", function(x) slot(x, "fun"))
setMethod("setFun", "SampleControl", 
    function(x, fun) eval.parent(substitute(slot(x, "fun") <- fun)))

setMethod("getSize", "SampleControl", function(x) slot(x, "size"))
setMethod("setSize", "SampleControl", 
    function(x, size) eval.parent(substitute(slot(x, "size") <- size)))

setMethod("getProb", "SampleControl", function(x) slot(x, "prob"))
setMethod("setProb", "SampleControl", 
    function(x, prob) eval.parent(substitute(slot(x, "prob") <- prob)))

setMethod("getDots", "SampleControl", function(x) slot(x, "dots"))
setMethod("setDots", "SampleControl", 
    function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))


## class "SampleSetup"

# public accessors (getters)
setMethod("getIndices", "SampleSetup", function(x) slot(x, "indices"))
setMethod("getProb", "SampleSetup", function(x) slot(x, "prob"))
setMethod("getDesign", "SampleSetup", function(x) slot(x, "design"))
setMethod("getGrouping", "SampleSetup", function(x) slot(x, "grouping"))
setMethod("getCollect", "SampleSetup", function(x) slot(x, "collect"))
setMethod("getFun", "SampleSetup", function(x) slot(x, "fun"))
setMethod("getSeed", "SampleSetup", function(x) slot(x, "seed"))
setMethod("getCall", "SampleSetup", function(x) slot(x, "call"))

# private mutators (setters)
setMethod("setIndices", "SampleSetup", 
    function(x, indices) eval.parent(substitute(slot(x, "indices") <- indices)))
setMethod("setSeed", "SampleSetup", 
    function(x, seed) eval.parent(substitute(slot(x, "seed") <- seed)))
setMethod("setCall", "SampleSetup", 
    function(x, call) eval.parent(substitute(slot(x, "call") <- call)))

# summary
setMethod("getSize", "SummarySampleSetup", function(x) slot(x, "size"))


## class "ContControl"

setMethod("getTarget", "VirtualContControl", function(x) slot(x, "target"))
setMethod("setTarget", "VirtualContControl", 
    function(x, target) eval.parent(substitute(slot(x, "target") <- target)))

setMethod("getEpsilon", "VirtualContControl", function(x) slot(x, "epsilon"))
setMethod("setEpsilon", "VirtualContControl", 
    function(x, epsilon) eval.parent(substitute(slot(x, "epsilon") <- epsilon)))

setMethod("getGrouping", "ContControl", function(x) slot(x, "grouping"))
setMethod("setGrouping", "ContControl", 
    function(x, grouping) {
        eval.parent(substitute(slot(x, "grouping") <- grouping))
    })

setMethod("getAux", "ContControl", function(x) slot(x, "aux"))
setMethod("setAux", "ContControl", 
    function(x, aux) eval.parent(substitute(slot(x, "aux") <- aux)))

setMethod("getDistribution", "DCARContControl", 
    function(x) slot(x, "distribution"))
setMethod("setDistribution", "DCARContControl", 
    function(x, distribution) {
        eval.parent(substitute(slot(x, "distribution") <- distribution))
    })

setMethod("getDots", "DCARContControl", function(x) slot(x, "dots"))
setMethod("setDots", "DCARContControl", 
    function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))

setMethod("getFun", "DARContControl", function(x) slot(x, "fun"))
setMethod("setFun", "DARContControl", 
    function(x, fun) eval.parent(substitute(slot(x, "fun") <- fun)))

setMethod("getDots", "DARContControl", function(x) slot(x, "dots"))
setMethod("setDots", "DARContControl", 
    function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))


## class "NAControl"

setMethod("getTarget", "VirtualNAControl", function(x) slot(x, "target"))
setMethod("setTarget", "VirtualNAControl", 
    function(x, target) eval.parent(substitute(slot(x, "target") <- target)))

setMethod("getNArate", "VirtualNAControl", function(x) slot(x, "NArate"))
setMethod("setNArate", "VirtualNAControl", 
    function(x, NArate) eval.parent(substitute(slot(x, "NArate") <- NArate)))

setMethod("getGrouping", "NAControl", function(x) slot(x, "grouping"))
setMethod("setGrouping", "NAControl", 
    function(x, grouping) {
        eval.parent(substitute(slot(x, "grouping") <- grouping))
    })

setMethod("getAux", "NAControl", function(x) slot(x, "aux"))
setMethod("setAux", "NAControl", 
    function(x, aux) eval.parent(substitute(slot(x, "aux") <- aux)))

setMethod("getIntoContamination", "NAControl", 
    function(x) slot(x, "intoContamination"))
setMethod("setIntoContamination", "NAControl", 
    function(x, intoContamination) {
        eval.parent(substitute(slot(x, "intoContamination") <- intoContamination))
    })


## class "Strata"

# public accessors (getters)
setMethod("getValues", "Strata", function(x) slot(x, "values"))
setMethod("getSplit", "Strata", function(x) slot(x, "split"))
setMethod("getDesign", "Strata", function(x) slot(x, "design"))
setMethod("getNr", "Strata", function(x) slot(x, "nr"))
setMethod("getLegend", "Strata", function(x) slot(x, "legend"))
setMethod("getSize", "Strata", function(x) slot(x, "size"))
setMethod("getCall", "Strata", function(x) slot(x, "call"))

# private mutators (setters)
setMethod("setCall", "Strata", 
    function(x, call) eval.parent(substitute(slot(x, "call") <- call)))


## class "SimControl"

setMethod("getContControl", "SimControl", function(x) slot(x, "contControl"))
setMethod("setContControl", "SimControl", 
    function(x, contControl) {
        eval.parent(substitute(slot(x, "contControl") <- contControl))
    })

setMethod("getNAControl", "SimControl", function(x) slot(x, "NAControl"))
setMethod("setNAControl", "SimControl", 
    function(x, NAControl) {
        eval.parent(substitute(slot(x, "NAControl") <- NAControl))
    })

setMethod("getDesign", "SimControl", function(x) slot(x, "design"))
setMethod("setDesign", "SimControl", 
    function(x, design) eval.parent(substitute(slot(x, "design") <- design)))

setMethod("getFun", "SimControl", function(x) slot(x, "fun"))
setMethod("setFun", "SimControl", 
    function(x, fun) eval.parent(substitute(slot(x, "fun") <- fun)))

setMethod("getDots", "SimControl", function(x) slot(x, "dots"))
setMethod("setDots", "SimControl", 
    function(x, dots) eval.parent(substitute(slot(x, "dots") <- dots)))

setMethod("getSAE", "SimControl", function(x) slot(x, "SAE"))
setMethod("setSAE", "SimControl", 
    function(x, SAE) eval.parent(substitute(slot(x, "SAE") <- SAE)))


### class "SimResult"
#
## public accessors (getters)
#setMethod("getValues", "SimResult", function(x) slot(x, "values"))
#setMethod("getAdd", "SimResult", function(x) slot(x, "add"))


## class "SimResults"

# public accessors (getters)
setMethod("getValues", "SimResults", function(x) slot(x, "values"))
setMethod("getAdd", "SimResults", function(x) slot(x, "add"))
setMethod("getDesign", "SimResults", function(x) slot(x, "design"))
setMethod("getColnames", "SimResults", function(x) slot(x, "colnames"))
setMethod("getEpsilon", "SimResults", function(x) slot(x, "epsilon"))
setMethod("getNArate", "SimResults", function(x) slot(x, "NArate"))
setMethod("getSeed", "SimResults", function(x) slot(x, "seed"))
setMethod("getCall", "SimResults", function(x) slot(x, "call"))

# private mutators (setters)
setMethod("setValues", "SimResults", 
    function(x, values) eval.parent(substitute(slot(x, "values") <- values)))
setMethod("setSeed", "SimResults", 
    function(x, seed) eval.parent(substitute(slot(x, "seed") <- seed)))
setMethod("setCall", "SimResults", 
    function(x, call) eval.parent(substitute(slot(x, "call") <- call)))
