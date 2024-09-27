### =========================================================================
### The containsOutOfMemoryData() generic
### -------------------------------------------------------------------------

setGeneric("containsOutOfMemoryData",
    function(object) standardGeneric("containsOutOfMemoryData")
)

setMethod("containsOutOfMemoryData", "ANY", function(object) FALSE)

setClass("OutOfMemoryObject", representation("VIRTUAL"))

setMethod("containsOutOfMemoryData", "OutOfMemoryObject",
    function(object) TRUE
)

