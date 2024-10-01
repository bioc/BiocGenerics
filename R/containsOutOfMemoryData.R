### =========================================================================
### The containsOutOfMemoryData() generic
### -------------------------------------------------------------------------

setGeneric("containsOutOfMemoryData",
    function(object) standardGeneric("containsOutOfMemoryData")
)

.S4_object_contains_out_of_memory_data <- function(object)
{
    slot_names <- slotNames(class(object))
    for (name in slot_names) {
        if (containsOutOfMemoryData(slot(object, name)))
            return(TRUE)
    }
    FALSE
}

.list_contains_out_of_memory_data <- function(object)
{
    for (x in object) {
        if (containsOutOfMemoryData(x))
            return(TRUE)
    }
    FALSE
}

.environment_contains_out_of_memory_data <- function(object)
{
    for (name in names(object)) {
        if (containsOutOfMemoryData(object[[name]]))
            return(TRUE)
    }
    FALSE
}

setMethod("containsOutOfMemoryData", "ANY",
    function(object)
    {
        if (isS4(object))
            return(.S4_object_contains_out_of_memory_data(object))
        FALSE
    }
)

setMethod("containsOutOfMemoryData", "list",
    .list_contains_out_of_memory_data
)

setMethod("containsOutOfMemoryData", "environment",
    .environment_contains_out_of_memory_data
)

setClass("OutOfMemoryObject", representation("VIRTUAL"))

setMethod("containsOutOfMemoryData", "OutOfMemoryObject",
    function(object) TRUE
)

