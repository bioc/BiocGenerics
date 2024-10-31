### =========================================================================
### The union(), intersect(), setdiff(), and setequal() generics
### -------------------------------------------------------------------------
###
### The default methods are defined in CRAN package generics.

setGeneric("union",
    function(x, y, ...) standardGeneric("union"),
    useAsDefault=generics::union
)

setGeneric("intersect",
    function(x, y, ...) standardGeneric("intersect"),
    useAsDefault=generics::intersect

)

setGeneric("setdiff",
    function(x, y, ...) standardGeneric("setdiff"),
    useAsDefault=generics::setdiff
)

setGeneric("setequal",
    function(x, y, ...) standardGeneric("setequal"),
    useAsDefault=generics::setequal
)

