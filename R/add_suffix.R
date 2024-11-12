### =========================================================================
### The add_prefix() and add_suffix() generics
### -------------------------------------------------------------------------
###

setGeneric("add_prefix", signature="x",
    function(x, prefix="") standardGeneric("add_prefix")
)

setGeneric("add_suffix", signature="x",
    function(x, suffix="") standardGeneric("add_suffix")
)

### Recycling: Like with arithmetic operations (+, *, etc...), the longest
### argument wins i.e. the shortest argument is recycle to the length of
### the longest. Except when one of the two arguments has length 0, in which
### case a zero-length vector is returned.
### Note that the current implementation does not issue a "longer object
### length is not a multiple of shorter object length" warning which deviates
### slightly from the behavior of arithmetic operations in base R.
### It would be easy to "fix" this though.
###
### Name propagation: The longest argument also wins. If the 2 arguments
### have the same length then the names on the first argument are propagated,
### if any. Otherwise the names on the second argument are propagated, if
### any.
.add_prefix_or_suffix <- function(x, prefix="", is.suffix=FALSE)
{
    ## Turn both arguments into character vectors.
    if (!is.character(x))
        x <- setNames(as.character(x), names(x))
    if (!is.character(prefix))
        prefix <- setNames(as.character(prefix), names(prefix))

    ## Zero-length case.
    if (length(x) == 0L) {
        if (length(prefix) == 0L && is.null(names(x)))
            names(x) <- names(prefix)
        return(x)
    }
    if (length(prefix) == 0L)
        return(prefix)

    ## Non zero-length case.
    ans <- if (is.suffix) paste0(x, prefix) else paste0(prefix, x)
    if (length(x) > length(prefix)) {
        ans_names <- names(x)
    } else if (length(x) < length(prefix)) {
        ans_names <- names(prefix)
    } else if (is.null(names(x))) {
        ans_names <- names(prefix)
    } else {
        ans_names <- names(x)
    }
    setNames(ans, ans_names)
}

setMethod("add_prefix", "vector",
    function(x, prefix="")
        .add_prefix_or_suffix(x, prefix=prefix)
)

setMethod("add_suffix", "vector",
    function(x, suffix="")
        .add_prefix_or_suffix(x, prefix=suffix, is.suffix=TRUE)
)

### TODO: Add methods for array objects.

