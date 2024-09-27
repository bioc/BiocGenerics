### =========================================================================
### The saveRDS() generic
### -------------------------------------------------------------------------
###
### Need to explicitly define this generic otherwise the implicit generic
### in package "base" would dispatch on all its argument ('object', 'file',
### 'ascii', etc...). Here we set dispatch on the 1st arg (the 'object' arg)
### only!

setGeneric("saveRDS", signature="object")

### Note that this overwrites base::saveRDS()!
setMethod("saveRDS", "ANY",
    function(object, file="", ascii=FALSE, version=NULL,
             compress=TRUE, refhook=NULL)
    {
        ## Only a warning for now. Should we make this an error?
        if (containsOutOfMemoryData(object))
            warning("Object contains out-of-memory data so cannot be ",
                    "serialized reliably.\n  See '?containsOutOfMemoryData'.")
        base::saveRDS(object, file=file, ascii=ascii, version=version,
                      compress=compress, refhook=refhook)
    }
)

