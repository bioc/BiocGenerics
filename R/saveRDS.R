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
        if (containsOutOfMemoryData(object))
            warning("object contains out-of-memory data so cannot ",
                    "be serialized reliably")
        base::saveRDS(object, file=file, ascii=ascii, version=version,
                      compress=compress, refhook=refhook)
    }
)

