### =========================================================================
### The saveRDS() generic
### -------------------------------------------------------------------------
###
### Need to explicitly define this generic otherwise the implicit generic
### in package "base" would dispatch on all its argument ('object', 'file',
### 'ascii', etc...). Here we set dispatch on the 1st arg (the 'object' arg)
### only!

setGeneric("saveRDS", signature="object")

