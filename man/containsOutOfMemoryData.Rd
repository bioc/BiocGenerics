\name{containsOutOfMemoryData}

\alias{containsOutOfMemoryData}
\alias{containsOutOfMemoryData,ANY-method}

\title{Does an object contain out-of-memory data?}

\description{
  Some objects in Bioconductor can use on-disk or other out-of-memory
  representation for their data, typically (but not necessarily) when
  the data is too big to fit in memory. For example the data in a
  \link[GenomicFeatures]{TxDb} object is stored in an SQLite database,
  and the data in an \link[HDF5Array]{HDF5Array} object is stored in
  an HDF5 file.

  The \code{containsOutOfMemoryData()} function determines whether an
  object contains out-of-memory data or not.

  Note that objects with out-of-memory data are usually not compatible
  with a serialization/unserialization cycle. More concretely,
  \code{base::\link[base]{saveRDS}()}/\code{base::\link[base]{readRDS}()}
  tend to silently break them!

  See \code{?\link[HDF5Array]{saveHDF5SummarizedExperiment}} in the
  \pkg{HDF5Array} package for a more extensive discussion about this.
}

\usage{
containsOutOfMemoryData(object)
}

\arguments{
  \item{object}{
    The object to be tested.
  }
}

\details{
  Note that an object can store \emph{some} of its data on disk
  and \emph{some} of it in memory. This is the case for example when a
  \link[SummarizedExperiment]{SummarizedExperiment} object (or derivative)
  has some of its assays on disk (e.g. in \link[HDF5Array]{HDF5Matrix}
  objects) and others in memory (e.g. in ordinary matrices and/or
  \link[SparseArray]{SparseMatrix} objects).

  Of course in this case, \code{containsOutOfMemoryData()} will still return
  \code{TRUE}. In other words, \code{containsOutOfMemoryData(object)} will
  only return \code{FALSE} when all the data in \code{object} resides in
  memory, that is, when the object can safely be serialized.
}

\value{
  \code{TRUE} or \code{FALSE}.
}

\seealso{
  \itemize{
    \item \code{\link[methods]{showMethods}} for displaying a summary of the
          methods defined for a given generic function.

    \item \code{\link[methods]{selectMethod}} for getting the definition of
          a specific method.

    \item \link[HDF5Array]{containsOutOfMemoryData,HDF5ArraySeed-method}
          in the \pkg{HDF5Array} package for an example of a specific
          \code{containsOutOfMemoryData} method (defined for
          \link[HDF5Array]{HDF5ArraySeed} objects).

    \item \link{BiocGenerics} for a summary of all the generics defined
          in the \pkg{BiocGenerics} package.
  }
}

\examples{
containsOutOfMemoryData
showMethods("containsOutOfMemoryData")
selectMethod("containsOutOfMemoryData", "ANY")  # the default method

m <- matrix(0, nrow=7, ncol=10)
m[sample(length(m), 20)] <- runif(20)
containsOutOfMemoryData(m)  # FALSE

library(SparseArray)
svt <- as(m, "SparseArray")
svt
containsOutOfMemoryData(m)  # FALSE

library(HDF5Array)
M <- as(m, "HDF5Array")
M
containsOutOfMemoryData(M)  # TRUE
}

\keyword{methods}
