\name{saveRDS}

\alias{saveRDS}
\alias{saveRDS,ANY-method}

\title{The saveRDS() S4 generic and default method}

\description{
  Generic function to write a single R object to a file.

  NOTE: This man page is for the \code{saveRDS} \emph{S4 generic function}
  and default method defined in the \pkg{BiocGenerics} package.
  See \code{?base::\link[base]{saveRDS}} for the corresponding function
  defined in base R.
}

\usage{
saveRDS(object, file="", ascii=FALSE, version=NULL,
        compress=TRUE, refhook=NULL)
}

\details{
  The default \code{saveRDS} method defined in this package is a thin
  wrapper around \code{base::\link[base]{saveRDS}} that issues a warning
  if the object to serialize contains out-of-memory data.
  See \code{?\link{containsOutOfMemoryData}} for more information.

  Bioconductor packages can override this default method with more
  specialized methods.
}

\arguments{
  \item{object, file, ascii, version, compress, refhook}{
    See \code{?base::\link[base]{saveRDS}} for a description of
    these arguments.
  }
}

\value{
  An invisible \code{NULL}.
}

\seealso{
  \itemize{
    \item \code{base::\link[base]{saveRDS}} in the \pkg{base} package
          for the default \code{saveRDS} method.

    \item \code{\link{containsOutOfMemoryData}} for determining whether
          an object contains out-of-memory data or not.

    \item \code{\link[methods]{showMethods}} for displaying a summary of the
          methods defined for a given generic function.

    \item \code{\link[methods]{selectMethod}} for getting the definition of
          a specific method.

    \item \link[SummarizedExperiment]{saveRDS,SummarizedExperiment-method}
          in the \pkg{SummarizedExperiment} package for an example
          of a specific \code{saveRDS} method (defined for
          \link[SummarizedExperiment]{SummarizedExperiment} objects
          and derivatives).

    \item \link{BiocGenerics} for a summary of all the generics defined
          in the \pkg{BiocGenerics} package.
  }
}

\examples{
saveRDS  # note the dispatch on the 'object' arg only
showMethods("saveRDS")
selectMethod("saveRDS", "ANY")  # the default method
}

\keyword{methods}
