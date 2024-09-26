\name{saveRDS}

\alias{saveRDS}

\title{The saveRDS() S4 generic}

\description{
  Generic function to write a single R object to a file.

  NOTE: This man page is for the \code{saveRDS} \emph{S4 generic function}
  defined in the \pkg{BiocGenerics} package.
  See \code{?base::\link[base]{saveRDS}} for the default method
  (defined in the \pkg{base} package).

  Bioconductor packages can define specific methods for objects not
  supported by the default method.
}

\usage{
saveRDS(object, file="", ascii=FALSE, version=NULL,
        compress=TRUE, refhook=NULL)
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
