\name{version}

\alias{version}

\title{Get the version of an object}

\description{
  Get the "version" of an object.

  Note that the S4 class system does not define the notion of "version" of
  an object, nor does it provide any recommendation for how to implement
  this feature. So it's entirely up to the authors/developers of an S4 class
  to decide whether they want to support the notion of "version" for their
  objects or not, and to come up with their own approach for storing and
  retrieving this information.
}

\usage{
version(object, ...)
}

\arguments{
  \item{object}{
    An object for which the notion of "version" is defined.
  }
  \item{...}{
    Additional arguments, for use in specific methods.
  }
}

\value{
  What kind of value gets returned is up to the authors/developers of
  a particular \code{version()} method. Please check the man page of the
  method for the details.
}

\seealso{
  \itemize{
    \item \code{\link[methods]{showMethods}} for displaying a summary of the
          methods defined for a given generic function.

    \item \code{\link[methods]{selectMethod}} for getting the definition of
          a specific method.

    \item \link[msa]{version,MsaMetaData-method} in the \pkg{msa} package
          for an example of a specific \code{version} method (defined for
          \link[msa]{MsaMetaData} objects).

    \item \link{BiocGenerics} for a summary of all the generics defined
          in the \pkg{BiocGenerics} package.
  }
}

\examples{
version
showMethods("version")

library(msa)
showMethods("version")
selectMethod("version", "MsaMetaData")
}

\keyword{methods}
