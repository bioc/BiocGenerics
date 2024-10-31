\name{setops}

\alias{setops}
\alias{sets}

\alias{union}
\alias{intersect}
\alias{setdiff}
\alias{setequal}

\title{Set operations}

\description{
  Performs \emph{set} union, intersection, (asymmetric!) difference, and
  equality on two or more vector-like objects.

  NOTE: This man page is for the \code{union}, \code{intersect},
  \code{setdiff}, and \code{setequal} \emph{S4 generic functions} defined
  in the \pkg{BiocGenerics} package.
  See \code{?generics::\link[generics]{union}} for the default methods
  (defined in CRAN package \pkg{generics}).
  Bioconductor packages can define specific methods for objects
  (typically vector-like) not supported by the default methods.
}

\usage{
union(x, y, ...)
intersect(x, y, ...)
setdiff(x, y, ...)
setequal(x, y, ...)
}

\arguments{
  \item{x, y}{
    Vector-like objects (typically of the same class, but not necessarily).
  }
  \item{...}{
    Additional arguments, for use in specific methods.
  }
}

\value{
  See \code{?generics::\link[generics]{union}} in CRAN package \pkg{generics}
  for the value returned by the default methods.

  Specific methods defined in Bioconductor packages will typically act as
  \emph{endomorphisms}, that is, they'll return an object of the same class
  as the input objects.
}

\note{
  The default S4 methods for these S4 generics are the
  \code{\link[generics]{union}}, \code{\link[generics]{intersect}},
  \code{\link[generics]{setdiff}}, and \code{\link[generics]{setequal}}
  functions defined in CRAN package \pkg{generics}, which are themselves
  \emph{S3 generic functions}.
  These S3 generics in turn have default methods that simply call
  the corresponding base R function i.e. the \code{\link[base]{union}},
  \code{\link[base]{intersect}}, \code{\link[base]{setdiff}}, or
  \code{\link[base]{setequal}} function defined in the \pkg{base} package.
  See for example \code{generics:::union.default}.

  Note that the base R functions only take 2 arguments. However, the S3
  generics in CRAN package \pkg{generics}, and the S4 generics in
  \pkg{BiocGenerics}, add \code{...} (a.k.a. ellipsis) to the argument list.
  This allow these generics to be called with an arbitrary number of
  effective arguments.

  For \code{union} or \code{intersect}, this means that Bioconductor
  packages can implement \emph{N-ary} union or intersection operations,
  that is, methods that compute the union or intersection of more
  than 2 objects.

  However, for \code{setdiff} and \code{setequal}, which are conceptually
  binary-only operation, the presence of the ellipsis typically allows methods
  to support extra arguments to control/alter the behavior of the operation.
  Like for example the \code{ignore.strand} argument supported by the
  \code{setdiff} method for \link[GenomicRanges]{GenomicRanges} objects
  (defined in the \pkg{GenomicRanges} package). (Note that the \code{union}
  and \code{intersect} methods for those objects also support the
  \code{ignore.strand} argument.)
}

\seealso{
  \itemize{
    \item \code{generics::\link[generics]{union}} for the default \code{union},
          \code{intersect}, \code{setdiff}, and \code{setequal} S4 methods
          defined in CRAN package \pkg{generics}.

    \item \code{\link[methods]{showMethods}} for displaying a summary of the
          methods defined for a given generic function.

    \item \code{\link[methods]{selectMethod}} for getting the definition of
          a specific method.

    \item \link[GenomicRanges]{union,GenomicRanges,GenomicRanges-method} in
          the \pkg{GenomicRanges} package for examples of specific
          \code{union}, \code{intersect}, and \code{setdiff} methods (defined
          for \link[GenomicRanges]{GenomicRanges} objects).

    \item \link{BiocGenerics} for a summary of all the generics defined
          in the \pkg{BiocGenerics} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## union()
## ---------------------------------------------------------------------

## S4 generic:
union  # note the dispath on 'x' and 'y'

showMethods("union")

## The default S4 method is an S3 generic function defined in
## CRAN package generics:
selectMethod("union", c("ANY", "ANY"))

## The default S3 method just calls base::union():
generics:::union.default

## ---------------------------------------------------------------------
## intersect()
## ---------------------------------------------------------------------

## S4 generic:
intersect  # note the dispath on 'x' and 'y'

showMethods("intersect")

## The default S4 method is an S3 generic function defined in
## CRAN package generics:
selectMethod("intersect", c("ANY", "ANY"))

## The default S3 method just calls base::intersect():
generics:::intersect.default

## ---------------------------------------------------------------------
## setdiff()
## ---------------------------------------------------------------------

## S4 generic:
setdiff  # note the dispath on 'x' and 'y'

showMethods("setdiff")

## The default S4 method is an S3 generic function defined in
## CRAN package generics:
selectMethod("setdiff", c("ANY", "ANY"))

## The default S3 method just calls base::setdiff():
generics:::setdiff.default

## ---------------------------------------------------------------------
## setequal()
## ---------------------------------------------------------------------

## S4 generic:
setequal  # note the dispath on 'x' and 'y'

showMethods("setequal")

## The default S4 method is an S3 generic function defined in
## CRAN package generics:
selectMethod("setequal", c("ANY", "ANY"))

## The default S3 method just calls base::setequal():
generics:::setequal.default
}

\keyword{methods}
