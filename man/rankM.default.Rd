\name{rankM.default}
\alias{rankM.default}
\alias{rankM}
\title{Rank by magnitude}
\description{Produces a numeric vector of ranks by magnitude (absolute value) of values of the input vector.}
\usage{
\method{rankM}{default}(x, ...)
}

\arguments{
  \item{x}{numeric vector}
  \item{\dots}{optional additional arguments}
}

\details{
 \code{rankM} is a generic S3 method.  \code{rankM.default} accepts a numeric vector and 
 returns a vector of ranks of the absolute values of each element from the input vector,
 with the largest value having a rank of 1. 
}

\value{Returns a numeric vector of ranks.}

\author{Sandrah P. Eckel}

\note{
 Uses the \code{\link{rank}} function and will accept 
 the corresponding \code{ties.method} argument for options on 
 how to treat ties (default: average).
}

\seealso{\code{\link{rank}},\code{\link{rankM.scaleCoef}}}

\examples{
 rankM(c(1,2,3))
 rankM(c(1,2,-3))
 rankM(c(1,1,2,-3))
 rankM(c(1,1,2,-3),ties.method="max")
}

\keyword{methods}
\keyword{manip}

