\name{rankM.scaleCoef}
\alias{rankM.scaleCoef}
\title{Rank by magnitude}
\description{
Produces a numeric vector of ranks by magnitude (absolute value) of scaled regression coefficients 
from a \code{scaleCoef} object.
}
\usage{
\method{rankM}{scaleCoef}(x, ...)
}

\arguments{
  \item{x}{\code{scaleCoef} object}
  \item{\dots}{optional additional arguments}
}

\value{
Returns a vector of ranks of the absolute values of the scaled regression coefficients
from the \code{scaleCoef} object, with the largest magnitude scaled regression coefficient 
having a rank of 1.
}
\author{Sandrah P. Eckel}

\note{
 Uses the \code{\link{rank}} function and will accept 
 the corresponding \code{ties.method} argument for options on 
 how to treat ties (default: average).
}

\seealso{
\code{\link{rank}}, \code{\link{rankM.default}}
}
\examples{
set.seed(1)
N <- 2001
X1 <- rnorm(N,50,8)
X2 <- rt(N, df=4) + 10
X3 <- rchisq(N,1)*10

Y <- 2 + 0.075*X1 + 0.5*X2 + 0.057*X3 + rnorm(N,0,4)

fit1 <- lm(Y~X1)
fit2 <- lm(Y~X2)
fit3 <- lm(Y~X3)

sc2SD <- scaleCoef( fit = list(fit1,fit2,fit3), 
                    scaleVarName = c("X1","X2","X3"), 
                    scaleBy = function(x) 2*sd(x), 
                    diffAcrossModels = TRUE)
scIQR <- scaleCoef( fit = list(fit1,fit2,fit3), 
                    c("X1","X2","X3"), 
                    scaleBy = IQR, 
                    diffAcrossModels = TRUE)
    
rankM(sc2SD)
rankM(scIQR)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\keyword{manip}
