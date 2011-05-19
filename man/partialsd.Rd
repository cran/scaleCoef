\name{partialsd}
\alias{partialsd}
\title{Partial standard deviation}
\description{
 Partial standard deviation (sd) calculated using Equation 3.3 in Bring (1994). 
}
\usage{
 partialsd(x, fit, xname)
}
\arguments{
  \item{x}{a numeric vector containing data for predictor of interest}
  \item{fit}{a multiple linear regression model fit object of class "lm" 
        that has x as one of several predictors (no interactions)}
  \item{xname}{character string name of x used in fit model specification}
}
\details{
 In multiple linear regression, the partial standard deviation of a predictor of 
 interest x is defined by Bring (1994) as the square root of the mean squared
 error of the residuals of x regressed on the other predictors. 
}
\value{
Returns a numeric vector of length 1. 
}
\references{
 Bring, J.(1994) How to Standardize Regression Coefficients. 
 \emph{The American Statistician}. 48(3):209-213.
}
\author{Sandrah P. Eckel}
\note{
 The variance inflation factor (VIF) required for Equation 3.3 in Bring (1994) 
 is calculating using the \code{vif()} function from the \code{\link{car}} package.
}
\seealso{
 \code{\link{sd}} for the usual calculation of sd. \code{partialsd} may be  
 particularly useful with \code{\link{scaleCoef}}.
}
\examples{
 # generate data and fit a multiple linear regression
 x1 <- rnorm(100)
 x2 <- x1 + rnorm(100)
 y <- 1 + .4*x1 + .2*x2 + rnorm(100)
 fit <- lm(y~x1+x2)
 # calculate two versions of the sd of x1
 sd(x1)
 partialsd(x1,fit,"x1")
}
\keyword{models}
\keyword{univar}
