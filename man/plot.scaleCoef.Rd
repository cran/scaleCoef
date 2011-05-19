\name{plot.scaleCoef}
\alias{plot.scaleCoef}
\title{Plot scaled regression coefficients}
\description{
Side by side plot of scaled regression coefficients from a \code{scaleCoef} object.
}
\usage{
\method{plot}{scaleCoef}(x, add = FALSE, offsetby = 0, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, xaxt = NULL, yaxt = NULL, horizontal = FALSE, firstToLast = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{scaleCoef} object}
  \item{add}{logical, if true add plot to current plot, default=FALSE}
  \item{offsetby}{amount on x-axis (or y-axis if horizontal=TRUE) by which to offset the plotted estimates and confidence intervals, useful when add is TRUE}
  \item{xlim}{the x limits (x1, x2) of the plot}
  \item{ylim}{the y limits (y1, y2) of the plot}
  \item{xlab}{a label for the x axis, defaults to \dQuote{Predictor variable(s)}}
  \item{ylab}{a label for the y axis, defaults to \dQuote{Scaled regression coefficient}}
  \item{xaxt}{specify \dQuote{n} to add the x-axis later yourself}
  \item{yaxt}{specify \dQuote{n} to add the y-axis later yourself}
  \item{horizontal}{logical, when FALSE predictors will be plotted on the x-axis and scaled regression coefficient values on the y-axis, default=FALSE}
  \item{firstToLast}{logical, when FALSE, plots variables last to first instead of first to last, default=TRUE}
  \item{\dots}{optional additional arguments}
}

\author{Sandrah P. Eckel}

\seealso{\code{\link{scaleCoef}}}

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

# IQR to SD ratio
IQR(X1)/sd(X1); IQR(X2)/sd(X2); IQR(X3)/sd(X3)
# R2 of each model
summary(fit1)$r.s; summary(fit2)$r.s; summary(fit3)$r.s


sc2SD <- scaleCoef( fit = list(fit1,fit2,fit3), 
                    scaleVarName = c("X1","X2","X3"), 
                    scaleBy = function(x) 2*sd(x), 
                    diffAcrossModels = TRUE)
scIQR <- scaleCoef( fit = list(fit1,fit2,fit3), 
                    c("X1","X2","X3"), 
                    scaleBy = IQR, 
                    diffAcrossModels = TRUE)

par(mfrow=c(1,2))
boxplot(cbind(X1,X2,X3),at=3:1,
        xlab="x", main="Distribution of X",horizontal=TRUE)    
plot(scIQR, offsetby=.08, pch=1, ylim=c(0.6,3.8), xlim=c(0,2), ylab="", 
    main="Scaled regression coefficients", horizontal=TRUE, firstToLast=FALSE)
plot(sc2SD, add=TRUE, offsetby=-.08, pch=4, horizontal=TRUE, firstToLast=FALSE)
legend("topright",bty="n", lty=1, pch=c(1,4), c("IQR scaling","2s scaling")) 
}

\keyword{hplot}
\keyword{aplot}
\keyword{methods}
