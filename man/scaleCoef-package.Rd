\name{scaleCoef-package}
\alias{scaleCoef-package}
\docType{package}
\title{Tools for post-hoc scaling of regression coefficients}
\description{
 The \code{scaleCoef} package contains tools to scale regression coefficients
 from previously fitted model objects according to user-specified constants or
 functions of the predictor variables of interest. Generic S3 methods are 
 available to rank or plot the results. 
}
\details{
\tabular{ll}{
Package: \tab scaleCoef\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-05-19\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}

\author{
 Sandrah P. Eckel,
 Maintainer: Sandrah P. Eckel <eckel@usc.edu>
}

\references{
Eckel S.P., Gauderman W.J., Berhane K.T. (2011)
On scaling regression coefficients by interquartile
ranges for comparison. 
}

\keyword{package}

\seealso{
\code{\link{scale}},
\code{\link{sweep}},
\code{\link[arm]{standardize}},
\code{\link[QuantPsyc]{lm.beta}}
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

rankM(c(1,2,3))
rankM(c(1,2,-3)) 
    
rankM(sc2SD)
rankM(scIQR)

fit <- lm(Y~X1+X2+X3)
sd(X1)
partialsd(X1,fit,"X1")
}
