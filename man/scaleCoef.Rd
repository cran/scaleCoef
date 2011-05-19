\name{scaleCoef}
\alias{scaleCoef}
\alias{print.scaleCoef}
\title{Post-hoc scaling of regression coefficients}
\description{
Returns post-hoc scaled regression coefficients and confidence intervals (CI) 
for specified predictor variable(s) from fitted regression models.
}

\usage{
  scaleCoef(fit, scaleVarName, scaleBy = sd, diffAcrossModels = FALSE, CILevel = 0.95)
}

\arguments{
  \item{fit}{
    a single model fit object (or a list of such objects) of class 
    \dQuote{lm}, \dQuote{glm}, \dQuote{gam} or \dQuote{lme}
  }
  \item{scaleVarName}{
    character string vector of unique predictor variable name(s) whose regression 
    coefficient(s) are to be rescaled
  }
  \item{scaleBy}{
    vector of constants with same length as scaleVarName, or
    existing function (e.g., \code{sd}, \code{IQR}, \code{range}, etc...), or
    user defined function (e.g., \code{function(x){2*sd(x)}})
  }
  \item{diffAcrossModels}{
    logical, if TRUE: specifying multiple models and scaling 
    coefficients for a different predictor variable in each 
    (\code{length(fit)} must equal \code{length(scaleVarName)})
  }
  \item{CILevel}{
    confidence level for confidence intervals (default is \code{0.95} corresponding to 95\% CI)
  }
}

\details{
The \code{scaleCoef} function will readily return post-hoc scaled regression coefficients in three
scenarios for pre-existing fitted model objects:
(1) a single model fit; 
(2) a list of model fits, each fit to the same data, where the regression coefficients of interest 
are on the \emph{same} predictor variable in each model fit; or 
(3) a list of model fits, each fit to the same data, where the regression coefficients of interest are 
on a \emph{different} predictor variable in each model fit.
}

\value{
\item{scaledResults}{A list whose length matches the number of scaled regression coefficients. 
Each component of \code{scaledResults} is itself a list containing three components: 
\code{scaleBy}, the numerical quantity that was used to scale the regression coefficient;
\code{estSE}, the scaled regression coefficient and the corresponding scaled standard error; and
\code{CI}, the scaled confidence interval.
}
\item{CILevel}{The value of \code{CILevel} specified in the call to \code{scaleCoef}}
}

\author{Sandrah P. Eckel}

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
}

\keyword{models}

