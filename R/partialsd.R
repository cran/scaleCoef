partialsd <-
function(  x,      # a numeric vector containing data for predictor of interest

                        fit,    # a multiple linear regression model 
                                # fit object of class "lm" that has x as 
                                # one of several predictors (no interactions)
                                
                        xname   # character string name of x used in fit model specification
                        ){
    n <- length(x)
    k <- length(vif(fit))
    sd(x)/sqrt(vif(fit)[xname]) * sqrt((n-1) / (n - k))
}

