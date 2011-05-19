print.scaleCoef <-
function(x, ...){
    cat(paste("Scaled regression coefficients and ",
        x$CILevel*100,"% confidence intervals:\n \n",sep=""))
    cat("$scaledResults \n \n")
    print(x$scaledResults)
}
