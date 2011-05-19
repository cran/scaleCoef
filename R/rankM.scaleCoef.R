rankM.scaleCoef <-
function(x, ...){  
    # find out if there are multiple fits in x
    oneFit <- "estSE"%in%names(x$scaledResults)
    if(oneFit){
        if(!"matrix"%in%class(x$scaledResults$estSE)){
            scaledEsts <- x$scaledResults$estSE["Estimate"]
            rankNames <- names(x$scaledResults$scaleBy)
        }
        else{
            scaledEsts <- x$scaledResults$estSE[,"Estimate"]
            rankNames <- names(scaledEsts)
        }
    }
    else{
        # single scaled coefficient from each model
        if(!"matrix"%in%class(x$scaledResults[[1]]$estSE)){
            scaledEsts <- sapply(x$scaledResults, function(y) y$estSE)["Estimate",]
            unnamedFits <- x$scaledResults
            names(unnamedFits) <- NULL
            namesTemp <- names(sapply(unnamedFits, function(y) y$scaleBy))
            # single different coefficient from each model
            if(length(scaledEsts) == length(unique(namesTemp))){
                rankNames <- namesTemp
            }
            # single same coefficient from each model
            else{
                ifelse(is.null(names(x$scaledResults)), 
                    {fitNames <- paste("fit",1:length(x$scaledResults),sep="")}, 
                    {fitNames <- names(x$scaledResults)})
                rankNames <- paste( fitNames, namesTemp, sep=".")
            }
        }
        # mutiple scaled coefficients from each model
        else{
            scaledEsts <- matrix(
                            sapply(x$scaledResults, function(y) y$estSE[,"Estimate"]), 
                            nrow=1, byrow=FALSE
                            )
            namesTemp <- rownames(sapply(x$scaledResults, function(y) y$scaleBy))
            # the same set of coefficients for each model
            ifelse(is.null(names(x$scaledResults)), 
                {fitNames <- paste("fit",1:length(x$scaledResults),sep="")}, 
                {fitNames <- names(x$scaledResults)})
            rankNames <- paste( rep(fitNames, each=length(namesTemp)),
                                rep(namesTemp,length(x$scaledResults)), sep=".")
        }
    }
    out <- length(scaledEsts)+1-rank(abs(scaledEsts),...)
    names(out) <- rankNames
    return(out)
}

