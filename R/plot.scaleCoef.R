plot.scaleCoef <-
function(x, 
                        add=FALSE,  # logical, if true add plot to current plot
                        offsetby=0, # amount on x-axis by which to offset the plotted 
                                    # estimates and confidence intervals, useful when add is TRUE
                        xlim=NULL,
                        ylim=NULL,
                        xlab=NULL,
                        ylab=NULL,
                        xaxt=NULL,  # specify "n" to add later yourself
                        yaxt=NULL,  # specify "n" to add later yourself
                        horizontal=FALSE,   # FALSE: predictors will be plotted
                                            # on x-axis, scaled coefficient values on y-axis
                        firstToLast=TRUE,   # when FALSE, plots variables last to first 
                                            # instead of first to last
                        ...
                        ){
    # specify axis for predictor variables
    ifelse(horizontal, predaxis <- 2, predaxis <- 1)
    # if null, change predictor axt to "n", since label according to variable names
    if(horizontal){
        if(is.null(yaxt)) {
            yaxt <- "n"
            predaxt <- TRUE
        }
    }
    else{
        if(is.null(xaxt)) {
            xaxt <- "n"
            predaxt <- TRUE
        }
    }
    # if no specified axt, change to default
    if(is.null(xaxt)) xaxt="s"
    if(is.null(yaxt)) yaxt="s"
    if(!exists("predaxt"))  predaxt <- FALSE

    # specify x and y axis labels, depending on horizontal
    if(horizontal){
        if(is.null(ylab)) ylab <- "Predictor variable(s)"
        if(is.null(xlab)) xlab <- "Scaled regression coefficient"
    }
    else{
        if(is.null(xlab)) xlab <- "Predictor variable(s)"
        if(is.null(ylab)) ylab <- "Scaled regression coefficient"
    }
    # find out if there are multiple fits in x
    oneFit <- "estSE"%in%names(x$scaledResults)
    # one fit: plot scaled coefficients from same model in a single plot
    if(oneFit){
        num <- length(x$scaledResults$scaleBy)
        if(firstToLast){
            predAt <- 1:num
        }
        else{
            predAt <- num:1
        }
        # detemine coefficient axis limits
        if(!is.null(x$scaledResults$CI)){
            ub <- max(x$scaledResults$CI)
            lb <- min(x$scaledResults$CI)
        }
        else{
            ub <- max(x$scaledResults$estSE)
            lb <- min(x$scaledResults$estSE)
            warning("Confidence interval(s) omitted from plot")            
        }
        if(horizontal){
            if(is.null(xlim)) xlim <- c(lb,ub)
        }
        else{
            if(is.null(ylim)) ylim <- c(lb,ub)
        }
        # determine predictor axis limits
        if(horizontal){
            if(is.null(ylim)) ylim <- c(.5, num +.5)
        }
        else{
            if(is.null(xlim)) xlim <- c(.5, num +.5)
        } 
        if(!add){
            plot(1,1,ylim=ylim,xlim=xlim, type="n", xaxt=xaxt, yaxt=yaxt, 
                xlab=xlab, ylab=ylab, ...)
            if(predaxt) {axis(predaxis, at=predAt, names(x$scaledResults$scaleBy))}
        }
        if(horizontal){
            if(!"matrix"%in%class(x$scaledResults$estSE)){
                xplot1 <- x$scaledResults$estSE["Estimate"]
                xplot2 <- x$scaledResults$CI["2.5 %"]
                xplot3 <- x$scaledResults$CI["97.5 %"]
            }
            else{
                xplot1 <- x$scaledResults$estSE[,"Estimate"]
                xplot2 <- x$scaledResults$CI[,"2.5 %"]
                xplot3 <- x$scaledResults$CI[,"97.5 %"]
            }
            yplot1 <- predAt+offsetby
            yplot2 <- predAt+offsetby
            yplot3 <- predAt+offsetby
        }
        else{
            if(!"matrix"%in%class(x$scaledResults$estSE)){
                yplot1 <- x$scaledResults$estSE["Estimate"]
                yplot2 <- x$scaledResults$CI["2.5 %"]
                yplot3 <- x$scaledResults$CI["97.5 %"]
            }
            else{
                yplot1 <- x$scaledResults$estSE[,"Estimate"]
                yplot2 <- x$scaledResults$CI[,"2.5 %"]
                yplot3 <- x$scaledResults$CI[,"97.5 %"]
            }
            xplot1 <- predAt+offsetby
            xplot2 <- predAt+offsetby
            xplot3 <- predAt+offsetby
        }
        points(     xplot1,yplot1, ...)
        if(!is.null(x$scaledResults$CI)){
            segments(   xplot2,yplot2,
                        xplot3,yplot3, ...)
        }
    }
    # multiple fits, plotting depends on whether diffAcrossModels
    if(!oneFit){
        numFit <- length(x$scaledResults)
        if(is.null(names(x$scaledResults))){ 
            fitNames <- paste("fit",1:length(x$scaledResults),sep="")
        }
        else{ 
            fitNames <- names(x$scaledResults)
        }
        # matrix containing all CI
        CIs <- NULL 
        for(f in 1:numFit){
            fitCI <- x$scaledResults[[f]]$CI
            if(is.null(fitCI)) fitCI <- c(NA,NA)
            CIs <- rbind(CIs, fitCI)
        }
        if(any(is.na(CIs))) warning("Confidence interval(s) omitted from plot")
        # detemine coefficient axis limits
        ub <- max(CIs, na.rm=TRUE)
        lb <- min(CIs, na.rm=TRUE)
        # assign coefficient axis limits
        if(horizontal){
            if(is.null(xlim)){ xlim <- c(lb,ub)}
        }
        else{
            if(is.null(ylim)){ ylim <- c(lb,ub) }
        }
        # identify whether diffAcrossModels
        vars <- sapply(x$scaledResults, function(y) names(y$scaleBy))
        if(!is.matrix(vars) & length(unique(vars))!=1){
            diffAcrossModels <- TRUE
        }
        else(
            diffAcrossModels <- FALSE
        )
        # when diffAcrossModels=TRUE OR only one coef in all models, make one plot
        if(diffAcrossModels | (!diffAcrossModels & length(unique(vars))==1)){
            if(length(unique(vars))==1){
                vars <- paste(fitNames,vars,sep=".")
            }
            # possibly change order of plotting
            if(firstToLast){
                predAt <- 1:numFit
            }
            else{
                predAt <- numFit:1
            }
            # determine predictor axis limits
            if(horizontal){
                if(is.null(ylim)) ylim <- c(.5, numFit +.5)
            }
            else{
                if(is.null(xlim)) xlim <- c(.5, numFit +.5)
            } 
            if(!add){
                plot(1,1,ylim=ylim, xlim=xlim, type="n", xaxt=xaxt, yaxt=yaxt, 
                    xlab=xlab, ylab=ylab, ...)
                if(predaxt) {axis(predaxis, at=predAt, vars)}
            }
            for(f in 1:numFit){
                if(horizontal){
                    yplot1 <- predAt[f]+offsetby
                    yplot2 <- predAt[f]+offsetby
                    yplot3 <- predAt[f]+offsetby
                    xplot1 <- x$scaledResults[[f]]$estSE["Estimate"]
                    xplot2 <- CIs[f,"2.5 %"]
                    xplot3 <- CIs[f,"97.5 %"]
                }
                else{
                    xplot1 <- predAt[f]+offsetby
                    xplot2 <- predAt[f]+offsetby
                    xplot3 <- predAt[f]+offsetby
                    yplot1 <- x$scaledResults[[f]]$estSE["Estimate"]
                    yplot2 <- CIs[f,"2.5 %"]
                    yplot3 <- CIs[f,"97.5 %"]
                }
                points(     xplot1,yplot1, ...)
                segments(   xplot2,yplot2,
                            xplot3,yplot3, ...)

            }
        }            
        # otherwise, make numFit plots, can't add to previous plots
        else{ 
            if(add) stop("Can't add to multiple previously constructed plots")           
            vars <- names(x$scaledResults[[1]]$scaleBy)
            num <- length(vars)
            # possibly change order of plotting
            if(firstToLast){
                predAt <- 1:num
            }
            else{
                predAt <- num:1
            }   
            # determine predictor axis limits
            if(horizontal){
                if(is.null(ylim)) ylim <- c(.5, num +.5)
            }
            else{
                if(is.null(xlim)) xlim <- c(.5, num +.5)
            }
            par(mfrow=c(numFit,1))
            # possibly change order of plotting
            if(firstToLast){
                predAt <- 1:num
            }
            else{
                predAt <- num:1
            }
            for(v in 1:numFit){
                plot(1,1,ylim=ylim,xlim=xlim, type="n", xaxt=xaxt, yaxt=yaxt, 
                    main=paste("Model:",fitNames[v]), xlab=xlab, ylab=ylab, ...)
                if(predaxt) {axis(predaxis, at=predAt, vars)}
                for(f in 1:num){
                    if(horizontal){
                        yplot1 <- predAt[f]+offsetby
                        yplot2 <- predAt[f]+offsetby
                        yplot3 <- predAt[f]+offsetby
                        xplot1 <- x$scaledResults[[v]]$estSE[f,"Estimate"]
                        if(!is.null(x$scaledResults[[v]]$CI)){
                            xplot2 <- x$scaledResults[[v]]$CI[f,"2.5 %"]
                            xplot3 <- x$scaledResults[[v]]$CI[f,"97.5 %"]
                        }
                        else{
                            xplot2 <- NA
                            xplot3 <- NA
                        }
                    }
                    else{
                        xplot1 <- predAt[f]+offsetby
                        xplot2 <- predAt[f]+offsetby
                        xplot3 <- predAt[f]+offsetby
                        yplot1 <- x$scaledResults[[v]]$estSE[f,"Estimate"]
                        if(!is.null(x$scaledResults[[v]]$CI)){
                            yplot2 <- x$scaledResults[[v]]$CI[f,"2.5 %"]
                            yplot3 <- x$scaledResults[[v]]$CI[f,"97.5 %"]
                        }
                        else{
                            yplot2 <- NA
                            yplot3 <- NA
                        }
                    }
                    points(     xplot1,yplot1, ...)
                    segments(   xplot2,yplot2,
                                xplot3,yplot3, ...)
                }
            }
            par(mfrow=c(1,1))
        }
    }            
}

