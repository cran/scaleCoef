scaleCoef <-
function(
    fit,                    # a single model fit object (or a list of such objects) of class 
                            # "lm", "glm", "gam" or "lme" 

    scaleVarName,           # character string vector of unique variable name(s) 
                            # whose regression coefficient(s) are to be rescaled

    scaleBy=sd,             # vector of constants with same length as scaleVarName, or
                            # existing function (e.g., sd, IQR, range, etc...), or
                            # user defined function (e.g., function(x){2*sd(x)})

    diffAcrossModels=FALSE, # logical, if TRUE: specifying multiple models and scaling 
                            # coefficients for a different variable in each 
                            # (required that length(fit)=length(scaleVarName))

    CILevel=0.95            # confidence level for confidence intervals (default is 95% CI)
    ){
    # basic error check for unique variable names
    if(length(scaleVarName)!=length(unique(scaleVarName))){
        stop("scaleVarName should be a vector of unique variable names")
    }
    # if fit contains a single model fit object, make it into a list of length 1 
    isListFit <- "list"%in%class(fit)
    if(!isListFit) fit <- list(fit)
    # prepare object to be returned at end of function: fitout
    fitout <- vector("list", length=length(fit))
    names(fitout) <- names(fit)

    # check length requirement when scaling coef on different variables in multiple models
    if(diffAcrossModels & length(fit)!=length(scaleVarName)){
        stop("number of models in fit doesn't match with number of variables in scaleVarName")
    }
    
    #######################################################################
    # cycle through each object in fit
    scaleVarNamef <- scaleVarName
    for (f in 1:length(fit)){
        if(diffAcrossModels) {scaleVarNamef <- scaleVarName[f]}
        # ensure that variable of interest appears only once in model formula
        for(j in 1:length(scaleVarNamef)){
            formulaTerms <- unlist(strsplit(as.character(formula(fit[[f]])[3]),"\\+"))
            appearances <- grep(scaleVarNamef[j],formulaTerms)
            # variable of interest does not appear! 
            if(length(appearances)==0) stop("Variable of interest not a predictor in model")
            if(length(appearances)>1) stop("Variable of interest appears more than once in model")
        }
        #######################################################################
        # identify the regression coefficient estimate and standard errror (SE)
        # gam fits
        if("gam"%in%class(fit[[f]])){estSE <- summary(fit[[f]])$p.table[scaleVarNamef,c("Estimate","Std. Error")]}
        # lme fits
        if("lme"%in%class(fit[[f]])){estSE <- summary(fit[[f]])$tTable[scaleVarNamef,c("Value","Std.Error")]} 
        # lm,glm fits
        if("lm"%in%class(fit[[f]]) & !"gam"%in%class(fit[[f]])){
                                estSE <- summary(fit[[f]])$coef[scaleVarNamef,c("Estimate","Std. Error")]
        }
        # return an error if the fit[[f]] object is not one of: gam, lme, lm or glm
        if(!exists("estSE")) stop("fit[[f]] object class not supported, rescale manually")
        #########################################################################
        # if scaleBy is a function, calculate the quantity(ies) by which to scale 
        if("function"%in%class(scaleBy)){
            # identify data specified in function call for fit[[f]]
            fitdata <- as.list(fit[[f]]$call)$data
            # if data specified, run scaleBy function
            if(!is.null(fitdata)){
                scaleByOut <- scaleBy(get(as.character(fitdata))[,scaleVarNamef])
            }
            # if no data specified, look for scaleVarNamef in working environment
            # then run scaleBy function
            if(is.null(fitdata)){
                if(!exists(scaleVarNamef)){
                    stop("ScaleVarName not in working environment, specify a constant for scaleBy")
                }
                scaleVar <- NULL
                for(v in 1:length(scaleVarNamef)){
                    scaleVar <- cbind(scaleVar, get(scaleVarNamef[v]))
                }
                scaleByOut <- scaleBy(scaleVar)
            }
        }
        #####################################################
        # if scaleBy is a constant, use the constant directly
        else(scaleByOut <- scaleBy[f])
        names(scaleByOut) <- scaleVarNamef
        ########################################
        # calculate the scaled estimate and SE
        # for only one variable
        if(length(scaleVarNamef)==1){
            scaledEstSE <- estSE*scaleByOut
        }
        # for more than one variable
        else{
            scaledEstSE <- NULL
            for(v in scaleVarNamef){
                scaledEstSE <- rbind(scaledEstSE, estSE[v,]*scaleByOut[v])
            }
            rownames(scaledEstSE) <- scaleVarNamef
        }
        #########################################################
        # calculate the rescaled CILevel*100% confidence interval
        # NOTE: gam fits not current supported
        CI <- NULL
        # only calculate CI for "lm" fits that aren't "gam"
        # special case with glm for 1 variable to have uniform format
        if( !"gam"%in%class(fit[[f]]) & "glm"%in%class(fit[[f]]) & length(scaleVarNamef)==1){
            CI <- confint(fit[[f]],scaleVarNamef,CILevel)*scaleByOut
            CI <- t(as.matrix(CI))
            rownames(CI) <- scaleVarNamef
        }
        if( "lm"%in%class(fit[[f]]) & !"gam"%in%class(fit[[f]]) & 
            !("glm"%in%class(fit[[f]]) & length(scaleVarNamef)==1)){
            CI <- (confint(fit[[f]],scaleVarNamef,CILevel)*scaleByOut)[scaleVarNamef,]
        }
        ###########################
        # save results to return
        fitout[[f]] <- list(scaleBy = scaleByOut,
                            estSE = scaledEstSE,
                            CI = CI
                    )
    }

    if(!isListFit){fitout <- fitout[[1]]}
    out <- list(scaledResults=fitout,
                CILevel=CILevel)
    class(out) <- "scaleCoef"
    out
}

