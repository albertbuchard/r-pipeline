# statistics

#' correctForVariables
#' @note DevStatus: one pass - utility 4/5
#' TODO(*): to discuss what is the best way to correct for variables \
#'
#' @description Correct each variables from the variables vector using the correctVariables vector for each row of data.
#' If regressOnAllVariables is T the coeficient are extraced from a regression with all variables in variables as well as correctVariables
#' - not recommended since colinear variable might produce strange results. Univariate regression by default.
#' Works only for continuous dependant variables.
#'
#' @export

correctForVariables = function (data,
                                        variables,
                                        correctVariables,
                                        regressOnAllVariables = F,
                                        normalizeVariables = T) {
    data = copy(data)
    if ("id"%nin%colnames(data))
        data[,id:=1:NROW(data)]

    variables = variables[variables%nin%correctVariables]

    ## Nested utility function for normalizing vectors
    scaleNormalize = function (vector)
    {
        vector = vector - mean(vector, na.rm = T)
        vector = vector / sd(vector, na.rm = T)
        return(vector)
    }

    if (normalizeVariables) {
        newCorrectVariables = NULL
        for (correctVariable in correctVariables) {
            if(isContinuous(data[,correctVariable, with=F])) {
                data[, paste0(correctVariable,"Normalized") := scaleNormalize(get(correctVariable))]
                newCorrectVariables = c(newCorrectVariables,paste0(correctVariable,"Normalized"))
            } else {
                newCorrectVariables = c(newCorrectVariables, correctVariable)
            }
        }
        correctVariables = newCorrectVariables

        newVariables = NULL
        for (variable in variables) {
            if(isContinuous(data[,variable, with=F])) {
                data[, paste0(variable,"Normalized") := scaleNormalize(get(variable))]
                newVariables = c(newVariables,paste0(variable,"Normalized"))
            } else {
                newVariables = c(newVariables, variable)
            }
        }
        variables = newVariables
    }

    correctedVariablesNameVector = NULL
    for (i in 1:NROW(variables)) {

        variable = variables[i]
        otherVariables = variables[variables!=variable]
        correctedVariableName = paste0(variable,"CorrectedFor", paste0(correctVariables, collapse = ""))
        correctedVariablesNameVector = c(correctedVariablesNameVector, correctedVariableName)

        data[, (correctedVariableName) := list(as.double(get(variable)))]

        # only correct continuous variable
        if (isContinuous(data[, variable, with=F])) {

            independantVariablesVector = correctVariables

            # Regress over the variables
            if (regressOnAllVariables) {


                independantVariablesVector = c(otherVariables, correctVariables)

                formulaString = paste0(variable, "~", paste0(independantVariablesVector, collapse = "+"))
                formulaObject = as.formula(formulaString)

                resGLM = glm(formulaObject, data = data, family = "gaussian")

                summaryGlm = summary(resGLM)
                coefVector = coef(summaryGlm)
                coefNamesVector = dimnames(coefVector)[[1]]

                # from the regression select continuous and categorical correctVariable (categorical have a dummy variable created for each group like "genderfemale")
                continuousVariables = correctVariables[correctVariables%in%coefNamesVector]
                dummyVariables = correctVariables[correctVariables%nin%continuousVariables]

                # correct for continuous correction variables
                getContinuousCorrection = function (valuesVector, coefVector) {
                    offset = valuesVector[[1]]
                    valuesVector = valuesVector[,2:NCOL(valuesVector),with=F]

                    return(offset-sum(valuesVector*coefVector))
                }

                if (!is.null(continuousVariables)) {
                    data[,(correctedVariableName) := list(as.numeric(getContinuousCorrection(.SD,coefVector[continuousVariables,1]))),
                         .SDcols = c(correctedVariableName,continuousVariables),
                         by="id"]
                }


                # correct for categorical correction variables
                for (dummyVariable in dummyVariables) {
                    # indices of dummy variable as well as the value of the dummy var
                    variableDummyIndice = grep(dummyVariable,coefNamesVector)
                    variableDummyFactor = strsplit(x=coefNamesVector[variableDummyIndice],split = dummyVariable)[[1]][2]

                    data[get(dummyVariable)==variableDummyFactor, (correctedVariableName) := list(as.numeric(get(correctedVariableName))-coefVector[variableDummyIndice])]
                }

            } else {

                for (correctVariable in correctVariables) {

                    independantVariablesVector = correctVariable

                    formulaString = paste0(variable, "~", paste0(independantVariablesVector, collapse = "+"))
                    formulaObject = as.formula(formulaString)

                    resGLM = glm(formulaObject, data = data, family = "gaussian")

                    summaryGlm = summary(resGLM)
                    coefVector = coef(summaryGlm)
                    coefNamesVector = dimnames(coefVector)[[1]]

                    # from the regression select continuous and categorical correctVariable (categorical have a dummy variable created for each group like "genderfemale")
                    continuousVariables = NULL
                    dummyVariables = NULL
                    if(correctVariable%in%coefNamesVector)
                        continuousVariables = correctVariable
                    else
                        dummyVariables = correctVariable

                    # correct for continuous correction variables
                    getContinuousCorrection = function (valuesVector, coefVector) {
                        offset = valuesVector[[1]]
                        if (NCOL(valuesVector)<2)
                            stop()
                        valuesVector = valuesVector[,2:NCOL(valuesVector),with=F]

                        return(offset-sum(valuesVector*coefVector))
                    }

                    if (!is.null(continuousVariables)) {
                        data[,(correctedVariableName) := list(as.numeric(getContinuousCorrection(.SD,coefVector[continuousVariables,1]))),
                             .SDcols = c(correctedVariableName,continuousVariables),
                             by="id"]
                    }


                    # correct for categorical correction variables
                    for (dummyVariable in dummyVariables) {
                        # indices of dummy variable as well as the value of the dummy var
                        variableDummyIndice = grep(dummyVariable,coefNamesVector)
                        variableDummyFactor = strsplit(x=coefNamesVector[variableDummyIndice],split = dummyVariable)[[1]][2]

                        data[get(dummyVariable)==variableDummyFactor, (correctedVariableName) := list(as.numeric(get(correctedVariableName))-coefVector[variableDummyIndice])]
                    }
                }
            }

        }
    }
    return(list(correctedVariablesNameVector,data))
}

#' checkForPartialCorrelation
#' @note DevStatus: one pass - utility 4/5.
#' TODO(Albert): make example - discuss the methodology
#'
#' Uses GLM to check the correlation of couples of variables store in a DT containing two columns v1 and v2
#' controlling for other variables specified in controled variables.
#'
#' Normality assumption for variables : http://www.theanalysisfactor.com/dependent-variables-never-meet-normality/
#'
#' @export
checkForPartialCorrelation = function (data,
                                               variableCouplesDT,
                                               controledVariables,
                                               glmFamily = "gaussian",
                                               interaction = F,
                                               type = "glm",
                                               printOut = F,
                                               printFullGLM = T,
                                               normalize = F) {

    if (any((c("v1", "v2")%nin%names(variableCouplesDT)))) {
        stop("variableCouplesDT is not in the valid format (two columns DT named v1 and v2)")
    }

    collapseFormulaOn = "+"
    if(interaction)
        collapseFormulaOn = "*"

    partialCorrelationTable = NULL
    for (i in 1:NROW(variableCouplesDT)) {

        v1 = variableCouplesDT[i, v1]
        v2 = variableCouplesDT[i, v2]

        if (any(c(v1,v2)%in%controledVariables))
            next()

        entry = data.table (v1 = v1, v2 = v2, significant = T)

        scrubbedData = data[!is.na(get(v2))&!is.na(get(v1)),]
        columnsToNormalize = c(v2,controledVariables)

        if (normalize) {
            scrubbedData[, (columnsToNormalize) := lapply(.SD, function(x) { return(as.numeric(as.factor(x))) }), .SDcols = columnsToNormalize]
            scrubbedData[, (columnsToNormalize) := lapply(.SD, function(x) { return(scale(x)) }), .SDcols = columnsToNormalize]
        }


        scrubbedData = scrubbedData[, c(v1,columnsToNormalize), with=F]

        switch(type,
               glm = {

                   glmFormulaIndependent = paste0("~ ", paste0(c(v2,controledVariables), collapse = collapseFormulaOn))
                   formulaGlm = as.formula(paste0(v1, glmFormulaIndependent))
                   resGLM = glm(formulaGlm, data = scrubbedData, family = glmFamily)

                   summaryGlm = summary(resGLM)

                   significantPValue = coef(summaryGlm)[2,4]<0.05
                   entry[, significant := significantPValue]

                   #                    droppedRes = drop1(resGLM, test="Chisq")
                   #                    droppedSignif = droppedRes$`Pr(>Chi)`[2] < 0.05

                   if (printOut) {
                       print(paste("@@@@ Correlation between", v1, "and", v2, " @@@@"))
                       if (significantPValue) {
                           print("// SIGNIFICANT \\")
                           print(summaryGlm)
                       } else {

                           print("@@@@ Not significant @@@@")
                       }
                   }


                   entry[, glmBetaPValue := coef(summaryGlm)[2,4]]
                   if (!is.null(names(glmFamily))) {
                       if (glmFamily$link == "logit")
                           entry[, glmBetaOdds := logitToOdds(coef(summaryGlm)[2,1])]
                   }

               },
               pcor = {
                   #' Really only valid with one controlled variable
                   entry[, (paste0(controledVariables, "PValue")) := as.list(rep(1,NROW(controledVariables)))]
                   for (j in 1:NROW(controledVariables)) {
                       partialCorrelation = (pcor.test(as.numeric(as.factor(scrubbedData[, get(v1)])),
                                                       as.numeric(as.factor(scrubbedData[, get(v2)])),
                                                       as.numeric(as.factor(scrubbedData[, get(controledVariables[j])]))))
                       entry[i, (paste0(controledVariables[j], "PValue")) := partialCorrelation$p.value]
                       if ((partialCorrelation$p.value > 0.05)&(entry[, significant]==T))
                           entry[,significant := F]

                   }
               })


        partialCorrelationTable = rbind(partialCorrelationTable, entry)
    }


    #' Print Functions
    #' For GLM
    if (type == "glm") {

        if (printOut) {
            glmFormulaIndependent = paste0("~ ", paste0(c("v2 ",controledVariables), collapse = collapseFormulaOn))
            formula = as.formula(paste0(v1, glmFormulaIndependent))
            print("@@@@@@@@ GLM Formula @@@@@@@@")
            print(formula)
        }

        if (printFullGLM) {
            #' full GLM results
            print("@@@@@@@@ FULL GLM Formula @@@@@@@@")
            v1Vector = variableCouplesDT[1, v1]
            v2Vector = variableCouplesDT[v1==v1Vector, v2]
            glmFormulaIndependent = paste0("~ ", paste0(c(v2Vector,controledVariables), collapse = collapseFormulaOn))
            formulaGlm = as.formula(paste0(v1, glmFormulaIndependent))
            resGLM = glm(formulaGlm, data = data, family = glmFamily)

            summaryGlm = summary(resGLM)

            print(summaryGlm)
        }
    }

    return(partialCorrelationTable)

}

#' slidingWindowComputation
#' @note DevStatus: one pass - utility 5/5
#' TODO(Albert): make example
#'
#' @description Function that can compute various measures (e.g mean, SD, max...) of one or several variable (computedVariables) grouped on a sliding window running across another base variable of optional size sizeOfWindow, or with number of bin numberOfBins
#'
#' @param data : dataframe or data.table containing the variables
#' @param computedVariables : vector of character representing the name of the columns in the data
#' @param computedFunction : either a character ("mean") or a function (mean)
#'          specifying the computation you want to use. If computedFunction is a
#'           vector, the function will compute for all variables iteratively
#' @param baseVariable : character representing the name of the columns of the base variable
#'          upon which the window is sliding
#' @param groupBy : optional - if set, compute grouped by variable "groupBy" and
#'           add a column named with the name of that variable to the output DT
#' @param sizeOfWindow (optional) : numeric valuein the metric of the base variable defining the size of the sliding window
#' @param step(optional) : numeric value in the metric of the base variable definig how much the windows moves at each step
#'          if the step is smaller than sizeOfWindow, overlap value will be generated
#' @param ... (optional) : the rest of the arguments are to be passed to the computedFunction when it is called (eg na.rm=T)
#'
#' @export

slidingWindowComputation = function (data,
                                             computedVariables,
                                             baseVariable,
                                             computedFunction = mean,
                                             groupBy = NULL,
                                             numberOfBins = NULL,
                                             sizeOfWindow = NULL,
                                             step = NULL,
                                             type = "sliding",
                                             ...){
    # Nested function to compute the window and step size
    # If none of the parameters are given step = 1/2 window, sizeOfWindow = 2*range/numberOfBins
    # and numberOfBins = floor(0.05 * (baseRange[2]-baseRange[1]) + 5)
    getWindowOptions = function(baseVariable,
                                numberOfBins = NULL,
                                sizeOfWindow = NULL,
                                step = NULL,
                                type = "sliding"){
        baseUnique = unique(baseVariable)

        if (NROW(baseUnique) < 5)
            stop("baseVariable has too few unique values")

        baseRange = c(min(baseUnique), max(baseUnique))

        # numberOfBins takes precedence over sizeOfWindow if none are declared
        if(is.null(numberOfBins)&is.null(sizeOfWindow))
        {
            numberOfBins = floor(0.05 * (baseRange[2]-baseRange[1]) + 5)
        }


        if(!is.null(numberOfBins))
        {
            sizeOfWindow = 2*baseRange[2]/numberOfBins
            step = sizeOfWindow/2
        }

        switch(type,
               sliding = {
                   return(list(baseRange, sizeOfWindow, 1))
               },
               {
                   # if sizeOfWindow is declared, check for step declaration
                   if(is.null(step))
                       step = sizeOfWindow/2

                   return(list(baseRange, sizeOfWindow, step))

               })


    }

    windowOptions = getWindowOptions(data[,baseVariable, with=F],
                                     numberOfBins,
                                     sizeOfWindow,
                                     step,
                                     type)

    baseRange = windowOptions[[1]]
    sizeOfWindow = windowOptions[[2]]
    step = windowOptions[[3]]

    computedDT = NULL

    for (windowPosBegin in seq(baseRange[1], baseRange[2], step))
    {
        if (windowPosBegin == baseRange[2])
            break

        # Center the computed point on the window center
        windowPosCenter = windowPosBegin + sizeOfWindow/2
        windowPosEnd = windowPosBegin + sizeOfWindow

        dtTemp = data[get(baseVariable)>windowPosBegin&get(baseVariable)<windowPosEnd, lapply(.SD, computedFunction, ...), .SDcols=computedVariables, by=groupBy]
        dtTemp[, c(baseVariable) := windowPosCenter]

        computedDT = rbind(computedDT,dtTemp)

    }

    return(computedDT)
}



#' compareToFullModel
#' @note : actually note sure if this function works
#' TODO(Albert): to test - change name - integrate to larger function
#'
#' @description Performs a likelyhood ratio test lrtest on a simple model of correlatedVariable[1] ~ correlatedVariable[2] vs a full model
#' coreletedVariable[1] ~ correlatedVariable[2] + regressOverVariables...
#'
#'
#' @param data dta table containing the columns of data present in coreletedVariable and resgressedVariable
#' @param correlatedVariable : string vector with 2 rows contianing the correlated variable
#' @param regressOverVariables : string vector containing all variable names to regress on
#'
#' @export

compareToFullModel = function (data,
                                           correlatedVariable,
                                           regressOverVariables,
                                           outputType = "store")
{
		# make sure the correlatedVariable are not also specified in the regressOverVariables vector
    regressOverVariables = regressOverVariables[!regressOverVariables%in%correlatedVariable]

    if (outputType=="print")
        print(paste0("Comparison of simple correlation models for ", paste(correlatedVariable, collapse = " and "),
                     " over a full model comprising ", paste(regressOverVariables, collapse = " and ")))

    if (NROW(regressOverVariables)>0)
    {
        model = "glm"
        priorFamily = "gaussian"
        X = data[, correlatedVariable[1], with=F]
        Y = data[, correlatedVariable[2], with=F]

        if(!isContinuous(X))
        {
            if(isContinuous(Y))
            {
                ## Dependant variable is not continuous Y is continous
                ## We swap variable
                t = correlatedVariable[2]
                correlatedVariable[2] = correlatedVariable[1]
                correlatedVariable[1] = t
            }
            else
            {
                if(isBinomial(X))
                    priorFamily = "binomial"
                else
                    model = "multinomial"
            }


        }

        switch (model,
                multinomial = {
                    if (outputType=="print")
                        print("We will use multinomial regression")

                    processedData = mlogit.data(data[,c(correlatedVariable,regressOverVariables), with=F],shape="wide",choice=correlatedVariable[1])
                    modelFullFormula= as.formula(paste0(correlatedVariable[1], '~ ', "0 | ", paste0( correlatedVariable[2], "+", paste0( regressOverVariables, collapse=' + ' ))))
                    modelSimpleFormula= as.formula(paste0(correlatedVariable[1], '~ ', "0 | ", correlatedVariable[2]))

                    modelFull = try(mlogit(modelFullFormula, data=processedData, na.action = na.exclude))
                    modelSimple = try(mlogit(modelSimpleFormula, data=processedData, na.action = na.exclude))

                    output = try(lrtest(modelSimple, modelFull))

                    if (outputType=="print")
                        print(output)

                    return(output)
                },
                glm = {
                    if (outputType=="print")
                        print(paste0("We will use a global linear model regression with ", priorFamily ," prior"))
                    modelFullFormula= as.formula(paste0(correlatedVariable[1], '~ ', paste0( correlatedVariable[2], "+", paste0( regressOverVariables, collapse=' + ' ))))
                    modelSimpleFormula = as.formula(paste0(correlatedVariable[1], '~ ', correlatedVariable[2]))

                    modelFull = try(glm( modelFullFormula, data = data, na.action = na.exclude, family = priorFamily))
                    modelSimple = try(glm( modelSimpleFormula, data = data, na.action = na.exclude, family = priorFamily))

                    output = try(lrtest(modelSimple, modelFull))

                    if (outputType=="print")
                        print(output)

                    return(output)
                },
                {return(NA)}
        )

    } else {
    	stop("")
    }

}


#' getDensityForVector
#' @notes DevStatus: one pass - utility 1/5
#' TODO(Albert): Redundant function. Make a more robust function that test for the type of data input.
#'
#' @description Wrapper function for getDensityForX
#' PDF or CDF of a variable.
#'
#' \code{atecFuncGetDensityForVector} is an helper function to call \code{getDensityForVector} with a vector.
#'
#' @export

getDensityForVector = function(vector, variableName = "variable", ...)
{
    dataTable = data.table(v1 = as.numeric(vector))
    setnames(dataTable, "v1", variableName)
    return(getDensityForVector(dataTable, variableName, ...))
}


#' PDF or CDF of a variable.
#' @note DevStatus: One pass - utility: ?/5
#' TODO(Albert): Check if that function is not redundant - if not make it more robust
#'
#' \code{atecFuncGetDensityForX} returns a data.table containing either the PDF
#' or CDF of a variable.
#'
#' Function takes a data table and return a data table containing Either a CDF
#' or a PDF for a variable. The data.table contains two variable varX the bin
#' position and histForX the bin probability. The variable of interest can be
#' average accross a grouping variable declared in meanOnVariable. Permits to
#' obtain precise distribution and interaction with an optional variable.
#'
#' Zoom interval has not been added yet
#' If nBins is NULL number of bin = Number of observations/10
#'
#' @param data
#' @param densityForVariableName
#' @param type
#' @param nBins
#' @param meanOnVariable
#' @param globalInterval
#' @param zoomInterval
#' @param optionalVariableName
#' @param optionalVariableFormat
#'
#' @return A data table containing two variable :
#' \describe{
#'      \item{varX}{contains the bins position on the variable axis}
#'      \item{histForX}{contains the bins density}
#' }
#'
#' @export

getDensityForX = function (data, densityForVariableName,
                                   type = "PDF",
                                   nBins = NULL,
                                   meanOnVariable = NULL,
                                   globalInterval = NULL,
                                   zoomInterval = NULL,
                                   optionalVariableName = NULL,
                                   optionalVariableFormat = "density") ##or "mean"
{
    if (!any(class(data)=="data.table"))
        stop("Object data is not of the class data.table")

    occurenceVector = NULL
    occurenceDT = NULL
    if (is.null(meanOnVariable))
    {
        ## if meanOnVariable is null all occurences are used
        occurenceDT = data[,c(densityForVariableName), with=F]
        occurenceVector = data[, get(densityForVariableName)]
    }
    else
    {
        ## if grouping is used the mean of densityForVariableName is processed
        data[,(densityForVariableName) := as.double(get(densityForVariableName))]
        data[, (densityForVariableName) := mean(get(densityForVariableName)),
             by=c(meanOnVariable)]
        occurenceDT = data[!duplicated(get(meanOnVariable)),
                           c(densityForVariableName), with=F]
        occurenceVector = data[!duplicated(get(meanOnVariable)),
                               get(densityForVariableName)]
    }

    if (is.null(globalInterval))
    {
        if(is.null(nBins))
            nBins = NROW(occurenceVector)/10

        globalInterval = c(min(occurenceVector),
                           max(occurenceVector),
                           max(occurenceVector)/nBins)
    }

    vectorX = NULL
    optionalVarByX = NULL
    histForX = NULL

    ## Nested function used for density binning - gives a either p(X<a) for CDF
    ## or p(a<X<b) for PDF -- and p(Y==T|X<a) if optionalVar is used
    getNumberOfOccurence = function(dt, var, a, b = NULL, optionalVar = NULL)
    {
        if (is.null(optionalVar))
        {
            if (is.null(b))
                occurences = NROW(dt[get(var)<=a,])
            else
                occurences = NROW(dt[get(var)>=a&get(var)<=b,])
        }
        else
        {
            if (is.null(b))
                occurences = NROW(dt[get(var)<=a&get(optionalVar)==T,])
            else
                occurences = NROW(dt[get(var)>=a&get(var)<=b&get(optionalVar)==T,])
        }

        return(occurences)
    }

    ## Nested function used for mean binning - gives a either mean of X (X<a) for CDF
    ## or mean of X p(a<X<b) for PDF -- and mean of Y if optionalVar is used
    getMean = function(dt, var, a, b = NULL, optionalVar = NULL)
    {
        if (is.null(optionalVar))
        {
            if (is.null(b))
                meanVar = mean(dt[get(var)<=a, get(var)])
            else
                meanVar = mean(dt[get(var)>=a&get(var)<=b, get(var)])
        }
        else
        {
            if (is.null(b))
                meanVar = mean(dt[get(var)<=a, get(optionalVar)])
            else
                meanVar = mean(dt[get(var)>=a&get(var)<=b, get(optionalVar)])
        }

        return(meanVar)
    }

    for (iValue in seq(globalInterval[1],globalInterval[2],globalInterval[3]))
    {
        vectorX = c(vectorX, iValue)
        switch (type,
                PDF = {
                    endInterval = iValue+globalInterval[3]

                    if (endInterval>globalInterval[2])
                        endInterval = globalInterval[2]

                    nOccurences = getNumberOfOccurence(occurenceDT,
                                                       densityForVariableName,
                                                       iValue,
                                                       endInterval)

                    histForX = c(histForX, nOccurences/NROW(occurenceDT))

                    if (!is.null(optionalVariableName))
                    {
                        switch (optionalVariableFormat,
                                density = {
                                    optionalVarByX = c(optionalVarByX, getNumberOfOccurence(data,
                                                                                            densityForVariableName,
                                                                                            iValue,
                                                                                            endInterval,
                                                                                            optionalVar = optionalVariableName)
                                                       /getNumberOfOccurence(data,
                                                                             densityForVariableName,
                                                                             iValue,
                                                                             endInterval))
                                },
                                mean = {
                                    optionalVarByX = c(optionalVarByX, getMean( data,
                                                                                densityForVariableName,
                                                                                iValue,
                                                                                endInterval,
                                                                                optionalVar = optionalVariableName))
                                },
                                {
                                    optionalVarByX = c(optionalVarByX, getNumberOfOccurence(data,
                                                                                            densityForVariableName,
                                                                                            iValue,
                                                                                            endInterval,
                                                                                            optionalVar = optionalVariableName)
                                                       /getNumberOfOccurence(data,
                                                                             densityForVariableName,
                                                                             iValue,
                                                                             endInterval))

                                    optionalVariableFormat = "density"
                                    warning("Wrong format for optionalVariableFormat. Either choose 'mean' or 'density'.
                                            Here density was calculated")
                                }
                        )

                    }

                },
                CDF = {
                    histForX = c(histForX, getNumberOfOccurence(occurenceDT,
                                                                densityForVariableName,
                                                                iValue)/NROW(occurenceDT))
                    if (!is.null(optionalVariableName))
                    {
                        switch (optionalVariableFormat,
                                density = {
                                    optionalVarByX = c(optionalVarByX, getNumberOfOccurence(data,
                                                                                            densityForVariableName,
                                                                                            iValue,
                                                                                            optionalVar = optionalVariableName)
                                                       /getNumberOfOccurence(data,
                                                                             densityForVariableName,
                                                                             iValue))
                                },
                                mean = {
                                    optionalVarByX = c(optionalVarByX, getMean(data,
                                                                               densityForVariableName,
                                                                               iValue,
                                                                               optionalVar = optionalVariableName))
                                },
                                {
                                    optionalVarByX = c(optionalVarByX, getNumberOfOccurence(data,
                                                                                            densityForVariableName,
                                                                                            iValue,
                                                                                            optionalVar = optionalVariableName)
                                                       /getNumberOfOccurence(data,
                                                                             densityForVariableName,
                                                                             iValue))

                                    optionalVariableFormat = "density"
                                    warning("Wrong format for optionalVariableFormat. Either choose 'mean' or 'density'.
                                            Here density was calculated")
                                }
                        )
                    }
                },
                {
                    histForX = c(histForX, getNumberOfOccurence(occurenceDT,
                                                                densityForVariableName,
                                                                iValue)/NROW(occurenceDT))
                }
        )

    }

    if (is.null(optionalVarByX))
        return(data.table(varX = vectorX, histForX = histForX))
    else
        return(data.table(varX = vectorX, histForX = histForX, optionalVar = optionalVarByX))

    ## ADD THE ZOOM THINGY
    #     for (iValue in seq(zoomInterval[1],zoomInterval[2],zoomInterval[3]))
    #     {
    #         vectorX = c(vectorX, iValue)
    #         optionalVarByX = c(optionalVarByX, NROW(d[rt<=iValue&signal==choice,])/NROW(d[rt<=iValue,]))
    #         histForX = c(histForX, NROW(d[rt<=iValue,])/NROW(d))
    #     }
    #
    #     zoomInterval = c(1, 100, 1)

}

#' getDensityForXByGroupOnY
#' @note DevStatus: one pass - utility 5/5
#' TODO(Albert): usability - name
#'
#' @description  Get the density distribution of a variable for several groups
#'
#' @return data.table with each distribution
#'
#' @export

getDensityForXByGroupOnY = function (data,
                                             densityForVariableName,
                                             groupByVariable,
                                             globalInterval = NULL,
                                             useGlobalInterval = FALSE,
                                             type = "PDF",
                                             nBins = NULL)
{
    data = data[!is.na(get(groupByVariable))]

    if (is.null(globalInterval)&useGlobalInterval)
    {
        occurenceVector = data[, get(densityForVariableName)]

        if(is.null(nBins))
            nBins = NROW(occurenceVector)/10

        globalInterval = c(min(occurenceVector),
                           max(occurenceVector),
                           max(occurenceVector)/nBins)
    }

    levelsOfGroupingVariable = levels(data[, get(groupByVariable)])
    densityDT = NULL


    for (i in 1:NROW(levelsOfGroupingVariable))
    {
        level = levelsOfGroupingVariable[i]
        densityDTi = getDensityForVector(data[get(groupByVariable)==level,],
                                            densityForVariableName,
                                            type = type,
                                            globalInterval = globalInterval,
                                            nBins = 10)
        densityDTi$group = level
        densityDT = rbind(densityDT, densityDTi)
    }

    return(densityDT)
}


#'CheckForCorrelates
#' @note DevStatus: second pass - utility 5/5
#' TODO(Albert): Examples, name, make filter customizable
#'
#' @description Wrapper function for PlotEachCorrelation: Filters variables with less than non-NA values, and less than 2 unique values.
#' If screenSignificant is set to TRUE, only significant correlation using rcorr are specified to PlotEachCorrelation correlation between a set of variables and call plotEachCorrelation with
#' only the significantly correlated couples. The generate a data.table with all possible couples of one to one correlations to test.
#'
#'  @param data a data frame or data table containing observations
#'  @param columnsToCorelate Character vector (optional) - Names of all variable to correlate (optional)
#'  @param format permit to choose a return format (optional)
#' @export

CheckForCorrelates = function (data,
                                       columnsToCorrelate = NULL,
                                       versusColumns = NULL,
                                       screenSignificant = F,
                                       correctPValue = "sidak",
                                       format = "plots") {
    ## RCORR ONLY WORKS WITH NUMERIC VALUES - Check validity of numerical coercicion

    ## checkForColumnValidity
    # nested function eliminate columns with less than 4 non NA value or less than 2 modality (unique values)
    # return a data table with only filtered columns
    checkForColumnValidity = function(d)
    {
        filteredColumns = colnames(d)
        for (column in colnames(d))
        {
            if(NROW(d[!is.na(get(column)),])<=4)
                filteredColumns = filteredColumns[filteredColumns!=column]

            if(NROW(unique(d[!is.na(get(column)),get(column)]))<2)
                filteredColumns = filteredColumns[filteredColumns!=column]
        }
        return(filteredColumns)
    }


    ## createSyntheticCorrelationDataTable
    # creates a data table containing all the different couple combination
    # of column names contained in a and b, if b is null all combination of
    # coloumns names in a are used. If b is a string vector only a vs b combination
    # are used.
    # dt [ row, column, pValue, rValue]
    createSyntheticCorrelationDataTable = function (a,b=NULL,pValue=0,rValue=1)
    {
        dt = as.data.table(t(combn(c(a,b), 2, simplify = T)))
        setnames(dt, c("V1","V2"), c("row","column"))
        dt[, `:=`(p=pValue, r=rValue)]

        if (!is.null(b))
            dt = dt[((row%in%a)&(column%in%b))|((row%in%b)&(column%in%a)),]

        return(dt)
    }

    if (any(class(data)=="data.frame"))
    {

        data = as.data.table(data)

        ## if no columns are given in columnsToCorrelate
        ## all columns are selected
        if (is.null(columnsToCorrelate))
            columnsToCorrelate = colnames(data)
        else
            data = data[, .SD, .SDcols = c(columnsToCorrelate,versusColumns)]

        ## Get only columns with more than 4 non NA values
        columnsValidated = checkForColumnValidity(data)


        ## Isolate them in the columns vectors
        columnsToCorrelate = columnsToCorrelate[columnsToCorrelate%in%columnsValidated]
        versusColumns = versusColumns[versusColumns%in%columnsValidated]

        ## Isolate those valid columns in the data
        data = data[, .SD, .SDcols = columnsValidated]

        ## If polyserial correlation is used
        ## we filter columns using p values obtained from rcorr()
        if (screenSignificant)
        {
            dataValidated = data[, (columnsValidated) := lapply(.SD, function(feature) { as.numeric(feature) }), .SDcols = columnsValidated]
            columnsToCorrelate = colnames(dataValidated)
            correlationResults = rcorr(na.omit(as.matrix(dataValidated)))
            correlationDT = as.data.table(flattenCorrMatrix(correlationResults$r, correlationResults$P))
        }
        else
        {
            ## If rcorr is not used we create a synthetic table containing all
            ## couples of interest. P value is set to 0 to be significant for each couple.
            correlationDT = createSyntheticCorrelationDataTable(columnsToCorrelate,
                                                                versusColumns)
        }


        ## Check if at least one couple is possible
        if (NROW(correlationDT)<1)
        {
            warning("No valid variable couples were given (need more than 4 observations and at least 2 unique values)")
            return()
        }


        switch (correctPValue,
                sidak = {pValueCutoff = 1-(1-0.05)^(1/(NROW(correlationDT)^2))},
                strong = {pValueCutoff = 0.01},
                {
                    if (is.numeric(correctPValue))
                        pValueCutoff = correctPValue
                    else
                        pValueCutoff = 0.05
                }
        )

        correlationDT = correlationDT[p<pValueCutoff,]

        switch (format,
                DT = {
                    if(screenSignificant) {
                        return(correlationDT)
                    } else {
                        setnames(correlationDT, c("row", "column"), c("var1", "var2"))
                        significantCorrelation = PlotEachCorrelation(data, correlationDT, forPValueInferiorTo = pValueCutoff, noPrint = T)
                        return(significantCorrelation)
                    }

                },
                mainPlot = {
                    correlatedColumns = correlationDT[, unique(c(paste(row),paste(column)))]
                    chart.Correlation(data[,correlatedColumns], histogram=TRUE, pch=19)
                },
                plots = {
                    setnames(correlationDT, c("row", "column"), c("var1", "var2"))
                    significantCorrelation = PlotEachCorrelation(data, correlationDT, forPValueInferiorTo = pValueCutoff)
                    return(significantCorrelation)
                },
                {
                    print("Invalid Format")
                    return(correlationDT)
                }
        )
    }
    else
        stop("data needs to be a data.frame or a data.table")

}

#' PlotEachCorrelation(data, couples)
#' @note DevStatus: second pass - utility 5/5
#' TODO(Albert): Discuss methodology ++, Discrete as categorical (as underlying phenomena could drive discrete...)
#'
#' @description  Exploratory data analysis function testing correlation between variable couples, using adapted statistical methodology based on the variable types.
#'	Note: variable are categorized mainly by the isContinuous function - discrete variable are treated as categorical
#'
#' @param data a data.table
#' @param couples data.table needs to contain 2 columns named var1 and var2
#' @param forPValueInferiorTo numeric value for the significance threshold
#' @param noPrint boolean. If set to false no plots a build and only text and a final a correlation table is produce
#' @export

PlotEachCorrelation = function (data, couples, forPValueInferiorTo = "0.05", noPrint = F)
{
    significantCorrelations = list()


    ## Nested utility functions
    addSignificantCorrelation = function (d,v1,v2,p, correlation = NA)
    {
        d[[1]] = c(unlist(d[1]),v1)
        d[[2]]= c(unlist(d[2]),v2)
        d[[3]] = c(unlist(d[3]),p)
        d[[4]] = c(unlist(d[4]),correlation)
        return(d)
    }

    aovPValue = function (aovSummary)
    {
        if(any(class(aovSummary)=="aov"))
            aovSummary = summary(aovSummary)

        if(any(class(aovSummary)=="summary.aov"))
            return(aovSummary[[1]][["Pr(>F)"]][[1]])
        else
            stop("Not an object of class 'aov' or 'summary.aov'")
    }

    lmPValue = function (lmObject) {
        if (class(lmObject) != "lm")
            stop("Not an object of class 'lm' ")
        f = summary(lmObject)$fstatistic
        p = pf(f[1],f[2],f[3],lower.tail=F)
        attributes(p) <- NULL
        return(p)
    }


    ## Plot-types - to scale
    scatterPlot = function(d)
    {
        colnamesCouple = colnames(d)
        output = ggplot(d, aes_string(x=colnamesCouple[1], y=colnamesCouple[2]))+
            geom_jitter()+
            geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = TRUE, size = 2)
        return(output)
    }

    frequencyPlot = function(d)
    {
        colnamesCouple = colnames(d)

        theme_nogrid <- function (base_size = 12, base_family = "") {
            theme_bw(base_size = base_size, base_family = base_family) %+replace%
                theme(panel.grid = element_blank())
        }

        output = ggplot(d, aes_string(x=colnamesCouple[1], y=colnamesCouple[2])) +
            geom_point(aes(size = Freq, color = Freq, stat = "identity", position = "identity"), shape = 15) +
            scale_size_continuous(range = c(3,15)) +
            scale_color_gradient(low = "white", high = "black") +
            theme_nogrid()
        return(output)
    }

    heatmapPlot = function(d)
    {
        colnamesCouple = colnames(d)
        output = ggplot(tableDF, aes_string(x=colnamesCouple[1], y=colnamesCouple[2])) +
            geom_tile(aes(fill = Freq), colour = "black") +
            scale_fill_gradient(low = "white", high = "steelblue")
        return(output)
    }

    ## Analysis type
    regressionAnalysis = function(d)
    {
        colnamesCouple = colnames(d)

        for(i in 1:NROW(colnamesCouple))
        {
            groupVar = colnamesCouple[i]
            d = d[!is.na(get(colnamesCouple)),]
        }

        tempFormula = paste(colnamesCouple[1], '~', colnamesCouple[2] )
        tempFormula = as.formula(tempFormula)
        fit = lm(tempFormula, data=d)

        #pValue = shorterSafer(lmPValue,fit)

        ## Not using pearson anymore, Spearman for all analysis
        fitSpearman = cor.test(as.numeric(as.factor(d[, get(colnamesCouple[1])])), as.numeric(as.factor(d[, get(colnamesCouple[2])])), method = "spearman")
        print(fitSpearman)

        pValue = fitSpearman$p.value

        if (is.na(pValue)) {
            print(paste0(" @@@@@ ERROR @@@@ Correlation between ",colnamesCouple[1],
                         " and ", colnamesCouple[2], " returned an ERROR"))
        } else {

            if (pValue<=forPValueInferiorTo)
            {
                print(paste0(" @@@@@ Correlation between ",colnamesCouple[1],
                             " and ", colnamesCouple[2], " significant (p <= ",
                             as.character(forPValueInferiorTo), ") @@@@@"))
                print(summary(fit))



                correlation = cor(d, use = "complete.obs")[1,2]

                if (!noPrint)
                    print(scatterPlot(d))

                significantCorrelations <<- addSignificantCorrelation(significantCorrelations,colnamesCouple[1],colnamesCouple[2],pValue, correlation)
            }
            else
            {
                print(paste0(" @@@@@ Correlation between ",colnamesCouple[1],
                             " and ", colnamesCouple[2], " not significant (p <= ",
                             as.character(forPValueInferiorTo), ") @@@@@"))
                print(paste0(" --- Not significant with p = ", as.character(pValue)))
            }
        }

    }

    anovaAnalysis = function(d, reOrder = F)
    {
        colnamesCouple = colnames(d)
        plotsXvsY = PlotXAgainstY(dependantVar=colnamesCouple[1], groupingVars = colnamesCouple[2],
                                          d, outputType = "store")
        summaryAov = summary(plotsXvsY[[1]])
        pValue = aovPValue(summaryAov)


        if (pValue<=forPValueInferiorTo)
        {
            print(paste0(" @@@@@ Correlation between ",colnamesCouple[1],
                         " and ", colnamesCouple[2], " significant (p <= ",
                         as.character(forPValueInferiorTo), ") @@@@@"))
            print(summaryAov)

            if (!noPrint)
                print(plotsXvsY[[2]][[3]])

            ## Re order variable if necessary to preserve a readable table output
            v1Text = colnamesCouple[1]
            v2Text = colnamesCouple[2]
            if (reOrder) {
                v1Text = colnamesCouple[2]
                v2Text = colnamesCouple[1]
            }

            correlation = paste("~",paste(plotsXvsY[[1]]$coefficients,collapse = "+"))

            significantCorrelations <<- addSignificantCorrelation(significantCorrelations,v1Text,v2Text,pValue,correlation)
        }
        else
        {
            print(paste0(" @@@@@ Correlation between ",colnamesCouple[1],
                         " and ", colnamesCouple[2], " not significant (p <= ",
                         as.character(forPValueInferiorTo), ") @@@@@"))
            print(paste0(" --- Not significant with p = ", as.character(pValue)))
        }
    }

    chi2Analysis = function(d, reOrder = F)
    {
        colnamesCouple = colnames(d)

        for(i in 1:NROW(colnamesCouple))
        {
            groupVar = colnamesCouple[i]
            d = d[!is.na(get(colnamesCouple)),]
        }

        dValid = d
        dValid[[1]] = as.character(d[[1]])
        dValid[[2]] = as.character(d[[2]])
        tableD = table(dValid)
        tableD = tableD[,which(!apply(tableD,2,FUN = function(x){all(x == 0)}))]
        tableD = t(t(tableD)[,which(!apply(t(tableD),2,FUN = function(x){all(x == 0)}))])
        chiTest = chisq.test(tableD, simulate.p.value = T, B = 100000)

        if (chiTest$p.value<=forPValueInferiorTo)
        {
            print(paste0(" @@@@@ Correlation between ",colnamesCouple[1],
                         " and ", colnamesCouple[2], " significant (p <= ",
                         as.character(forPValueInferiorTo), ") @@@@@"))
            print(chiTest)

            if (!noPrint)
                print(frequencyPlot(as.data.frame(table(d))))

            ## Re order variable if necessary to preserve a readable table output
            v1Text = colnamesCouple[1]
            v2Text = colnamesCouple[2]
            if (reOrder) {
                v1Text = colnamesCouple[2]
                v2Text = colnamesCouple[1]
            }

            significantCorrelations <<- addSignificantCorrelation(significantCorrelations,v1Text,v2Text,chiTest$p.value)
        }
        else
        {
            print(paste0(" @@@@@ Correlation between ",colnamesCouple[1],
                         " and ", colnamesCouple[2], " not significant (p <= ",
                         as.character(forPValueInferiorTo), ") @@@@@"))
            print(paste0(" --- Not significant with p = ", as.character(chiTest$p.value)))
        }


    }

    if (NROW(couples)<1)
    {
        warning("No couples were given")
        return()
    }


    ## Loop through each variable couple and decide which analysis to use
    for (i in 1:NROW(couples))
    {

        nameVar1 = as.character(couples[,var1][i])
        nameVar2 = as.character(couples[,var2][i])
        X = data[, get(nameVar1)]
        Y = data[, get(nameVar2)]

        ## Order output to prevent swaping of variables in the output table
        reOrder = F

        if(isContinuous(X))
        {
            if(isContinuous(Y))
            {
                ## X is continuous Y is continous
                ## Pearson regression linear model
                d = data.table(X=X,Y=Y)
                setnames(d, c("X","Y"),c(nameVar1,nameVar2))
                regressionAnalysis(d)
            }
            else
            {
                ## X is continuous Y is discrete
                ## ANOVA X~Y
                d = data.table(X=X,Y=Y)
                setnames(d, c("X","Y"), c(nameVar1,nameVar2))
                anovaAnalysis(d)
            }
        }
        else
        {
            if(isContinuous(Y))
            {
                ## X is discrete Y is continous
                ## ANOVA Y~X
                d = data.table(X=Y,Y=X)
                setnames(d, c("X","Y"), c(nameVar2,nameVar1))
                reOrder = T
                anovaAnalysis(d, reOrder)
            }
            else
            {
                ## X is discrete Y is discrete
                ## chi-squared test
                if (NROW(unique(X))>NROW(unique(Y)))
                {
                    d = data.table(X=Y,Y=X)
                    setnames(d, c("X","Y"), c(nameVar2,nameVar1))
                    reOrder = T
                }
                else
                {
                    d = data.table(X=X,Y=Y)
                    setnames(d, c("X","Y"), c(nameVar1,nameVar2))
                }


                chi2Analysis(d, reOrder)
            }
        }
    }

    return(data.table(v1 = significantCorrelations[[1]],
                      v2 = significantCorrelations[[2]],
                      p = significantCorrelations[[3]],
                      correlations = significantCorrelations[[4]]))

}



#' GetSignificantPlotsXAgainstY
#' @notes DevStatus: one pass - utility : 4/5
#'	TODO(Albert): Discuss if this function should be public (export ?) - better description
#'
#' @description Extract information from the Anova contained in the output of PlotXAgainstY to produce a vector of plot names better fit to show those significant data
#'


GetSignificantPlotsXAgainstY = function(aovTest, variableNames, plotPrefix = NULL)
{
    # make an associative table from PValue number in the aov summary to the names of the variable,
    # since it is not posible to get it directly from the summary... go figure
    # the NROW(variableNames) firsts are main effect P Values the rest is cross term (we stop at two variable per term)

    associationList = paste0(plotPrefix, variableNames, "Summary")
    nVariables = NROW(variableNames)
    crossTerms = NULL

    if (nVariables>1)
    {
        for (i in 2:nVariables)
        {
            for (j in 1:(i-1))
            {
                crossTerms = c(crossTerms,paste0(plotPrefix, variableNames[j],"Vs",variableNames[i]))
            }
        }
    }

    associationList = c(associationList,crossTerms)

    if (any(class(aovTest) == "aov"))
    {
        summaryAov = summary(aovTest)
        return(associationList[summaryAov[[1]][["Pr(>F)"]]<0.05&!is.na(summaryAov[[1]][["Pr(>F)"]])])
    }
    else
        stop("aovTest needs to be of class aov")

}