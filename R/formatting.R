# formatting


#' SummaryToText BNICE
#' TODO(Albert): Example - Put in BeNice? - make it more general able to summarize all kinds of tests
#'
#' @description Produce text description of 95% CI APA style from a summary table, or an anovaTest.
#'
#' @param summaryTable A data table following the classic summarySE format group N value sd se and ci
#' @param type Either CI or SD
#'
#' @export
#'
#'
SummaryToText = function (summaryTable, anovaTest = NULL, type = "CI", precision = 2, printOut = F, significance = 0.05) {
    summaryTable = as.data.table(summaryTable)
    tableColumns = names(summaryTable)
    if (tableColumns[2]%nin%"N")
        stop("function SummaryToText works only for one independent variable summaries")

    nameOfVariable = tableColumns[3]
    nameOfGroupVariable = tableColumns[1]

    testText = NULL
    if (!is.null(anovaTest)&("aov"%in%class(anovaTest))) {

        # get the different result labels
        labels = attr(anovaTest$terms,"term.labels")

        # get the summary of the anova as well as dfs pvalues and fvalues
        summaryTest = summary(anovaTest)[[1]]
        dfs = summaryTest$Df
        df2 = dfs[NROW(dfs)]
        pValues =  summaryTest$`Pr(>F)`
        fValues = summaryTest$`F value`

        # those variables are not used for now
        mainEffectIndex = NULL
        interactionEffectIndices = NULL
        interactingVariable = NULL

        # start looping on the different result labels to format main and interaction effect of the group variable of interest
        for (i in 1:NROW(labels)) {
            # check if the current label hold the name of the group variable of interest
            hasVariableOfInterest = gregexpr(pattern = paste0("(",nameOfGroupVariable,")"), labels[i])[[1]][[1]] != -1

            # check if the effect is a main effect or an interaction effect
            isMainEffect = gregexpr(pattern = ":", labels[i])[[1]][[1]] == -1

            if(hasVariableOfInterest) {
                # label has variable of interest
                df1 = dfs[i]
                fValue = specify_decimal(fValues[i],precision)
                pValue = specify_decimal(pValues[i],precision)

                if(isMainEffect) {
                    mainEffectIndex = i
                    if (pValues[i]<significance) {
                        testText = c(testText, paste0("With a significant main effect of ", nameOfGroupVariable, " on ",nameOfVariable," F(",df1,", ",df2,") = ",fValue,", p = ", pValue,","))
                    } else {
                        testText = c(testText, paste0("No significant main effect of ", nameOfGroupVariable, " on ",nameOfVariable," F(",df1,", ",df2,") = ",fValue,", p = ", pValue,","))
                    }

                } else {
                    splitedVariables = strsplit(labels[i], ":")[[1]]
                    if (NROW(splitedVariables)==2) {
                        # this is an interaction effect with only two variables, get the interacting variable name (strip as factor gets  the variable name in a as.factor() string)
                        if (stripAsFactor(splitedVariables[1])==nameOfGroupVariable) {
                            interactingVariable = c(interactingVariable, stripAsFactor(splitedVariables[2]))
                        } else {
                            interactingVariable = c(interactingVariable, stripAsFactor(splitedVariables[1]))
                        }
                        interactionEffectIndices = c(interactionEffectIndices, i)

                        if (pValues[i]<significance) {
                            testText = c(testText, paste0("With a significant interaction effect between ", nameOfGroupVariable, " and ", interactingVariable," for ",nameOfVariable," F(",df1,", ",df2,") = ",fValue,", p = ", pValue,","))
                        } else {
                            testText = c(testText, paste0("No interaction effect between ", nameOfGroupVariable, " and ", interactingVariable," for ",nameOfVariable," F(",df1,", ",df2,") = ",fValue,", p = ", pValue,","))
                        }
                    }
                }
            }


        }

        if(printOut)
            writeLines(testText)
    }


    formatOut = NULL

    switch(type,
           CI = {
               for (i in 1:NROW(summaryTable)) {
                   value = summaryTable[i, get(nameOfVariable)]
                   lowCI = value - summaryTable[i, ci]
                   highCI = value + summaryTable[i, ci]

                   value = specify_decimal(value, precision)
                   lowCI = specify_decimal(lowCI, precision)
                   highCI = specify_decimal(highCI, precision)
                   string = paste0(nameOfVariable, " for group ", summaryTable[i,get(nameOfGroupVariable)]," (N = ", summaryTable[i,N],")")
                   string = paste(string, paste0(" is ", value, ", 95% IC [",lowCI,", ",highCI,"]"))
                   formatOut = c(formatOut, string)

                   if (printOut)
                           writeLines(string)
               }
           },
           {
               stop("type variable is not a valid formatting type")
           })


        if(!printOut)
                return(c(testText,formatOut))
}

#' VariableTableDescription
#' TODO(Albert): Put in BeNice ??
#'
#' @description Creates an HTML table to describe one or several variables in terms of Mean, SD, Range, Skewness, Kurtosis, Internal consistency methodology varies for variable types and will not be included.
#' Previously computed column can be added in the form of a vector of a matrix (m variables * n columns)
#'
#' @export

VariableTableDescription = function (data, variables, idVariable = "id", precision=2) {
    data = as.data.table(data)
    meltedData = melt(data, id.vars = idVariable, measure.vars = variables, na.rm=T)
    summaryData = as.data.table(summarySE(meltedData, measurevar = "value", groupvars = "variable"))

    allVariablesDataTable = NULL
    for (i in 1:NROW(variables)) {
        variableName = variables[i]
        variableData = summaryData[variable==variableName,]
        variableMean = specify_decimal(variableData[, value], precision)
        variableSD = specify_decimal(variableData[, sd], precision)
        variableRange = paste0(specify_decimal(variableData[,min], precision)," - ", specify_decimal(variableData[,max], precision))
        variableSkew = specify_decimal(skew(data[,get(variableName)]), precision)
        variableKurtosis = specify_decimal(kurtosi(data[,get(variableName)]), precision)

        toAdd = data.table(variable = variableName,
                           M = variableMean,
                           SD = variableSD,
                           Range = variableRange,
                           Skew = variableSkew,
                           Kurtosis = variableKurtosis)

        allVariablesDataTable = rbind(allVariablesDataTable, toAdd)
    }

    setnames(allVariablesDataTable, "variable", "")

    return(htmlTable(allVariablesDataTable))
}
