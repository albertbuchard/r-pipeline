## dimensionality reduction

#' faReady
#' @note Dev status : no pass yet - Usefulness: ?/5
#
#' @description After scaling the quantitative variables of the data table,
#' the function filters out subject with more than looseSubjectsWithMissingValuesOverAtLeast NAs.
#' It return the results of the fa function on those subjects for the specified variables.
#'
#' @export

faReady = function (data,
                    variables,
                    numberOfComponents = NULL,
                    toNumeric = T,
                    looseSubjectsWithMissingValuesOverAtLeast = NULL)
{

    ## Nested utility function for normalizing vectors
    scaleNormalize = function (vector)
    {
        vector = vector - mean(vector, na.rm = T)
        vector = vector / sd(vector, na.rm = T)
        return(vector)
    }

    ## Do not assume all variable are quantitative
    quantitativeVariables = NULL
    qualitativeVariable = NULL

    ## if data is not transformed to numerical values, the limit set for the
    ## limit of unique values defining a continuous variable is set to 7
    limitContinuous = 7
    if (toNumeric) {
        data = copy(data)
        toNumeric(data)
        limitContinuous = 1
    }

    ## Separate quantitative from qualitative variables ( default is to convert everything to numeric and make no separation)
    ## then columns are standardized (centered and scaled)
    for (i in 1:NROW(variables))
    {
        if (isContinuous(data[, variables[i], with=F], limitContinuous))
            quantitativeVariables = c(quantitativeVariables, variables[i])
        else
            qualitativeVariable = c(qualitativeVariable, variables[i])

        if(is.numeric(data[, get(variables[i])]))
            data[, (variables[i]) := scaleNormalize(get(variables[i]))]
    }

    ## Principal rotated with varimax
    if(is.null(numberOfComponents)) {
        parallelAnalysisObject = fa.parallel(data[, quantitativeVariables, with=F])
        numberOfComponents = parallelAnalysisObject$ncomp
    }


    rowIndices = 1:NROW(data)
    if (!is.null(looseSubjectsWithMissingValuesOverAtLeast)) {
        rowIndices = NULL
        newData = NULL
        j = 1
        for (i in 1:NROW(data)) {
            if (ntrue(is.na(data[i, variables, with=F]))<looseSubjectsWithMissingValuesOverAtLeast) {
                newData = rbind(newData,data[i,variables, with=F])
                rowIndices = c(rowIndices, j)
                j = j + 1
            } else {
                rowIndices = c(rowIndices, NA)
            }
        }
        data = newData
    }

    resultFA = fa(data[,variables,with=F], nfactors = numberOfComponents)

    return(resultFA)

}


#' PCAReady
#' @note Dev status: one pass - to clean -- Usefulness: 4/5
#'
#' Produce NA safe PCA using principal, as well as PCA from factominr.
#'
#' @description does fa analysis if numberOfComponents is NULL. Rotate varimax default.
#'
#' @export
PCAReady = function (data, variables,
                     numberOfComponents = NULL,
                     principalOnly = T,
                     toNumeric = T,
                     rotation = T,
                     looseSubjectsWithMissingValuesOverAtLeast = NULL)
{

    ## Nested utility function for normalizing vectors
    scaleNormalize = function (vector)
    {
        vector = vector - mean(vector, na.rm = T)
        vector = vector / sd(vector, na.rm = T)
        return(vector)
    }

    ## Do not assume all variable are quantitative
    quantitativeVariables = NULL
    qualitativeVariable = NULL

    ## if data is not transformed to numerical values, the limit set for the
    ## limit of unique values defining a continuous variable is set to 7
    limitContinuous = 7
    if (toNumeric) {
        data = copy(data)
        toNumeric(data, variables)
        limitContinuous = 1
    }

    ## separate quantitative from qualitative variables ( default is to convert everything to numeric and make no separation)
    ## then columns are standardized (centered and scaled)
    for (i in 1:NROW(variables))
    {
        if (isContinuous(data[, variables[i], with=F], limitContinuous))
            quantitativeVariables = c(quantitativeVariables, variables[i])
        else
            qualitativeVariable = c(qualitativeVariable, variables[i])

        if(is.numeric(data[, get(variables[i])]))
            data[, (variables[i]) := scaleNormalize(get(variables[i]))]
    }

    ## Principal rotated with varimax
    if(is.null(numberOfComponents)) {
        parallelAnalysisObject = fa.parallel(data[, quantitativeVariables, with=F])
        numberOfComponents = parallelAnalysisObject$ncomp
    }

    if (rotation)
        rotateString = "varimax"
    else
        rotateString = "none"

    rowIndices = 1:NROW(data)
    if (!is.null(looseSubjectsWithMissingValuesOverAtLeast)) {
        rowIndices = NULL
        newData = NULL
        j = 1
        for (i in 1:NROW(data)) {
            if (ntrue(is.na(data[i, variables, with=F]))<looseSubjectsWithMissingValuesOverAtLeast) {
                newData = rbind(newData,data[i,variables, with=F])
                rowIndices = c(rowIndices, j)
                j = j + 1
            } else {
                rowIndices = c(rowIndices, NA)
            }
        }
        data = newData
    }

    principalQuantitative = principal(data[, quantitativeVariables, with=F], nfactors = numberOfComponents, rotate = rotateString)

    if(principalOnly)
        return(list(principalQuantitative = principalQuantitative))

    ## Estimate the number of components
    ## nb = estim_ncpPCA(dataForFAMD[, quantitativeVariables, with=F], ncp.min = 0, ncp.max = 7, method.cv = "Kfold")

    ## SIMPLE REPLACE
    ## dataNAReplaced = data[, (variables) := lapply(.SD, function(feature) { replaceNAByMedianOrMostComonValue(feature) }), .SDcols = (variables)]
    ## PCAQuantitativeWithNAReplaced = PCA(dataNAReplaced[, quantitativeVariables, with=F], ncp = numberOfComponents)


    ## IMPUTED OBS
    imputedObservations = imputePCA(data[, quantitativeVariables, with=F], ncp = numberOfComponents)
    if("completeObs"%in%names(imputedObservations))
        imputedObservations = imputedObservations$completeObs
    PCAQuantitativeWithNAImputed = PCA(imputedObservations, ncp = numberOfComponents)

    ## Principal w/ rotation and imputed obs
    principalQuantitativeWithNAImputed = principal(imputedObservations, nfactors = numberOfComponents, rotate = rotateString)

    ## Principal w/ rotation without imputation beforehand but imputation afterwards to compute "imputed PCA scores"
    principalQuantitativeWithNAImputedAfterwards = principalQuantitative
    principalQuantitativeWithNAImputedAfterwards$scores = predict(principalQuantitative, imputedObservations, data[, quantitativeVariables, with=F])

    PCAQuantitativeJustObject = PCAQuantitativeWithNAImputed

    ## Rebuild score matrix with deleted subjects
    if (any(is.na(rowIndices))) {
        cat(ntrue(is.na(rowIndices))," subjects with ", looseSubjectsWithMissingValuesOverAtLeast, " or more NAs have been taken out of the PCA, without imputation, their score is NA.\n")
        principalQuantitativeWithNAImputedAfterwards$scores = principalQuantitativeWithNAImputedAfterwards$scores[rowIndices,]
        principalQuantitativeWithNAImputed$scores = principalQuantitativeWithNAImputed$scores[rowIndices,]
        principalQuantitative$scores = principalQuantitative$scores[rowIndices,]
        PCAQuantitativeWithNAImputed$ind$coord = PCAQuantitativeWithNAImputed$ind$coord[rowIndices,]
    }

    cat("Avg Sum squared difference in scores between no imputation and imputation before (different PCA components) is ", mean((principalQuantitativeWithNAImputed$scores - principalQuantitative$scores)^2, na.rm=T)/ntrue(!is.na(principalQuantitative$scores)),"\n")
    cat("Avg Sum squared difference in scores between no imputation and imputation after (same PCA components, sanity check) is ", mean((principalQuantitativeWithNAImputedAfterwards$scores - principalQuantitative$scores)^2, na.rm=T),"\n")
    cat("Avg Sum squared difference in scores between imputation before and after is ", mean((principalQuantitativeWithNAImputedAfterwards$scores - principalQuantitativeWithNAImputed$scores)^2, na.rm=T))


    ## Implementation for quantitative variables
    if (!is.null(qualitativeVariable))
        quantitativeAndQualitativeDataTable = cbind(imputedObservations,data[, qualitativeVariable, with=F])

    return(list(principalQuantitative = principalQuantitative,
                PCAQuantitativeWithNAImputed = PCAQuantitativeWithNAImputed,
                principalQuantitativeWithNAImputedBeforeHand = principalQuantitativeWithNAImputed,
                principalQuantitativeWithNAImputedAfterwards = principalQuantitativeWithNAImputedAfterwards,
                PCAjustObject = PCAQuantitativeJustObject))
}

#'
#'