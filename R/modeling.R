# modeling

#' crossValidatedPredictor
#' @note DevStatus: one pass - utility ?/5
#' TODO(Albert): Discuss methodology ++ Use caret prob better
#'
#' @description Model selection tool with K-time cross validation and final validation on preserved data.
#'
#' @param data
#' @param dVarname = NULL
#' @param dependantVariable
#' @param independantVariables
#' @param type = "leaveOneOut"
#' @param family = "gaussian"
#' @param kTimes = NULL
#' @param loopOverModels = T
#' @param preservedData = F
#' @param plotInfo = T
#' @param TestWithRandomError = T
#'
#' @export

crossValidatedPredictor = function (data,
                                    idVarname = NULL,
                                    dependantVariable,
                                    independantVariables ,
                                    type = "leaveOneOut",
                                    family = "gaussian",
                                    kTimes = NULL,
                                    loopOverModels = T,
                                    preservedData = F,
                                    plotInfo = T,
                                    FTestWithRandomError = T) {

    data = copy(data)

    if (is.null(idVarname)) {
        data[, crossValId := 1:NROW(data)]
        idVarname = "crossValId"
    }

    ## K-time cross validation based on the number of ids
    idList = data[, get(idVarname)]

    if (is.null(kTimes)) {
        kTimes = NROW(idList)
    }
    writeLines(paste("Starting a", kTimes ,"Times Cross validation algorithm"))


    # If presevedData option set
    # Preserve test set for a final test with the best model
    if (preservedData) {
        preservedProportion = 0.3
        writeLines(paste("Preserving", preservedProportion,"of data for ultimate test"))
        preservedTestSet = sample(idList, round(preservedProportion*NROW(idList)))
        idList = idList[idList%nin%preservedTestSet]
        kTimes = NROW(idList)
    }

    # If model selection
    # Build formula strings for each combination of dimension for the regression
    formulaStrings = NULL
    nIndependant = NROW(independantVariables)
    if (loopOverModels) {
        for (i in 1:nIndependant) {
            colnamesCombination = combinations(n = nIndependant, r = i, v = independantVariables)

            for (j in 1:NROW(colnamesCombination)) {
                formulaString = paste0(dependantVariable, "~",paste0(colnamesCombination[j,], collapse = "+"))
                formulaStrings = c(formulaStrings, formulaString)
            }

        }
    } else {
        # no model selection
        formulaString = paste0(dependantVariable, "~",paste0(independantVariables, collapse = "+"))
        formulaStrings = formulaString
    }

    # setup the output variables
    residualLog = NULL
    coeficientsLog = NULL

    # Start the K-Time loop of crossvalidation
    writeLines("Starting K-Times cross validation loop")
    for (i in 1:kTimes) {
        # test/training depends on type of crossvalidation
        if (type=="leaveOneOut") {
            # take out the ith subject
            trainingIds = idList[(1:kTimes)%nin%i]
            testId = idList[i]
        }

        # setup added row
        residualLogToAdd = data.table(kId = i)

        # test for all the possible formulas
        for (j in 1:NROW(formulaStrings)) {
            # get a formula object from the jth formula string
            formulaObject = as.formula(formulaStrings[j])

            # Linear regression using a gaussian GLM or a betaregression (logitLink with approximated variance)
            coefList = rep(0,(nIndependant+1))
            if (family == "betareg") {
                resGLM = betareg(formula = formulaObject, data = data[id%in%trainingIds,])
                coefList[1:NROW(resGLM$coefficients[[1]])] = resGLM$coefficients[[1]]
            } else {
                resGLM = glm(formula = formulaObject, data = data[id%in%trainingIds,], family = "gaussian")
                coefList[1:NROW(resGLM$coefficients)] = resGLM$coefficients
            }

            # residuals here are the sum of squared residuals SSR
            # training error
            predictTraining = predict(resGLM, data = data[id%in%trainingIds,])
            nonNaIndex = as.numeric(names(predict(resGLM, data = data[id%in%trainingIds,])))

            ssrTraining = sum((data[id%in%trainingIds,][[dependantVariable]][nonNaIndex] - predictTraining)^2, na.rm=T)

            # SSR of training with the null model
            if (is.na(ssrTraining)) {
                ssrTrainingNullModel = NA
            } else {
                ssrTrainingNullModel = sum((data[id%in%trainingIds,][[dependantVariable]] - mean(data[id%in%trainingIds,][[dependantVariable]], na.rm =T))^2, na.rm=T)
            }

            # predict the test set with the coef obtained with the training set
            resPredict = predict(resGLM, data[id==testId,])

            # compute the residuals
            ssrToAdd = sum((data[id==testId, ][[dependantVariable]] - resPredict)^2)

            # get residuals of the null model
            if (is.na(ssrToAdd)) {
                ssrNullModel = NA
            } else {
                ssrNullModel = sum((data[id==testId, ][[dependantVariable]] - mean(data[id%in%trainingIds,][[dependantVariable]], na.rm =T))^2)
            }


            # add the residuals of this specific model
            residualLogToAdd[, (formulaStrings[j]) := list(abs(ssrToAdd))]

            # log the coeficients for that model and that kValue
            coefsToAdd = data.table(kId = i, formula = formulaStrings[j])

            coefsToAdd[, c(paste0("Coefs",1:(nIndependant+1))) := as.list(coefList)]

            # number of subjects
            N = NROW(nonNaIndex)
            # number of variables
            P = NROW(all.vars(as.formula( formulaStrings[j])))-1

            coefsToAdd[, c("trainingSSR", "trainingSSRNullModel", "N", "P","testSSRFullModel", "testSSRNullModel", "F", "pHO") := list(ssrTraining, ssrTrainingNullModel, N, P, ssrToAdd, ssrNullModel)]

            coeficientsLog = rbind(coeficientsLog, coefsToAdd)
        }

        # Add the residuals to the final residual log
        residualLog = rbind(residualLog, residualLogToAdd)

    }

    r2Training =  1 - (sum(coeficientsLog[, trainingSSR]/coeficientsLog[, N], na.rm= T) / sum(coeficientsLog[, trainingSSRNullModel]/coeficientsLog[, N], na.rm=T))
    r2Test = 1 - (sum(coeficientsLog[, testSSRFullModel], na.rm= T) / sum(coeficientsLog[, testSSRNullModel], na.rm=T))
    writeLines(paste("Full model on training set with mean R2 = ", r2Training, " and sqrt(MSE) = ", sqrt(mean(coeficientsLog[, trainingSSR]/coeficientsLog[, N], na.rm=T))))
    writeLines(paste("Full model on test set with mean R2 = ", r2Test, " and sqrt(MSE) = ", sqrt(mean(coeficientsLog[, testSSRFullModel], na.rm=T))))

    # F test is not adequate.. if models have the same number of predictors - do a F test between all full models and null models (same df)
    # if (NROW(unique(coeficientsLog[, P]))==1) {
    #     numberOfNonNA = ntrue(!is.na(coeficientsLog[, testSSRFullModel]))
    #     P = unique(coeficientsLog[, testSSRFullModel])[1]
    #     probabilityTestFullModelIsSameAsNull = 1-pf(1-r2Test, P-1, numberOfNonNA-P)
    #     if (probabilityTestFullModelIsSameAsNull<0.05) {
    #         writeLines(paste("After cross validation, full model predict data significantly better on training (p = ", probabilityTestFullModelIsSameAsNull, ", F = ", 1-r2Test," [",P-1,",",numberOfNonNA-P,"])"))
    #     }
    #
    # }
 stop()


    if (loopOverModels) {

        # Find the most predictive formula/beta
        if (NCOL(residualLog)==2) {
            nameCol = colnames(residualLog)[2]

            meanResiduals = residualLog[, nameCol, with=F]/ntrue(!is.na(residualLog[[2]]))
            meanResiduals = colSums(meanResiduals, na.rm=T)
        } else {
            meanResiduals = colSums(residualLog[[2:NCOL(residualLog)]], na.rm=T)/ntrue(!is.na(residualLog[[2]]))
        }

        bestResidualValue = meanResiduals[which(meanResiduals == min(meanResiduals, na.rm = T))]
        bestResidualValueModel = names(meanResiduals)[which(meanResiduals == min(meanResiduals, na.rm = T))]

        # get all variables from the formula string - the first one is the independant variable
        writeLines(paste("The best model after K-Times crossvalidation is:", bestResidualValueModel))
        writeLines(paste("With residuals equal to", bestResidualValue))
        variablesInFormula = all.vars(as.formula(bestResidualValueModel))
        independantVariables = variablesInFormula[2:NROW(variablesInFormula)]

        if (plotInfo) {
            meltedResidualLog = melt(residualLog, id.vars = "kId")
            ggplot(meltedResidualLog[variable!=bestResidualValueModel,], aes(x=kId, y=value))+
                geom_point(col=1) + geom_smooth(col=1)+
                geom_point(data = meltedResidualLog[variable==bestResidualValueModel,], col=2)+
                geom_smooth(data = meltedResidualLog[variable==bestResidualValueModel,], col=2) + theme_bw()
            meltedCoeficients = melt(coeficientsLog[formula==bestResidualValueModel,], id.vars = c("kId","formula") )
            ggplot(meltedCoeficients[variable!="Coefs1"], aes(x=kId, y=value, col=variable)) + geom_point()
        }

        # Test it on the excluded set varying the parameters using those found for each kIteration

        # Get the residuals from a fit on the whole excluded set
        writeLines("Starting to test on the preserved data")
        dataPreserved = copy(data[id%in%preservedTestSet,])

        fit = glm(formula = as.formula(bestResidualValueModel), data = dataPreserved, family = "gaussian")
        real = data.matrix(dataPreserved[, dependantVariable, with=F])
        predicted = predict(fit, dataPreserved)
        residuals = abs(predicted-real)

        coeficientsForBestModel = coeficientsLog[formula==bestResidualValueModel, ]
        residualsVector = NULL
        minResidual = 900000

        bestFitResiduals = NULL
        bestFitPredicted = NULL
        bestFitCoeficients = NULL
        for(k in 1:NROW(coeficientsForBestModel)) {
            coeficientsForK = coeficientsForBestModel[k]

            # predict dependant variable
            fit$coefficients = as.matrix(coeficientsForK[, paste0("Coefs", 1:(NROW(independantVariables)+1)), with=F])
            predicted = predict(fit, dataPreserved)
            residuals = abs(predicted-real)
            percentError = residuals/real

            # manual predict
            # Get the columns of the data corresponding to the variables in the formula
            featureMatrix = cbind(matrix(data = 1,ncol = 1,nrow = NROW(dataPreserved)),data.matrix(dataPreserved[, independantVariables, with=F]))
            predictedMatrix = featureMatrix %*% t(as.matrix(coeficientsForK[, paste0("Coefs", 1:(NROW(independantVariables)+1)), with=F]))
            residualMatrix = abs(predictedMatrix - data.matrix(dataPreserved[, dependantVariable, with=F]))

            residualsVector = c(residualsVector, mean(residuals, na.rm=T))

            # add a FTest here comparing the new model with the best model so far
            if (mean(residuals, na.rm=T)<minResidual) {
                bestFitResiduals = residuals
                bestFitPredicted = predicted
                bestFitCoeficients = coeficientsForK
                minResidual = mean(residuals, na.rm=T)
                bestFitpredicitonAccuracy = mean(1 -residuals / real, na.rm= T)
            }

        }

        if (plotInfo) {
            print(ggplot(data.table(realValue = real, residuals = bestFitResiduals), aes(real, residuals)) + geom_point() + geom_smooth())
            plot = ggplot(data.table(realValue = real, predicted = bestFitPredicted), aes(real, predicted))+geom_point()+stat_function(fun = function(x){return(x)}, geom="line")
            print(plot)
        }

        fTestconclusionString = ""
        if (FTestWithRandomError) {
            writeLines("Comparing the selected model to a normally distributed random model")
            # Check wether the model better predict the dependant variable than a mixed model with only a normally distriuted error term centered on the mean
            # Build the model centered on the mean of the dependant variable and using the variance of the dependant variable

            # "training"
            meanDependantVariable = mean(data[, get(dependantVariable)], na.rm=T)
            sdDependantVariable = sd(data[, get(dependantVariable)], na.rm=T)

            writeLines(paste("Model built using a normal distriution of Mean ", meanDependantVariable, " and standard deviation of ", sdDependantVariable))
            # Prediction == random sampling for that distribution
            sizePreserved = NROW(dataPreserved)
            predictedVector = rnorm(n = sizePreserved, mean = meanDependantVariable, sd = sdDependantVariable)

            # Get residuals
            residualsSimpleModel = abs(predictedVector - real)

            writeLines(paste("Mean of residuals for normal model is ", mean(residualsSimpleModel, na.rm=T)))

            # Compute F value follows F-distribution (L - L0, N - L)

            model1residuals = as.matrix(bestFitResiduals)
            model0residuals = as.matrix(residualsSimpleModel)
            L = NROW(independantVariables)
            L0 = 1
            N = NROW(na.omit(model0residuals))

            # check with Van der Ville which one is good
            fValue = ((t(na.omit(model0residuals)) %*% na.omit(model0residuals)) - (t(na.omit(model1residuals)) %*% na.omit(model1residuals))) / (L-L0)
            fValue = fValue / ((t(na.omit(model1residuals)) %*% na.omit(model1residuals))/(N-L))
            writeLines(paste("F value (", (L-L0) ,",", (N-L) ,") between best model and normal model : ", fValue))
            probabilityH0 = df(fValue, df1 = (L-L0), df2 = (N-L))
            fTestconclusionString = paste("The probability that the best fit model gives the same amount of information as a random model is ", probabilityH0)

            ## Prediction accuracy = 1 - (abs(val-predicted)/val)))
            restictedModelPredicitonAccuracy = mean(1 - abs(predictedVector - real) / real, na.rm= T)

            ## Training on preserved data
            # get a formula object from the jth formula string
            formulaObject = as.formula(bestResidualValueModel)

            # regress using a gaussian GLM on the dimension of independant variables to predict dependant variable
            if (family == "betareg") {
                resGLM = betareg(formula = formulaObject, data = dataPreserved)
            } else {
                resGLM = glm(formula = formulaObject, data = dataPreserved, family = "gaussian")
            }


            # predict conners scores
            resPredict = predict(resGLM, dataPreserved)

            # compute the residuals
            residualsFullPreserved = abs(dataPreserved[[dependantVariable]] - resPredict)

            fullPreservedPredicitonAccuracy = mean(1 - residualsFullPreserved / dataPreserved[[dependantVariable]], na.rm= T)

            # ## comparison FTest with training data
            # fValue = ((t(na.omit(model0residuals)) %*% na.omit(model0residuals)) - (t(na.omit(model1residuals)) %*% na.omit(model1residuals))) / (L-L0)
            # fValue = fValue / ((t(na.omit(model1residuals)) %*% na.omit(model1residuals))/(N-L))
            # writeLines(paste("F value (", (L-L0) ,",", (N-L) ,") between best model and normal model : ", fValue))
            #
            # probabilityH0 = df(fValue, df1 = (L-L0), df2 = (N-L))
            # fTestconclusionString = paste("The probability that the best fit model gives the same amount of information as a random model is ", probabilityH0)

        }
        stop()
        writeLines("---------------------------------------")
        kBest = which(residualsVector==min(residualsVector, na.rm=T))
        writeLines(paste("Best model:", bestResidualValueModel))
        writeLines(fTestconclusionString)
        writeLines(paste0("Mean residual for best model is ", mean(residualsVector, na.rm=T)))
        writeLines(paste0("Min Error [k == ", kBest ,"] is ", min(residualsVector, na.rm=T)))
        writeLines(paste("Best coeficients:"))
        variablesInFormula[1] = "(Intercept)"
        setnames(bestFitCoeficients, paste0("Coefs", 1:(NROW(independantVariables)+1)), variablesInFormula)
        print(bestFitCoeficients[, variablesInFormula, with=F])
        #
        # writeLines(paste("Restricted model predict the data with", restictedModelPredicitonAccuracy, "accuracy."))
        # writeLines(paste("Full model trained on training set predicts the preserved data with", bestFitpredicitonAccuracy, "accuracy."))
        # writeLines(paste("Full model trained on preserved data predicts the data with", fullPreservedPredicitonAccuracy, "accuracy."))
        #
    }



  #  writeLines(paste("If preserved dataset used for training and testing, mean residuals would have been : ", mean(residualsIfTrainedOnIt)))

}

#' sigmoid function
#'
#'@description credits: Kyriakos Chatzidimitriou
#'http://kyrcha.info/2012/07/08/tutorials-fitting-a-sigmoid-function-in-r/
#'
sigmoid = function(x, params=c(1,1,0), type = "exp") {
  switch(type,
         exp={
           return(params[1] / (1 + exp(-params[2] * (x - params[3]))))
         },
         power={
           return(param[1]^param[3]/param[1]^param[3]+param[2]^param[3])
         },
         stop("sigmoid: invalid type (should be exp or power)."))

}