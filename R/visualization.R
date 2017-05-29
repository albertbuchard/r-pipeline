# visualization
# TODO(Albert): Discuss utility of all those functions - and how we should articulate BeNice and r-pipeline in the best possible way

#' PlotMeanSliding
#'
#' Plot the mean with error bars
#'
#' @export
#'

PlotMeanSliding = function (data, variable, slideOverVariable, groupBy = NULL, printTable = F, nbins=10, range=NULL) {

    data = as.data.table(data)

    # account for the bin 0
    nbins = nbins-1

    getGroup = function (range, nbins, value) {
        return(round(((value-range[1])/(range[2]-range[1]))*nbins)*(range[2]-range[1])/nbins)
    }

    if(is.null(range)) {
        range = range(data[,get(slideOverVariable)], na.rm = T)
    }

    data[, paste0(slideOverVariable,"grouped") := NULL]
    data[get(slideOverVariable)<=range[2]&get(slideOverVariable)>=range[1], paste0(slideOverVariable,"grouped") := getGroup(range, nbins, get(slideOverVariable))]

    tableData = summarySE(data, measurevar = variable, groupvars = c(paste0(slideOverVariable,"grouped"), groupBy), na.rm = T)

    if (printTable) {
        print(tableData)
    }

    pd = position_dodge(0.1)

    colorString = NULL
    if (!is.null(groupBy)) {
        colorString = paste0("as.factor(",groupBy,")")
    }

    plot = ggplot(tableData, aes_string(x=paste0(slideOverVariable,"grouped"), y=variable, col= colorString, group=groupBy)) +
        geom_errorbar(aes_string(ymin=paste0(variable,"-ci"), ymax=paste0(variable,"+ci")), colour="black", width=.05, position=pd) +
        geom_line(position=pd) +
        geom_point(position=pd, size=3)


    return(plot)
}

#' PlotOutlier
#'
#' Outlier visualization.
#'
#' \code{PlotOutlier} plots a specified outlier on various variable in
#' the dataset.
#'
#' @param data
#' @param idValue Either a numeric value indicating the row, or a string that will be
#'   match to the idVariable
#' @param idVariable Optional, A string containing the name of the idValue variable
#'   used for string matching with idValue. Default with "id"
#' @param variablesToPlot

PlotOutlier = function (data, idValue, idVariable = "id", variablesToPlot = NULL) {
    if (is.null(variablesToPlot)) {
        variablesToPlot = names(data)
        variablesToPlot = variablesToPlot[variablesToPlot!=idVariable]
    }

    if (is.character(idValue))
        outlier = data[get(idVariable)==idValue,]
    else
        outlier = data[idValue,]

    nVariables = NROW(variablesToPlot)
    if (nVariables>10)
    {
        nColForPlot = 2
        nRowForPlot = 5
    }else{
        if(nVariables<2){
            nColForPlot = 1
            nRowForPlot = 1
        }else{
            nColForPlot = 2
            nRowForPlot = ceil(nVariables/2)
        }
    }

    par(mar = rep(2, 4))
    par(mfrow=c(nRowForPlot,nColForPlot))

    for (i in 1:nVariables) {
        dataToPlot = data[,variablesToPlot[i], with=F]

        if (NROW(unique(dataToPlot))>1){
            cat("Plotting ", variablesToPlot[i], " \n")
            plot(dataToPlot)

            if (!is.na(outlier[,variablesToPlot[i], with=F])){
                title(variablesToPlot[i])
                abline(v = outlier[,variablesToPlot[i], with=F], col=2)
            }else{
                title(paste0(variablesToPlot[i], " is NA for outlier"))
            }
        }else{
            cat("Not enough data to plot ", variablesToPlot[i], " \n")
        }

    }

}

#' PrintSignificant
#'
#' Print significant ANOVA and plots from PlootXAgainstY
#'

PrintSignificant = function (XVsYOutput, type="short")
{
    if(!any(class(XVsYOutput)=="XVsYOutput"))
        stop("XVsYOutput argument needs to be of class XVsYOuput")

    for(i in 1:NROW(XVsYOutput[[4]]))
    {
        dependantVar = XVsYOutput[[4]][i]
        print(summary(XVsYOutput[[1]][[dependantVar]]))
        if (type=="all")
            print(XVsYOutput[[2]][grep(dependantVar, names(XVsYOutput[[2]]))])

        if (type=="short")
            lapply(XVsYOutput[[3]][grep(dependantVar, XVsYOutput[[3]])],
                   function(X){
                       print(XVsYOutput[[2]][[X]])
                   })

    }


}



#' PlotXAgainstY
#'
#' Produce an ANOVA and graphs for dependantVar ~ groupingVars, in data
#'
#' @description If outputType == "store" (default)
#' --- Returns in [[1]] ANOVA test for DependantVar ~ groupingVar[1]*groupingVar[2]*...
#' --- Returns in [[2]] various plots with descriptive name
#' --- Returns in [[3]] the names of plots in [[2]] that are significant
#' --- Returns in [[4]] the names of the dependant variables that were significant
#' If outputType == "print" the function printsall anova summaries and all plots
#' If outputType == "printSignificant" the functions only print significant results
#'
#' @export


PlotXAgainstY = function(dependantVar, groupingVars, data,
                                 outputType = "store",
                                 addNameOfDependantVarToPlot = F)
{

    ## Get RID of rows with at least one NA : complete obs
    allVars = c(groupingVars)
    naGroupingVarText = NULL
    for(i in 1:NROW(allVars))
    {
        varName = allVars[i]
        nna = NROW(data[is.na(get(varName)),])
        naGroupingVarText = c(naGroupingVarText,paste0(nna, " NA for ", varName))
        data = data[!is.na(get(varName)),]
    }

    data[, (groupingVars) := lapply(.SD, function(feature) { as.factor(feature) }), .SDcols = (groupingVars)]

    ## Prepare for recursion on all dependant variable if necessary (only one dependant var at a time)
    ## Option for a MANOVA for main statistics would be nice. TO ADD AS OPTION
    otherDependantVariable = NULL
    keptData = copy(data)
    if (NROW(dependantVar)>1)
    {
        otherDependantVariable = dependantVar[2:NROW(dependantVar)]
        addNameOfDependantVarToPlot = T
        dependantVar = dependantVar[1]
    }

    nna = NROW(data[is.na(get(dependantVar)),])
    naDependantVarText = paste0(nna, " NA for ", dependantVar)
    data = data[!is.na(get(dependantVar)),]


    plotPrefix = ""
    if (addNameOfDependantVarToPlot)
        plotPrefix = paste0(dependantVar,"By")


    aovFormula = paste(dependantVar, '~ ', paste0( "as.factor(", groupingVars, ")", collapse=' * ' ) )
    aovFormula = as.formula(aovFormula)
    anovaTest = aov(aovFormula, data = data, na.action=na.omit)

    summaryTable = summarySE(data, measurevar = dependantVar, groupvars = groupingVars, na.rm=T)

    if(outputType=="print") {
        print(summary(anovaTest))
        print(summaryTable)
    } else {
        plots = list()
    }




    ## Get significant plots right away
    significantPlots = GetSignificantPlotsXAgainstY (aovTest = anovaTest,
                                                             variableNames = groupingVars,
                                                             plotPrefix = plotPrefix)

    #allText contains all the summarizing text for all variable as well as all couples of variables
    allText = NULL

    # loop over all grouping variables for the plots and paired variable plots
    for(i in 1:NROW(groupingVars))
    {
        groupVar = groupingVars[i]

        ## Jitter Plot
        plotNew = ggplot(data, aes_string(y=dependantVar, x=groupVar, col=groupVar)) + geom_jitter(alpha=0.7)+theme_bw()

        if(outputType=="print")
            print(plotNew)
        else
            plots[[paste0(plotPrefix,groupVar,"Jitter")]] = plotNew

        ## Density Plot
        plotNew = DensityPlot(data, dependantVar, groupVar)
        # plotNew = DensityPlot(data.table(v1 = data[,get(dependantVar)], v2 = as.factor(data[,get(groupVar)])))

        if(outputType=="print")
            print(plotNew)
        else
            plots[[paste0(plotPrefix,groupVar,"Density")]] = plotNew

        # histogram dodged
        plotNew = ggplot(data, aes_string(x=dependantVar, fill=paste0("as.factor(",groupVar,")"))) +
            geom_histogram(binwidth=data[, mean(get(dependantVar))]*.1, position="dodge")

        if(outputType=="print")
            print(plotNew)
        else
            plots[[paste0(plotPrefix,groupVar,"HistogramDodged")]] = plotNew

        ## Summary bar plot
        dataGroupSummary = summarySE(data, measurevar=dependantVar, groupvars=groupVar, na.rm = T)

        plotNew = ggplot(dataGroupSummary, aes_string(y=dependantVar, x=groupVar, fill=groupVar)) +
            geom_bar(position=position_dodge(), stat="identity") +
            geom_errorbar(aes_string(ymin=paste0(dependantVar,"-ci"), ymax=paste0(dependantVar,"+ci")),
                          width=.2,                    # Width of the error bars
                          position=position_dodge(.9))+
            xlab(groupVar) +
            ylab(dependantVar) +
            ggtitle(paste0(dependantVar, " by ", groupVar, " (confidence interval)")) +
            theme_bw()

        if(outputType=="print")
            print(plotNew)
        else
            plots[[paste0(plotPrefix,groupVar,"Summary")]] = plotNew

        # check for homoskedasticity using the bartlett test
        bartlettRes = NULL
        bartlettPValue = NULL
        formula_string = paste(dependantVar, '~ ', groupVar)
        tryCatch({
          bartlettRes =  bartlett.test(as.formula(formula_string), data = data)
          bartlettPValue = specify_decimal(bartlettRes$p.value,2)
          varianceText = paste("non significative, in favor of homogenous variance across groups with p=",bartlettPValue)
          if (bartlettPValue <= 0.05) {
            varianceText = paste("significative, in favor of heterogenous variance across groups with p=",bartlettPValue)
          }
          allText = c(allText, paste(bartlettRes$method, "for", bartlettRes$data.name, "is", varianceText))

        }, error = function(e) {
          print(paste("Pipeline_r.PlotXAgainstY: bartlett test returned an error for forumla : ", formula_string))
        })


        #Summary text for each grouping variable depending on the significativity of the effect (and interaction will come in the next loop)
        allText = c(allText, SummaryToText(dataGroupSummary, anovaTest = anovaTest))

        ## Do every couple of variable
        j = 1
        while (j <= NROW(groupingVars))
        {
            if (j!=i)
            {
                dataGroupPairSummary = summarySE(data, measurevar=dependantVar, groupvars=c(groupVar,groupingVars[j]), na.rm = T)

                plotNew = ggplot(dataGroupPairSummary, aes_string(x=groupVar, y=dependantVar, fill=groupingVars[j])) +
                    geom_bar(position=position_dodge(), stat="identity") +
                    geom_errorbar(aes_string(ymin=paste0(dependantVar,"-ci"), ymax=paste0(dependantVar,"+ci")),
                                  width=.2,                    # Width of the error bars
                                  position=position_dodge(.9))+
                    xlab(groupVar) +
                    ylab(dependantVar) +
                    ggtitle(paste0(dependantVar, " by ", groupVar, " and ", groupingVars[j]," (confidence interval)")) +
                    theme_bw()

                if(outputType=="print")
                    print(plotNew)
                else
                    plots[[paste0(plotPrefix,groupVar,"Vs",groupingVars[j])]] = plotNew

            }

            j = j + 1

        }

    }

    if(outputType!="print")
    {
        anovaToConcatenate = NULL
        plotsToConcatenate = NULL
        significantPlotsToConcatenate = NULL
        significantDependantVariablesToConcatenate =  NULL
        summaryTablesToConcatenate = NULL
        allTextToConcatenate = NULL

        if (!is.null(otherDependantVariable))
        {
            storedRecursion = PlotXAgainstY(otherDependantVariable,
                                                    groupingVars,
                                                    keptData,
                                                    outputType = "store",
                                                    addNameOfDependantVarToPlot = addNameOfDependantVarToPlot)
            plotsToConcatenate = storedRecursion[[2]]
            anovaToConcatenate = storedRecursion[[1]]
            significantPlotsToConcatenate = storedRecursion[[3]]
            significantDependantVariablesToConcatenate = storedRecursion[[4]]
            summaryTablesToConcatenate = storedRecursion[[5]]
            allTextToConcatenate = storedRecursion[[6]]
        }


        if(!addNameOfDependantVarToPlot) {
            anovaOutput = anovaTest
            summaryTables = summaryTable
            fullText = c(allText, allTextToConcatenate, naDependantVarText)
        } else {
            anovaOutput = listWithKeys(dependantVar, anovaTest)
            anovaOutput = c(anovaOutput, anovaToConcatenate)
            summaryTables = listWithKeys(dependantVar, summaryTable)
            summaryTables = c(summaryTables, summaryTablesToConcatenate)
            fullText = listWithKeys(dependantVar, allText)
            fullText = c(allText, allTextToConcatenate, naDependantVarText)
        }


        plots = c(plots, plotsToConcatenate)
        significantPlots = c(significantPlots, significantPlotsToConcatenate)

        if (!is.null(summaryTablesToConcatenate)) {

        }
        c(summaryTable,summaryTablesToConcatenate)

        ## The last output is a vector of dependant variables name that hold at least one
        ## significant main or interaction effect
        significantVariable =  significantDependantVariablesToConcatenate
        if (any(summary(anovaTest)[[1]][["Pr(>F)"]]<0.05, na.rm = T))
            significantVariable = c(dependantVar,
                                    significantDependantVariablesToConcatenate)

        output =  list(anovaOutput,plots,significantPlots,significantVariable,summaryTables,fullText, naGroupingVarText)
        class(output) = c("list", "XVsYOutput")


        switch (outputType,
                printSignificantShort = { PrintSignificant(output, "short") },
                printSignificant = { PrintSignificant(output, "all") }
        )

        return(output)
    }
    else
        if (!is.null(otherDependantVariable))
            PlotXAgainstY(otherDependantVariable, groupingVars, data,
                                  outputType = outputType,
                                  addNameOfDependantVarToPlot = addNameOfDependantVarToPlot)




}

#' ScatterPlot
#'
#' General Plotting function - only plot column 1 versus column 2
#'
ScatterPlot = function(d)
{
    colnamesCouple = colnames(d)
    output = ggplot(d, aes_string(x=colnamesCouple[1], y=colnamesCouple[2]))+
        geom_jitter()+
        geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = TRUE, size = 2)
    return(output)
}

#' FrequencyPlot
#'
#' Plots column 1 vs column 2
#'

FrequencyPlot = function(d)
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


#' HeatmapPlot
#'
#' Plots Heatmap for column 1 vs column 2
#'
HeatmapPlot = function(d)
{
    colnamesCouple = colnames(d)
    output = ggplot(tableDF, aes_string(x=colnamesCouple[1], y=colnamesCouple[2])) +
        geom_tile(aes(fill = Freq), colour = "black") +
        scale_fill_gradient(low = "white", high = "steelblue")
    return(output)
}


#' DensityPlot
#'
#' Plots the density of a dependantVar, with the option to group the plot on one groupingVar
#'
DensityPlot = function (d, dependantVar = NULL, groupingVar = NULL)
{
    output = NULL

    makePlot = function (v1, v2=NULL)
    {
        if (is.null(v2))
            o = ggplot(d, aes_string(x=v1)) +
                geom_density(fill=2, alpha=0.3) +
                ggtitle(paste0("Density of ", v1)) +
                ylab("Density") +
                xlab(paste0(v1)) +
                theme_bw()
        else
            o = ggplot(d, aes_string(x=v1, col=v2)) +
                geom_density(aes_string(group=v2, colour=v2, fill=v2), alpha=0.3) +
                ggtitle(paste0("Density of ", v1, " accross ", v2)) +
                ylab("Density") +
                xlab(paste0(v1)) +
                theme_bw()
        return(o)
    }

    if(is.null(dependantVar))
    {
        colnamesCouple = colnames(d)
        if (NROW(colnamesCouple)>1)
            output = makePlot(colnamesCouple[1], colnamesCouple[2])
        else
            output = makePlot(colnamesCouple[1])
    }
    else
    {
        if (!is.null(groupingVar))
            output = makePlot(dependantVar, groupingVar)
        else
            output = makePlot(dependantVar)
    }

    return(output)

}
