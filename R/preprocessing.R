# Preprocessing 

#' toNumeric
#' @note DevStatus: one pass - utility 2/5
#' @description Safely fransforms variable in a data table as.numeric.
#'
#' @export
toNumeric = function (data, variables = NULL) {
    if(!any(class(data)=="data.frame"))
        stop("Data is not a data.frame or a data.table")


    if (is.null(variables))
        variables = names(data)

    checkAndConvert = function (x){
        if (!is.numeric(x))
            return(as.numeric(as.factor(x)))
        else
            return(x)
    }

    data[, (variables) := lapply(.SD, checkAndConvert), .SDcols = variables]
}

#' atecFuncSetNAToXFromDT
#' @note DevStatus: one pass - utility 5/5
#'
#' Sets NA to a certain value X in a DT (all columns)
#'
setNAToX = function(DT, X=0) { 
    for (j in names(DT))
        set(DT,which(is.na(DT[[j]])),j,x)
}

#' scrubNA
#' @notes DevStatus: one pass - utility 5/5
#' 
#' Delete rows with NA in at least one of the specified variables. If no variable is specified, the algorithm only deletes columns filled only with NA. 
#'
#' \code{atecFuncScribNA} srubs row or columns from a data.frame or data.table.
#'
#' @param data Contains a data.table or a data.frame.
#' @param variableNames Vector containing the names of the variables to scrub.
#'   If left empty, the algorithm will try to scrub the columns
#'
scrubNA = function(data, variableNames = NULL, duplicateData = T){
    if (duplicateData==T)
        data = copy(data)

    if (is.null(variableNames)) {
        # Performs a columnWise search - if columns has only NAs it is deleted
        variableNames = names(data)
        for(i in 1:NROW(variableNames)) {
            uniques = unique(data[,get(variableNames[i])])
            if (NROW(uniques)==1&is.na(uniques))
                data[, get(variableNames[i]) := NULL]
        }
    } else {
        # Performs a rowWise search and deletes row with NA on each variables 
        for (i in 1:NROW(variableNames)) {
            data = data[!is.na(get(variableNames[i])),]
        }
    }

    if(duplicateData==T)
        return(data)
}

#' CenterOnMeanOfMean
#'
#'  center the variable for each subject then add the mean of mean

CenterOnMeanOfMean = function (d, centeredVar, idVariable = "id", groupingVariable = NULL)
{
    if(any(class(d)=="data.frame"))
    {
        d= as.data.table(d)
        d[, (paste0(centeredVar,"CenterOnMeanOfMean")) := get(centeredVar) - mean(get(centeredVar)), by=idVariable]
        if (is.null(groupingVariable))
            d[, (paste0(centeredVar,"CenterOnMeanOfMean")) := get(paste0(centeredVar,"CenterOnMeanOfMean")) + mean(get(centeredVar))]
        else
            d[, (paste0(centeredVar,"CenterOnMeanOfMean")) := get(paste0(centeredVar,"CenterOnMeanOfMean")) + mean(get(centeredVar)), by=groupingVariable]
    }
    else
    {
        stop("d needs to be of class data.frame or data.table")
    }
}
