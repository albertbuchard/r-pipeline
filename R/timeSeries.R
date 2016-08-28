# time series tools

#' timeSeriesTotalVariation
#' @note DevStatus: one pass - utility 3/5
#' TODO(Albert): Integrate in more general time series analysis tools 
#'
#' Function to calculate the total variation of a time series contained in a vector timeSeries
#' @export
timeSeriesTotalVariation = function(timeSeries) {
    if (!is.list(timeSeries))
        timeSeries = list(timeSeries)

    output = NULL
    for (timeSerie in timeSeries)
    {
        totalVariation = 0
        for (t in 2:NROW(timeSerie))
        {
            totalVariation = totalVariation + abs(timeSerie[t] - timeSerie[t-1])
        }
        output = c(output, totalVariation)
    }
    return(output)
}