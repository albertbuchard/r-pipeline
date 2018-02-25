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



#' Applies a function using a bilateral window step = 1
#'
#' @param v vector upon which to apply funs
#' @param width size of the window
#' @param funs function to apply to the vector
#' @param ... additional parameters to pass to funs
#'
#' @export
rollsmooth = function (v, width, funs = mean, ...) {
  if (!is.vector(v)) {stop("v is not a vector")}

  l = NROW(v)
  if (width > l) { width = l}
  count = vector(mode = "numeric", length = l)
  values = vector(mode = "numeric", length = l)
  for (start in 1:(l-width+1)) {
    end = min(start + width -1, l)
    i_start = max(l-start+1,width)
    i_end = max(i_start-width+1, 1)
    count[start:end] = count[start:end] +1
    count[i_start:i_end] = count[i_start:i_end] +1

    values[start:end] = values[start:end] + funs(v[start:end], ...)
    values[i_start:i_end] = values[i_start:i_end] + funs(v[i_start:i_end], ...)
  }

  values / count
}

#' Fills a vector with either previous value if no fill is given or with the fill argument
#' if the predicate is true for the vector entry
#'
#' @param .v vector to fill
#' @param .predicate either
#'        an atomic in this case values that are equal to it will be filled
#'        a formula: will be transformed to a function by calling as_mapper
#'        a function: will be used as is
#' @param .default default value if no previous correct value was encountered
#' @param .fill if set all element of the vector for which the predicate is true will be replaced by
#' this value. If NULL, the previous correct value (or default if non encountered) will be used.
#'
#' @export
fill_at = function(.v, .predicate = 0, .default = NA, .fill = NULL) {
  if (is.atomic(.predicate)) {
    .predicate = as_mapper(~ . == .predicate)
  } else if (is.formula(.predicate)) {
    .predicate = as_mapper(.predicate)
  } else if (!is.function(.predicate)) {
    stop("fill_last_if: .predicate should be either atomic or a function.")
  }

  lastGood = ifelse(is.null(.fill), .default, .fill)
  for (i in 1:NROW(.v)) {
    if (.predicate(.v[i])) {
      .v[i] = lastGood
    } else if (!is.null(.fill)) {
      lastGood = .fill
    } else {
      lastGood = .v[i]
    }
  }

  return(.v)
}
