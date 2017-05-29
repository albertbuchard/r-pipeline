# small utility functions
# TODO(Albert): Discuss use - Filter


#' stripAsFactor
#'
#'  get rid of the as.factor() around a variable name
#'
#' @export

stripAsFactor = function (str) {

    strPos = gregexpr(pattern = "(as.factor\\(([a-z]+)\\))", str)

    if (strPos[[1]][[1]]==-1) {
        return(str)
    }

    beginPos = strPos[[1]][[1]]+10
    endPos = beginPos+attr(strPos[[1]], "match.length")-12

    strippedStr = substr(str, beginPos, endPos)


    return(strippedStr)

}

#' last returns the last element of a vector
#'
#' @export
#'
last = function (vector) {

  nrow = NROW(vector)

  return(vector[nrow])

}

#' getDistanceFromLastOccurenceInVector
#'
#' Returns a distance vector of the same size as input each eleemtn representing the distance from the last occurence of the value in the vector
#'
getDistanceFromLastOccurenceInVector = function (vector, value, searchDirection = 'before') {
  returnVector = NULL
  for(i in 1:NROW(vector)) {
    if (searchDirection == 'before') {
      pastOccurences = which(vector[1:i]==value)
      if (NROW(pastOccurences)<1){
        returnVector = c(returnVector,NA)
      } else {
        returnVector = c(returnVector, (i-last(pastOccurences)))
      }
    }

  }
  print(returnVector)
  return(returnVector)

}

#' isBetween check in a number is contained in an interval
#'
#' @export
isBetween = function (x,a,b) {
    if (is.na(x)) {
        warning("x was NA, returned False")
        return(F)
    }


    if (is.numeric(x)&is.numeric(a)&is.numeric(b)) {
        if (b<a) {
            c = b
            b = a
            a = c
        }
        return(x<b&x>a)
    } else {
        stop("a and b need to be of numeric type")
    }
}

#' isInRange
#'
#' check if a number is contained in an range tuple
#'
#' @export
isInRange = function (x,range) {
    if (is.na(x)) {
        warning("x was NA, returned False")
        return(F)
    }
    if (is.numeric(x)) {
        return(isBetween(x,range[1], range[2]))
    } else {
        stop("x needs to be of numeric type or NA")
    }
}

#' Euler's method for ODE first order
#'
#' @export

eulersMethod = function(a=1,b=-1,c=1,d=0, x0 = 1, fx0 = 2, step = 0.5, goal = 3) {
    # af' = bf + cx + d

    fx = fx0
    df = (b*fx+c*x0+d)/a
    for(x in seq(x0, goal, step)) {
        fx = fx + step*df
        df = (b*fx+c*x+d)/a
    }

    writeLines(paste0("f(",x,") = ",fx,"  --- using euler's method and a step of ", step))
    return(fx)
}


## 0.2 added functions
#' getInterpolationFunction
#'
#' @export

findExtremumsInRange = function (splineFunc,
                                 range,
                                 delta,
                                 errorTolerance = 0.1,
                                 excapeDistance = NULL) {
  if (class(splineFunc)!="function")
    stop("splineFunc not a function")

  if (is.null(excapeDistance)) {
    # the curve need to stay convex up or down for at least this distance to be considered a real turning point
    excapeDistance = 0.01*range[2]
  }

  prevDfunc = splineFunc(range[1], deriv = 1)

  beginRangeValue = splineFunc(range[1])
  endRangeValue = splineFunc(range[2])

  for(x in seq((range[1]+delta), range[2], delta)) {
    dfunc = splineFunc(x, deriv = 1)
    if (((abs(dfunc) < errorTolerance)&(sign(dfunc) == -sign(prevDfunc)))|(dfunc==0)) {

      writeLines(paste0("Turning point found at ", x))
    }
    prevDfunc = dfunc
  }
}


#' logit to odds
#'
#' Converts logit values from e.g a GLM logit, to odds ratio
#'
#' @export
#'

logitToOdds = function (logitValue) {
    return(exp(logitValue))
}

#' logit to probs
#'
#' Converts logit values from e.g a GLM logit, to probabilities
#'
#' @export
#'
logitToProbs = function (logitValue) {
    return(exp(logitValue)/(1+exp(logitValue)))
}

#' Winsorize a vector
#'
#' Permits to set a threshold for extreme values over a certain quantile. Outlier correction technique.
#'
#' @export
#'
Winsorize <- function(x, minval = NULL, maxval = NULL,
                      probs=c(0.05, 0.95), na.rm = FALSE) {

    # following an idea from Gabor Grothendieck
    # http://r.789695.n4.nabble.com/how-to-winsorize-data-td930227.html

    # in HuberM things are implemented the same way

    # don't eliminate NAs in x, moreover leave them untouched,
    # just calc quantile without them...

    # pmax(pmin(x, maxval), minval)

    # the pmax(pmin()-version is slower than the following

    if(is.null(minval) || is.null(maxval)){
        xq <- quantile(x=x, probs=probs, na.rm=na.rm)
        if(is.null(minval)) minval <- xq[1]
        if(is.null(maxval)) maxval <- xq[2]
    }

    x[x<minval] <- minval
    x[x>maxval] <- maxval

    return(x)

    # see also Andreas Alfons, KU Leuven
    # roubustHD, Winsorize

    # Jim Lemon's rather clumsy implementation:

    # #added winsor.var and winsor.sd and winsor.mean (to supplement winsor.means)
    # #August 28, 2009 following a suggestion by Jim Lemon
    # #corrected January 15, 2009 to use the quantile function rather than sorting.
    # #suggested by Michael Conklin in correspondence with Karl Healey
    # #this preserves the order of the data
    # "wins" <- function(x,trim=.2, na.rm=TRUE) {
    # if ((trim < 0) | (trim>0.5) )
    # stop("trimming must be reasonable")
    # qtrim <- quantile(x,c(trim,.5, 1-trim),na.rm = na.rm)
    # xbot <- qtrim[1]
    # xtop <- qtrim[3]
    # if(trim<.5) {
    # x[x < xbot]  <- xbot
    # x[x > xtop] <- xtop} else {x[!is.na(x)] <- qtrim[2]}
    # return(x) }

}

#' specifiy decimal
#'
#' Specify the decimal of a numeric input
#'
#' @export
#'
specify_decimal <- function(x, k) {
  if (is.double(x)) {
    return(format(round(x, k), nsmall=k))
  } else {
    return(x)
  }
}

## Match with negation
'%nin%' = Negate('%in%')
'%notin%' = Negate('%in%')


#' Sliding vector
#'
#' Slides vector by a certain amount of indices.
#'
#' @param vector
#' @param by numeric value indicating by how many indices the vector has to be slided
#'
#' @export
#'
slideVector = function (vector, by, fillIn = 0, keepSameLength = T)
{
    nrows = NROW(vector)

    if (abs(by) >= nrows)
        return(rep(fillIn, nrows))

    if (by > 0)
        vector = c(rep(fillIn, by), vector[1:(nrows-by)])
    else
        vector = c(vector[(abs(by)+1):nrows], rep(fillIn, abs(by)))
    return(vector)
}

####@@@@ Math Function / Matrix Manipulation @@@@####
#' vectorProportions
#'
#' simple utility function to get a proportion vector summing to 1
#'
#' @export
vectorProportions = function (vector)
{
    vector = vector / sum(vector)
    return(vector)
}

#' vectorRatioOfSimilarity
#'
#'  returns the point to point ratio of similarity between two vectors
#'
#'  @description if vector1 = vector2 - return 1. If no index  i exist such as vector1[i] == vector2[i] returns 0
#'
#' @export

vectorRatioOfSimilarity = function(vector1, vector2, na.rm = T)
{
    if(NROW(vector2)==1)
    {
        vector2 = rep(vector2, NROW(vector1))
    }
    else
        if (NROW(vector1)!=NROW(vector2))
            stop("Vectors need to be he same lenght to compare them (or vector2 needs to be a scalar)")

    if (na.rm)
    {
        vector1b = vector1
        vector1 = vector1[!is.na(vector1)&!is.na(vector2)]
        vector2 = vector2[!is.na(vector1b)&!is.na(vector2)]
    }

    return(ntrue(vector1==vector2)/NROW(vector1))
}

####@@@@ Variable classification @@@####
#' isContinuous
#'
#' Check if a variable as more than a limit number of unique values
#'
#' @export
#'
isContinuous = function(x, limit = 7)
{
    if(NROW(unique(x))>limit)
        return(T)
    else
        return(F)
}

isBinomial = function(x)
{
    if(NROW(unique(x))==2)
        return(T)
    else
        return(F)
}

####@@@@ multiple variable assignment in one line @@@@####
# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
    Envir = as.environment(-1)

    if (length(r) > length(l))
        warning("RHS has more args than LHS. Only first", length(l), "used.")

    if (length(l) > length(r))  {
        # warning("LHS has more args than RHS. RHS will be repeated.")
        r <- extendToMatch(r, l)
    }

    for (II in 1:length(l)) {
        do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
    }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
    s <- length(source)
    d <- length(destin)

    # Assume that destin is a length when it is a single number and source is not
    if(d==1 && s>1 && !is.null(as.numeric(destin)))
        d <- destin

    dif <- d - s
    if (dif > 0) {
        source <- rep(source, ceiling(d/s))[1:d]
    }
    return (source)
}

# Grouping the left hand side
g = function(...) {
    List = as.list(substitute(list(...)))[-1L]
    class(List) = c('lbunch')
    return(List)
}


# listWithKeys
# assign keys as key name either of a new list from all element entered as arguments
# or to an already created list contained in listToKey
listWithKeys = function (keys, ..., listToKey = NULL)
{
    if (is.null(listToKey))
        listToKey = list(...)

    if (NROW(keys)==NROW(listToKey))
    {
        names(listToKey) = keys
        return(listToKey)
    }
    else
        stop("keys and listToKey, or the number of item in the list, need to be of same length")
}

####@@@@ OTHERS @@@@####
nna = function (vector) {
    return(ntrue(is.na(vector)))
}
# NA Replacement
# replaceNAByMedianOrMostComonValue
replaceNAByMedianOrMostComonValue = function (vector)
{
    newVector = vector
    ## Categoriclal or continuous
    if (isContinuous(vector))
    {
        newVector[is.na(newVector)] = median(vector, na.rm = T)
    }
    else
    {
        mostComonValue = as.data.table(table(vector))[N == max(N, na.rm = T), vector]
        newVector[is.na(newVector)] = mostComonValue
    }

    return(newVector)
}

## Median.quartile
median.quartile <- function(x){
    out <- quantile(as.numeric(x), probs = c(0.5))
    names(out) <- c("y")
    return(out)
}

#' shorterSafer = function (f,...)
#'
#' Try catch, return NA if an error occurs
#'
#' @export

shorterSafer = function (f,...) {
    e = tryCatch(f(...), error = function(e) { return(NULL) })
    return(e)
}

#' ntrue
#'
#' little function without suffix to get the number of occurence of T in a logical vector
#'
#' @export
ntrue = function (vector) {
    return(NROW(vector[!is.na(vector)&vector==TRUE]))
}


#' summarySE
#'
#' @description Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#'
#' @param data a data frame.
#' @param measurevar the name of a column that contains the variable to be summariezed.
#' @param groupvars a vector containing names of columns that contain grouping variables.
#' @param na.rm a boolean that indicates whether to ignore NA's.
#' @param conf.interval the percent range of the confidence interval, default is 95%.
#'
#' @export

summarySE = function(data=NULL,
                     measurevar,
                     groupvars=NULL,
                     na.rm=FALSE,
                     conf.interval=.95,
                     .drop=TRUE)
{

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm),
                         min  = min    (xx[[col]], na.rm=na.rm),
                         max  = max    (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )

    # Rename the "mean" column
    datac <- plyr::rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

#' Circle drawing function
#'
#'http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#'@export
circleFun = function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}