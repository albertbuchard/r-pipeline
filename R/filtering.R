# filtering

#' constraintVariable
#' @DevStatus : one pass - utility 4/4 - clarify the scope, this function should be made more general 
#'
#' Filter a constrained variable for some subject who do not respect certain observation constraints
#'
#' @description Function that replace values of a certain constrained variables to a selected value (default to NA) for subject who did not fill certain constraints.
#' Constraints are described in term of a list of minimum number of observation either equal or differnt from a certain value. It is an OR testing.
#'
#' @export

constraintVariable = function (data, idVariable, constrainedVariable, constrainingVariables, asManyObservations = 5, equalTo = NULL, differentFrom = NULL, replaceBy = NA) {

    # helper function for classifying subjects based on the constrained variable vector in the data.table lookup
    isConstraintRespected = function (variableVector, asManyObservations, equalTo, differentFrom) {
        if (ntrue(variableVector==equalTo)>asManyObservations)
            return(T)
        else
            return(F)
    }

    data = as.data.table(data)
    data = copy(data)
    # make sure that if only scalar are given as arguement for multiple constraint variable, the arguement are replicated for each variables
    nConstraints = NROW(constrainingVariables)
    if (nConstraints>1) {
        if (NROW(equalTo)==1) {
            equalTo = rep(equalTo, nConstraints)
        }

        if (NROW(differentFrom)==1) {
            differentFrom = rep(differentFrom, nConstraints)
        }

        if (NROW(asManyObservations)==1) {
            asManyObservations = rep(asManyObservations, nConstraints)
        }
    }

    writeLines(paste0("Adding ", paste0(constrainedVariable,"Constrained"), " to the Data."))

    validSubjectDT = NULL

    data[, paste0(constrainedVariable,"Constrained") := get(constrainedVariable)]
    data[, paste0(constrainedVariable,"Constrained") := replaceBy]


    for (i in 1:nConstraints) {
        constrainingVariable = constrainingVariables[i]
        asManyObservationsForI = asManyObservations[i]
        equalToForI = equalTo[i]
        differentFromForI = differentFrom[i]


        if (!is.null(equalToForI)&!is.na(equalToForI)) {
            # select only the subjects that have more than asManyObservation of constrainingVariable equal to equalTo
            # find id that satisfy constraints
            print(c(constrainingVariable, asManyObservationsForI,  equalToForI))
            data[, tmpIsSelectedId := isConstraintRespected(get(constrainingVariable), asManyObservations = asManyObservationsForI, equalTo = equalToForI), by=idVariable]


            # Set constrained variable column to its value only for selected subjects

            data[(tmpIsSelectedId == T)&(get(constrainingVariable) == equalToForI), paste0(constrainedVariable,"Constrained") := get(constrainedVariable)]




            if (NROW(constrainingVariables)>1) {
                data[, paste0(constrainedVariable,"ConstrainedFor",constrainingVariable,equalToForI) := get(constrainedVariable)]
                data[, paste0(constrainedVariable,"ConstrainedFor",constrainingVariable,equalToForI) := replaceBy]
                data[(tmpIsSelectedId == T)&(get(constrainingVariable) == equalToForI), paste0(constrainedVariable,"ConstrainedFor",constrainingVariable,equalToForI) := get(constrainedVariable)]
            }

            tempDT = data.table(constrainingVariable = paste0(constrainingVariable,equalToForI),
                                numberOfValidSubjects = NROW(data[((tmpIsSelectedId == T)&!duplicated(get(idVariable))), get(idVariable)]))
            data[, tmpIsSelectedId := NULL]
            validSubjectDT = rbind(validSubjectDT, tempDT)
        } else {
            if (!is.null(differentFromForI)&!is.na(differentFromForI)) {
                # select only the subjects that have more than asManyObservation of constrainingVariable different from differentFrom
                # find id that satisfy constraints
                data[, tmpIsSelectedId := isConstraintRespected(get(constrainingVariable), asManyObservations = asManyObservationsForI, differentFrom = differentFromForI), by=idVariable]

                # Set constrained variable column to its value only for selected subjects
                data[(tmpIsSelectedId == T)&(get(constrainedVariable) != differentFromForI), paste0(constrainedVariable,"Constrained") := get(constrainedVariable)]

                if (NROW(constrainingVariables)>1) {
                    data[, paste0(constrainedVariable,"ConstrainedFor",constrainingVariable,differentFromForI) := get(constrainedVariable)]
                    data[, paste0(constrainedVariable,"ConstrainedFor",constrainingVariable,differentFromForI) := replaceBy]
                    data[(tmpIsSelectedId == T)&(get(constrainingVariable) != differentFromForI), paste0(constrainedVariable,"ConstrainedFor",constrainingVariable,differentFromForI) := get(constrainedVariable)]
                }


                tempDT = data.table(constrainingVariable = paste0(constrainingVariable,differentFromForI),
                                    numberOfValidSubjects = NROW(data[((tmpIsSelectedId == T)&!duplicated(get(idVariable))), get(idVariable)]))
                data[, tmpIsSelectedId := NULL]
                validSubjectDT = rbind(validSubjectDT, tempDT)

            } else {
                stop("equalTo or differentFrom needs to be different from NULL to specify the constrain")
            }

        }
    }

    print(validSubjectDT)

    return(data)
}