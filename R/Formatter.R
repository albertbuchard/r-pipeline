#' Formatter Object
#'
#'
#' @description Class allowing to format data following a certain data_definition. If no data_definition is given, the varibale names
#' can be reformated to snake_case automatically using rename
#'
#'
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(data = NULL, data_definition = NULL, rename = T, check_type = T)}}{This method is used to create an instance of the Formatter class }
#'   \item{\code{set_data (data, data_definition = NULL, auto_rename = F, check_type = T) }}{This method is used to set the data to be formatted.  }
#'   \item{\code{set_data_definition (data_definition = NULL, check_type = T)}}{This methods allows to setup the data definition propert and reformat the data accordingly. }
#'   \item{\code{get_data_definition ()}}{Returns current data_definition, if none is set it returns a default object. }
#'   }
#'
#' @section Fields:
#' \describe{
#'      \item{\code{data_definition}}{data.table or data.frame defining the format of data.
#' Fields are : variable_name, formatted_name, variable_type, is_subject_id, is_group_variable, source (raw measurement : "raw" , or computed from: "variable_a,variable_b")
#' description: range values - how was it obtained - what does it represent - how can it be reproduced }
#'   }
#'
#' @examples
#' \dontrun{
#' data <- data.table(a = c(10,7), b = c("A", "B"))
#' data_definition <- data.table(variable_name = c("a", "b", "c"), formatted_name = c("a", "beta", "ceta"), variable_type = c("numeric", "factor", "character"), source = c("raw", "a", "raw"), description = c("0 to 10 - measure of awesomness", "A,B, or C - category of awesomnes computed from a", "just C"))
#' }
#'
#' @docType class
#' @keywords format, formatting, naming, data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @export

Formatter <- R6Class (classname = "Formatter",

                      public = list(
                        subject_id_variable = NULL,
                        group_variables = NULL,

                        # Public Methods
                        initialize = function  (data = NULL, data_definition = NULL, auto_rename = T, check_type = T) {
                          if (!is.null(data)) {
                            self$set_data(data, data_definition, rename, check_type)
                          }
                        },

                        set_data = function (data, data_definition = NULL, auto_rename = T, check_type = T) {

                          # check and set data
                          if(!any(class(data)%in%c("data.table", "data.frame"))) {
                            stop("Formatter.set_data: data needs to be of type data.frame or data.table")
                          }

                          private$unformatted_data <- as.data.table(copy(data))
                          use_definition <- F

                          if (!is.null(data_definition)){
                            if(!any(class(data_definition)%in%c("data.table", "data.frame")))
                              stop("Formatter.set_data: data_definition needs to be of type data.frame or data.table")

                            if (auto_rename) {
                              auto_rename <- F
                              warning("Formatter.set_data: auto_rename will not be used if data_definition is != NULL")
                            }

                            private$format_and_set_data_definition(data_definition)
                            use_definition <- T
                          }

                          private$format_data(use_definition, auto_rename, check_type)

                        },

                        set_data_definition = function (data_definition, check_type = T) {
                          if (class(data_definition)%nin%c("data.frame", "data.table")) {
                            stop("Formatter.set_data_definition: data_definition needs to be of class data.frame or data.table")
                          }

                          private$format_and_set_data_definition(data_definition)
                          private$update_definition()
                          private$format_data(T, F, check_type)
                        },

                        get_data_definition = function (forget_unformatted_names = F) {
                          if (!is.null(private$data_definition)) {
                            private$update_definition()
                            return(private$data_definition)
                          } else {

                            formatted <- private$formatted_data

                            if (forget_unformatted_names) {
                              variable_names <- names(formatted)
                            } else {
                              variable_names <- names(private$unformatted_data)
                            }

                            variable_names_formatted <- names(formatted)

                            variables_data = NULL
                            for (i in seq(1, NROW(variable_names_formatted))) {
                              column <- formatted[[variable_names_formatted[i]]]
                              column_type <- NULL
                              column_value <- NULL
                              column_description <- ""
                              if (is.logical(column)) {
                                column_type <- "logical"
                              }
                              if (is.numeric(column)) {
                                column_type <- "numeric"
                                column_value <- paste("Value range [", specify_decimal(min(column, na.rm = T),2),",",specify_decimal(max(column, na.rm = T),2),"], average is ", specify_decimal(mean(column, na.rm = T),2), " (SD ",specify_decimal(sd(column, na.rm = T),2),"). ")
                              }
                              if (is.character(column)) {
                                column_type <- "character"
                              }
                              if (is.factor(column)) {
                                column_type <- "factor"
                                column_value <- paste("Factors are: ", paste(unique(column), collapse = ", "),". ")
                              }

                              if (is.null(column_type)) {
                                stop(paste("Formatter.get_data_definition: column ", variable_names_formatted[i], " is neither of type numeric, character, facotr or logical : ", class(column)))
                              }

                              column_value <- paste(column_value, "Number of NAs ", ntrue(is.na(column)), ". ")
                              column_data <- data.table(name = variable_names[i], name_formatted = variable_names_formatted[i], type = column_type, values = column_value, description = column_description)

                              variables_data <- rbind(variables_data, column_data)
                            }

                            data_definition <- data.table (variable_name = as.character(variables_data$name),
                                                           variable_type = as.character(variables_data$type),
                                                           formatted_name = as.character(variables_data$name_formatted),
                                                           is_subject_id = F,
                                                           is_group_variable = F,
                                                           values = as.character(variables_data$values),
                                                           description = as.character(variables_data$description))
                            # TODO : make a column for names which probably needs to be redone if _a_ or _aa_ in it

                            return(data_definition)
                          }
                        },

                        # Utility Methods
                        get_variable_values = function (variable_name, use_type = NULL) {
                          if (variable_name%in%names(private$unformatted_data)) {
                            column <- private$unformatted_data[[variable_name]]
                          } else if (variable_name%in%names(private$formatted_data)) {
                            column <- private$formatted_data[[variable_name]]
                          } else {
                            stop(paste0("Formatter.get_variable_values: variable with name ", variable_name, " not found."))
                          }

                          column_type <- NULL
                          column_value <- NULL
                          column_description <- ""
                          if ((is.logical(column))||(use_type == "logical")) {
                            column_type <- "logical"
                          } else if ((is.numeric(column))||(use_type == "numeric")) {
                            column_type <- "numeric"
                            column_value <- paste("Value range [", specify_decimal(min(column, na.rm = T),2),",",specify_decimal(max(column, na.rm = T),2),"], average is ", specify_decimal(mean(column, na.rm = T),2), " (SD ",specify_decimal(sd(column, na.rm = T),2),"). ")
                          } else if ((is.character(column))||(use_type == "character")) {
                            column_type <- "character"
                          } else if ((is.factor(column))||(use_type == "factor")) {
                            column_type <- "factor"
                            column_value <- paste("Factors are: ", paste(unique(column), collapse = ", "),". ")
                          }

                          if (is.null(column_type)) {
                            stop(paste("Formatter.get_data_definition: column ", variable_name, " is neither of type numeric, character, facotr or logical : ", class(column)))
                          }

                          column_value <- paste(column_value, "Number of NAs ", ntrue(is.na(column)), ". ")

                          return(column_value)
                        },

                        print = function(...) {
                          cat("Formatter object\n", sep = "")
                          writeLines(paste("data is set :", is.null(private$unformatted_data)))
                          if (!is.null(private$data)) {
                            writeLines(c("variable names :", names(private$formatted_data)))
                            writeLines(paste("data_definition is set :", is.null(private$data_definition)))
                            if (is.null(private$data_definition)) {
                              writeLines(paste("Variables are auto_renamed to snake_case:", private$is_auto_renamed))
                            } else {
                              writeLines(paste("Variable type is checked :", is.null(private$data)))
                            }
                          }
                          invisible(self)
                        },

                        get_formatted_names = function (old_names, definition = NULL, use_definition = T) {
                          if (use_definition) {
                            if (!is.null(definition)) {
                              writeLines("Formatter.get_formatted_name: Using specified definition to return formatted names.")
                              definition <- data.table(definition)
                              names <- c(definition[,as.character(variable_name)],definition[,as.character(formatted_name)])
                              nins <- old_names%nin%names
                              result = NULL
                              for(i in 1:NROW(old_names)) {
                                if (nins[i]) {
                                  writeLines(paste0("Formatter.get_formatted_name: ",  old_names[i], " not found"))
                                  result = c(result, old_names[i])
                                } else {
                                  result = c(result, definition[(variable_name%in%old_names[i])|(formatted_name%in%old_names[i]), as.character(formatted_name)])
                                }
                              }
                              return(result)
                            } else if (!is.null(private$data_definition)) {
                              writeLines("Formatter.get_formatted_name: Using already set definition to return formatted names.")
                              names <- c(private$data_definition[,as.character(variable_name)],private$data_definition[,as.character(formatted_name)])
                              nins <- old_names%nin%names
                              result = NULL
                              for(i in 1:NROW(old_names)) {
                                if (nins[i]) {
                                  writeLines(paste0("Formatter.get_formatted_name: ",  old_names[i], " not found"))
                                  result = c(result, old_names[i])
                                } else {
                                  result = c(result, private$data_definition[(variable_name%in%old_names[i])|(formatted_name%in%old_names[i]), as.character(formatted_name)])
                                }
                              }
                              return(result)
                            }
                          }

                          # if no definition found auto-format to snake case
                          writeLines("Formatter.get_formatted_name: No definition specified - auto-formatting names to snake_case")
                          names <- copy(old_names)
                          names <- gsub("([[:space:]])(\\w)", "\\U\\2", names, perl = T)
                          names <- gsub("([-|.])", "_", names)
                          names <- gsub("([A-Z])", "_\\L\\1", names, perl = T)
                          names <- sub("^_", "", names)
                          names <- gsub("[_]{2,}", "_", names)
                          names <- gsub("_([a-z]{1})_([a-z]{1})_([a-z]{1})_",  "_\\1\\2\\3_", names)
                          names <- gsub("_([a-z]{1})_([a-z]{1})_",  "_\\1\\2_", names)
                          names <- gsub("_([a-z]{1})$",  "\\1", names)
                          names <- gsub("([a-z]{1})([0-9]{1,})$",  "\\1_\\2", names)
                          names <- sub("^([a-z]{1})_", "\\1", names)

                          return(names)
                        },

                        # Getters and Setters
                        get_data = function() {
                          return(private$unformatted_data)
                          invisible(self)
                        },
                        formatted = function () {
                          return(private$formatted_data)
                          invisible(self)
                        },
                        unformatted = function () {
                          return(self$get_data())
                          invisible(self)
                        },
                        averaged = function () {
                          return(self$getAveragedData())
                          invisible(self)
                        },
                        get_averaged_data = function () {
                          return(private$averagedData)
                          invisible(self)
                        }
                      ),

                      private = list(
                        # Fields
                        test = NULL,
                        unformatted_data = NULL,
                        data_definition = NULL,
                        formatted_data = NULL,
                        is_defined = NULL,
                        is_auto_renamed = NULL,
                        is_typed = NULL,

                        # Methods
                        format_data = function (use_definition = T, auto_rename = T, check_type = T) {
                          if (!is.null(private$unformatted_data)) {
                            writeLines("Data is being reformated, $formatted() will give you the new formated Data, $unformatted() will get you the unformated data.")

                            data <- private$unformatted_data
                            formatted <- copy(private$unformatted_data)
                            data_definition <- private$data_definition


                            private$is_auto_renamed <- F
                            private$is_typed <- F

                            if ((!is.null(data_definition))&&(use_definition)) {
                              # format of data definition is :
                              # --- variable_name
                              # --- formatted_name
                              # --- variable_type
                              # --- is_subject_id
                              # --- is_group_variable
                              # --- source:
                              #       * raw measurement : "raw"
                              #       * or computed from: "variable_a,variable_b"
                              # --- description: range values - how was it obtained - what does it represent - how can it be reproduced
                              # TODO: discuss about tests or constraining values
                              # @example
                              # data <- data.table(a = c(10,7), b = c("A", "B"))
                              # data_definition <- data.table(variable_name = c("a", "b", "c"), formatted_name = c("a", "beta", "ceta"), variable_type = c("numeric", "factor", "character"), source = c("raw", "a", "raw"), description = c("0 to 10 - measure of awesomness", "A,B, or C - category of awesomnes computed from a", "just C"))
                              if (any(names(data)%nin%c(data_definition$variable_name, data_definition$formatted_name)))
                                stop(paste(c("Formatter.format_data: ", names(data)[names(data)%nin%data_definition$variable_name]," is not defined in data_definition")))

                              setnames(formatted, data_definition$variable_name, data_definition$formatted_name)
                              reformated_names <- names(data) != names(formatted)
                              if (any(reformated_names)) {
                                writeLines(paste("Formatter.format_data: column names reformated from:", paste(names(data)[reformated_names]) ,"to:", paste(names(formatted)[reformated_names])))
                              } else {
                                writeLines("Formatter.format_data: column names were already the same as specified in the data definition, no name formatting necessary.")
                              }

                              self$subject_id_variable <- NULL
                              self$group_variables <- NULL
                              for (i in seq(1,NROW(data_definition))) {
                                variable_definition = data_definition[i,]

                                if (check_type) {
                                  switch (variable_definition$variable_type,
                                          "numeric" = {
                                            if (!is.numeric(formatted[[variable_definition$formatted_name]])) {
                                              writeLines(paste("Formatter.format_data: column ", variable_definition$formatted_name, " coerced to numeric (was ", class(formatted[[variable_definition$formatted_name]])))
                                              formatted[[variable_definition$formatted_name]] = as.numeric(formatted[[variable_definition$formatted_name]])
                                            }

                                          },
                                          "character" = {
                                            if (!is.character(formatted[[variable_definition$formatted_name]])) {
                                              writeLines(paste("Formatter.format_data: column ", variable_definition$formatted_name, " coerced to character (was ", class(formatted[[variable_definition$formatted_name]])))
                                              formatted[[variable_definition$formatted_name]] = as.character(formatted[[variable_definition$formatted_name]])
                                            }

                                          },
                                          "factor" = {
                                            if (!is.factor(formatted[[variable_definition$formatted_name]])) {
                                              writeLines(paste("Formatter.format_data: column ", variable_definition$formatted_name, " coerced to factor (was ", class(formatted[[variable_definition$formatted_name]])))
                                              formatted[[variable_definition$formatted_name]] = as.factor(formatted[[variable_definition$formatted_name]])
                                            }
                                          },
                                          "logical" = {
                                            if (!is.logical(formatted[[variable_definition$formatted_name]])) {
                                              writeLines(paste("Formatter.format_data: column ", variable_definition$formatted_name, " coerced to logical (was ", class(formatted[[variable_definition$formatted_name]])))
                                              formatted[[variable_definition$formatted_name]] = as.logical(formatted[[variable_definition$formatted_name]])
                                            }
                                          },
                                          {
                                            stop(paste(c("Formatter.format_data: ", variable_definition$formatted_name," is of invalid type: ", variable_definition$variable_type," in data_definition")))
                                          })
                                  private$is_typed <- T
                                }

                                if (variable_definition$is_subject_id) {
                                  # Check if subject_id_variable is not already set to another variable name
                                  if ((!is.null(self$subject_id_variable))&&(self$subject_id_variable != variable_definition$formatted_name)) {
                                    stop(paste("Formatter.format_data: At least two variables ", variable_definition$formatted_name," and ", self$subject_id_variable, " are both set as subject IDs. Pick one."))
                                  }
                                  self$subject_id_variable <- variable_definition$formatted_name

                                  writeLines(paste(c("Formatter.format_data: ", variable_definition$formatted_name, " set as subject IDs.")))
                                }

                                if (variable_definition$is_group_variable) {
                                  self$group_variables <- c(self$group_variables, variable_definition$formatted_name)
                                  writeLines(paste("Formatter.format_data: ", variable_definition$formatted_name, " added to grouping variables."))
                                }
                              }


                              private$is_defined <- T
                            } else {
                              private$is_defined <- F

                              if (auto_rename) {
                                # transform data table names to snake_case
                                names <- gsub("([[:space:]])(\\w)", "\\U\\2", names(data), perl = T)
                                names <- gsub("([-|.])", "_", names)
                                names <- gsub("([A-Z])", "_\\L\\1", names, perl = T)
                                names <- sub("^_", "", names)
                                names <- gsub("[_]{2,}", "_", names)
                                names <- gsub("_([a-z]{1})_([a-z]{1})_([a-z]{1})_",  "_\\1\\2\\3_", names)
                                names <- gsub("_([a-z]{1})_([a-z]{1})_",  "_\\1\\2_", names)
                                names <- gsub("_([a-z]{1})$",  "\\1", names)
                                names <- gsub("([a-z]{1})([0-9]{1,})$",  "\\1_\\2", names)
                                names <- sub("^([a-z]{1})_", "\\1", names)
                                names(formatted) <- names

                                private$is_auto_renamed <- T

                                # inform user
                                reformated_names <- names(data) != names(formatted)
                                if (any(reformated_names)) {
                                  writeLines(paste("Formatter.format_data: column names reformated from:", paste(names(data)[reformated_names]) ,"to:", paste(names(formatted)[reformated_names])))
                                } else {
                                  writeLines("Formatter.format_data: column names were already the same as auto format, no name formatting necessary.")
                                }


                              }
                            }

                            private$formatted_data <- formatted

                          } else {
                            stop("Formatter.format_data: Data is not set (== NULL)")
                          }

                        },

                        format_and_set_data_definition = function (data_definition) {
                          if (class(data_definition)%nin%c("data.frame", "data.table")) {
                            stop("Formatter.format_and_set_data_definition: data_definition needs to be of class data.frame or data.table")
                          }
                          data_definition <- as.data.table(copy(data_definition))
                          data_definition$variable_name <- as.character(data_definition$variable_name)
                          data_definition$variable_type <- as.character(data_definition$variable_type)
                          data_definition$formatted_name <- as.character(data_definition$formatted_name)
                          data_definition$description <- as.character(data_definition$description)

                          data_names <- names(data)
                          missing_variables <- names(data)%nin%c(data_definition$variable_name)
                          if (any(missing_variables)) {
                            writeLines(paste0("Formatter.format_data: ", ntrue(missing_variables)," variables are missing from the definition : ", paste0(data_names[missing_variables], collapse = ", ")))
                          }

                          current_definition = private$data_definition
                          if (is.null(current_definition)) {
                            current_definition = self$get_data_definition()
                          }

                          for (i in 1:NROW(current_definition)) {
                            variable_name = current_definition[i, variable_name]
                            definition_index = data_definition$variable_name == variable_name

                            if(ntrue(definition_index)>1) {
                              warning(paste0("Formatter.format_and_set_data_definition: more than one variable with the name ", variable_name," in the new definition "))
                              next
                            }

                            if(!any(definition_index)) {
                              warning(paste0("Formatter.format_and_set_data_definition: no definition for variable ", variable_name," in the new definition "))
                              next
                            }

                            current_definition[i, ] <- data_definition[definition_index]
                          }

                          private$data_definition <- current_definition
                        },

                        update_definition = function () {
                          #updates data_definition "values" field
                          data_definition <- private$data_definition

                          if (is.null(data_definition)) {
                            data_definition <- self$get_data_definition()
                          }

                          for (i in 1:NROW(data_definition)) {
                            variable_name <- data_definition[i, variable_name]
                            data_definition[i, values := self$get_variable_values(variable_name, data_definition[i, variable_type])]
                          }

                          writeLines("Formatter.update_definition: data_definition updated with variable values.")
                        }

                      ))



