#' Impute missing values due to validation errors in a survey dataset
#'
#' This function imputes observations that triggered first-level validation alerts to
#' missing values (-777) within a data frame. It's designed to handle various data types
#' (numeric, character, and date) based on a separate validation dummy data frame
#'
#' @param data A data frame containing the original survey data.
#' @param data_val A data frame containing validation dummy variables. These dummies indicate
#' whether a specific variable in @param data has a first-level validation error.
#' @param error A character vector specifying the type(s) of first-level errors to impute.
#'
#' Possible options are:
#'
#' \itemize{
#'   \item \strong{error1}: Missing value. The response is missing, even though the enabling condition indicates there should be a response.
#'   \item \strong{error2}: Failed filter. A response is present, even though the enabling condition indicates there should be no response.
#'   \item \strong{error3}: Out of range. The response is outside the expected range.
#' }
#'
#' @details
#'
#' \itemize{
#'   \item \strong{Numeric variables}: Imputed to \code{-777}.
#'   \item \strong{Character variables}: Imputed to \code{"-777"} (as a string).
#'   \item \strong{Date variables}: Imputed to \code{as.Date("777-01-01")}.
#' }
#'
#' @section Assumptions:
#' This function relies on the following assumptions regarding the input data frames
#' \enumerate{
#'   \item \code{data_val} must be a wide data frame.
#'   \item \code{data_val} must contain dummy columns where \code{1} indicates a validation error for a specific variable.
#'   \item The variable names in \code{data_val} must follow a specific structure: error_varname,
#' where \code{@{param error}} is one of the error types specified (e.g., \code{"error1"}, \code{"error2"}, \code{"error3"})
#' and \code{varname} is the original variable name from the \code{data} data frame.
#' For example, if you are imputing \code{"error1"} for a variable named \code{"age"}, the dummy column
#' in \code{data_val} should be named \code{"error1_age"}.
#' }
#'
#' @return A modified version of \code{data} with the specified first-level validation values
#'   imputed according to their data type.
#'
#' @importFrom lubridate is.Date
#' @importFrom stringr str_remove
#'
#' @export

edit_valprimer <- function(data, data_val, error) {
    # Create vectors with names of variables, according to their class in 'data',
    numericas <- paste0(error, "_", names(data %>% select(where(is.numeric))))
    categoricas <- paste0(
        error,
        "_",
        names(data %>% select(where(is.character)))
    )
    fechas <- paste0(error, "_", names(data %>% select(where(is.Date))))

    # Create vectors with colnames to impute
    col_imput_numericas <- data_val %>%
        select(any_of(numericas)) %>% # Select numeric variables of 'data' available in 'data_val'
        names() %>%
        str_remove(., "^[^_]*_") # Extract the name of the variables after the first underscore (delete "'error'_" of variable name)

    col_imput_categoricas <- data_val %>%
        select(any_of(categoricas)) %>% # Select character variables of 'data' available in 'data_val'
        names() %>%
        str_remove(., "^[^_]*_")

    col_imput_fechas <- data_val %>%
        select(any_of(fechas)) %>% # Select date variables of 'data' available in 'data_val'
        names() %>%
        str_remove(., "^[^_]*_")

    df <- data %>%
        # Impute numeric missing values (-777)
        mutate(
            across(
                all_of(col_imput_numericas), # Across al columns in vector...
                ~ if_else(
                    data_val[[paste0(error, "_", cur_column())]] %in% 1,
                    -777,
                    .
                ) # if current column of data_val takes value 1, impute -777; otherwise, don't do anything.
            ) # ! IMPORTANTE: hay que usar %in% porque sino imputa NA's para MUCHOS casos
        ) %>%
        # Impute character missing values ("-777")
        mutate(
            across(
                all_of(col_imput_categoricas),
                ~ if_else(
                    data_val[[paste0(error, "_", cur_column())]] %in% 1,
                    "-777",
                    .
                )
            )
        ) %>%
        # Impute date missing values ("777-01-01")
        mutate(
            across(
                all_of(col_imput_fechas),
                ~ if_else(
                    data_val[[paste0(error, "_", cur_column())]] %in% 1,
                    as.Date("777-01-01"),
                    .
                )
            )
        )

    return(df)
}
