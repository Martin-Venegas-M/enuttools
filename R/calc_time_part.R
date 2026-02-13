#' Derive participation or time-use variables from survey data
#'
#' This function creates derived variables for participation or time use
#' in time-use surveys based on a list of source variables.
#' It can generate either a participation indicator (\code{"part"}) or
#' a time-use variable (\code{"tiempo"}) depending on the specified type.
#'
#' @param data A data frame containing the source variables.
#' @param type A character string indicating the type of derived variable.
#'   Options are:
#'   \itemize{
#'     \item \code{"part"}: Creates a binary participation variable (0/1)
#'           or assigns a missing code if applicable.
#'     \item \code{"tiempo"}: Creates a time-use variable by summing across
#'           source variables, after recoding zeros and missing codes to \code{NA}.
#'   }
#' @param vars_fuente A character vector of column names used as source variables.
#' @param var_derivada A symbol or string giving the name of the new derived variable.
#' @param cod_perdido A numeric value indicating the code used for missing data
#'   in the source variables. Default is \code{-777}.
#'
#' @details
#' For \code{type = "part"}:
#' \itemize{
#'   \item If \code{informante_kish == 0}, the derived variable is set to \code{NA}.
#'   \item If all source variables equal the missing code, the derived variable is
#'         set to \code{cod_perdido}.
#'   \item If at least one source variable equals 1, the derived variable is set to \code{1}.
#'   \item Otherwise, it is set to \code{0}.
#' }
#'
#' For \code{type = "tiempo"}:
#' \itemize{
#'   \item Temporary variables are created from each source variable, where values equal
#'         to \code{0} or \code{cod_perdido} are recoded as \code{NA}.
#'   \item The derived variable is the row sum of these temporary variables, ignoring \code{NA}.
#'   \item If the resulting sum is 0, it is recoded as \code{NA}.
#'   \item Temporary variables are dropped from the final output.
#' }
#'
#' @return A data frame with the new derived variable added.
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- tibble(
#'   informante_kish = c(1, 0, 1),
#'   act1 = c(1, -777, 0),
#'   act2 = c(0, -777, 1)
#' )
#'
#' # Example: participation variable
#' derivar_vars(df, type = "part", vars_fuente = c("act1", "act2"), var_derivada = "part_act")
#'
#' # Example: time-use variable
#' derivar_vars(df, type = "tiempo", vars_fuente = c("act1", "act2"), var_derivada = "time_act")
#' }
#' @importFrom dplyr mutate across case_when if_else select
#' @importFrom rlang .data !!
#' @export

calc_time_part <- function(data, type = c("part", "tiempo"), vars_fuente, var_derivada, cod_perdido = -777) {
  type <- match.arg(type)

  if (type == "part") {
    result_part <- data %>% mutate(
      # Cramos las variable derivada con los siguientes criterios:
      !!var_derivada := case_when(
        informante_kish == 0 ~ NA, # Si no es la informante kish fija un NA
        rowSums(across(all_of(vars_fuente), ~ . == cod_perdido), na.rm = TRUE) == length(vars_fuente) ~ cod_perdido, # Si todas las variables fuente tienen codigo perdido, fija codigo perdido
        rowSums(across(all_of(vars_fuente), ~ . == 1), na.rm = TRUE) > 0 ~ 1, # Si hay al menos un 1 en en las variables fuente fija 1
        rowSums(across(all_of(vars_fuente), ~ . == 1), na.rm = TRUE) == 0 ~ 0 # Si no hay al menos un 1 en las variables fuente fija 0
      )
    )

    return(result_part)
  }

  if (type == "tiempo") {
    # ! IMPORTANTE: Esta estrategia es suceptible a que exista una variable que empiece por "temp_" en la bbddd
    result_tiempo <- data %>%
      mutate(
        across(all_of(vars_fuente), ~ replace(., . %in% c(0, cod_perdido), NA), .names = "temp_{col}"), # Crea una variable temporal por cada variable fuente, donde los 0 y el código de perdido fueron recodificados a NA
        !!var_derivada := rowSums(across(starts_with("temp_")), na.rm = TRUE), # Crea la variable derivada sumando todas las variables fuente temporales
        !!var_derivada := if_else(.data[[var_derivada]] == 0, NA, .data[[var_derivada]]) # Si es que la suma de la variable derivada es 0, recodifica a NA
      ) %>%
      select(-starts_with("temp_")) # Elimina las columnas temporales

    return(result_tiempo)
  }
}
