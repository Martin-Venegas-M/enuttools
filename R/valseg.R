#' Validate questions in a survey dataset
#'
#' This function creates a data frame with dummy columns indicating whether each
#' second-level validation condition for a survey dataset is met.
#'
#' For each validation condition evaluated, the function generates a new column.
#' If \eqn{c} conditions are evaluated, the output will contain \eqn{c + 1} columns, where
#' the additional column is an id variable. Each column is a dummy variable
#' that indicates with \eqn{1} when an observation does not meet the validation criteria.
#'
#' The function assumes the existence of a structured metadata input that includes
#' the validation names and validation conditions.
#'
#' @param data A data frame containing the survey data.
#' @param name A character vector with the names identifying the validations to be
#' executed.
#' @param expr A character vector of R expressions specifying the conditions for each
#' validation.
#' @param id The name of the survey dataset column containing the unique identifier
#' variable for each row.
#' @param nec_justificar Logical value. If TRUE indicates necessity of justify some columns
#' @param justificar A character vector with columns to justify (imputes 0 to the result).
#' Example: c("inc_rph_r8", "inc_ch_r71")
#'
#' @return A wide \code{data.frame} with dummy columns indicating validation errors. Each
#' column contains a specific validation.
#' @export

valseg <- function(data, name, expr, id, nec_justificar = FALSE, justificar) {
  # Crear funcion base
  valseg_ifelse <- function(data, name, expr) {
    data <- data %>%
      dplyr::transmute(
        !!sym(name) := if_else((!!rlang::parse_expr(expr)), 1, 0)
      )
  }

  # Iterar para cada fila de la data
  df <- purrr::map2_dfc(name, expr, ~ valseg_ifelse(data, .x, .y))

  # Pegar identificador de fila a la base valseg
  df <- dplyr::bind_cols(
    tibble::tibble(!!id := data %>% dplyr::pull(!!sym(id))),
    df
  )

  if (nec_justificar) {
    df <- df %>% mutate(across(all_of(justificar), ~ if_else(. == 1, 0, .)))
  }

  return(df)
}
