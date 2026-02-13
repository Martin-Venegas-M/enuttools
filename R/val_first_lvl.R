#' Validate questions in a survey dataset
#'
#' This function creates a data frame with dummy columns indicating whether each
#' variable in a survey dataset meets first-level validation criteria.
#'
#' The first-level validation criteria are:
#'
#' \enumerate{
#'   \item The cell value is allowed by the enabling condition associated with the column.
#'   \item The cell value falls within the expected range for the column.
#' }
#'
#' These criteria can produce the following types of errors:
#' \itemize{
#'   \item \strong{error1}: Missing value. The response is missing, even though the enabling condition indicates there should be a response.
#'   \item \strong{error2}: Failed filter. A response is present, even though the enabling condition indicates there should be no response.
#'   \item \strong{error3}: Out of range. The response is outside the expected range.
#' }
#'
#' For each variable evaluated, the function generates up to three new columns
#' (one per error type). If \eqn{c} columns are validated, the output will contain
#' \eqn{c * 3} columns. Each column is a dummy variable indicating with \eqn{1} whether
#' the error occurred for that case.
#'
#' The function assumes the existence of a structured metadata input that includes
#' the question name (column name), enabling condition, valid range, and type of range.
#'
#' @param data A data frame containing the survey data.
#' @param variables A character vector with the names of the variables to validate.
#' @param expressions A character vector of R expressions specifying the enabling
#' @return A wide \code{data.frame} with dummy columns indicating validation errors.
#' @export
#' @examples
#' df <- tibble::tibble(x = c(1, 1, 0), y = c(1, NA, NA))
#'
#' # You can uses string or expression inputs
#' val_first_lvl(df, "y", "x == 1")
#' val_first_lvl(df, y, x == 1)
#'
#' # Or even a combination...
#' val_first_lvl(df, "y", x == 1)
#' val_first_lvl(df, y, "x == 1")
#'
#' # Also accepts vectors!
#' val_first_lvl(df, variables = c(x, y), expressions = c(TRUE, x == 1))
#' val_first_lvl(df, variables = c("x", "y"), expressions = c("TRUE", "x == 1"))

val_first_lvl <- function(data, variables, expressions) {
  #! IMPLEMENTAR error2
  #! IMPLEMENTAR rlang::abort() para validar que variables y expressions sean del mismo largo
  #? IMPLEMENTACION FUTURA: que el nombre de "error1" sea variable
  # Capturar expresiones
  variables_expr <- rlang::enquo(variables) |> rlang::quo_get_expr()
  expressions_expr <- rlang::enquo(expressions) |> rlang::quo_get_expr()

  # Detectar si los inputs son escalares o vectores
  # Nota: cuando los inputs son vectores, las listas siempre parten con c (por el c())
  if (expressions_expr[[1]] == expr(c)) {
    # Eliminar el c de la lista de expresiones
    variables_expr[[1]] <- NULL
    expressions_expr[[1]] <- NULL

    # Crear listas de variables y expresiones
    # (que cada elemento del vector de inpur sea un sym o quo por si solo dentro de una lista)
    variables <- purrr::map(
      variables_expr,
      \(x) x |> rlang::enquo() |> rlang::as_name()
    )

    expressions <- purrr::map(
      expressions_expr,
      \(x) x |> rlang::enquo()
    )

    # Aplicar error1 en loop!
    df_val <- purrr::map2(
      variables,
      expressions,
      ~ error1(data = data, var = .x, exp_quo = .y)
    ) |>
      purrr::list_cbind()
  } else {
    # Si no parte con c, significa que es un scalar
    df_val <- error1(data, rlang::ensym(variables), rlang::enquo(expressions))
  }

  return(df_val)
}
