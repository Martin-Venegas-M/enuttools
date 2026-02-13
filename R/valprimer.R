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
#' @param datos A data frame containing the survey data.
#' @param variables A character vector with the names of the variables to validate.
#' @param expresiones A character vector of R expressions specifying the enabling
#'   conditions for each variable.
#' @param rangos A character vector indicating the allowed values or range for
#'   each variable. For text-type variables, this should indicate the minimum number
#'   of characters expected.
#' @param tipo_rangos A character vector indicating the type of range for each variable.
#'   Possible values: \code{normal}, \code{texto}, \code{tiempo} or \code{tiempo0}.
#' @param id The name of the column containing the unique identifier for each case.
#' @param nec_justificar Logical value. If \code{TRUE} indicate the necessity of justify some variables.
#' @param justificar A character vector with the name of the validation columns to justify.
#' For example: \code{c("error1_se3_2", "error1_tc1_n2_ds")}. This imputes 0 to the columns.
#' @param consider_edits Logical value. If \code{TRUE} a \code{-777} (missing value code) is inserted in the variable range.
#'
#' @return A wide \code{data.frame} with dummy columns indicating validation errors.
#'
#' @importFrom dplyr select filter mutate transmute if_else bind_cols pull any_of
#' @importFrom rlang sym parse_expr
#' @importFrom purrr map2_dfc
#' @importFrom magrittr %>%
#' @export

valprimer <- function(
  datos,
  variables,
  expresiones,
  rangos,
  tipo_rangos,
  id,
  nec_justificar = FALSE,
  justificar,
  consider_edits = FALSE
) {
  #! IMPORTANTE: PROBABLEMENTE SERA DEPRECADA POR val_first_lvl()
  # Iterar
  # Error1 y 2
  datos_error1 <- map2_dfc(variables, expresiones, ~ error1(datos, .x, .y))
  datos_error2 <- map2_dfc(variables, expresiones, ~ error2(datos, .x, .y))

  # Error3 #? NOTA: ESTO HAY QUE DARLE UNA VUELTA, YA ME PARECE DEMASIADO COPY-PASTE DE CÓDIGO

  rng <- data.frame(
    col1 = variables,
    col2 = rangos,
    col3 = tipo_rangos
  )

  if (consider_edits) {
    rng <- rng %>%
      mutate(col2 = if_else(col3 == "normal", rng_edit(col2), col2))
  }

  rng_normal <- rng %>% filter(col3 == "normal")
  rng_texto <- rng %>% filter(col3 == "texto")
  rng_tiempo <- rng %>% filter(col3 == "tiempo")
  rng_tiempo0 <- rng %>% filter(col3 == "tiempo0")

  datos_error3_normal <- map2_dfc(
    rng_normal$col1,
    rng_normal$col2,
    ~ error3(datos, .x, .y, type = "normal")
  )
  datos_error3_texto <- map2_dfc(
    rng_texto$col1,
    rng_texto$col2,
    ~ error3(datos, .x, .y, type = "texto")
  )
  datos_error3_tiempo <- map2_dfc(
    rng_tiempo$col1,
    rng_tiempo$col2,
    ~ error3(datos, .x, .y, type = "tiempo")
  )
  datos_error3_tiempo0 <- map2_dfc(
    rng_tiempo0$col1,
    rng_tiempo0$col2,
    ~ error3(datos, .x, .y, type = "tiempo0")
  )

  datos_error3 <- bind_cols(list(
    datos_error3_normal,
    datos_error3_texto,
    datos_error3_tiempo,
    datos_error3_tiempo0
  )) %>%
    select(any_of(paste0("error3_", variables)))

  # Combinar
  datos_valprimer <- bind_cols(
    list(
      id_per = datos %>% pull(!!sym(id)),
      datos_error1,
      datos_error2,
      datos_error3
    )
  )

  # Justificar casos
  if (nec_justificar) {
    datos_valprimer <- datos_valprimer %>%
      mutate(across(all_of(justificar), ~ if_else(. == 1, 0, .)))
  }

  return(datos_valprimer)
}
