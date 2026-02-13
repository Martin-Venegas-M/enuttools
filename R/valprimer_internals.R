#' Detect missing values in a variable under a given condition
#'
#' @description
#' Internal helper function that generates a binary indicator when a variable
#' has missing values and a specified condition is met.
#'
#' @param data A data.frame or tibble.
#' @param var A string with the variable name.
#' @param exp A string with the expression that defines the enabling condition
#'            (e.g., "other_var == 1").
#'
#' @return A tibble with one binary column (`error1_<var>`),
#'         equal to 1 if the variable is missing and the condition is met,
#'         and 0 otherwise.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' df <- tibble(x = c(1, 1, 0), y = c(1, NA, NA))
#'
#' # You can uses string or expression inputs
#' error1(df, "y", "x == 1")
#' error1(df, y, x == 1)
#'
#' # Or even a combination...
#' error1(df, "y", x == 1)
#' error1(df, y, "x == 1")
#'
#' # Also works with logical scalars
#' error1(df, "y", "TRUE")
#' error1(df, y, TRUE)
#' error1(df, "y", "FALSE")
#' error1(df, y, FALSE)

error1 <- function(data, var, exp_quo) {
  # Validar!
  exp_values <- .validate_error(exp_quo, data)

  # Evaluar!
  data |>
    dplyr::transmute(
      "error1_{rlang::as_string(var)}" := dplyr::if_else(
        is.na(.data[[rlang::as_string(var)]]) & exp_values,
        1,
        0
      )
    )
}

#' Detect unexpected values in a variable under a given condition
#'
#' @description
#' Internal helper function that generates a binary indicator when a variable
#' has a value, but a specified enabling condition is NOT met.
#'
#' @param data A data.frame or tibble.
#' @param var A string with the variable name.
#' @param exp A string with the expression defining the enabling condition
#'            (the variable should only have a value if this condition is TRUE).
#'
#' @return A tibble with one binary column (`error2_<var>`),
#'         equal to 1 if the variable has a value but the condition is not met,
#'         and 0 otherwise.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' df <- tibble(x = c(1, 0, 0), y = c(1, 1, NA))
#'
#' # You can uses string or expression inputs
#' error2(df, "y", "x == 1")
#' error2(df, y, x == 1)
#'
#' # Or even a combination...
#' error2(df, "y", x == 1)
#' error2(df, y, "x == 1")
#'
#' # Also works with logical scalars
#' error2(df, "y", "TRUE")
#' error2(df, y, TRUE)
#' error2(df, "y", "FALSE")
#' error2(df, y, FALSE)

error2 <- function(data, var, exp) {
  # Capturar inputs
  var <- rlang::ensym(var)
  exp_quo <- rlang::enquo(exp)
  exp_values <- .validate_error(exp_quo, data)

  # Ejecutar
  data |>
    dplyr::transmute(
      "error2_{rlang::as_string(var)}" := dplyr::if_else(
        !(is.na(.data[[rlang::as_string(var)]])) & !exp_values, # Si la variable a evaluar no es NA y la expresión no se cumple, marcar 1
        1,
        0
      )
    )
}

#' Detect values outside allowed range
#'
#' @description
#' Internal helper function that generates a binary indicator when a variable
#' has a value outside a defined range. Supports four types:
#' normal (numeric), texto (string length), tiempo (time in decimal format), time0 (time in decimal format, but 00:00 is a valid value).
#'
#' @param data A data.frame or tibble.
#' @param var A string with the variable name.
#' @param rang For type = "normal" or "texto": allowed values or string length.
#'             Ignored for type = "tiempo".
#' @param type Type of check: "normal" (default), "texto", "tiempo" or "tiempo0".
#'
#' @return A tibble with one binary column (`error3_<var>`),
#'         equal to 1 if the variable value is out of range, 0 otherwise.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' df <- tibble(x = c(1, 10, NA), y = c("a", "abc", "abcd"), z = c(0, 0.5, 24))
#' error3(df, "x", "c(1, 5)")
#' error3(df, "y", "3", type = "texto")
#' error3(df, "z", "t", type = "tiempo")
#' }

error3 <- function(data, var, rang, type = "normal") {
  # Pasar texto a expresión
  rang <- rlang::parse_expr(rang)

  # Evaluar rangos de tipo normal
  if (type == "normal") {
    df <- data |>
      dplyr::transmute(
        "error3_{var}" := dplyr::if_else(
          # Si la variable a evaluar no es NA y esta no está dentro del rango, marcar 1
          !(is.na(.data[[var]])) & !(.data[[var]] %in% !!rang),
          1,
          0
        )
      )
    return(df)
  }

  # Evaluar rangos de tipo texto
  if (type == "texto") {
    df <- data |>
      dplyr::transmute(
        "error3_{var}" := dplyr::if_else(
          # Si la variable a evaluar no es NA y la cantidad de caracteres del valor no es igual o mayor a las establecidas en el rango, marcar 1
          !(is.na(.data[[var]])) & !(nchar(.data[[var]]) >= !!rang),
          1,
          0
        )
      )
    return(df)
  }

  # Evaluar rangos de tipo tiempo
  if (type == "tiempo") {
    df <- data |>
      dplyr::transmute(
        "error3_{var}" := dplyr::if_else(
          !(is.na(.data[[var]])) & # Si la variable a evaluar no es NA y
            !((.data[[var]] >= 1 / 60) & (.data[[var]] <= 23 + 59 / 60)), # (...) el valor no está entre 1 / 60 y 23 + 59 / 60, marcar 1
          1,
          0
        )
      )
    return(df)
  }

  # Evaluar rangos de tipo tiempo0
  if (type == "tiempo0") {
    df <- data |>
      dplyr::transmute(
        "error3_{var}" := dplyr::if_else(
          !(is.na(.data[[var]])) & # Si la variable a evaluar no es NA y
            !((.data[[var]] >= 0) & (.data[[var]] <= 23 + 59 / 60)), # (...) el valor no está entre 0 y 23 + 59 / 60, marcar 1
          1,
          0
        )
      )
    return(df)
  }
}

#' Edit a range string to include the missing value code (-777)
#'
#' Appends the value -777 to the end of a range expression (as a string)
#' representing allowed values (e.g., "c(1,2,3)" becomes "c(1,2,3, -777)").
#' Intended for ranges of type "normal", where allowed values are evaluated
#' with parse_expr() and used with %in%.
#'
#' @param x A string containing the range expression that ends with ')'
#'   (for example, "c(1, 2, 3)" or "c(1:5)").
#'
#' @return A string with ', -777)' inserted before the closing parenthesis.
#'
#' @details This function does not validate the input syntax: it assumes that
#'   x is a valid R expression ending with ')'. It is restricted to "normal"
#'   ranges inside valprimer() when consider_edits = TRUE.
#' @keywords internal
#' @noRd

rng_edit <- function(x) {
  x <- x |> stringr::str_sub(1, stringr::str_length(x) - 1)
  x <- paste0(x, ", -777)")
  return(x)
}

#' Validate and evaluate a logical enabling condition
#'
#' Internal helper that validates an enabling condition (`exp`) and evaluates it
#' in the context of a dataset. The expression must represent a valid logical
#' rule applicable row-wise to `data`.
#'
#' `exp` can be provided as:
#' \itemize{
#'   \item a logical constant (`TRUE` / `FALSE`),
#'   \item a call expression (e.g. `x == 1`),
#'   \item a character string representing a logical expression.
#' }
#'
#' The evaluated expression must return a logical vector of length
#' `nrow(data)` and must not contain `NA` values.
#'
#' @param exp A logical condition, expression or character string representing
#'   a logical expression.
#' @param data A data.frame or tibble used as evaluation environment.
#'
#' @return A logical vector of length `nrow(data)`.
#'
#' @keywords internal
#' @noRd

.validate_error <- function(exp_quo, data) {
  exp_expr <- rlang::get_expr(exp_quo)

  # Transformar strings a expresiones
  if (rlang::is_character(exp_expr)) {
    exp_expr <- rlang::parse_expr(exp_expr)
  }

  # Pre-evaluación de la expresión en data
  exp_values <- rlang::try_fetch(
    rlang::eval_tidy(exp_expr, data = data),
    error = function(e) {
      rlang::abort(
        c(
          "Invalid `exp` input.",
          i = "The expression could not be evaluated on `data`."
        )
      )
    }
  )

  # Validar resultado
  if (!is.logical(exp_values)) {
    rlang::abort(
      c(
        "Invalid `exp` input.",
        "`exp` must evaluate to a logical vector or scalar",
        i = "Numeric expressions like `1`, `1 + 1`, or `c(1, 2)` are not allowed."
      )
    )
  }

  #? ¿IMPLEMENTAR? QUIZÁS CON UN ARGUMENTO
  # if (any(is.na(exp_values))) {
  #   rlang::abort(
  #     c(
  #       "Invalid `exp` input.",
  #       "`exp` must evaluate to a logical vector or scalar without NA."
  #     )
  #   )
  # }

  return(exp_values)
}
