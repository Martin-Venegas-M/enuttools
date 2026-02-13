#' Check Activated First-Level Validation
#'
#' Creates a \code{data.frame} with the information required to review
#' cases that triggered a first-level validation in the survey data.
#'
#' @param data \code{data.frame}. Original dataset containing the variables of interest.
#' @param data_val \code{data.frame}. Dataset containing validation variables with
#'        indicators (0/1), typically the output from the \code{valprimer()} function.
#' @param vars \code{character}. Vector with all survey variables.
#' @param exps \code{character}. Vector with all enabling conditions of the survey in R format.
#' @param vars_exp \code{character}. Vector with the variables required for
#'   each enabling condition (comma-separated).
#' @param ranges \code{character}. Vector with the valid ranges of all survey variables.
#' @param error \code{character}. A string specifying the type of error to review.
#'   Can be \code{"error1"}, \code{"error2"}, or \code{"error3"}.
#' @param var_constant \code{character}. Vector with constant variables to be included
#'   in the review (e.g., sex, age).
#' @param var_to_check \code{character}. Name of the variable to review.
#' @param id \code{character}. Name if the unique survey identifier.
#'
#' @return A \code{data.frame} with the following elements:
#' \itemize{
#'   \item \code{var}: Variable being checked.
#'   \item \code{expr}: Enabling condition for the variable.
#'   \item \code{range}: Valid range of the variable.
#'   \item Characterization variables, the variables used in the condition,
#'         and the variable itself.
#'   \item The survey identifier for cases that triggered the validation.
#' }
#'
#' @details
#' Only cases that activated the selected validation (value equal to 1
#' in \code{data_val}) are included in the output.
#'
#' The function assumes the existence of a metadata source containing
#' information about all variables, enabling conditions, variables used in
#' the conditions, and survey ranges.
#'
#' @examples
#' library(dplyr)
#'
#' # Example of metadata
#' metadata <- tibble(
#'   vars = c("age", "working", "sex"),
#'   exps = c(TRUE, "age >= 18", "TRUE"), # all participants in the survey must respond age and sex
#'   vars_exp = c(NA, "age", NA),
#'   ranges = c("c(0:110)", "c(0:1)", "c(1:2)")
#' )
#'
#' # Example survey data
#' data <- tibble(
#'   id = 1:5,
#'   age = c(18, 46, 12, 22, 16),
#'   working = c(NA, 1, 0, 1, 0),
#'   sex = c("M", "F", "M", "F", "M")
#' )
#'
#' # Example validation output (from valprimer)
#' data_val <- tibble(
#'   id = 1:5,
#'   error1_age = c(0, 0, 0, 0, 0),
#'   error1_working = c(1, 0, 0, 0, 0), # Example of an activation
#'   error1_sex = c(0, 0, 0, 0, 0),
#'   error2_age = c(0, 0, 0, 0, 0),
#'   error2_working = c(0, 0, 0, 0, 0),
#'   error2_sex = c(0, 0, 0, 0, 0),
#'   error3_age = c(0, 0, 0, 0, 0),
#'   error3_working = c(0, 0, 0, 0, 0),
#'   error3_sex = c(0, 0, 0, 0, 0)
#' )
#'
#' # Run check for error1 on variable 'working'
#' valprimer_check(
#'   data = data,
#'   data_val = data_val,
#'   vars = metadata$vars,
#'   exps = metadata$exps,
#'   vars_exp = metadata$vars_exp,
#'   ranges = metadata$ranges,
#'   error = "error1",
#'   var_constant = "sex",
#'   var_to_check = "working",
#'   id = "id"
#' )
#'
#' @importFrom dplyr filter pull select all_of mutate everything
#' @importFrom rlang sym
#' @importFrom purrr map
#' @export

valprimer_check <- function(
  data,
  data_val,
  vars,
  exps,
  vars_exp,
  ranges,
  error,
  var_constant,
  var_to_check,
  id
) {
  # Crear vector con las variables de las condiciones habilitantes de R
  variables_condicion <- strsplit(vars_exp, ",\\s*")
  names(variables_condicion) <- vars
  variables_condicion <- map(
    variables_condicion,
    ~ ifelse(is.na(.x), "dummy", .x)
  ) #! PARCHE: Evitar error por NA en select

  # Crear vector con las condiciones de validación
  expresiones <- exps
  names(expresiones) <- vars

  # Crear vector con los rangos
  rangos <- ranges
  names(rangos) <- vars

  # Crear subset con las validaciones activadas para el error y la var_to_check solicitadas en el argumento
  acts <- data_val %>%
    filter(!!sym(paste0(error, "_", var_to_check)) == 1) %>%
    pull(!!sym(id))

  # Mostrar tabla de activaciones según lo requerido
  data %>%
    mutate(dummy = NA) %>% #! PARCHE: Evitar error por NA en select
    select(
      all_of(id),
      all_of(var_constant),
      all_of(variables_condicion[[var_to_check]]),
      all_of(var_to_check)
    ) %>%
    mutate(
      var = var_to_check,
      expr = expresiones[[var_to_check]],
      range = rangos[[var_to_check]]
    ) %>%
    select(var, expr, range, everything()) %>%
    filter(!!sym(id) %in% acts)
}
