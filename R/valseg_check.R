#' Check Activated Second-Level Validations
#'
#' Creates a \code{data.frame} with the information required to review
#' cases that triggered a second-level validation in the survey data.
#'
#' @param data \code{data.frame}. Original dataset containing the variables of interest.
#' @param data_val \code{data.frame}. Dataset containing validation variables with
#'        indicators (0/1) for rule activation, typically the output from the \code{valprimer()} function.
#' @param id_reglas \code{character}. Vector with all rule identifiers.
#' @param exp_regla \code{character}. Vector with all logical expressions for the rules.
#' @param vars_regla \code{character}. Vector with variables involved in each rule
#'        (comma-separated).
#' @param desc_regla \code{character}. Vector with rule descriptions.
#' @param var_constant \code{character}. Vector with constant variables to be included
#'   in the review (e.g., sex, age).
#' @param regla_to_check \code{character}. Identifier of the rule to be reviewed.
#' @param id \code{character}. Name of the unique survey identifier.
#' @param level \code{character}. Desired unit of analysis for the review.
#' Can be a \code{person} or a \code{household}.
#' @param id_hog \code{character}. Name of the household identifier.
#'
#' @return A \code{data.frame} containing:
#' \itemize{
#'   \item \code{regla}: Identifier of the reviewed rule.
#'   \item \code{expr}: Logical expression of the rule.
#'   \item \code{desc}: Rule description.
#'   \item Constant variables and variables involved in the rule.
#'   \item The identifiers of the cases that triggered the rule.
#' }
#'
#' @details
#' The function filters only those cases that actually triggered the rule
#' (value 1 in the corresponding variable in \code{data_val}).
#'
#' The function assumes the existence of a metadata source containing
#' information about all validation rules, their logical expressions and the
#' variables used in the expression.
#'
#' @examples
#' library(dplyr)
#'
#' # Example of metadata
#' metadata <- tibble(
#'   id_regla = c("rule1", "rule2"),
#'   exp_regla = c("age < 18 & working == 1", "age < 5 & educ == 3"),
#'   vars_regla = c("age, working", "age, educ"),
#'   desc_regla = c("Minors should not be working", "Infants can't have tertiary education")
#' )
#'
#' # Example data
#' data <- tibble(
#'   id = 1:5,
#'   age = c(6, 20, 17, 22, 2),
#'   working = c(1, 1, 0, 1, 0),
#'   educ = c(1, 3, 2, 3, 3),
#'   sex = c("M", "F", "M", "F", "M")
#' )
#'
#' # Example validation output (from valseg)
#' data_val <- tibble(
#'   id = 1:5,
#'   rule1 = c(1, 0, 0, 0, 0),
#'   rule2 = c(0, 0, 0, 0, 1)
#' )
#'
#' valseg_check(
#'   data = data,
#'   data_val = data_val,
#'   id_reglas = metadata$id_regla,
#'   exp_regla = metadata$exp_regla,
#'   vars_regla = metadata$vars_regla,
#'   desc_regla = metadata$desc_regla,
#'   var_constant = "sex",
#'   regla_to_check = "rule1",
#'   id = "id"
#' )
#'
#' @importFrom dplyr filter pull select all_of mutate everything
#' @importFrom rlang sym
#' @export

valseg_check <- function(
  data,
  data_val,
  id_reglas,
  exp_regla,
  vars_regla,
  desc_regla,
  var_constant,
  regla_to_check,
  id,
  level = "person",
  id_hog
) {
  # Crear vector con las variables involucradas en la regla
  variables_regla <- strsplit(vars_regla, ",\\s*")
  names(variables_regla) <- id_reglas

  # Crear vector con las expresiones de la regla
  expresiones <- exp_regla
  names(expresiones) <- id_reglas

  # Crear vector con las descripciones de la regla
  descripciones <- desc_regla
  names(descripciones) <- id_reglas

  if (level == "person") {
    # Crear subset con las validaciones activadas para la regla
    acts <- data_val %>%
      filter(!!sym(regla_to_check) == 1) %>%
      pull(!!sym(id))

    data_person <- data %>%
      select(
        all_of(id),
        all_of(var_constant),
        all_of(variables_regla[[regla_to_check]])
      ) %>%
      mutate(
        regla = regla_to_check,
        expr = expresiones[[regla_to_check]],
        desc = descripciones[[regla_to_check]]
      ) %>%
      select(regla, expr, desc, everything()) %>%
      filter(!!sym(id) %in% acts)

    return(data_person)
  }

  if (level == "household") {
    # Crear subset con las validaciones activadas para la regla
    acts <- data_val %>%
      filter(!!sym(regla_to_check) == 1) %>%
      left_join(data %>% select(!!sym(id), !!sym(id_hog))) %>% # Traer identificador del hogar
      pull(!!sym(id_hog))

    data_household <- data %>%
      select(
        all_of(id),
        all_of(var_constant),
        all_of(variables_regla[[regla_to_check]])
      ) %>%
      mutate(
        regla = regla_to_check,
        expr = expresiones[[regla_to_check]],
        desc = descripciones[[regla_to_check]],
        act_regla = data_val[[regla_to_check]]
      ) %>%
      select(regla, expr, desc, act_regla, everything()) %>%
      filter(!!sym(id_hog) %in% acts)

    return(data_household)
  }
}
