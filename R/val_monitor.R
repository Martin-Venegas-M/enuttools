#' Count the number of triggered validations
#'
#' This function returns a list of three data frames in long format. Each
#' data frame reports the number of triggered validations at different levels
#' of analysis: by variable, by module, and by questionnaire.
#'
#' A triggered validation can be understood in two ways:
#' \itemize{
#'   \item For first-level validations, it refers to the number of rows with errors (error1, error2, or error3). For details, see the documentation of the \code{valprimer()} function.
#'   \item For second-level validations, it refers to the number of rows that violate a validation rule.
#' }
#'
#' The function assumes two inputs:
#' \itemize{
#'   \item A metadata table that indicates the variables in the questionnaire, the modules they belong to, and the corresponding questionnaire.
#'   \item A metadata table that defines the validation rules in the questionnaire, the type of validation (e.g., warning or inconsistency), and the corresponding questionnaire.
#' }
#'
#' @param data A wide-format data frame where each column represents a validation,
#' either first-level (variable x type of error) or second-level (validation rule).
#' This is the output of the \code{valprimer()} function.
#'
#' @param vars A character vector with the names of the validated variables.
#' @param mod A character vector indicating the module to which each variable belongs.
#' @param qst_valprimer A character vector indicating the questionnaire to which
#' each variable belongs (for first-level validations).
#' @param id_regla A character vector with the unique identifiers of the validation rules.
#' @param tipo_val A character vector indicating the type of validation rule.
#' Possible values: \code{"adv"} (warning) or \code{"inc"} (inconsistency).
#' @param qst_valseg A character vector indicating the questionnaire to which each
#' validation rule belongs (for second-level validations).
#'
#' @return A list with three data frames showing the number of triggered validations
#' at different levels of analysis: variable, module, and questionnaire.
#'
#' @importFrom dplyr mutate bind_rows left_join select arrange group_by reframe ungroup
#' @importFrom tibble as_tibble
#' @importFrom data.table as.data.table melt ':='
#' @importFrom magrittr %>%
#' @export

val_monitor <- function(
  data,
  vars,
  mod,
  qst_valprimer,
  id_regla,
  tipo_val,
  qst_valseg
) {
  # Crear insumo valprimer
  df_valprimer <- data.frame(
    var = c(rep(vars, 3)),
    var_qst = c(rep(qst_valprimer, 3)),
    var_mod = c(rep(mod, 3)),
    val_error = c(
      rep("error1", length(vars)),
      rep("error2", length(vars)),
      rep("error3", length(vars))
    )
  ) %>%
    mutate(var_val = paste0(val_error, "_", var))

  # Crear insumo valseg
  df_valseg <- data.frame(
    var = id_regla,
    var_qst = qst_valseg,
    var_mod = NA,
    val_error = qst_valseg,
    var_val = id_regla
  )

  # Crear insumo general
  df_val <- bind_rows(df_valprimer, df_valseg)

  # Main vector
  vars_vals <- df_val$var_val

  # Pasar a data.table
  vars_present <- intersect(vars_vals, names(data))
  vars_missing <- setdiff(vars_vals, names(data))

  # Advertir si faltan variables
  if (length(vars_missing) > 0) {
    warning(
      "Following variables are not present in the dataset: ",
      paste(vars_missing, collapse = ", ")
    )
  }
  dt <- as.data.table(data)[, .SD, .SDcols = vars_present]
  dt[, id := .I]

  dt_long <- melt(
    dt,
    id.vars = "id",
    variable.name = "var_val",
    value.name = "value"
  ) # Equivalente a pivot_longer()
  dt_long[is.na(value), value := 0] # Recodificar NA's
  df_sum <- dt_long[, .(value = sum(value, na.rm = TRUE)), by = var_val] # Sumar

  # Volver a dplyr - Conteo de validaciones activadas a nivel de variable
  vals_sum_vars <- df_sum %>%
    as_tibble() %>%
    left_join(x = ., y = df_val, by = c("var_val")) %>%
    select(var_val, val_error, var_qst, var_mod, var, value) %>%
    arrange(factor(var_val, levels = vars_vals)) %>%
    select(-var_val)

  # Conteo de validaciones activadas a nivel de modulo
  vals_sum_mod <- vals_sum_vars %>%
    group_by(val_error, var_qst, var_mod) %>%
    reframe(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      val_error = factor(val_error, levels = unique(df_val$val_error)),
      var_mod = factor(var_mod, levels = unique(df_val$var_mod))
    ) %>%
    arrange(val_error, var_mod)

  # Conteo de validaciones activadas a nivel de cuestionario
  vals_sum_qst <- vals_sum_mod %>%
    group_by(val_error, var_qst) %>%
    reframe(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      var_qst = factor(var_qst, levels = unique(df_val$var_qst))
    ) %>%
    arrange(val_error, var_qst)

  vals_list <- list(
    vals_sum_vars,
    vals_sum_mod,
    vals_sum_qst
  )

  print(vals_list[[1]])
  print(vals_list[[2]])
  print(vals_list[[3]])

  return(vals_list)
}
