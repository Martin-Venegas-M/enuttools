#' Create an SDC object from survey data (using sdcMicro)
#'
#' This function takes a survey dataset and converts it into an
#' \code{sdcMicro} object suitable for Statistical Disclosure Control (SDC)
#' analyses. It requires identifiers, weights, and key variables
#' (categorical) to evaluate re-identification risk.
#'
#' @param mydata A data frame containing survey or sample data. Must include
#' unit IDs, weights, and categorical key variables.
#'
#' @param idvars A character vector with the names of the ID variables
#' for the units in the dataset.
#'
#' @param weightvars A character vector with the names of the weight variables
#' corresponding to the units in the dataset.
#'
#' @param keyvars A character vector with the names of the key variables
#' used for risk evaluation (global risk, individual risk, k-anonymity).
#' All key variables must be categorical. Numeric variables are not suitable.
#'
#' @return An object of class \code{sdcMicro}.
#'
#' @importFrom sdcMicro createSdcObj
#'
#' @export
anon_crear_objetosdc <- function(mydata,
                                 idvars,
                                 weightvars,
                                 keyvars) {
  # Subset relevant variables
  dfanon <- mydata %>%
    select(all_of(na.omit(unique(c(
      idvars, weightvars, keyvars
    )))))

  # Convert key variables to factors
  dfanon <- dfanon %>%
    mutate(across(all_of(keyvars), ~ as.factor(as.numeric(.))))

  # Create SDC object
  sdcobject <- sdcMicro::createSdcObj(
    dat = dfanon,
    keyVars = keyvars,
    weightVar = weightvars
  )

  return(sdcobject)
}

#' Assess global disclosure risk from an \code{sdcMicro} object
#'
#' This function calculates global risk indicators from a given
#' \code{sdcMicro} object and returns them in a tidy data frame.
#'
#' @param sdcobject An object of class \code{sdcMicro}.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{riesgo_global}{Unadjusted value of global risk.}
#'   \item{riesgo_global_chr}{Global risk expressed as a percentage
#'   (rounded to two decimals).}
#'   \item{n_esperado_reidentificaciones}{Expected number of individuals
#'   that could be re-identified, rounded up to the next integer.}
#'   \item{riesgo_global_int}{A character string providing an
#'   interpretative summary of the global risk.}
#'   \item{keyvars}{A character string with the names of the key
#'   variables (comma-separated).}
#' }
#'
#' @export
anon_calc_riesgo_global <- function(sdcobject) {
  # Global risk value
  riesgo_global <- sdcobject@risk$global$risk

  # Expected number of re-identifications
  n_esperado_reidentificaciones <- sdcobject@risk$global$risk_ER

  # Build output data frame
  mydf <- data.frame(
    riesgo_global = riesgo_global,
    n_esperado_reidentificaciones = n_esperado_reidentificaciones
  ) %>%
    mutate(
      riesgo_global_chr = paste0(round(riesgo_global * 100, 2), "\u0025"),
      n_esperado_reidentificaciones = ceiling(n_esperado_reidentificaciones),
      riesgo_global_int = paste0(
        "El riesgo global es de ",
        riesgo_global_chr,
        ", con un N esperado de ",
        n_esperado_reidentificaciones,
        " reidentificaciones"
      ),
      keyvars = anon_extraer_vars_clave(sdcobject)
    )

  return(mydf)
}

#' Assess hierarchical disclosure risk from an \code{sdcMicro} object
#'
#' This function calculates hierarchical disclosure risk indicators from a given
#' \code{sdcMicro} object and returns them in a tidy data frame.
#'
#' @param sdcobject An object of class \code{sdcMicro}.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{riesgo_jerarq}{Unadjusted value of hierarchical risk.}
#'   \item{riesgo_jerarq_chr}{Hierarchical risk expressed as a percentage
#'   (rounded to two decimals).}
#'   \item{n_esperado_reidentificaciones}{Expected number of individuals
#'   that could be re-identified, rounded up to the next integer.}
#'   \item{riesgo_jerarq_int}{A character string providing an
#'   interpretative summary of the hierarchical risk.}
#'   \item{keyvars}{A character string with the names of the key
#'   variables (comma-separated).}
#' }
#'
#' @export
anon_calc_riesgo_jerarq <- function(sdcobject) {
  # Hierarchical risk value
  riesgo_jerarq <- sdcobject@risk$global$hier_risk

  # Expected number of re-identifications
  n_esperado_reidentificaciones <- sdcobject@risk$global$hier_risk_ER

  # Build output data frame
  mydf <- data.frame(
    riesgo_jerarq = riesgo_jerarq,
    n_esperado_reidentificaciones = n_esperado_reidentificaciones
  ) %>%
    mutate(
      riesgo_jerarq_chr = paste0(round(riesgo_jerarq * 100, 2), "\u0025"),
      n_esperado_reidentificaciones = ceiling(n_esperado_reidentificaciones),
      riesgo_jerarq_int = paste0(
        "El riesgo jer\u00f3rquico es de ",
        riesgo_jerarq_chr,
        ", con un N esperado de ",
        n_esperado_reidentificaciones,
        " reidentificaciones"
      ),
      keyvars = anon_extraer_vars_clave(sdcobject)
    )

  return(mydf)
}

#' Generate a data frame of individual risk indicators for multiple thresholds
#'
#' This function evaluates an \code{sdcMicro} object across multiple individual risk
#' thresholds and returns a data frame summarizing the results for each threshold.
#'
#' @param sdcobject An object of class \code{sdcMicro}.
#' @param umbral_riesgo Numeric vector. One or more risk thresholds to evaluate.
#'   For example, 0.01 corresponds to 1\%. Default is:
#'   \code{c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999, 0.9999)}.
#'
#' @return A data frame with one row per threshold and the following columns:
#' \describe{
#'   \item{umbral_riesgo}{Threshold value expressed as a percentage (character).}
#'   \item{est_riesgo}{Percentage of units exceeding the threshold (character).}
#'   \item{glosa}{Text describing the interpretation of the individual risk (\%).}
#'   \item{keyvars}{Names of the key variables in the \code{sdcMicro} object, comma-separated.}
#' }
#'
#' @export
anon_calc_pct_obs_riesgo_loop <- function(
  umbral_riesgo = c(
    0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 0.999, 0.9999
  ),
  sdcobject
) {
  tabla_obs_riesgo_ind <- purrr::map(
    .x = umbral_riesgo,
    ~ anon_calc_pct_obs_riesgo_tabla(
      umbral_riesgo = .x,
      sdcobject = sdcobject
    )
  ) %>%
    purrr::list_rbind()

  return(tabla_obs_riesgo_ind)
}

#' Generate a data frame of k-anonymity indicators for multiple k-values
#'
#' This function evaluates an \code{sdcMicro} object for several k-values and returns
#' a combined data frame summarizing the proportion of observations that violate
#' k-anonymity for each k-value, both weighted and unweighted.
#'
#' @param sdcobject An object of class \code{sdcMicro}.
#' @param k Numeric vector. The k-values to evaluate for k-anonymity. Default is \code{c(2, 3, 5)}.
#'
#' @return A data frame with one row per weighted/unweighted assessment for each k-value,
#' containing the following columns:
#' \describe{
#'   \item{weighted}{Logical. TRUE if the k-anonymity assessment used weights, FALSE otherwise.}
#'   \item{k}{The k-value used for the evaluation.}
#'   \item{pct_obs}{Percentage of observations violating k-anonymity.}
#'   \item{kanon_int}{Text describing the interpretation of the k-anonymity assessment (\%).}
#'   \item{keyvars}{Names of the key variables in the \code{sdcMicro} object, comma-separated.}
#' }
#'
#' @importFrom sdcMicro kAnon_violations
#' @export
anon_calc_pct_obs_kanon_loop <- function(sdcobject, k = c(2, 3, 5)) {
  tabla_k_anonimato <- purrr::map(
    .x = k,
    ~ anon_calc_pct_obs_kanon_tabla(
      sdcobject,
      k = .x
    )
  ) %>%
    purrr::list_rbind()

  return(tabla_k_anonimato)
}
