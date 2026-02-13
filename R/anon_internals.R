#' Extract key variables from an \code{sdcMicro} object
#'
#' This function retrieves the names of the key variables
#' that were specified when creating an \code{sdcMicro} object.
#'
#' @param sdcobject An object of class \code{sdcMicro}.
#'
#' @return A single character string with the names of the key variables,
#' separated by commas.
anon_extraer_vars_clave <- function(sdcobject) {
  keyvars <- names(sdcobject@origData)[sdcobject@keyVars]
  keyvars_chr <- paste(keyvars, collapse = ", ")
  return(keyvars_chr)
}

#' Calculate percentage of units above an individual risk threshold
#'
#' This function evaluates an \code{sdcMicro} object and computes the percentage
#' of sample units with an individual re-identification risk higher than
#' a specified threshold.
#'
#' @param umbral_riesgo Numeric. Risk threshold to evaluate. For example,
#' 0.01 corresponds to a 1\% threshold.
#' @param sdcobject An object of class \code{sdcMicro}.
#'
#' @return A numeric value indicating the percentage of sample units
#' whose individual risk exceeds the specified threshold.
anon_calc_pct_obs_riesgo_ind <- function(umbral_riesgo, sdcobject) {
  anon_data <- sdcobject@origData

  numerador <- nrow(anon_data[sdcobject@risk$individual[, "risk"] > umbral_riesgo, ])
  denominador <- nrow(anon_data)

  pct_riesgo <- numerador / denominador * 100
  return(pct_riesgo)
}

#' Generate a data frame of individual risk indicators for a given threshold
#'
#' This function calculates the percentage of units exceeding a specified
#' individual risk threshold in an \code{sdcMicro} object and returns a one-row
#' data frame summarizing the results.
#'
#' @param umbral_riesgo Numeric. Risk threshold to evaluate (e.g., 0.01 for 1\%).
#' @param sdcobject An object of class \code{sdcMicro}.
#'
#' @return A data frame with the following column
anon_calc_pct_obs_riesgo_tabla <- function(umbral_riesgo, sdcobject) {
  est_riesgo_pct <- anon_calc_pct_obs_riesgo_ind(
    umbral_riesgo = umbral_riesgo,
    sdcobject = sdcobject
  )

  est_riesgo_chr <- paste0(round(est_riesgo_pct, 2), "\u0025")
  umbral_riesgo_chr <- paste0(100 * umbral_riesgo, "\u0025")

  texto <- paste0(
    "\u0025 de observaciones con riesgo individual >",
    umbral_riesgo_chr,
    " es ",
    est_riesgo_chr
  )

  mydf <- data.frame(
    umbral_riesgo = umbral_riesgo_chr,
    est_riesgo = est_riesgo_chr,
    glosa = texto
  ) %>%
    mutate(
      keyvars = anon_extraer_vars_clave(sdcobject = sdcobject)
    )

  return(mydf)
}
#' Generate a data frame of k-anonymity indicators for a single k-value
#'
#' This function evaluates an \code{sdcMicro} object for a given k-value and returns
#' a data frame summarizing the proportion of observations that violate k-anonymity,
#' both weighted and unweighted.
#'
#' @param sdcobject An object of class \code{sdcMicro}.
#' @param k Numeric. The k-value to evaluate for k-anonymity.
#'
#' @return A data frame with one row per weighted/unweighted assessment and the
#' following columns:
#' \describe{
#'   \item{weighted}{Logical. TRUE if the k-anonymity assessment used weights, FALSE otherwise.}
#'   \item{k}{The k-value used for the evaluation.}
#'   \item{pct_obs}{Percentage of observations violating k-anonymity.}
#'   \item{kanon_int}{Text describing the interpretation of the k-anonymity assessment (\%).}
#'   \item{keyvars}{Names of the key variables in the \code{sdcMicro} object, comma-separated.}
#' }
#'
#' @importFrom sdcMicro kAnon_violations
anon_calc_pct_obs_kanon_tabla <- function(sdcobject, k) {
  ### Numerador (con ponderador)
  numerador_w <- sdcMicro::kAnon_violations(
    object = sdcobject,
    weighted = TRUE,
    k = k
  )

  ### Numerador (sin ponderador)
  numerador_uw <- sdcMicro::kAnon_violations(
    object = sdcobject,
    weighted = FALSE,
    k = k
  )

  ### Denominador
  anon_data <- sdcobject@origData
  denominador <- nrow(anon_data)

  ### Pct. (con ponderador)
  pct_obs_w <- numerador_w / denominador * 100

  ### Pct. (sin ponderador)
  pct_obs_uw <- numerador_uw / denominador * 100

  ### Crear df
  mydf <- data.frame(
    weighted = TRUE,
    k = k,
    pct_obs = pct_obs_w
  ) %>%
    bind_rows(
      .,
      data.frame(
        weighted = FALSE,
        k = k,
        pct_obs = pct_obs_uw
      )
    )

  ### Crear glosa de interpretación
  mydf <- mydf %>%
    mutate(
      kanon_int = paste0(
        "El ",
        round(pct_obs, digits = 2),
        "\u0025 de las observaciones son combinaciones de valores en las variables clave que no tienen k = ",
        k,
        " o m\u00e1s realizaciones (repeticiones) en la base de datos"
      )
    )

  ### Pegar columna de variables clave
  mydf <- mydf %>%
    mutate(
      keyvars = anon_extraer_vars_clave(sdcobject = sdcobject)
    )

  ### Return
  return(mydf)
}
