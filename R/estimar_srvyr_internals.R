#' Format a survey estimates table (totals, means, proportions)
#'
#' Utility to post-process a table of survey estimates produced upstream
#' (e.g., by estimar_srvyr()). It can:
#' (a) convert proportion columns to percentages,
#' (b) round totals/means/proportions with separate precisions,
#' (c) format numbers using Spanish-style decimal/thousand separators, and
#' (d) append a percent sign to proportion columns.
#'
#' The function identifies columns by prefix:
#' columns starting with "total", "mean", and "prop".
#'
#' @param tab A data frame or tibble with columns whose names start with
#'   "total", "mean", and/or "prop".
#' @param round Logical; if TRUE, round numeric estimate and precision columns.
#'   Defaults to TRUE.
#' @param round.decimals.total Integer; number of decimals for columns starting
#'   with "total". Default 0.
#' @param round.decimals.mean Integer; number of decimals for columns starting
#'   with "mean". Default 2.
#' @param round.decimals.prop Integer; number of decimals for columns starting
#'   with "prop". Default 2.
#' @param make.percentage.prop Logical; if TRUE, multiply "prop" columns by
#'   100 to express them as percentages. Default TRUE.
#' @param add.percentage.sym.prop Logical; if TRUE, append "%" to "prop"
#'   columns after any numeric formatting. Default TRUE.
#' @param decimales.esp Logical; if TRUE, format numerics using Spanish decimal
#'   mark (comma). Implemented via scales::number(decimal.mark = ",").
#'   Default TRUE.
#' @param miles.esp Logical; if TRUE, format numerics using Spanish thousands
#'   separator (dot). Implemented via scales::number(big.mark = ".").
#'   Default TRUE.
#'
#' @details
#' The steps are applied in this order:
#' \enumerate{
#'   \item If make.percentage.prop = TRUE, multiply all "prop*" columns by 100.
#'   \item If round = TRUE, round "total*", "mean*", and "prop*" columns
#'         with their respective precisions.
#'   \item If decimales.esp = TRUE, format all numeric columns with comma as
#'         decimal separator.
#'   \item If miles.esp = TRUE, format all numeric columns with dot as
#'         thousands separator.
#'   \item If add.percentage.sym.prop = TRUE, append "%"
#'         to all "prop*" columns.
#' }
#'
#' Note that once number formatting is applied (scales::number()), affected
#' columns become character vectors. Subsequent numeric transforms (e.g., a second
#' where(is.numeric)) will not apply to those columns.
#'
#' @return A tibble/data frame with formatted columns. Depending on the options,
#'   numeric columns may be returned as character vectors due to formatting.
#'
#' @examples
#' # Minimal example
#' df <- tibble::tibble(
#'   variable = "x",
#'   total = 12345.678,
#'   total_se = 12.3,
#'   mean = 3.14159,
#'   mean_se = 0.12,
#'   prop = 0.4567,
#'   prop_se = 0.0123
#' )
#'
#' format_tab(df)
#'
#' # Keep proportions as fractions (0-1) and no percent sign
#' format_tab(
#'   df,
#'   make.percentage.prop = FALSE,
#'   add.percentage.sym.prop = FALSE
#' )
#'
#' # Custom rounding and disable Spanish separators
#' format_tab(
#'   df,
#'   round.decimals.total = 1,
#'   round.decimals.mean = 3,
#'   round.decimals.prop = 1,
#'   decimales.esp = FALSE,
#'   miles.esp = FALSE
#' )
#'
#' @importFrom dplyr mutate across starts_with where
#' @importFrom scales number
#' @export

format_tab <- function(
  tab,
  round = TRUE,
  round.decimals.total = 0,
  round.decimals.mean = 2,
  round.decimals.prop = 2,
  make.percentage.prop = TRUE,
  add.percentage.sym.prop = TRUE,
  decimales.esp = TRUE,
  miles.esp = TRUE
) {
  # Hacer porcentaje las proporciones
  if (make.percentage.prop) {
    tab <- tab %>%
      dplyr::mutate(
        across(starts_with("prop"), ~ (. * 100))
      )
  }

  # Redondear estimaciones e indicadores de calidad
  if (round) {
    tab <- tab %>%
      dplyr::mutate(
        across(starts_with("total"), ~ round(., round.decimals.total)),
        across(starts_with("mean"), ~ round(., round.decimals.mean)),
        across(starts_with("prop"), ~ round(., round.decimals.prop))
      )
  }

  # Formatear decimales a español
  if (decimales.esp) {
    tab <- tab %>%
      dplyr::mutate(across(
        where(is.numeric),
        ~ scales::number(., decimal.mark = ",")
      ))
  }

  # Formatear miles a español
  if (miles.esp) {
    tab <- tab %>%
      dplyr::mutate(across(
        where(is.numeric),
        ~ scales::number(., big.mark = ".")
      ))
  }

  # Agregar simbolo de porcentaje
  if (add.percentage.sym.prop) {
    tab <- tab %>%
      dplyr::mutate(
        across(starts_with("prop"), ~ paste0(., "%"))
      )
  }

  # Return
  return(tab)
}
