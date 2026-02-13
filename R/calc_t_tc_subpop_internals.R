#' Generate participation dataframe for care activities
#'
#' This internal helper function constructs a long-format dataframe identifying
#' caregiver–cared-for pairs for each activity, based on ENUT-style activity prefixes.
#'
#' @param data Data frame containing caregiver data and activity-related columns.
#' @param actividades Character vector of activity prefixes (e.g., "tc1", "tc2").
#'   If NULL, prefixes are inferred automatically from column names.
#' @param sufijo Character. Suffix used to identify variable types (default: "_ds").
#'
#' @return A long-format data frame with the following variables:
#' \describe{
#'   \item{interview__key}{Interview identifier.}
#'   \item{id_per}{Caregiver’s ID.}
#'   \item{actividad}{Activity prefix.}
#'   \item{id_cuid}{Unique ID for each cared-for person, combining interview and line number.}
#'   \item{participa}{Integer flag (1L) indicating participation.}
#' }
#'
#' @importFrom purrr keep reduce pmap_lgl
#' @importFrom stringr str_detect
#'
#' @keywords internal
#' @noRd

generar_t_reg_cols <- function(data, actividades, sufijo = "_ds") {

  acts_validas <- actividades %>%
    keep(~ any(str_c(.x, c("_t", "_t_n1"), sufijo) %in% names(data)))

  if (length(acts_validas) == 0) {
    message("error: no hay actividades con columnas base.")
    return(data)
  }

  data <- reduce(acts_validas, function(df, act) {
    col_t     <- paste0(act, "_t", sufijo)
    col_t_reg <- paste0(act, "_t_reg", sufijo)
    col_t_n   <- names(df)[str_detect(names(df), paste0("^", act, "_t_n[1-4]", sufijo, "$"))]

    # si la columna ya existe, saltar
    if (col_t_reg %in% names(df)) {
      return(df)
    }

    # vector lógico: hay tiempo individual por fila
    tiene_tiempo_ind <- if (length(col_t_n) > 0) {
      df %>%
        select(all_of(col_t_n)) %>%
        pmap_lgl(~ any(!is.na(c(...))))
    } else {
      rep(FALSE, nrow(df))
    }

    df %>%
      mutate(
        !!sym(col_t_reg) := case_when(
          (col_t %in% names(df)) & !is.na(!!sym(col_t)) ~ 2,
          tiene_tiempo_ind ~ 1,
          TRUE ~ NA_real_
        )
      )
  }, .init = data)

  return(data)
}

#' #' Build a long-format dataframe of caregiving time by activity and recipient
#'
#' Converts activity variables from wide to long format, producing
#' one row per caregiver–recipient–activity combination.
#'
#' @param data A dataframe containing time-use variables.
#' @param actividades A character vector of activity prefixes.
#' @param sufijo A character string specifying the suffix for time variables (default is "_ds").
#'
#' @return A dataframe with the columns:
#' \describe{
#'   \item{interview__key}{Unique interview identifier.}
#'   \item{id_per}{Caregiver identifier.}
#'   \item{actividad}{Activity code.}
#'   \item{id_cuid}{Care recipient identifier.}
#'   \item{tiempo}{Reported caregiving time.}
#' }
#' @keywords internal
#'
#'
#'
#'

tc_df_total <- function(data, actividades, sufijo = "_ds") {
  purrr::map_dfr(actividades, function(act) {

    col_t  <- paste0(act, "_t", sufijo)
    col_n0 <- paste0(act, "_n0", sufijo)
    col_t0 <- paste0(act, "_t_n0", sufijo)

    # si no existe el tiempo agregado, saltar
    if (!(col_t %in% names(data))) {
      return(tibble(
        interview__key = character(),
        id_per         = character(),
        actividad      = character(),
        id_cuid        = character(),
        tiempo         = double()
      ))
    }

    # crear columnas auxiliares "n0" y renombrar tiempo agregado
    data_aux <- data %>%
      mutate(!!sym(col_n0) := 0) %>%
      rename(!!sym(col_t0) := !!sym(col_t))

    filas_total <- purrr::map(0:4, function(i) {
      col_n <- paste0(act, "_n",   i, sufijo)
      col_t <- paste0(act, "_t_n", i, sufijo)

      # si no existe la columna de nombres de persona, saltar
      if (!(col_n %in% names(data_aux))) {
        return(NULL)
      }

      # si no existe columna de tiempo, crearla con NA
      if (!(col_t %in% names(data_aux))) {
        data_aux <- data_aux %>% mutate(!!sym(col_t) := NA_real_)
      }

      data_aux %>%
        transmute(
          interview__key,
          id_per,
          actividad = act,
          persona_linea = .data[[col_n]],
          tiempo = .data[[col_t]]
        ) %>%
        filter(!is.na(persona_linea)) %>%
        mutate(id_cuid = paste0(interview__key, "-", persona_linea)) %>%
        select(interview__key, id_per, actividad, id_cuid, tiempo)
    })

    filas_total <- purrr::compact(filas_total)

    if (length(filas_total) == 0) {
      tibble(
        interview__key = character(),
        id_per         = character(),
        actividad      = character(),
        id_cuid        = character(),
        tiempo         = double()
      )
    } else {
      dplyr::bind_rows(filas_total) %>% dplyr::distinct()
    }
  })
}
