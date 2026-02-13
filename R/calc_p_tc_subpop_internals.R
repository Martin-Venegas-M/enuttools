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
#' @importFrom stats na.omit
#' @importFrom stringr str_extract
#'
#' @keywords internal
#' @noRd

tc_df_participa <- function(data, actividades, sufijo = "_ds") {
  if (is.null(actividades)) {
    actividades <- names(data) %>%
      str_extract("^tc\\d+") %>%
      na.omit() %>%
      unique() %>%
      as.list()
  }

  purrr::map_dfr(actividades, function(act) {
    cols_exist <- paste0(act, "_n", 1:4, sufijo)
    cols_exist <- cols_exist[cols_exist %in% names(data)]

    col_p <- paste0(act, "_p", sufijo)
    tiene_p <- col_p %in% names(data)


    if (length(cols_exist) == 0 && !tiene_p) return(NULL)

    # --- Caso 1: hay participación

    filas_part <- purrr::map_dfr(cols_exist, function(col_n) {
      col_vec <- data[[col_n]]

      if (!is.null(attr(col_vec, "labels"))) {
        attr(col_vec, "labels") <- NULL
      }
      col_vec <- as.character(col_vec)

      data %>%
        mutate(persona_linea = col_vec) %>%
        transmute(
          interview__key,
          id_per,
          actividad = act,
          persona_linea = trimws(persona_linea),
          id_cuid = paste0(interview__key, "-", persona_linea),
          participa = 1L
        ) %>%
        filter(!is.na(persona_linea), persona_linea != "")
    })

    # --- Caso 2: Declara que n o participa

    if (tiene_p) {
      filas_no <- data %>%
        filter(.data[[col_p]] == 2) %>%
        transmute(
          interview__key,
          id_per,
          actividad = act,
          persona_linea = NA_character_,
          id_cuid = NA_character_,
          participa = 0L
        )
    } else {
      filas_no <- NULL
    }

    bind_rows(filas_part, filas_no) %>% distinct()
  })
}
