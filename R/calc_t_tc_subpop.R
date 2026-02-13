#' Calculate caregiving time for a specific subpopulation (v. 1.0.0.)
#'
#' Computes the amount of caregiving time provided by each caregiver
#' to a specific subpopulation of care recipients.
#'
#' The output can be returned either \strong{aggregated} (one row per caregiver,
#' with total caregiving time) or \strong{disaggregated by activity}
#' (one row per caregiver–activity combination).
#'
#' @details
#' This function assumes that the input data follow the structure
#' of Chile's II ENUT survey. Activities are identified by a prefix
#' (e.g., \code{"tc1"}), and each activity contains the following types of columns:
#'
#' - \code{tcX_p_ds}: numeric. Indicates whether the caregiver participated in activity \code{tcX}.
#' - \code{tcX_n[1-4]_ds}: numeric. Line numbers of care recipients (up to four per activity).
#' - \code{tcX_t_reg_ds}: numeric. Indicates whether time was reported individually
#'   (per recipient) or in aggregate (for all recipients). This column is optional;
#'   it can be generated automatically when \code{generar_t_reg = TRUE}.
#' - \code{tcX_t_ds}: numeric. Total caregiving time reported in aggregate.
#' - \code{tcX_t_n[1-4]_ds}: numeric. Caregiving time reported for each individual recipient.
#'
#' When applying this function to data from Chile’s II ENUT or the Rural Women Pilot Survey,
#' transport-related questions should be reprocessed to unify their prefixes.
#'
#' @param data \code{data.frame}
#'   A dataset containing time-use information.
#'   Each row represents an individual. Must include columns \code{interview__key} and \code{id_per}.
#'
#' @param id_edit \code{numeric} or \code{character}
#'   Value used to identify invalid responses in the dataset, which will be replaced with \code{NA}.
#'   Default is \code{-777}.
#'
#' @param cuidadora \code{logical} expression or \code{dplyr::filter()} condition
#'   Filter applied to select specific subsets of caregivers from \code{data}. \code{TRUE} to skip the filter.
#'
#' @param subpop \code{logical} expression or \code{dplyr::filter()} condition
#'   Filter applied to care recipients. Only caregiving time directed to this subpopulation is considered. \code{TRUE} to skip the filter.
#'
#' @param actividades \code{character} vector
#'   Activity prefixes to include in the computation (e.g., \code{"tc1"}, \code{"tc2"}, \code{"tc3"}).
#'
#' @param sufijo \code{character}
#'   Suffix used for time variables (e.g., \code{"_ds"} for weekday data).
#'
#' @param generar_t_reg \code{logical}
#'   If \code{TRUE}, generates \code{tcX_t_reg_ds} columns automatically based on available time-reporting variables.
#'
#' @param var_edad \code{character}
#'   Name of the column containing respondents’ age.
#'
#' @param var_dep_nna \code{character}
#'   Name of the binary column identifying dependent children (PSDF).
#'
#' @param var_dep_adulto \code{character}
#'   Name of the binary column identifying dependent adults (PSDF).
#'
#' @param total \code{logical}
#'   If \code{TRUE}, returns one row per caregiver with total caregiving time.
#'   If \code{FALSE}, returns a dataframe disaggregated by activity.
#'
#' @param name_newcol \code{character}
#'   Name of the output column that will store total caregiving time (only used when \code{total = TRUE}).
#'
#' @return \code{data.frame}
#' A dataframe with the following columns:
#' \describe{
#'   \item{interview__key}{Unique interview identifier.}
#'   \item{id_per}{Identifier of the caregiver.}
#'   \item{actividad}{Activity code (if `total = FALSE`).}
#'   \item{id_cuid}{Identifier of the care recipient.}
#'   \item{tiempo}{Amount of caregiving time (in minutes).}
#' }
#'
#' @examples
#' \dontrun{
#' df_time <- calc_t_tc_subpop(
#'   data = enut_data,
#'   cuidadora = TRUE,
#'   subpop = age < 15,
#'   actividades = c("tc1", "tc2", "tc3"),
#'   sufijo = "_ds",
#'   generar_t_reg = TRUE,
#'   var_edad = "age",
#'   var_dep_nna = "dep_child",
#'   var_dep_adulto = "dep_adult",
#'   total = TRUE,
#'   name_newcol = "total_care_time_subpop"
#' )
#' }
#'
#' @export

calc_t_tc_subpop <- function(data,
                            id_edit = NULL,
                            cuidadora = TRUE,
                            subpop = TRUE,
                            actividades = NULL,
                            sufijo = "_ds",
                            generar_t_reg = TRUE,
                            var_edad = "c3",
                            var_dep_nna = "d1",
                            var_dep_adulto = "d8",
                            total = FALSE,
                            name_newcol = "total_tiempo") {

  # Reemplazar valores editados
  if (!is.null(id_edit)) {

    names <- data %>% select(-{{var_edad}}) %>% names()

    data <- data %>%
      mutate(across(all_of(names), ~ ifelse(.x == id_edit, NA, .x)))
  }

  # Crear df_subpop (luego se usa para filtrar personas cuidadas)
  df_subpop <- data %>%
    mutate(
      marca_subpop = if_else({{ subpop }}, 1, NA_real_)
    ) %>%
    select(interview__key, id_per, marca_subpop) %>%
    rename(id_cuid = id_per) %>%
    bind_rows(
      data %>%
        transmute(
          interview__key,
          id_cuid = paste0(id_per, "-0"),
          marca_subpop = 0
        )
    )

  # Filtrar filas de cuidadoras
  data <- data %>% filter({{ cuidadora }})

  # Seleccionar columnas relevantes
  data <- data %>%
    select(
      interview__key,
      id_per,
      all_of(var_edad),
      all_of(var_dep_nna),
      all_of(var_dep_adulto),
      matches(glue::glue("^tc\\d+_(p|t(_reg)?|n[0-4]|t_n[0-4]){sufijo}$"))
    )

  # Generar columnas _t_ds si no existen
  if (generar_t_reg && !is.null(actividades)) {
    data <- generar_t_reg_cols(data, actividades, sufijo)
  }

  # Generar dataframe de tiempo
  df_total <- tc_df_total(data, actividades, sufijo)

  # Unir con df de marcas subpoblación personas cuidadas
  df_total <- df_total %>%
    left_join(
      df_subpop %>%
        select(interview__key, id_cuid, marca_subpop),
      by = c("interview__key", "id_cuid")
    )

  # Crear auxiliar que indique si se cuidó a al menos una persona de la subpop
  df_subpop_flag <- df_total %>%
    group_by(interview__key, id_per, actividad) %>%
    summarise(hay_subpop = any(marca_subpop == 1, na.rm = TRUE), .groups = "drop")

  # Usar aux del paso anterior para marcar subpop en tiempos agregados
  df_total <- df_total %>%
    left_join(df_subpop_flag, by = c("interview__key", "id_per", "actividad")) %>%
    mutate(
      marca_subpop = if_else(hay_subpop & stringr::str_ends(id_cuid, "-0"), 1, marca_subpop)
    ) %>%
    select(-hay_subpop)

  # Según parámetro total:
  if (total) {
    # Si total: TRUE, se suman todos los tiempos que apliquen a la subpoblación
    df_total <- df_total %>%
      filter(marca_subpop == 1, !is.na(tiempo)) %>%
      group_by(interview__key, id_per) %>%
      summarise(
        !!sym(name_newcol) := sum(tiempo, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Si total: FALSE, solamente se filtra por la subpoblación
    df_total <- df_total %>%
      filter(marca_subpop == 1, !is.na(tiempo))
  }

  return(df_total)
}
