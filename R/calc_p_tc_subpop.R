#' Calculate participation in care activities for a specific subpopulation of cared-for individuals
#'
#' This function identifies whether each caregiver provides care to individuals
#' belonging to a specified subpopulation. It can return either a detailed dataframe
#' (one row per caregiver–activity combination) or an aggregated version indicating
#' overall participation by activity.
#'
#' When applying this function to data from Chile’s II ENUT or the Rural Women Pilot Survey,
#' it assumes that transport-related questions have been reprocessed to unify their prefixes.
#'
#' @param data \code{data.frame}
#'   A dataset containing time-use information.
#'   Each row represents an individual. Must include columns \code{interview__key} and \code{id_per}.
#' @param id_edit \code{numeric} or \code{character}
#'   Value used to identify invalid responses in the dataset, which will be replaced with `NA`.
#'   Default is \code{-777}.
#' @param cuidadora \code{logical} expression or \code{dplyr::filter()} condition
#'   Filter applied to select specific subsets of caregivers from \code{data}. TRUE to skip the filter.
#'
#' @param subpop \code{logical} expression or \code{dplyr::filter()} condition
#'   Filter applied to care recipients. Only caregiving time directed to this subpopulation is considered. TRUE to skip the filter.
#' @param actividades \code{character} vector
#'   Activity prefixes to include in the computation (e.g., \code{"tc1"}, \code{"tc2"}, \code{"tc3"}).
#'
#' @param sufijo \code{character}
#'   Suffix used for time variables (e.g., \code{"_ds"} for weekday data).
#' @param var_edad \code{character}
#'   Name of the column containing respondents’ age.
#'
#' @param var_dep_nna \code{character}
#'   Name of the binary column identifying dependent children (PSDF).
#'
#' @param var_dep_adulto \code{character}
#'   Name of the binary column identifying dependent adults (PSDF).
#' @param total Logical. If TRUE, returns aggregated results.
#' @param name_newcol Character. Name for the output participation variable, if total = TRUE.
#'
#' @return A data frame indicating whether each caregiver provided care
#'   to individuals in the specified subpopulation.
#'
#' @details ...
#'
#' @importFrom dplyr mutate filter select left_join summarise transmute distinct across group_by if_else everything
#' @importFrom purrr map_dfr
#' @importFrom dplyr matches
#' @importFrom rlang sym
#'
#' @export

calc_p_tc_subpop <- function(data,
                              id_edit = NULL,
                              cuidadora = TRUE,
                              subpop = TRUE,
                              actividades = NULL,
                              sufijo = "_ds",
                              var_edad = "c3",
                              var_dep_nna = "d1",
                              var_dep_adulto = "d8",
                              total = FALSE,
                              name_newcol = "participa") {

  # Paso 1: reemplazar valores editados
  if (!is.null(id_edit)) {
    data <- data %>%
      mutate(across(everything(), ~ ifelse(.x == id_edit, NA, .x)))
  }

  # Paso 2: crear df_subpop antes de filtrar cuidadoras
  df_subpop <- data %>%
    mutate(
      id_cuid = as.character(id_per),
      marca_subpop = if_else({{ subpop }}, 1L, NA_integer_)
    ) %>%
    select(id_cuid, marca_subpop)

  # Paso 3: filtrar cuidadoras
  data_cuid <- data %>% filter({{ cuidadora }})

  # Paso 4: seleccionar columnas relevantes
  data_cuid <- data_cuid %>%
    select(
      interview__key,
      id_per,
      all_of(var_edad),
      all_of(var_dep_nna),
      all_of(var_dep_adulto),
      matches(paste0("^tc\\d+_.*", sufijo, "$"))
    )

  # Paso 5: generar dataframe de participación
  df_total <- tc_df_participa(data_cuid, actividades, sufijo)

  # Convertir id_cuid a character explícitamente
  df_total <- df_total %>% mutate(id_cuid = as.character(id_cuid))

  # Paso 6: unir con subpoblación
  df_total <- df_total %>%
    left_join(df_subpop, by = "id_cuid", suffix = c("", "_sub"))

  message("Coincidencias en join: ", sum(!is.na(df_total$marca_subpop)))

  # Paso 7: quedarnos solo con las filas donde la persona cuidada pertenece a la subpoblación
  df_total <- df_total %>%
    filter(participa == 0L | marca_subpop == 1L)

  # Paso 8: total vs detalle
  if (total) {
    df_total <- df_total %>%
      group_by(interview__key, id_per, actividad) %>%
      summarise(!!sym(name_newcol) := as.integer(any(participa == 1L)), .groups = "drop")
  } else {
    df_total <- df_total %>%
      select(interview__key, id_per, actividad, id_cuid, participa, marca_subpop) %>%
      distinct()
  }

  return(df_total)
}
