#' Calculate percentage of valid responses (internal)
#'
#' Calculates percentage of valid responses in specified modules.
#' @keywords internal
#' @noRd

calc_pct_preg <- function(df_validacion, mod_vector, id_edit, nombre_columna) {
  
  df_validacion <- df_validacion %>%
    dplyr::mutate(dplyr::across(-interview__key, as.character))
  
  df_mod <- df_validacion %>%
    tidyr::pivot_longer(cols = -interview__key, names_to = "variable", values_to = "valor") %>%
    dplyr::mutate(
      modulo = stringr::str_extract(variable, "^[a-z]{2,3}"),
      valor_numerico = dplyr::case_when(
        is.na(valor)     ~ NA_real_,
        valor == id_edit ~ 0,
        TRUE             ~ 1
      )
    ) %>%
    dplyr::filter(modulo %in% mod_vector)
  
  df_mod_resumen <- df_mod %>%
    dplyr::group_by(interview__key, modulo) %>%
    dplyr::summarise(
      n_habilitadas = sum(!is.na(valor_numerico)),
      n_validas     = sum(valor_numerico, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pct_validas = n_validas / n_habilitadas) %>%
    dplyr::group_by(interview__key) %>%
    dplyr::summarise(!!nombre_columna := sum(n_validas, na.rm = TRUE) / sum(n_habilitadas, na.rm = TRUE),
                     .groups = "drop")
  
  return(df_mod_resumen)
}

#' Evaluate sufficiency 1 (internal)
#'
#' Checks percentage of valid responses in all modules and essential modules.
#' @keywords internal
#' @noRd

eval_pct_valid_cut <- function(df_validacion, mod_suf_cut, mod_esen_cut = NULL,
                               id_edit, prop_suf, prop_esen = NA,
                               detalle_mod_esen = FALSE) {
  
  resultado_pct_valid <- calc_pct_preg(
    df_validacion = df_validacion,
    mod_vector = mod_suf_cut,
    id_edit = id_edit,
    nombre_columna = "pct_valid_cut"
  )
  
  mod_esen_presente <- !is.null(mod_esen_cut) &&
    length(mod_esen_cut) > 0 &&
    !is.na(prop_esen) &&
    any(stringr::str_detect(names(df_validacion), paste0("^(", paste(mod_esen_cut, collapse = "|"), ")")))
  
  if (mod_esen_presente) {
    
    if (detalle_mod_esen) {
      # Evaluación por módulo esencial
      resultados <- lapply(mod_esen_cut, function(mod) {
        df_mod <- calc_pct_preg(
          df_validacion = df_validacion,
          mod_vector = mod,
          id_edit = id_edit,
          nombre_columna = paste0("pct_esen_", mod)
        )
        names(df_mod)[2] <- paste0("pct_esen_", mod)
        return(df_mod)
      })
      
      resultado_pct_esen <- purrr::reduce(resultados, dplyr::left_join, by = "interview__key")
      
      resumen <- resultado_pct_valid %>%
        dplyr::left_join(resultado_pct_esen, by = "interview__key") %>%
        dplyr::mutate(
          insuf_cut_1 = dplyr::if_else(
            pct_valid_cut < prop_suf |
              rowSums(dplyr::across(starts_with("pct_esen_")) < prop_esen, na.rm = TRUE) > 0,
            1, 0
          )
        )
      
    } else {
      # Evaluación agregada
      resultado_pct_esen <- calc_pct_preg(
        df_validacion = df_validacion,
        mod_vector = mod_esen_cut,
        id_edit = id_edit,
        nombre_columna = "pct_esen_cut"
      )
      
      resumen <- resultado_pct_valid %>%
        dplyr::left_join(resultado_pct_esen, by = "interview__key") %>%
        dplyr::mutate(
          insuf_cut_1 = dplyr::if_else(pct_valid_cut < prop_suf | pct_esen_cut < prop_esen, 1, 0)
        )
    }
    
  } else {
    resumen <- resultado_pct_valid %>%
      dplyr::mutate(insuf_cut_1 = dplyr::if_else(pct_valid_cut < prop_suf, 1, 0))
  }
  
  return(resumen)
}

#' Evaluate sufficiency 2 (internal)
#'
#' Checks if the minimum number of activities was answered.
#' @keywords internal
#' @noRd

eval_n_act_cut <- function(df_validacion, umbral_act_cut, id_edit) {
  
  df_largo <- df_validacion %>%
    # Filtrar solo columnas que terminan en "_t_ds"
    dplyr::select(interview__key, dplyr::matches("_t_ds$")) %>%
    tidyr::pivot_longer(cols = -interview__key, names_to = "variable", values_to = "valor") %>%
    dplyr::mutate(
      respondido = !is.na(valor) & valor != id_edit
    ) %>%
    dplyr::group_by(interview__key) %>%
    dplyr::summarise(n_preg_tiempo_respondidas = sum(respondido), .groups = "drop") %>%
    dplyr::mutate(insuf_cut_2 = dplyr::if_else(n_preg_tiempo_respondidas < umbral_act_cut, 1, 0))
  
  return(df_largo)

}

#' Evaluate sufficiency 3 (internal)
#'
#' Checks if total hours fall within the acceptable range.
#' @keywords internal
#' @noRd

eval_t_cut <- function(df_validacion, umbral_min_t, umbral_max_t, id_edit) {
  
  tiempo_total <- df_validacion %>%
    dplyr::select(interview__key, dplyr::matches("_t_ds$"))%>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_horas = sum(dplyr::c_across(-interview__key)[dplyr::c_across(-interview__key) != id_edit], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(interview__key, total_horas) %>%
    dplyr::mutate(
      insuf_min_t = as.integer(total_horas < umbral_min_t),
      insuf_max_t = as.integer(total_horas > umbral_max_t),
      insuf_cut_3 = as.integer(total_horas < umbral_min_t | total_horas > umbral_max_t)
    )
  
  return(tiempo_total)
}
