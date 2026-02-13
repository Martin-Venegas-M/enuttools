# -------------------------------------------------------------------------
# Internal helper functions for `proc_transporte()`
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd

crear_p_transp <- function(data, preg_t, preg_p_raiz, preg_transp, cod_no_p = 10L, sufijo = "_ds") {
  preg_p     <- stringr::str_replace(preg_t, "^(.*)_t_(.*)$", "\\1_p_\\2")
  preg_t_reg <- stringr::str_replace(preg_t, paste0(sufijo, "$"), paste0("_reg", sufijo))

  data %>%
    dplyr::mutate(
      !!rlang::sym(preg_p) := dplyr::case_when(
        .data[[preg_p_raiz]] == 1 & !is.na(.data[[preg_transp]]) & .data[[preg_transp]] != cod_no_p ~ 1L,
        .data[[preg_p_raiz]] == 2 | (.data[[preg_p_raiz]] == 1 &
                                       (is.na(.data[[preg_transp]]) | .data[[preg_transp]] == cod_no_p)) ~ 2L,
        TRUE ~ NA_integer_
      ),
      !!rlang::sym(preg_t_reg) := dplyr::if_else(
        !is.na(.data[[preg_t]]) & .data[[preg_t]] >= 0, 2L, NA_integer_
      )
    )
}

#' @keywords internal
#' @noRd

crear_p_transp_batch <- function(data, specs, cod_no_p = 10L, sufijo = "_ds") {
  purrr::reduce(
    .x = specs,
    .init = data,
    .f = function(df, s) {
      crear_p_transp(
        data        = df,
        preg_t      = s$preg_t,
        preg_p_raiz = s$preg_p_raiz,
        preg_transp = s$preg_transp,
        cod_no_p    = cod_no_p,
        sufijo      = sufijo
      )
    }
  )
}

#' @keywords internal
#' @noRd

rename_t_per <- function(data, prefijo_n, preg_t, sufijo = "_ds") {
  prefijo_nuevo <- stringr::str_replace(preg_t, paste0("_t", sufijo, "$"), "")

  data %>%
    dplyr::rename_with(
      ~ stringr::str_replace(.x, paste0("^", prefijo_n), prefijo_nuevo),
      dplyr::matches(paste0("^", prefijo_n, "_n[1-4]", sufijo, "$"))
    )
}

#' @keywords internal
#' @noRd

rename_t_per_batch <- function(data, specs, sufijo = "_ds") {
  purrr::reduce(
    specs,
    .init = data,
    .f = function(df, s) {
      rename_t_per(
        data       = df,
        prefijo_n  = s$prefijo_n,
        preg_t     = s$preg_t,
        sufijo     = sufijo
      )
    }
  )
}
