#' Evaluate sufficiency of information in time-use enabling questions
#'
#' @description
#' The function checks, for each row, wether the set of questions required to
#' enable time-use modules contains sufficient data. It then aggregates this
#' information to the household level to assess the sufficiency of the interview.
#'
#' The function should be applied to a "validated dataframe" (the output of the
#' \code{valprimer()} function).
#'
#' @param df_valid A validated data frame with the characteristics described above.
#' @param p_eval Character vector with the variable names to check (without the
#' "error1_" prefix).
#' @param level Character string specifying the level of output:
#'   \code{"person"} returns results at the individual level;
#'   \code{"household"} aggregates results by \code{interview__key}.
#' @param detail \code{FALSE} to return only corresponding insufficiency columns.
#'
#' @return A data frame containing sufficiency indicators at the specified level.
#'
#' @export

suf_eval_ch <- function(df_valid, p_eval, level = c("person", "household"), detail = FALSE) {

  level <- match.arg(level)

  # Si interview__key no viene en la base validada, crearlo a partir de id_per
  if (!"interview__key" %in% names(df_valid)) {
    df_valid <- df_valid %>%
      dplyr::mutate(interview__key = sub("-\\d+$", "", id_per))
  }

  df_eval <- df_valid %>%
    dplyr::select(interview__key, id_per, dplyr::all_of(paste0("error1_", p_eval))) %>%
    dplyr::mutate(insuf_ch_per = dplyr::if_else(dplyr::if_any(-c(interview__key, id_per), ~ . %in% 1), 1, 0))

  if (level == "person") {
    df_eval <- df_eval %>%
    dplyr::group_by(interview__key) %>%
    dplyr::mutate(insuf_ch_hog = dplyr::if_else(any(insuf_ch_per == 1), 1, 0)) %>%
    dplyr::ungroup()

    if (!detail) {
      df_eval <- df_eval %>%
      dplyr::select(interview__key, id_per, insuf_ch_per, insuf_ch_hog)
    }


  } else if (level == "household") {
    df_eval <- df_eval %>%
      dplyr::group_by(interview__key) %>%
      dplyr::summarise(
        insuf_ch_hog = as.integer(any(insuf_ch_per == 1)),
        n_per_insuf = sum(insuf_ch_per),
        .groups = "drop"
      )
  }

  return(df_eval)
}
