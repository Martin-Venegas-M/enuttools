#' Evaluate sufficiency of information in time use datasets
#'
#' This function creates a data frame with binary columns indicating whether the
#' surveyed unit meets certain criteria for sufficient time use information.
#'
#' The criteria are as follows:
#' \itemize{
#'   \item \strong{Sufficiency 1}: the time use modules for the surveyed unit meet arequired percentage of valid information.
#'   \item \strong{Sufficiency 2}: the time use modules for the surveyed unit contain valid time data for a minimum number of activities.
#'   \item \strong{Sufficiency 3}: the total number of hours reported by the surveyed unit falls within a predefined acceptable range.
#' }
#'
#' Each insufficiency criterion has an associated binary column, and optionally,
#' complementary columns can be included for more detailed review.
#' \itemize{
#'   \item \code{insuf_cut_1}: indicates that the percentage of valid responses in the time use modules is greater than the defined threshold. If "essential" modules are specified, their valid response percentage will also be considered for sufficiency.
#'   \item \code{insuf_cut_2}: indicates that the interview does not meet the minimum number of activities with valid time use data.
#'   \item \code{insuf_cut_3}: indicates that the total number of hours reported in the interview is lower or higher, respectively, than the established minimum and maximum limits.
#' }
#'
#' The function assumes that the data is organized by different time use modules,
#' each providing the prefix to their respective questions. For example, if there
#' is an unpaid work time use module identified by \code{"td"}, variable names within that
#' module should start with the prefix \code{"td"} (for example, \code{td_1}).
#'
#' @param df Data frame containing the data to be evaluated. Rows should correspond
#' to individuals, columns to questions, and an \code{interview__key} column should be
#' present to identify each unit.
#' @param eval_suf_1 Logical. \code{TRUE} to run Sufficiency 1 evaluation.
#' @param eval_suf_2 Logical. \code{TRUE} to run Sufficiency 2 evaluation.
#' @param eval_suf_3 Logical. \code{TRUE} to run Sufficiency 3 evaluation.
#' @param mod_suf_cut Character vector identifying the modules that contain time
#' use questions. If the dataset is not split by modules, simply provide the prefix
#' of the time use questions.
#' @param mod_esen_cut Optional. For Sufficiency 1 evaluation. In addition to evaluating
#' all time use modules, a subset of modules can be defined as "essential", with their
#' own completeness threshold for sufficiency.
#' @param prop_suf Numeric. Proportion of valid responses required for the time use
#' modules to be considered sufficient.
#' @param prop_esen Numeric. Proportion of valid responses required for modules
#' identified as "essential".
#' @param detalle_mod_esen (pending)
#' @param id_edit Value used to identify responses considered invalid in the dataset.
#' Default is -777.
#' @param umbral_act_cut Integer. Minimum number of time use activities required
#' for sufficiency.
#' @param umbral_min_t Numeric. Lower limit of the acceptable range for total hours.
#' @param umbral_max_t Numeric. Upper limit of the acceptable range for total hours.
#' @param detail Logical. \code{FALSE} to return only the three insufficiency columns.
#'
#' @return A \code{data.frame} containing binary variables indicating insufficient information
#' in time use modules, based on different criteria.
#' @export

suf_eval_cut <- function(
    df,
    eval_suf_1 = TRUE,
    eval_suf_2 = TRUE,
    eval_suf_3 = TRUE,
    mod_suf_cut,
    mod_esen_cut = NULL,
    id_edit = -777,
    prop_suf = 0.5,
    prop_esen = NULL,
    detalle_mod_esen = TRUE,
    umbral_act_cut = 4,
    umbral_min_t = 10,
    umbral_max_t = 48,
    detail = FALSE
)  {
  
  df_suf <- df %>% dplyr::select(interview__key, id_per)
  
  if (eval_suf_1 || eval_suf_2 || eval_suf_3) {
    
    regex_mod <- paste0("^(", paste(mod_suf_cut, collapse = "|"), ")")
    
    vars_mod <- names(df)[stringr::str_detect(names(df), regex_mod)]
    
    df_eval <- df %>%
      dplyr::filter(informante_kish == 1) %>%
      dplyr::select(interview__key, all_of(vars_mod))
  }
  
  
  if (eval_suf_1) {
    
    df_suf_1 <- eval_pct_valid_cut(df_eval, mod_suf_cut, mod_esen_cut, id_edit, prop_suf, prop_esen, detalle_mod_esen)
    
    df_suf <- df_suf %>%
      dplyr::left_join(df_suf_1, by = "interview__key")
  }
  
  if (eval_suf_2 || eval_suf_3) {
    
    df_eval_t <- df_eval %>%
      dplyr::select(interview__key, dplyr::where(~ is.numeric(.x) || is.character(.x))) %>%
      dplyr::select(
        interview__key,
        dplyr::matches("^[a-z]{2,3}[0-9]+_t(_n[1-4])?_ds$")
      )
  }
  
  if (eval_suf_2) {
    
    df_suf_2 <- eval_n_act_cut(df_eval_t, umbral_act_cut, id_edit)
    
    df_suf <- df_suf %>%
      dplyr::left_join(df_suf_2, by = "interview__key")
  }
  
  
  if (eval_suf_3) {
    
    df_suf_3 <- eval_t_cut(df_eval_t, umbral_min_t, umbral_max_t, id_edit)
    
    df_suf <- df_suf %>%
      dplyr::left_join(df_suf_3, by = "interview__key")
  }
  
  if (!detail) {
    df_suf <- df_suf %>%
      select(id_per, dplyr::matches("^insuf_cut"))
  }
  
  if (!eval_suf_1 && !eval_suf_2 && !eval_suf_3) {
    
    stop("Select at least one type of sufficiency evaluation to apply to the CUT.")
  }
  
  return(df_suf)
}

