#' Evaluate estimation quality
#'
#' This function provides a unified interface to evaluate the quality of different
#' types of survey estimations. Depending on the selected estimation type, the function
#' assesses the reliability of proportions, means, or totals and returns a standardized
#' output with quality indicators.
#'
#' \code{eval_calidad()} is a wrapper around the quality evaluation tools implemented in the
#' \pkg{calidad} package, allowing users to assess estimation quality through a single
#' function call, regardless of the type of estimate.
#'
#' Estimations are classified according to their reliability (e.g. reliable, weakly
#' reliable, non-reliable), based on predefined quality criteria.
#'
#' @param mydata A data frame or tibble containing the survey data.
#' @param type_est A string specifying the type of estimation to be evaluated:
#'        proportions (\code{"prop"}), means (\code{"mean"}) or totals (\code{"total"}).
#'        Default is \code{"prop"}.
#' @param est_var Estimation variable. Its interpretation depends on the selected
#'        estimation type (e.g. binary or categorical for proportions, numeric for
#'        means and totals).
#' @param group_var Grouping variable used to produce disaggregated estimations.
#'        Must be categorical. If no grouping is required, set to \code{"NULL"} (as a string).
#' @param subpop Subpopulation definition used for the estimation, provided as a
#'        character string. If no subpopulation is required, use \code{"TRUE"}.
#' @param qst Questionnaire identifier to which the estimation variable belongs.
#'        Possible values are \code{"ch"} or \code{"cut"}.
#' @param id_estimacion A character string used to label and identify the estimation
#'        in the output. This parameter does not affect the computation itself.
#'
#' @return
#' A tibble containing estimation results and quality indicators, including a
#' classification of reliability.
#'
#' @details
#' This function simplifies the workflow for estimation quality assessment by:
#' \itemize{
#'   \item providing a single entry point for proportions, means, and totals;
#'   \item enforcing a consistent input structure across estimation types;
#'   \item returning a standardized output format for downstream analysis or reporting.
#' }
#'
#' @export

eval_calidad <-
  function(mydata,
           type_est = "prop",
           est_var = "p_tdnr_admnhog",
           group_var = "cui_psdf_o_resp_nna",
           subpop = "TRUE",
           qst = "cut",
           id_estimacion = "prop_cuid_p_tdnr_admnhog") {
    ### 1. Abrir ambiente de función ###########################################

    ### 2. Argumento condicional según tipo de estimación requerida ############
    ###### Proporciones ########################################################
    if (type_est %in% "prop") {
      evaluacion <-
        eval_calidad_prop_completar_niveles(
          mydata = mydata,
          est_var = est_var,
          group_var = group_var,
          subpop = subpop,
          qst = qst,
          id_estimacion = id_estimacion
        )

      return(evaluacion)
    }

    ###### Medias ##############################################################
    if (type_est %in% "mean") {
      evaluacion <-
        eval_calidad_mean(
          mydata = mydata,
          est_var = est_var,
          group_var = group_var,
          subpop = subpop,
          qst = qst,
          id_estimacion = id_estimacion
        )

      return(evaluacion)
    }

    ###### Totales #############################################################
    if (type_est %in% "total") {
      evaluacion <-
        eval_calidad_total(
          mydata = mydata,
          est_var = est_var,
          group_var = group_var,
          subpop = subpop,
          qst = qst,
          id_estimacion = id_estimacion
        )

      return(evaluacion)
    }

    ###### Ninguna de las estimaciones anteriores ##############################
    if (!type_est %in% c("prop", "mean", "total")) {
      cat(
        paste0(
          "\n",
          "#######################################",
          "Escoge entre 'prop', 'mean' y 'total'",
          "#######################################",
          "\n"
        )
      )
    }
  }
