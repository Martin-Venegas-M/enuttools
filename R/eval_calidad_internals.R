#' Evaluate reliability for totals estimation for all values of a given categorical variable
#'
#' The function creates a data frame where an estimation variable (for totals estimation), a
#' grouping variable and a subpopulation variable are specified, along with columns
#' necessary for complex survey analysis (strata, primary sampling units, and weights).
#' The function iterates the function \code{\link[enuttools]{eval_calidad_total_nivel}} for
#' all values of the categorical estimation variable, thus assessing separately an
#' estimation for each category (in terms of reliability). In terms of their reliability,
#' estimations can be classified as reliable, weakly reliable and non-reliable.
#'
#' @description
#' This function streamlines the process of evaluating estimation reliability for all
#' values of a categorical variable.
#'
#' @param mydata A data frame or tibble containing the survey data.
#' @param est_var Estimation variable. Must be categorical in nature, and in terms of R
#'        it must consist in a labelled numeric vector. Default value is "nivel_educ".
#' @param group_var Grouping variable. Must be categorical in nature. If no grouping
#'        variable is required for the estimation assessment, group_var must be specified
#'        to "NULL" (a string). Default value is "NULL" (a string).
#' @param subpop Subpopulation variable and values for the subpopulation variable, in
#'        string format. If no subpopulation variable is required for the estimation
#'        assessment, subpop must be specified to "TRUE" (a string). Default value is "cui_psdf_o_resp_nna == 1".
#' @param qst Variable denoting the questionnaire to which the estimation variable
#'        belongs. Only possible values are "ch" or "cut". Default value is "ch".
#' @param id_estimacion A given string which allows identification of a specific
#'        estimation across different settings. \code{id_estimacion} has no bearing
#'        on the process of generating estimations and is merely intended to function
#'        as a name. Default value is "tot_nivel_educ_cuid".
#'
#' @details
#' The function operates in the following way:
#' \itemize{
#'   \item The function generates a data frame for all values or categories of the
#'         estimation variable;
#'   \item Next, the function creates a numeric ID for each value in the data frame;
#'   \item Next, the function iterates the function \code{\link[enuttools]{eval_calidad_total_nivel}}
#'         for each value ID and binds the resulting list into a tibble df containing
#'         reliability information for all values of the categorical variable.
#' }
#'
#' @return
#' A data frame containing estimation reliability information.
#'
#' @seealso \code{\link[enuttools]{eval_calidad_total_nivel}}.
#'
#' @importFrom purrr map2
#' @importFrom sjlabelled get_values
#' 
#' @keywords internal
#' @noRd

eval_calidad_total <-
  function(mydata,
           est_var = "nivel_educ",
           group_var = "NULL",
           subpop = "cui_psdf_o_resp_nna == 1",
           qst = "ch",
           id_estimacion = "tot_nivel_educ_cuid") {
    ### 1. Abrir ambiente de función ###########################################

    ### 2. Crear data frame con categorías de la variable ######################
    catsdf <-
      data.frame(
        categorias = sjlabelled::get_values(mydata[[est_var]])
      ) %>%
      mutate(est_var = est_var) %>%
      relocate(est_var, .before = 1)

    ### 3. Crear data frame con estimaciones y evaluaciones ####################
    evaldf <-
      purrr::map2(
        .x = catsdf$est_var,
        .y = catsdf$categorias,
        .f = ~ eval_calidad_total_nivel(
          mydata = mydata,
          est_var = .x,
          value = .y,
          group_var = group_var,
          subpop = subpop,
          qst = qst,
          id_estimacion = id_estimacion
        )
      ) %>%
      purrr::list_rbind()

    ### 4. Señalar objeto a devolver ###########################################
    return(evaldf)

    ### 5. Cerrar ambiente de función ##########################################
  }

#' Evaluate reliability for totals estimation for a given variable and variable level
#'
#' The function creates a data frame where an estimation variable (for totals estimation), a
#' grouping variable and a subpopulation variable are specified, along with columns
#' necessary for complex survey analysis (strata, primary sampling units, and weights).
#' A given value or category for the estimation variable is then transformed into a dummy
#' variable inside the data frame. The dummy variable is used to generate a totals
#' estimation, which is then assessed in terms of its reliability. In terms of their
#' reliability, estimations can be classified as reliable, weakly reliable and
#' non-reliable.
#'
#' @description
#' This function streamlines the process of evaluating estimation reliability.
#'
#' @param mydata A data frame or tibble containing the survey data.
#' @param est_var Estimation variable. Must be categorical in nature, and in terms of R
#'        it must consist in a labelled numeric vector. Default value is "nivel_educ".
#' @param group_var Grouping variable. Must be categorical in nature. If no grouping
#'        variable is required for the estimation assessment, group_var must be specified
#'        to "NULL" (a string). Default value is "NULL" (a string).
#' @param subpop Subpopulation variable and values for the subpopulation variable, in
#'        string format. If no subpopulation variable is required for the estimation
#'        assessment, subpop must be specified to "TRUE" (a string). Default value is
#'        "cui_psdf_o_resp_nna == 1".
#' @param qst Variable denoting the questionnaire to which the estimation variable
#'        belongs. Only possible values are "ch" or "cut". Default value is "ch".
#' @param value Given categorical response for the estimation variable, expressed as
#'        a single numeric value. Default value is 1.
#' @param id_estimacion A given string which allows identification of a specific
#'        estimation across different settings. \code{id_estimacion} has no bearing
#'        on the process of generating estimations and is merely intended to function
#'        as a name. Default value is "tot_nivel_educ_cuid".
#'
#' @details
#' The function operates in the following way:
#' \itemize{
#'   \item The function generates a subpopulation variable from a given string specifying
#'         the subpopulation of interest;
#'   \item Next, the function creates a reduced data frame containing the estimation
#'         variable, the grouping variable (if specified), the subpopulation variable (if
#'         specified), and columns for strata, primary sampling units and weights;
#'   \item Next, the function transforms a labelled numeric vector into a factor;
#'   \item Next, the function transforms a given factor level into a dummy variable (with
#'         values 0 and 1);
#'   \item Next, the function estimates the population total for the given dummy, and
#'         evaluates its reliability (proportions can be classified into reliable, weakly
#'         reliable or non-reliable estimations);
#'   \item Finally, the function generates a data frame as its output, containing all the
#'         relevant information for reliability evaluation.
#' }
#'
#' @return
#' A data frame containing estimation reliability information.
#'
#' @seealso \code{\link[calidad]{create_size}}, \code{\link[calidad]{assess}}.
#'
#' @importFrom calidad create_size
#' @importFrom calidad assess
#' @importFrom sjlabelled get_values
#' @importFrom sjlabelled get_labels
#' @importFrom sjlabelled to_factor
#' @importFrom rlang parse_expr
#' @importFrom survey svydesign
#' @importFrom stats as.formula
#'
eval_calidad_total_nivel <-
  function(mydata,
           est_var = "nivel_educ",
           group_var = "NULL",
           subpop = "cui_psdf_o_resp_nna == 1",
           qst = "ch",
           value = 1,
           id_estimacion = "tot_nivel_educ_cuid") {
    ### 1. Abrir ambiente de función ###########################################

    ### 2. Crear variable de subpoblación ######################################
    mynewdata <-
      mydata %>%
      mutate(
        subpop =
          if_else(
            !!rlang::parse_expr(subpop),
            1,
            0
          )
      ) %>%
      mutate(subpop = tidyr::replace_na(subpop, 0))

    ### 3. Crear data frame reducido ###########################################
    ###### Crear strings que denotan variables
    strata <- "varstrat"
    ids <- "varunit"
    weights <- paste0("fe_", qst)

    ###### Filtrar por informante Kish == 1
    if (weights == "fe_cut") {
      mynewdata <-
        mynewdata %>%
        filter(informante_kish == 1)
    }

    ###### Reducir data frame
    mynewdata <-
      mynewdata %>%
      select(
        all_of(
          c(
            est_var,
            strata,
            ids,
            weights
          )
        ),
        !!rlang::parse_expr(group_var), # Necesario por si group_var es NULL
        subpop
      )

    ### 4. Extrar el string de la etiqueta que corresponde al valor ############
    label2 <-
      data.frame(
        get_values = sjlabelled::get_values(mynewdata[[est_var]]),
        get_labels = sjlabelled::get_labels(mynewdata[[est_var]])
      ) %>%
      filter(get_values == value)

    label2 <-
      label2 %>%
      select(get_labels) %>%
      pull() %>%
      unique() %>%
      na.omit()

    ### 5. Convertir variable de estimación a factor ###########################
    mynewdata <-
      mynewdata %>%
      mutate(
        est_var_factor =
          sjlabelled::to_factor(
            !!rlang::parse_expr(est_var)
          )
      )

    ### 6. Crear una dummy a partir del valor prestablecido de est_var #########
    mynewdata <-
      mynewdata %>%
      mutate(
        dummy_value =
          if_else(est_var_factor %in% value,
                  1,
                  0
          )
      )

    ### 7. Creación del objeto disenio #########################################
    disenio <-
      survey::svydesign(
        data = mynewdata,
        strata = as.formula(paste0("~", strata)),
        ids = as.formula(paste0("~", ids)),
        weights = as.formula(paste0("~", weights))
      )
    options(survey.lonely.psu = "certainty")

    ### 8. Evaluación de calidad para el nivel seleccionado de est_var #########
    if (!is.null(rlang::parse_expr(group_var))) {
      calidad <-
        calidad::create_size(
          var = "dummy_value",
          subpop = "subpop",
          domains = group_var,
          design = disenio
        ) %>%
        calidad::assess(publish = FALSE)
    } else {
      calidad <-
        calidad::create_size(
          var = "dummy_value",
          subpop = "subpop",
          design = disenio
        ) %>%
        calidad::assess(publish = FALSE)
    }

    ### 9. Agregar información a tabla de calidad ##############################
    calidad <-
      calidad %>%
      mutate(
        est_var = est_var,
        level = value,
        label2 = label2,
        id_estimacion = id_estimacion
      ) %>%
      relocate(
        id_estimacion,
        est_var,
        level,
        label2,
        .before = 1
      )

    ### 10. Señalar objeto a devolver ##########################################
    return(calidad)

    ### 11. Cerrar ambiente de función #########################################
  }

#' Evaluate reliability for proportions estimation for all values of a given categorical variable
#'
#' The function creates a data frame where an estimation variable (for a proportion), a
#' grouping variable and a subpopulation variable are specified, along with columns
#' necessary for complex survey analysis (strata, primary sampling units, and weights).
#' The function iterates the function \code{\link[enuttools]{eval_calidad_prop_nivel}} for
#' all values of the categorical estimation variable, thus assessing separately an
#' estimation for each category (in terms of reliability). In terms of their reliability,
#' estimations can be classified as reliable, weakly reliable and non-reliable.
#'
#' @description
#' This function streamlines the process of evaluating estimation reliability for all
#' values of a categorical variable (thus, works only for estimating proportions).
#'
#' @param mydata A data frame or tibble containing the survey data.
#' @param est_var Estimation variable. Must be categorical in nature, and in terms of R
#'        it must consist in a labelled numeric vector. Default value is "p_tdnr_admnhog".
#' @param group_var Grouping variable. Must be categorical in nature. If no grouping
#'        variable is required for the estimation assessment, group_var must be specified
#'        to "NULL" (a string). Default value is "cui_psdf_o_resp_nna".
#' @param subpop Subpopulation variable and values for the subpopulation variable, in
#'        string format. If no subpopulation variable is required for the estimation
#'        assessment, subpop must be specified to "TRUE" (a string). Default value is "TRUE" (a string).
#' @param qst Variable denoting the questionnaire to which the estimation variable
#'        belongs. Only possible values are "ch" or "cut". Default value is "cut".
#' @param id_estimacion A given string which allows identification of a specific
#'        estimation across different settings. \code{id_estimacion} has no bearing
#'        on the process of generating estimations and is merely intended to function
#'        as a name. Default value is "prop_cuid_p_tdnr_admnhog".
#'
#' @details
#' The function operates in the following way:
#' \itemize{
#'   \item The function generates a data frame for all values or categories of the
#'         estimation variable;
#'   \item Next, the function creates a numeric ID for each value in the data frame;
#'   \item Next, the function iterates the function \code{\link[enuttools]{eval_calidad_prop_nivel}}
#'         for each value ID and binds the resulting list into a tibble df containing
#'         reliability information for all values of the categorical variable.
#' }
#'
#' @return
#' A data frame containing estimation reliability information.
#'
#' @seealso \code{\link[enuttools]{eval_calidad_prop_nivel}}.
#'
#' @importFrom purrr map2
#' @importFrom sjlabelled get_values
#' @export
eval_calidad_prop_completar_niveles <-
  function(mydata,
           est_var = "p_tdnr_admnhog",
           group_var = "cui_psdf_o_resp_nna",
           subpop = "TRUE",
           qst = "cut",
           id_estimacion = "prop_cuid_p_tdnr_admnhog") {
    ### 1. Abrir ambiente de función ###########################################

    ### 2. Crear data frame con categorías de la variable ######################
    catsdf <-
      data.frame(
        categorias = sjlabelled::get_values(mydata[[est_var]])
      ) %>%
      mutate(est_var = est_var) %>%
      relocate(est_var, .before = 1)

    ### 3. Crear data frame con estimaciones y evaluaciones ####################
    evaldf <-
      purrr::map2(
        .x = catsdf$est_var,
        .y = catsdf$categorias,
        .f = ~ eval_calidad_prop_nivel(
          mydata = mydata,
          est_var = .x,
          value = .y,
          group_var = group_var,
          subpop = subpop,
          qst = qst,
          id_estimacion = id_estimacion
        )
      ) %>%
      purrr::list_rbind()

    ### 4. Señalar objeto a devolver ###########################################
    return(evaldf)

    ### 5. Cerrar ambiente de función ##########################################
  }

#' Evaluate reliability for proportions estimation for a given variable and variable level
#'
#' The function creates a data frame where an estimation variable (for a proportion), a
#' grouping variable and a subpopulation variable are specified, along with columns
#' necessary for complex survey analysis (strata, primary sampling units, and weights).
#' A given value or category for the estimation variable is then transformed into a dummy
#' variable inside the data frame. The dummy variable is used to generate a proportion
#' estimation, which is then assessed in terms of its reliability. In terms of their
#' reliability, estimations can be classified as reliable, weakly reliable and
#' non-reliable.
#'
#' @description
#' This function streamlines the process of evaluating estimation reliability.
#'
#' @param mydata A data frame or tibble containing the survey data.
#' @param est_var Estimation variable. Must be categorical in nature, and in terms of R
#'        it must consist in a labelled numeric vector. Default value is "p_tdnr_admnhog".
#' @param group_var Grouping variable. Must be categorical in nature. If no grouping
#'        variable is required for the estimation assessment, group_var must be specified
#'        to "NULL" (a string). Default value is "cui_psdf_o_resp_nna".
#' @param subpop Subpopulation variable and values for the subpopulation variable, in
#'        string format. If no subpopulation variable is required for the estimation
#'        assessment, subpop must be specified to "TRUE" (a string). Default value is "TRUE" (a string).
#' @param qst Variable denoting the questionnaire to which the estimation variable
#'        belongs. Only possible values are "ch" or "cut". Default value is "cut".
#' @param value Given categorical response for the estimation variable, expressed as
#'        a single numeric value. Default value is 1.
#' @param id_estimacion A given string which allows identification of a specific
#'        estimation across different settings. \code{id_estimacion} has no bearing
#'        on the process of generating estimations and is merely intended to function
#'        as a name. Default value is "prop_cuid_p_tdnr_admnhog".
#'
#' @details
#' The function operates in the following way:
#' \itemize{
#'   \item The function generates a subpopulation variable from a given string specifying
#'         the subpopulation of interest;
#'   \item Next, the function creates a reduced data frame containing the estimation
#'         variable, the grouping variable (if specified), the subpopulation variable (if
#'         specified), and columns for strata, primary sampling units and weights;
#'   \item Next, the function transforms a labelled numeric vector into a factor;
#'   \item Next, the function transforms a given factor level into a dummy variable (with
#'         values 0 and 1);
#'   \item Next, the function estimates the population proportion for the given dummy, and
#'         evaluates its reliability (proportions can be classified into reliable, weakly
#'         reliable or non-reliable estimations);
#'   \item Finally, the function generates a data frame as its output, containing all the
#'         relevant information for reliability evaluation.
#' }
#'
#' @return
#' A data frame containing estimation reliability information.
#'
#' @seealso \code{\link[calidad]{create_prop}}, \code{\link[calidad]{assess}}.
#'
#' @importFrom calidad create_prop
#' @importFrom calidad assess
#' @importFrom sjlabelled get_values
#' @importFrom sjlabelled get_labels
#' @importFrom sjlabelled to_factor
#' @importFrom rlang parse_expr
#' @importFrom survey svydesign
#' @importFrom stats as.formula
#'
eval_calidad_prop_nivel <-
  function(mydata,
           est_var = "p_tdnr_admnhog",
           group_var = "cui_psdf_o_resp_nna",
           subpop = "TRUE",
           qst = "cut",
           value = 1,
           id_estimacion = "prop_cuid_p_tdnr_admnhog") {
    ### 1. Abrir ambiente de función ###########################################

    ### 2. Crear variable de subpoblación ######################################
    mynewdata <-
      mydata %>%
      mutate(
        subpop =
          if_else(
            !!rlang::parse_expr(subpop),
            1,
            0
          )
      ) %>%
      mutate(subpop = tidyr::replace_na(subpop, 0))

    ### 3. Crear data frame reducido ###########################################
    ###### Crear strings que denotan variables
    strata <- "varstrat"
    ids <- "varunit"
    weights <- paste0("fe_", qst)

    ###### Filtrar por informante Kish == 1
    if (weights == "fe_cut") {
      mynewdata <-
        mynewdata %>%
        filter(informante_kish == 1)
    }

    ###### Reducir data frame
    mynewdata <-
      mynewdata %>%
      select(
        all_of(
          c(
            est_var,
            strata,
            ids,
            weights
          )
        ),
        !!rlang::parse_expr(group_var), # Necesario por si group_var es NULL
        subpop
      )

    ### 4. Extrar el string de la etiqueta que corresponde al valor ############
    label2 <-
      data.frame(
        get_values = sjlabelled::get_values(mynewdata[[est_var]]),
        get_labels = sjlabelled::get_labels(mynewdata[[est_var]])
      ) %>%
      filter(get_values == value)

    label2 <-
      label2 %>%
      select(get_labels) %>%
      pull() %>%
      unique() %>%
      na.omit()

    ### 5. Convertir variable de estimación a factor ###########################
    mynewdata <-
      mynewdata %>%
      mutate(
        est_var_factor =
          sjlabelled::to_factor(
            !!rlang::parse_expr(est_var)
          )
      )

    ### 6. Crear una dummy a partir del valor prestablecido de est_var #########
    mynewdata <-
      mynewdata %>%
      mutate(
        dummy_value =
          if_else(est_var_factor %in% value,
                  1,
                  0
          )
      )

    ### 7. Creación del objeto disenio #########################################
    disenio <-
      survey::svydesign(
        data = mynewdata,
        strata = as.formula(paste0("~", strata)),
        ids = as.formula(paste0("~", ids)),
        weights = as.formula(paste0("~", weights))
      )
    options(survey.lonely.psu = "certainty")

    ### 8. Evaluación de calidad para el nivel seleccionado de est_var #########
    if (!is.null(rlang::parse_expr(group_var))) {
      calidad <-
        calidad::create_prop(
          var = "dummy_value",
          subpop = "subpop",
          domains = group_var,
          design = disenio
        ) %>%
        calidad::assess(publish = FALSE)
    } else {
      calidad <-
        calidad::create_prop(
          var = "dummy_value",
          subpop = "subpop",
          design = disenio
        ) %>%
        calidad::assess(publish = FALSE)
    }

    ### 9. Agregar información a tabla de calidad ##############################
    calidad <-
      calidad %>%
      mutate(
        est_var = est_var,
        level = value,
        label2 = label2,
        id_estimacion = id_estimacion
      ) %>%
      relocate(
        id_estimacion,
        est_var,
        level,
        label2,
        .before = 1
      )

    ### 10. Señalar objeto a devolver ##########################################
    return(calidad)

    ### 11. Cerrar ambiente de función #########################################
  }

#' Evaluate reliability for means estimation for a given numeric variable
#'
#' The function creates a data frame where an estimation variable (for a mean), a
#' grouping variable and a subpopulation variable are specified, along with columns
#' necessary for complex survey analysis (strata, primary sampling units, and weights).
#' The function applies the functions \code{\link[calidad]{create_mean}} and
#' \code{\link[calidad]{assess}} for a given estimation (numeric) variable, its
#' grouping variable and its subpopulation variable. It generates as its output a tibble
#' data frame which contains information on estimation reliability. In terms of their
#' reliability, estimations can be classified as reliable, weakly reliable and
#' non-reliable.
#'
#' @description
#' This function streamlines the process of evaluating estimation reliability for
#' means.
#'
#' @param mydata A data frame or tibble containing the survey data.
#' @param est_var Estimation variable. Must be quantitative, and in terms of R
#'        it must consist of a numeric vector. Default value is "t_tvaoh_oh".
#' @param group_var Grouping variable. Must be categorical in nature. If no grouping
#'        variable is required for the estimation assessment, group_var must be specified
#'        to "NULL" (a string). Default value is "cui_psdf_o_resp_nna".
#' @param subpop Subpopulation variable and values for the subpopulation variable, in
#'        string format. If no subpopulation variable is required for the estimation
#'        assessment, subpop must be specified to "TRUE" (a string). Default value is "TRUE" (a string).
#' @param qst Variable denoting the questionnaire to which the estimation variable
#'        belongs. Only possible values are "ch" or "cut". Default value is "cut".
#' @param id_estimacion A given string which allows identification of a specific
#'        estimation across different settings. \code{id_estimacion} has no bearing
#'        on the process of generating estimations and is merely intended to function
#'        as a name. Default value is "mean_cuid_t_tvaoh_oh".
#'
#' @details
#' The function operates in the following way:
#' \itemize{
#'   \item The function generates a subpopulation variable from a given string specifying
#'         the subpopulation of interest;
#'   \item Next, the function creates a reduced data frame containing the estimation
#'         variable, the grouping variable (if specified), the subpopulation variable (if
#'         specified), and columns for strata, primary sampling units and weights;
#'   \item Next, the function estimates the population mean for the given variable, and
#'         evaluates its reliability (means can be classified into reliable, weakly
#'         reliable or non-reliable estimations);
#'   \item Finally, the function generates a data frame as its output, containing all the
#'         relevant information for reliability evaluation.
#' }
#'
#' @return
#' A data frame containing estimation reliability information.
#'
#' @seealso \code{\link[calidad]{create_mean}}, \code{\link[calidad]{assess}}.
#'
#' @importFrom calidad create_mean
#' @importFrom calidad assess
#' @importFrom rlang parse_expr
#' @importFrom survey svydesign
#' @export
eval_calidad_mean <-
  function(mydata,
           est_var = "t_tvaoh_oh",
           group_var = "cui_psdf_o_resp_nna",
           subpop = "TRUE",
           qst = "cut",
           id_estimacion = "mean_cuid_t_tvaoh_oh") {
    ### 1. Abrir ambiente de función ###########################################

    ### 2. Crear variable de subpoblación ######################################
    mynewdata <-
      mydata %>%
      mutate(
        subpop =
          if_else(
            !!rlang::parse_expr(subpop),
            1,
            0
          )
      ) %>%
      mutate(subpop = tidyr::replace_na(subpop, 0))

    ### 3. Crear data frame reducido ###########################################
    ###### Crear strings que denotan variables
    strata <- "varstrat"
    ids <- "varunit"
    weights <- paste0("fe_", qst)

    ###### Filtrar por informante Kish == 1
    if (weights == "fe_cut") {
      mynewdata <-
        mynewdata %>%
        filter(informante_kish == 1)
    }

    ###### Reducir data frame
    mynewdata <-
      mynewdata %>%
      select(
        all_of(
          c(
            est_var,
            strata,
            ids,
            weights
          )
        ),
        !!rlang::parse_expr(group_var), # Necesario por si group_var es NULL
        subpop
      )

    ### 4. Creación del objeto disenio #########################################
    disenio <-
      survey::svydesign(
        data = mynewdata,
        strata = as.formula(paste0("~", strata)),
        ids = as.formula(paste0("~", ids)),
        weights = as.formula(paste0("~", weights))
      )
    options(survey.lonely.psu = "certainty")

    ### 5. Evaluación de calidad para la media de est_var ######################
    if (!is.null(rlang::parse_expr(group_var))) {
      calidad <-
        calidad::create_mean(
          var = est_var,
          subpop = "subpop",
          domains = group_var,
          design = disenio,
          rm.na = TRUE
        ) %>%
        calidad::assess(publish = FALSE)
    } else {
      calidad <-
        calidad::create_mean(
          var = est_var,
          subpop = "subpop",
          design = disenio,
          rm.na = TRUE
        ) %>%
        calidad::assess(publish = FALSE)
    }

    ### 6. Agregar información a tabla de calidad ##############################
    calidad <-
      calidad %>%
      mutate(
        est_var = est_var,
        id_estimacion = id_estimacion
      ) %>%
      relocate(
        id_estimacion,
        est_var,
        .before = 1
      )

    ### 7. Señalar objeto a devolver ###########################################
    return(calidad)

    ### 8. Cerrar ambiente de función ##########################################
  }
