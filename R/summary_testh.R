#' Test of Hypotheses for Complex Survey Designs
#'
#' Performs multiple types of hypothesis tests to assess statistically significant differences
#' (proportions, totals, means, participation rates, etc.) between groups using complex survey designs.
#' The results for each test are stored in an Excel file, with one sheet per estimation.
#'
#' @description
#' The function iterates over the rows of the input specification table (`insumo`),
#' where each row defines a hypothesis test to be performed. Each test specifies:
#' - the variable to be analyzed (`est.var`)
#' - the grouping variable (`group.var`)
#' - the optional subpopulation filter (`subpop`)
#' - and the estimation type (`type.var`).
#'
#' Depending on the estimation type, the function applies the corresponding method:
#'
#' \itemize{
#'   \item **"categorica"** — Proportion test between two groups using \code{\link[survey]{svycontrast}}.
#'   \item **"nivel"** — Total counts comparison between two groups using \code{svycontrast}.
#'   \item **"participacion"** — t-test for proportions using \code{\link[survey]{svyttest}}.
#'   \item **"media"** — t-test for means using \code{svyttest}.
#'   \item **"media_niveles"** — Comparison of means across multiple groups using \code{\link[survey]{svyglm}}.
#'   \item **"prop_niveles"** — Comparison of proportions across multiple groups using logistic regression with \code{svyglm(..., family = quasibinomial())}.
#'   \item **"tot_niveles"** — Comparison of totals across multiple groups using \code{svycontrast}.
#'   \item **"0"** — Skips estimation.
#' }
#'
#' Each set of results is written to an Excel workbook (via \pkg{openxlsx}),
#' with one sheet per test, labeled according to the \code{id_estimacion} column in `insumo`.
#'
#' @param insumo A data frame or tibble specifying the tests to be performed.
#' Must include the following columns:
#' \describe{
#'   \item{cuestionario}{Specifies which survey design to use ("ch" or "cut").}
#'   \item{est.var}{Name of the variable to estimate (string).}
#'   \item{group.var}{Name of the grouping variable (string, must have exactly two levels for two-group comparisons).}
#'   \item{subpop}{Optional logical expression defining a subpopulation to subset.}
#'   \item{type.var}{Type of estimation to perform ("categorica", "nivel", "participacion", "media", "media_niveles", "prop_niveles", "tot_niveles", or "0").}
#'   \item{id_estimacion}{Unique identifier for naming the Excel sheet corresponding to the test.}
#' }
#'
#' @param archivo_salida Character string. Path and filename for the output Excel file.
#' Defaults to `"resultados_finales.xlsx"`.
#'
#' @details
#' The function requires that the survey design objects \code{design_ch} and \code{design_cut}
#' exist in the environment before execution.
#' Each row of `insumo` corresponds to one hypothesis test.
#'
#'
#' @details
#' The function relies on pre-defined survey design objects named `design_ch` and `design_cut`.
#' These must exist in the environment prior to running the function.
#' Each row of `insumo` represents one hypothesis test.
#'
#' **Data preprocessing**
#'
#' Before running the analysis, a data cleaning procedure is applied:
#' - Missing values, as well as responses coded as *"Don’t know"* or *"No answer"*,
#'   are recoded as `NA`.
#'
#' **Estimation assumptions**
#'
#' - When estimating proportions using `svyttest`, it is assumed that the variables
#'   of interest (`var_est`) are dummy variables, i.e., binary indicators taking values 0 and 1.
#' - Similarly, grouping variables (`group.var`) must also be binary (0/1) to ensure
#'   valid group comparisons.
#'
#' This preprocessing step ensures that the variables are properly formatted for
#' statistical testing and that the results are consistent with the assumptions
#' of survey-based hypothesis testing.
#'
#'
#' Internally, the function:
#' \enumerate{
#'   \item Selects the appropriate survey design (\code{design_ch} or \code{design_cut}).
#'   \item Applies the optional subpopulation filter (if defined in `subpop`).
#'   \item Runs the statistical test based on the \code{type.var} value.
#'   \item Extracts the estimates, standard errors, t-values, degrees of freedom, and p-values.
#'   \item Evaluates statistical significance (p < 0.05 = "Significativa").
#'   \item Exports the summarized results to the Excel file defined in \code{archivo_salida}.
#' }
#'
#' @return
#' The function creates an Excel workbook where each sheet contains the results of one hypothesis test.
#' It returns \code{NULL} (invisibly) after saving the file.
#'
#' @section Assumptions:
#' - Variables used in \code{participacion} or \code{prop_niveles} tests are binary (0/1).
#' - Grouping variables must have exactly two categories for two-group tests.
#' - The user must ensure that the survey design objects (\code{design_ch}, \code{design_cut}) are properly defined.
#'
#' @import survey
#' @import openxlsx
#' @import dplyr
#' @import tibble
#'
#' @importFrom stats update coef setNames pt vcov quasibinomial
#'
#' @export
#'

summary_testh <- function(insumo, archivo_salida = "resultados_finales.xlsx") {
  # --- Crear libro Excel donde se guardarán los resultados ---
  wb <- createWorkbook()

  # --- Recorrer fila por fila la tabla 'insumo' ---
  for (i in seq_len(nrow(insumo))) {
    fila <- insumo[i, ] # Extrae una fila del insumo
    resumen_final <- NULL # Inicializa el objeto de resultados para esa fila

    # =====================================================
    # 1. Seleccionar diseño muestral según cuestionario
    # =====================================================
    diseno <- switch(fila$cuestionario,
      "ch" = design_ch, # Usa el diseño del cuestionario del hogar (CH)
      "cut" = design_cut, # Usa el diseño del cuestionario de tiempo
      stop("Valor de 'cuestionario' no reconocido (usa 'ch' o 'cut').")
    )

    # =====================================================
    # 2. Definir variables a utilizar en la estimación
    # =====================================================
    var_est <- fila$est.var # Variable a estimar
    var_group <- if (!is.na(fila$group.var)) fila$group.var else NULL # Variable de agrupación
    var_subpop <- if (!is.na(fila$subpop)) fila$subpop else NULL # Filtro o subpoblación
    type_est <- fila$type.var # Tipo de estimación a realizar
    id_estimacion <- fila$id_estimacion # Nombre único del resultado a guardar en pestaña excel

    # =====================================================
    # 3. Aplicar subpoblación (si se define)
    # =====================================================
    diseno_sub <- if (!is.null(var_subpop)) subset(diseno, eval(parse(text = var_subpop))) else diseno

    # =====================================================
    # 4. Diferentes tipos de estimaciones según 'type_est'
    # =====================================================

    # -----------------------------------------------------
    # CASO 1: type_est == "categorica"
    # Calcula proporciones test de hipótesis a un 95% de confianza entre dos grupos
    # usando svycontrast
    # -----------------------------------------------------
    if (type_est == "categorica") {
      # Solo procede si existe variable de grupo binaria
      if (!is.null(var_group) && length(unique(diseno_sub$variables[[var_group]])) == 2) {
        # Convierte variables a factor en un diseño complejo temporal
        diseno_sub$variables[[var_est]] <- factor(diseno_sub$variables[[var_est]])
        diseno_sub$variables[[var_group]] <- factor(diseno_sub$variables[[var_group]])

        # Obtiene niveles de la variable estimada
        niveles_var <- levels(diseno_sub$variables[[var_est]])
        resumen_list <- list()

        # Recorre cada nivel de la variable categórica
        for (nivel_var in niveles_var) {
          # Crea variable dummy para el nivel específico
          diseno_tmp <- update(diseno_sub,
            dummy = as.numeric(as.character(diseno_sub$variables[[var_est]]) == as.character(nivel_var))
          )

          # Calcula proporciones por grupo de cada variable a estimar
          prop <- svyby(~dummy,
            as.formula(paste0("~", var_group)),
            design = diseno_tmp,
            FUN = svymean,
            vartype = "se",
            na.rm = TRUE,
            covmat = TRUE
          )

          # Si no hay dos grupos válidos, salta al siguiente
          coef_names <- names(coef(prop))
          if (length(coef_names) != 2) next

          # Calcula diferencia entre grupos con svycontrast
          contraste <- svycontrast(prop, setNames(c(1, -1), coef_names))

          # Calcula estadísticos del contraste
          est <- as.numeric(contraste)
          se <- SE(contraste)
          gl <- degf(diseno_tmp)
          t_val <- if (!is.na(se) & se != 0) est / se else NA
          p_val <- if (!is.na(t_val)) sprintf("%.3f", pt(abs(t_val), df = gl, lower.tail = FALSE) * 2) else NA
          resultado <- if (!is.na(p_val) & as.numeric(p_val) < 0.05) "Significativa" else "No significativa"

          # Prepara resultados en tibble
          prop_vals <- coef(prop)
          niveles_group <- levels(diseno_tmp$variables[[var_group]])

          resumen_list[[nivel_var]] <- tibble(
            var_est = var_est,
            nivel_var_est = nivel_var,
            var_group = var_group,
            grupo_ref = niveles_group[1],
            grupo_comp = niveles_group[2],
            prop_ref = prop_vals[1],
            prop_comp = prop_vals[2],
            diferencia = est,
            std_error = se,
            df = gl,
            t_value = t_val,
            p_value = p_val,
            significancia = resultado
          )
        }

        resumen_final <- bind_rows(resumen_list)
      } else {
        # Si no existe variable de grupo válida
        resumen_final <- tibble(mensaje = "No existe variable group.var")
      }
    }


    # -----------------------------------------------------
    # CASO 2: type_est == "nivel"
    # Calcula n totales y test de hipótesis a un 95% de confianza entre grupos
    # -----------------------------------------------------
    else if (type_est == "nivel") {
      if (!is.null(var_group) && length(unique(diseno_sub$variables[[var_group]])) == 2) {
        diseno_sub$variables[[var_est]] <- factor(diseno_sub$variables[[var_est]])
        diseno_sub$variables[[var_group]] <- factor(diseno_sub$variables[[var_group]])

        niveles_var <- levels(diseno_sub$variables[[var_est]])
        resumen_list <- list()

        for (nivel_var in niveles_var) {
          diseno_tmp <- update(diseno_sub,
            dummy = as.numeric(as.character(diseno_sub$variables[[var_est]]) == as.character(nivel_var))
          )


          # Calcula totales por grupo
          tot <- svyby(~dummy,
            as.formula(paste0("~", var_group)),
            design = diseno_tmp,
            FUN = svytotal,
            na.rm = TRUE,
            vartype = "se",
            covmat = TRUE
          )

          coef_names <- names(coef(tot))
          if (length(coef_names) != 2) next

          # Contraste entre grupos
          contraste <- svycontrast(tot, setNames(c(1, -1), coef_names))

          # Estadísticos
          est <- as.numeric(contraste)
          se <- SE(contraste)
          gl <- degf(diseno_tmp)
          t_val <- if (!is.na(se) & se != 0) est / se else NA
          p_val <- if (!is.na(t_val)) sprintf("%.3f", pt(abs(t_val), df = gl, lower.tail = FALSE) * 2) else NA
          resultado <- if (!is.na(p_val) & as.numeric(p_val) < 0.05) "Significativa" else "No significativa"

          prop_vals <- coef(tot)
          niveles_group <- levels(diseno_tmp$variables[[var_group]])

          resumen_list[[nivel_var]] <- tibble(
            var_est = var_est,
            nivel_var_est = nivel_var,
            var_group = var_group,
            grupo_ref = niveles_group[1],
            grupo_comp = niveles_group[2],
            n_ref = prop_vals[1],
            n_comp = prop_vals[2],
            diferencia = est,
            std_error = se,
            df = gl,
            t_value = t_val,
            p_value = p_val,
            significancia = resultado
          )
        }

        resumen_final <- bind_rows(resumen_list)
      } else {
        resumen_final <- tibble(mensaje = "No existe variable group.var")
      }
    }


    # -----------------------------------------------------
    # CASO 3: type_est == "participacion"
    # Prueba t entre dos grupos usando svyttest()
    # -----------------------------------------------------
    else if (type_est == "participacion") {
      if (!is.null(var_group) && length(unique(diseno_sub$variables[[var_group]])) == 2) {
        # Prueba t entre grupos
        f_test <- as.formula(paste0(var_est, " ~ ", var_group))
        test <- svyttest(f_test, design = diseno_sub)

        # Medias (proporciones) por grupo
        prop <- svyby(as.formula(paste0("~", var_est)),
          as.formula(paste0("~", var_group)),
          diseno_sub, svymean,
          na.rm = TRUE
        )

        # Se sume que la variable por la cual se agrupa es binaria
        prop_1 <- prop[[var_est]][prop[[var_group]] == 1]
        prop_0 <- prop[[var_est]][prop[[var_group]] == 0]

        # Valor p y resultado
        p_val <- sprintf("%.3f", test$p.value)
        resultado <- ifelse(as.numeric(p_val) < 0.05, "Significativa", "No significativa")

        resumen_final <- tibble(
          var_est = var_est,
          var_group = var_group,
          prop_grupo_0 = prop_0,
          prop_grupo_1 = prop_1,
          t_value = test$statistic,
          df = test$parameter,
          p_value = p_val,
          significancia = resultado
        )
      } else {
        resumen_final <- tibble(mensaje = "No existe variable group.var")
      }
    }


    # -----------------------------------------------------
    # CASO 4: type_est == "media"
    # Compara promedios (tiempos, por ejemplo)
    # entre dos grupos con svyttest()
    # -----------------------------------------------------
    else if (type_est == "media") {
      # Verifica que exista una variable de agrupación (var_group)
      # y que tenga exactamente dos categorías (por ejemplo: hombres/mujeres)
      if (!is.null(var_group) && length(unique(diseno_sub$variables[[var_group]])) == 2) {
        # Crea una fórmula para el test: variable dependiente ~ grupo
        f_test <- as.formula(paste0(var_est, " ~ ", var_group))

        # Realiza un test t de comparación de medias ponderado según el diseño complejo
        test <- svyttest(f_test, design = diseno_sub)

        # Calcula las medias estimadas para cada grupo con varianza robusta
        media <- svyby(as.formula(paste0("~", var_est)),
          as.formula(paste0("~", var_group)),
          diseno_sub, svymean,
          na.rm = TRUE
        )

        # Extrae las medias de los grupos 1 y 0 (asumiendo codificación binaria)
        media_1 <- media[[2]][media[[1]] == 1]
        media_0 <- media[[2]][media[[1]] == 0]

        # Obtiene el valor p del test y evalúa significancia estadística
        p_val <- sprintf("%.3f", test$p.value)
        resultado <- ifelse(as.numeric(p_val) < 0.05, "Significativa", "No significativa")

        # Crea el tibble de salida con los resultados del test
        resumen_final <- tibble(
          var_est = var_est,
          var_group = var_group,
          media_grupo_0 = media_0,
          media_grupo_1 = media_1,
          t_value = test$statistic,
          df = test$parameter,
          p_value = p_val,
          significancia = resultado
        )
      } else {
        # Si la variable de grupo no existe o no tiene dos niveles, se reporta el error
        resumen_final <- tibble(mensaje = "No existe variable group.var")
      }
    }


    # -----------------------------------------------------
    # CASO 5: medias con más de 2 grupos (svyglm)
    # -----------------------------------------------------
    else if (type_est == "media_niveles") {
      # Convierte la variable de agrupación en factor y obtiene los niveles
      diseno_sub$variables[[var_group]] <- factor(diseno_sub$variables[[var_group]])
      niveles_group <- levels(diseno_sub$variables[[var_group]])
      n_levels <- length(niveles_group)

      # Define la fórmula del modelo lineal generalizado (svyglm)
      f_test <- as.formula(paste0(var_est, " ~ factor(", var_group, ")"))
      modelo <- svyglm(f_test, design = diseno_sub)

      # Calcula las medias ponderadas por grupo
      medias <- svyby(
        as.formula(paste0("~", var_est)),
        as.formula(paste0("~", var_group)),
        design = diseno_sub,
        FUN = svymean,
        na.rm = TRUE
      )

      # Extrae los coeficientes, matriz de covarianza y grados de libertad
      b <- coef(modelo)
      V <- vcov(modelo)
      df <- degf(diseno_sub)

      comparaciones <- tibble()

      # Realiza todas las comparaciones posibles entre pares de grupos
      for (i in 1:(n_levels - 1)) {
        for (j in (i + 1):n_levels) {
          # Construye el contraste entre los grupos i y j
          contraste <- rep(0, length(b))
          names(contraste) <- names(b)

          if (i > 1 & j > 1) {
            contraste[paste0("factor(", var_group, ")", niveles_group[j])] <- 1
            contraste[paste0("factor(", var_group, ")", niveles_group[i])] <- -1
          } else if (i == 1 & j > 1) {
            contraste[paste0("factor(", var_group, ")", niveles_group[j])] <- 1
          }

          # Calcula diferencia, error estándar, valor t y p-value
          diff_est <- sum(contraste * b)
          se_diff <- sqrt(t(contraste) %*% V %*% contraste)
          t_value <- diff_est / se_diff
          p_pr <- sprintf("%.3f", 2 * pt(-abs(t_value), df))

          # Obtiene las medias de cada grupo comparado
          media_i <- medias[[var_est]][medias[[var_group]] == niveles_group[i]]
          media_j <- medias[[var_est]][medias[[var_group]] == niveles_group[j]]

          # Obtiene el valor p del test y evalúa significancia estadística
          resultado <- ifelse(as.numeric(p_pr) < 0.05, "Significativa", "No significativa")

          # Guarda los resultados de la comparación
          comparaciones <- bind_rows(
            comparaciones,
            tibble(
              var_est = var_est,
              var_group = var_group,
              comparacion = paste0(niveles_group[i], " vs ", niveles_group[j]),
              grupo_ref = niveles_group[i],
              grupo_comp = niveles_group[j],
              media_ref = media_i,
              media_comp = media_j,
              diferencia = diff_est,
              std_error = se_diff,
              t_value = t_value,
              p_value = p_pr,
              significancia = resultado
            )
          )
        }
      }

      resumen_final <- comparaciones
    }


    # -----------------------------------------------------
    # CASO 6: proporciones con más de 2 grupos (svyglm logístico)
    # -----------------------------------------------------
    else if (type_est == "prop_niveles") {
      # 1. Subset para omitir NA en la variable estimada
      diseno_sub <- subset(diseno_sub, !is.na(get(var_est)))

      # Convierte la variable de grupo en factor
      diseno_sub$variables[[var_group]] <- factor(diseno_sub$variables[[var_group]])
      niveles_group <- levels(diseno_sub$variables[[var_group]])
      n_levels <- length(niveles_group)

      # Define y ajusta un modelo logístico con diseño complejo
      f_test <- as.formula(paste0(var_est, " ~ factor(", var_group, ")"))
      modelo <- svyglm(f_test, design = diseno_sub, family = quasibinomial())

      # Calcula proporciones por grupo (promedio de una variable 0/1)
      props <- svyby(
        as.formula(paste0("~", var_est)),
        as.formula(paste0("~", var_group)),
        design = diseno_sub,
        FUN = svyciprop,
        na.rm = TRUE
      )

      # Extrae coeficientes, matriz de covarianza y grados de libertad
      b <- coef(modelo)
      V <- vcov(modelo)
      df <- degf(diseno_sub)

      comparaciones <- tibble()

      # Contrasta todas las combinaciones de pares de niveles
      for (i in 1:(n_levels - 1)) {
        for (j in (i + 1):n_levels) {
          contraste <- rep(0, length(b))
          names(contraste) <- names(b)

          if (i > 1 & j > 1) {
            contraste[paste0("factor(", var_group, ")", niveles_group[j])] <- 1
            contraste[paste0("factor(", var_group, ")", niveles_group[i])] <- -1
          } else if (i == 1 & j > 1) {
            contraste[paste0("factor(", var_group, ")", niveles_group[j])] <- 1
          }

          # Calcula diferencia en logits y su error estándar
          logit_diff <- sum(contraste * b)
          se_diff <- sqrt(t(contraste) %*% V %*% contraste)
          t_value <- logit_diff / se_diff
          p_pr <- sprintf("%.3f", 2 * pt(-abs(t_value), df))



          # Nombre correcto de columnas
          grp_col <- var_group
          col_prop <- var_est

          # Extraer proporciones correctas
          media_i <- as.numeric(props[[col_prop]][props[[grp_col]] == niveles_group[i]])
          media_j <- as.numeric(props[[col_prop]][props[[grp_col]] == niveles_group[j]])

          diferencia_aprox <- media_j - media_i


          # Obtiene el valor p del test y evalúa significancia estadística
          resultado <- ifelse(as.numeric(p_pr) < 0.05, "Significativa", "No significativa")

          # Crea tabla con resultados
          comparaciones <- bind_rows(
            comparaciones,
            tibble(
              var_est = var_est,
              var_group = var_group,
              comparacion = paste0(niveles_group[i], " vs ", niveles_group[j]),
              grupo_ref = niveles_group[i],
              grupo_comp = niveles_group[j],
              prop_ref = media_i,
              prop_comp = media_j,
              diferencia_prop = diferencia_aprox,
              logit_diff = logit_diff,
              std_error = se_diff,
              t_value = t_value,
              p_value = p_pr,
              significancia = resultado
            )
          )
        }
      }

      resumen_final <- comparaciones
    }


    # -----------------------------------------------------
    # CASO 7: totales con más de 2 grupos (svycontrast)
    # -----------------------------------------------------
    else if (type_est == "tot_niveles") {
      # Convierte variables en factores para análisis por niveles
      diseno_sub$variables[[var_est]] <- factor(diseno_sub$variables[[var_est]])
      diseno_sub$variables[[var_group]] <- factor(diseno_sub$variables[[var_group]])

      niveles_var <- levels(diseno_sub$variables[[var_est]])
      niveles_group <- levels(diseno_sub$variables[[var_group]])

      resumen_list <- list()

      # Recorre cada nivel de la variable principal (por ejemplo, tipo de actividad)
      for (nivel_var in niveles_var) {
        # Crea una variable dummy (1 si pertenece al nivel, 0 si no)
        diseno_tmp <- update(
          diseno_sub,
          dummy = as.numeric(as.character(diseno_sub$variables[[var_est]]) == as.character(nivel_var))
        )

        # Calcula el total estimado por grupo
        tot <- svyby(
          ~dummy,
          as.formula(paste0("~", var_group)),
          design = diseno_tmp,
          FUN = svytotal,
          na.rm = TRUE,
          vartype = "se",
          covmat = TRUE
        )

        coef_names <- names(coef(tot))
        n_levels <- length(coef_names)

        # Compara los totales entre todos los pares de grupos
        for (i in 1:(n_levels - 1)) {
          for (j in (i + 1):n_levels) {
            contraste <- rep(0, n_levels)
            names(contraste) <- coef_names
            contraste[i] <- -1
            contraste[j] <- 1

            # Aplica contraste lineal entre totales (svycontrast)
            contraste_result <- svycontrast(tot, contraste)

            # Calcula diferencia, error estándar, t, p y significancia
            est <- as.numeric(contraste_result)
            se <- SE(contraste_result)
            gl <- degf(diseno_tmp)
            t_val <- if (!is.na(se) & se != 0) est / se else NA
            p_val <- if (!is.na(t_val)) sprintf("%.3f", 2 * pt(-abs(t_val), df = gl)) else NA
            resultado <- ifelse(!is.na(p_val) & p_val < 0.05, "Significativa", "No significativa")

            prop_vals <- coef(tot)

            # Guarda los resultados en la lista
            resumen_list[[paste0(nivel_var, "_", niveles_group[i], "_vs_", niveles_group[j])]] <-
              tibble(
                var_est = var_est,
                nivel_var_est = nivel_var,
                var_group = var_group,
                grupo_ref = niveles_group[i],
                grupo_comp = niveles_group[j],
                comparacion = paste0(niveles_group[i], " vs ", niveles_group[j]),
                n_ref = prop_vals[i],
                n_comp = prop_vals[j],
                diferencia = est,
                std_error = se,
                df = gl,
                t_value = t_val,
                p_value = p_val,
                significancia = resultado
              )
          }
        }
      }
      # Une todos los resultados parciales en una sola tabla
      resumen_final <- bind_rows(resumen_list)
    }

    # -----------------------------------------------------
    # CASO 8: type_est == "0"
    # Casos que no deben estimarse (se omiten)
    # -----------------------------------------------------
    else if (type_est == "0") {
      resumen_final <- tibble(mensaje = "No existe variable group.var")
    }



    # =====================================================
    # 5. Guardar resultados
    # =====================================================
    addWorksheet(wb, as.character(id_estimacion))
    writeData(wb, as.character(id_estimacion), resumen_final)
  }

  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  message("Resultados guardados en: ", archivo_salida)
  invisible(NULL)
}
