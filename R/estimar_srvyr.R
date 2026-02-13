#' Estimate totals, means, and proportions with \code{srvyr} objects
#'
#' Helper function to compute estimates from a \code{srvyr::tbl_svy} object using
#' tidy evaluation. Supports totals, means, proportions (and combinations),
#' optionally by domains (grouping variables) and/or for a defined subpopulation.
#'
#' @description
#' Depending on the value of \code{type.est}, the function computes:
#' \itemize{
#'   \item \code{"total"}: total (and its standard error) of \code{est.var}.
#'   \item \code{"mean"}: mean (and its standard error) of \code{est.var}.
#'   \item \code{"prop"}: proportion (and its standard error) for each level of \code{est.var}
#'         (for categorical or dichotomous variables).
#'   \item \code{"total.mean"}: both total and mean for \code{est.var}.
#'   \item \code{"total.prop"}: both total and proportion for each level of \code{est.var}.
#' }
#'
#' @param svy.obj A \code{srvyr::tbl_svy} object.
#' @param type.est Character string indicating the type of estimate to compute.
#'   Must be one of \code{c("total", "mean", "total.mean", "prop", "total.prop")}.
#' @param est.var Variable to estimate (tidy-evaluation style). For \code{"prop"} and
#'   \code{"total.prop"}, this should be a categorical (or dichotomous) variable.
#' @param subpop Logical expression (tidy-evaluation style) defining the
#'   subpopulation. Defaults to \code{TRUE} (entire sample).
#' @param group.vars Grouping variables (tidy-select syntax), e.g.
#'   \code{c(var1, var2)}. Defaults to \code{NULL} (no domains).
#' @param use.labels Logical. If \code{TRUE}, converts grouping variables (and
#'   \code{est.var} for proportion estimates) to labels using
#'   \code{sjlabelled::to_label()}.
#' @param est.var.label A string with the label of the estimation variable.
#' @param na.rm.est.var Logical. If \code{TRUE}, removes NA values from estimation variable
#' @param na.rm.group.vars Lofical. If \code{TRUE}, removas NA values from grouping variables
#' @param format Logical. If \code{TRUE}, applies final formatting to the output
#'   table via a helper function \code{format_tab()}.
#' @param format.args Optional named list of arguments passed to
#'   \code{format_tab()} through \code{rlang::call2()} (e.g.
#'   \code{list(digits = 1)}). Ignored if \code{format = FALSE}.
#' @param ... Additional arguments passed to the underlying \pkg{srvyr} functions,
#'   typically \code{survey_total()} or \code{survey_mean()}
#'   (e.g., \code{vartype = "se"}).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters the subpopulation (\code{srvyr::filter({{ subpop }})}).
#'   \item Optionally converts grouping variables and, for proportions,
#'         \code{est.var} to labels.
#'   \item Groups by \code{group.vars}, if provided.
#'   \item Computes the requested estimate(s) using \pkg{srvyr}.
#'   \item Adds a \code{variable} column with the name of \code{est.var}
#'         and reorders columns to the front.
#'   \item Optionally formats the output table with \code{format_tab()}.
#' }
#'
#' @return
#' A \code{tibble} with:
#' \itemize{
#'   \item \code{variable}: name of \code{est.var}.
#'   \item Domain columns (if \code{group.vars} not \code{NULL}).
#'   \item For \code{"total"}: \code{total}, \code{total_se}.
#'   \item For \code{"mean"}: \code{media}, \code{media_se}.
#'   \item For \code{"prop"}: \code{val} (level of \code{est.var}),
#'         \code{prop}, \code{prop_se}.
#'   \item For \code{"total.mean"}: \code{total}, \code{total_se},
#'         \code{media}, \code{media_se}.
#'   \item For \code{"total.prop"}: \code{val}, \code{total}, \code{total_se},
#'         \code{prop}, \code{prop_se}.
#' }
#'
#' @examples
#' \dontrun{
#'
#' pmrural_srvy <- list(
#'   cut = pmrural %>% fast_survey_design(weights = "fe_cut", pop = informante_kish == 1),
#'   ch = pmrural %>% fast_survey_design(weights = "fe_ch")
#' )
#'
#'
#' # 1) Mean of t_tcnr by age group
#' pmrural_srvy$cut %>%
#'   estimar_srvyr(
#'     type.est = "mean",
#'     est.var = t_tcnr,
#'     group.vars = tr_edad
#'   )
#'
#' # 2) Mean of t_tcnr among caregivers only
#' pmrural_srvy$cut %>%
#'   estimar_srvyr(
#'     type.est = "mean",
#'     est.var = t_tcnr,
#'     group.vars = tr_edad,
#'     subpop = cui_psdf_o_resp_nna == 1
#'   )
#'
#' # 3) Mean of t_tcnr by caregiver status and age group
#' pmrural_srvy$cut %>%
#'   estimar_srvyr(
#'     type.est = "mean",
#'     est.var = t_tcnr,
#'     group.vars = c(cui_psdf_o_resp_nna, tr_edad)
#'   )
#'
#' # 4) Totals and proportions of rph5 among caregivers
#' pmrural_srvy$ch %>%
#'   estimar_srvyr(
#'     type.est = "total.prop",
#'     est.var = rph5,
#'     subpop = cui_psdf_o_resp_nna == 1
#'   )
#'
#' # 5) Totals and proportions of rph5 by caregiver status
#' pmrural_srvy$ch %>%
#'   estimar_srvyr(
#'     type.est = "total.prop",
#'     est.var = rph5,
#'     group.vars = cui_psdf_o_resp_nna
#'   )
#' }
#'
#' @seealso \code{\link[srvyr]{survey_mean}}, \code{\link[srvyr]{survey_total}},
#'   \code{format_tab()}.
#'
#' @importFrom srvyr filter mutate summarise group_by survey_total survey_mean survey_prop
#' @importFrom dplyr rename relocate across everything
#' @importFrom rlang ensym as_label eval_tidy call2
#' @importFrom sjlabelled to_label to_factor
#' @export

estimar_srvyr <- function(
    svy.obj,
    type.est = c("total", "mean", "total.mean", "prop", "total.prop"),
    est.var,
    subpop = TRUE,
    group.vars = NULL,
    use.labels = TRUE,
    est.var.label = NULL,
    na.rm.est.var = TRUE,
    na.rm.group.vars = TRUE,
    format = FALSE,
    format.args = NULL,
    ...) {
  # Match de argumentos
  type.est <- match.arg(type.est)

  # Si se usan etiquetas, convierte las variables a factor usando sus etiquetas
  if (use.labels) {
    svy.obj <- svy.obj %>%
      srvyr::mutate(across({{ group.vars }}, ~ sjlabelled::to_label(.)))

    if (type.est %in% c("prop", "total", "total.prop")) {
      svy.obj <- svy.obj %>%
        srvyr::mutate({{ est.var }} := sjlabelled::to_label({{ est.var }}))
    }
  }

  # Si no se usan etiquetas, las variables se convierten a factor manteniendo los valores
  if (!use.labels) {
    svy.obj <- svy.obj %>%
      srvyr::mutate(across({{ group.vars }}, ~ sjlabelled::to_factor(.)))

    if (type.est %in% c("prop", "total", "total.prop")) {
      svy.obj <- svy.obj %>%
        srvyr::mutate({{ est.var }} := sjlabelled::to_factor({{ est.var }}))
    }
  }

  # Filtrar por subpoblación
  svy.obj <- svy.obj %>% srvyr::filter({{ subpop }})

  # Filtrar NA's
  if (na.rm.est.var) svy.obj <- svy.obj %>% srvyr::filter(!is.na({{ est.var }})) # De la variable estimación
  if (na.rm.group.vars) svy.obj <- svy.obj %>% srvyr::filter(across({{ group.vars }}, \(x) !is.na(x))) # De las variables agrupadas

  # Generar estimaciones unicas
  if (type.est == "total") {
    tab <- svy.obj %>%
      srvyr::group_by(across({{ group.vars }}), {{ est.var }}, .drop = FALSE) %>%
      srvyr::summarise(total = survey_total(...)) %>%
      srvyr::rename(val = {{ est.var }})
  } else if (type.est == "mean") {
    tab <- svy.obj %>%
      srvyr::group_by(across({{ group.vars }}), .drop = FALSE) %>%
      srvyr::summarise(media = survey_mean({{ est.var }}, ...))
  } else if (type.est == "prop") {
    tab <- svy.obj %>%
      srvyr::group_by(across({{ group.vars }}), {{ est.var }}, .drop = FALSE) %>%
      srvyr::summarise(prop = survey_prop(...)) %>%
      srvyr::rename(val = {{ est.var }})
  }

  # Generar estimaciones dobles
  if (type.est == "total.mean") {
    tab <- svy.obj %>%
      srvyr::group_by(across({{ group.vars }}), .drop = FALSE) %>%
      srvyr::summarise(
        total = survey_total({{ est.var }}, ...),
        media = survey_mean({{ est.var }}, ...)
      )
  } else if (type.est == "total.prop") {
    tab <- svy.obj %>%
      srvyr::group_by(across({{ group.vars }}), {{ est.var }}, .drop = FALSE) %>%
      srvyr::summarise(
        total = survey_total(...),
        prop = survey_prop(...)
      ) %>%
      srvyr::rename(val = {{ est.var }})
  }

  # Ordenar tabla
  tab <- tab %>%
    dplyr::mutate(variable = rlang::as_label(ensym(est.var))) %>%
    dplyr::relocate(
      variable,
      everything()
    ) %>%
    dplyr::ungroup()

  # Orden de tabla cuando se usan etiquetas
  if (use.labels) {
    # Incluir columna con la etiqueta sustantiva (si no se especifica, se usa la de la bbdd)
    if (is.null(est.var.label)) est.var.label <- sjlabelled::get_label(svy.obj %>% pull({{ est.var }})) #! Si no hay etiqueta en la bbdd no se crea label y luego sale error!

    tab <- tab %>%
      dplyr::mutate(label = est.var.label) %>%
      dplyr::relocate(
        label,
        .after = variable
      )

    # Filtrar fila de valor perdido (como ya se recodificó a NA, esta fila siempre tendrá prevalencia 0)
    if (type.est %in% c("prop", "total", "total.prop")) {
      tab <- tab %>% filter(!val %in% c("Valor perdido", "Valor por defecto si ninguna condici\u00f3n se cumple"))
    }
  }

  # Formatear tabla
  if (format) {
    if (!is.null(format.args)) {
      tab <- eval_tidy(rlang::call2("format_tab", tab = tab, !!!format.args))
    } else {
      tab <- tab %>% format_tab()
    }
  }

  # Devolver
  return(tab)
}
