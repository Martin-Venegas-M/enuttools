#' Wrapper for First- and Second-Level Validation Checks
#'
#' Provides a simplified interface to review cases that triggered either
#' a first-level (valprimer_check()) or a second-level
#' (valseg_check()) validation. This wrapper centralizes the
#' common arguments and allows the user to specify only the case-specific
#' parameters (var_to_check, error, or regla_to_check).
#' For now, this is an ad-hoc function for the proyect Piloto Mujeres Rurales.
#'
#' @param type A character string indicating the type of validation.
#'   Must be either "primer" (first-level validation) or "seg"
#'   (second-level validation).
#' @param var_to_check A character string with the variable to check.
#'   Required when type = "primer".
#' @param error A character string specifying the error type for first-level
#'   validation. Can be "error1", "error2", or "error3".
#'   Default is "error1".
#' @param regla_to_check A character string with the identifier of the rule
#'   to check. Required when type = "seg".
#' @param level A character string with the level desired for the review of
#' second-level validation. Can be a person or a household.
#' @param var_constant A character vector with constant variables to include
#'   in the review (e.g., interview key, household identifiers).
#'   Default is c("interview__key", "rph1", "rph3", "rph4").
#'
#' @return A data.frame with the validation check results:
#' \itemize{
#'   \item For first-level validation: the variable checked, the enabling
#'         condition, the valid range, characterization variables, and
#'         the identifiers of cases that triggered the error.
#'   \item For second-level validation: the rule identifier, the logical
#'         expression, the description of the rule, characterization variables,
#'         and the identifiers of cases that triggered the rule.
#' }
#'
#' @details
#' This wrapper automatically uses the pmrural dataset, the
#' pmrural_val validation results, and the corresponding metadata
#' (valprimer_insumo or valseg_insumo) defined in the workspace.
#' The user only needs to specify the case-specific arguments.
#'
#' @examples
#' \dontrun{
#' # First-level validation (variable 'c5' with error2)
#' val_check_pmrural(type = "primer", var_to_check = "c5", error = "error2")
#'
#' # Second-level validation (rule 'inc_cut_r11')
#' val_check_pmrural(type = "seg", regla_to_check = "inc_cut_r11")
#'
#' # Customize constant variables
#' val_check_pmrural(
#'   type = "seg",
#'   regla_to_check = "inc_cut_r11",
#'   var_constant = c("interview__key", "rph1")
#' )
#' }
#' @keywords internal
#' @noRd

val_check_pmrural <- function(
    type = c("primer", "seg"), # type of validation
    var_to_check = NULL, # variable to check (for first-level only)
    error = "error1", # error type (for first-level only)
    regla_to_check = NULL, # rule to check (for second-level only)
    level = "person",
    var_constant = c("interview__key", "rph1", "rph4", "rph5") # default constants
) {
  # Matchear argumentos
  type <- match.arg(type)

  # Evaluar si es que existen los insumos requeridos en el ambiente global
  if (!exists("valprimer_insumo", envir = .GlobalEnv)) {
    rlang::abort(
      message = "Object 'valprimer_insumo' not found in the global environment.
                 Please load it with the exact name 'valprimer_insumo'.",
      class = "pmrural_error_missing_valprimer"
    )
  }
  if (!exists("valseg_insumo", envir = .GlobalEnv)) {
    rlang::abort(
      message = "Object 'valseg_insumo' not found in the global environment.
                 Please load it with the exact name 'valseg_insumo'.",
      class = "pmrural_error_missing_valseg"
    )
  }

  # Evaluar si es que existen las columnas requeridas en valprimer_insumo
  required_cols_valprimer <- c("var_rename", "valid_sustantiva", "rango", "tipo_rango")
  missing_cols_valprimer <- setdiff(required_cols_valprimer, names(valprimer_insumo))
  if (length(missing_cols_valprimer) > 0) {
    rlang::abort(
      message = paste0(
        "The following required columns are missing in 'valprimer_insumo': ",
        paste(missing_cols_valprimer, collapse = ", ")
      ),
      class = "pmrural_error_valprimer_cols"
    )
  }

  # Evaluar si es que existen las columnas requeridas en valseg_insumo
  required_cols_valseg <- c("id_regla", "exp_regla", "tipo_val_short", "cuestionario")
  missing_cols_valseg <- setdiff(required_cols_valseg, names(valseg_insumo))
  if (length(missing_cols_valseg) > 0) {
    rlang::abort(
      message = paste0(
        "The following required columns are missing in 'valseg_insumo': ",
        paste(missing_cols_valseg, collapse = ", ")
      ),
      class = "pmrural_error_valseg_cols"
    )
  }

  # Aplicar valprimer_check
  if (type == "primer") {
    stopifnot(!is.null(var_to_check))

    valprimer_check(
      data = pmrural,
      data_val = pmrural_val,
      vars = valprimer_insumo$var_rename,
      exps = valprimer_insumo$valid_sustantiva,
      vars_exp = valprimer_insumo$vars_valid,
      ranges = valprimer_insumo$rango,
      error = error,
      var_constant = var_constant,
      var_to_check = var_to_check,
      id = "id_per"
    )
    # Aplicar valseg_check
  } else if (type == "seg") {
    stopifnot(!is.null(regla_to_check))

    valseg_check(
      data = pmrural,
      data_val = pmrural_val,
      id_reglas = valseg_insumo$id_regla,
      exp_regla = valseg_insumo$exp_regla,
      vars_regla = valseg_insumo$vars_regla,
      desc_regla = valseg_insumo$desc_regla,
      var_constant = var_constant,
      regla_to_check = regla_to_check,
      id = "id_per",
      level = level,
      id_hog = "interview__key"
    )
  }
}

#' Apply first- and second-level validation rules for the PMRural project
#'
#' This function runs a complete validation pipeline for the PMRural dataset.
#' It applies both first-level validations (range and flow checks) and
#' second-level validations (logical consistency checks). Additionally,
#' auxiliary variables required for the second-level checks are automatically
#' created within the process.
#'
#' The function is ad hoc to the PMRural project, since it combines general
#' validation functions (valprimer(), valseg(), val_monitor())
#' with project-specific inputs and auxiliary variable creation.
#'
#' @param df A data frame containing the raw PMRural survey data.
#' @param monitor Logical. If TRUE (default), the output will include
#' a monitoring summary of the validations. If FALSE, only the validation
#' results are returned.
#' @param nec_just_valprimer Logical value. If TRUE indicates necessity of justify some columns from valpimrer()
#' @param just_valprimer A character vector with the name of the validation columns to justify.
#' For example: c("error1_se3_2", "error1_tc1_n2_ds"). This imputes 0 to the columns.
#' @param nec_just_valseg Logical value. If TRUE indicates necessity of justify some columns from valseg()
#' @param just_valseg A character vector with the name of the validation columns to justify.
#' For example: c("inc_rph_r8", "inc_ch_r71")
#' @param cons_edit A logical value indicating if missing value must be considered in variable ranges
#'
#' @details
#' The validation process includes:
#' \itemize{
#'   \item Applying first-level validations (valprimer()), which
#'   check ranges and skip patterns defined in valprimer_insumo.
#'   \item Creating auxiliary variables required for second-level validations
#'   using varaux_pmrural().
#'   \item Applying second-level validations (valseg()), which check
#'   logical consistency between answers based on valseg_insumo.
#'   \item Combining first- and second-level validations into a single dataset.
#'   \item (Optional) Generating a monitoring summary of validation results
#'   using val_monitor().
#' }
#'
#' @return
#' If monitor = FALSE, a data frame with all first- and second-level
#' validations merged by respondent ID (id_per).
#' If monitor = TRUE, a list with two elements:
#' \enumerate{
#'   \item A data frame with validation results.
#'   \item A data frame with aggregated monitoring summaries.
#' }
#'
#' @examples
#' \dontrun{
#' # Run the full validation pipeline with monitoring
#' results <- val_pmrural(df = pmrural_data, monitor = TRUE)
#'
#' # Extract validation data only
#' val_data <- val_pmrural(df = pmrural_data, monitor = FALSE)
#' }
#'
#' @keywords internal
#' @noRd

val_pmrural <- function(
    df,
    monitor = TRUE,
    nec_just_valprimer = TRUE,
    just_valprimer = default_justify(),
    nec_just_valseg = FALSE,
    just_valseg = NULL,
    cons_edit = FALSE) {
  # Mensaje para avisar que variables se están justificando
  message(length(just_valprimer), " variables are being justified. These are the following: ", paste(just_valprimer, collapse = ", "))
  message(length(just_valseg), " rules are being justified. These are the following: ", paste(just_valseg, collapse = ", "))

  # Evaluar si es que existen los insumos requeridos en el ambiente global
  if (!exists("valprimer_insumo", envir = .GlobalEnv)) {
    rlang::abort(
      message = "Object 'valprimer_insumo' not found in the global environment.
                 Please load it with the exact name 'valprimer_insumo'.",
      class = "pmrural_error_missing_valprimer"
    )
  }
  if (!exists("valseg_insumo", envir = .GlobalEnv)) {
    rlang::abort(
      message = "Object 'valseg_insumo' not found in the global environment.
                 Please load it with the exact name 'valseg_insumo'.",
      class = "pmrural_error_missing_valseg"
    )
  }

  # Evaluar si es que existen las columnas requeridas en valprimer_insumo
  required_cols_valprimer <- c("var_rename", "valid_sustantiva", "rango", "tipo_rango")
  missing_cols_valprimer <- setdiff(required_cols_valprimer, names(valprimer_insumo))
  if (length(missing_cols_valprimer) > 0) {
    rlang::abort(
      message = paste0(
        "The following required columns are missing in 'valprimer_insumo': ",
        paste(missing_cols_valprimer, collapse = ", ")
      ),
      class = "pmrural_error_valprimer_cols"
    )
  }

  # Evaluar si es que existen las columnas requeridas en valseg_insumo
  required_cols_valseg <- c("id_regla", "exp_regla", "tipo_val_short", "cuestionario")
  missing_cols_valseg <- setdiff(required_cols_valseg, names(valseg_insumo))
  if (length(missing_cols_valseg) > 0) {
    rlang::abort(
      message = paste0(
        "The following required columns are missing in 'valseg_insumo': ",
        paste(missing_cols_valseg, collapse = ", ")
      ),
      class = "pmrural_error_valseg_cols"
    )
  }

  # Aplicar validaciones de primer nivel
  pmrural_valprimer <- valprimer(
    datos = df,
    variables = valprimer_insumo$var_rename,
    expresiones = valprimer_insumo$valid_sustantiva,
    rangos = valprimer_insumo$rango,
    tipo_rangos = valprimer_insumo$tipo_rango,
    id = "id_per",
    nec_justificar = nec_just_valprimer,
    justificar = just_valprimer,
    consider_edits = cons_edit
  )

  # Crear variables auxiliares para validaciones de segundo nivel
  pmrural_varaux <- suppressWarnings(varaux_pmrural(df, valprimer_insumo)) # ! TEMPORAL, SUPRIMIR WARNINGS AL CREAR VARAUXS

  # Aplicar validaciones de segundo nivel
  pmrural_valseg <- valseg(
    data = pmrural_varaux,
    name = valseg_insumo$id_regla,
    expr = valseg_insumo$exp_regla,
    id = "id_per",
    nec_justificar = nec_just_valseg,
    justificar = just_valseg
  )

  # Juntar validaciones de primer y segundo nivel
  val_df <- dplyr::inner_join(pmrural_valprimer, pmrural_valseg, by = "id_per")

  # Generar output de monitoreo de validaciones
  if (monitor == TRUE) {
    vals_sum <- val_monitor(
      data = val_df, # IMPORTANTE: Resultado de las validaciones
      vars = valprimer_insumo$var_rename,
      mod = valprimer_insumo$modulo,
      qst_valprimer = valprimer_insumo$seccion,
      id_regla = valseg_insumo$id_regla,
      tipo_val = valseg_insumo$tipo_val_short,
      qst_valseg = valseg_insumo$cuestionario
    )

    # Guardar en lista las validaciones y su monitoreo
    pmrural_val_list <- list(val_df, vals_sum)

    return(pmrural_val_list)
  } else {
    return(val_df)
  }
}

#' Default justify codes for val_pmrural()
#'
#' This helper function creates the default set of variable names
#' used in the just_valprimer argument of val_pmrural().
#' It combines a base error code with all combinations of
#' task categories (tc) and levels (n) to generate
#' standardized variable names.
#'
#' @return A character vector of variable names.
#' @keywords internal
#' @noRd

default_justify <- function() {
  base <- c(
    "error1_se3_2",
    "error3_cd3", "error3_cd4",
    paste0("error2_viv5_", c(1:6, 85)),
    "error2_con3_1",
    "error2_c5_2"
  ) #* <- AQUI EXPLICITAR LAS VARIABLES QUE SE JUSTIFICARÁN
  tc_grid <- tidyr::expand_grid(
    tc = c(1:8, 11, 14, 16, 17, 20, 24, 26, 27, 30),
    n  = 2:4
  ) %>%
    dplyr::mutate(name = paste0("error1_tc", tc, "_n", n, "_ds")) %>%
    dplyr::pull(name)

  c(base, tc_grid)
}

#' Create auxiliary variables in Mujeres Rurales pilot survey dataset
#'
#' This function creates auxiliary variables to be used in the second-level validation
#' process. New variables are added to the existing data frame from 2025 pilot survey
#' Mujeres Rurales.
#'
#' pmrural_varaux function is entirely adhoc to Mujeres Rurales data and its specific
#' conditions for second-level validation. It is discouraged to apply it to other datasets.
#'
#' @param data A data frame containing the survey data.
#' @param insumo A data frame containing survey metadata
#'
#' @return A wide data.frame containing the original and new auxiliary variables from
#' the data.
#' @importFrom lubridate '%within%' parse_date_time interval
#' @keywords internal
#' @noRd

varaux_pmrural <- function(data, insumo = valprimer_insumo) {
  #* ######################################## GENERAL ##############################################################
  #* --- Crear variables auxiliares para reglas inc_ch_r113:inc_ch_r115, inc_ch_r120, inc_ch_r124:inc_ch_r125, inc_cut_r1, inc_cut_r7:inc_cut_r9, inc_cut_r37:inc_cut_r39, inc_cut_r46, inc_cut_r48:inc_cut_r50 ---
  vectores_var <- insumo %>%
    dplyr::select(modulo, var_rename) %>%
    dplyr::group_by(modulo) %>%
    dplyr::summarise(vars = list(var_rename), .groups = "drop") %>%
    tibble::deframe()

  ## Crear tests de módulos que sólo debe responder la informante Kish (identifica, para cada módulo de interés, la cantidad de respuestas diferentes a NA)
  solo_kish <- c("so", "ci", "ot", "se", "cd", "to", "pa", "tc", "td", "tv", "cp", "ed", "vs", "bs", "sa")
  for (mod in solo_kish) {
    data <- data %>% dplyr::mutate(
      !!paste0("test_", mod) := rowSums(!is.na(dplyr::across(all_of(vectores_var[[mod]]))))
    )
  }

  #* --- Crear variables auxiliares para reglas inc_ch_r25, inc_cut_r10:inc_cut_r11 ---
  # Crear tres test adicionales (se1, tc_ce, tc_ce_psdf), que son secciones dentro de módulos:
  # test_se1: se utiliza en inc_ch_r125
  se1_list <- vectores_var[["se"]][c(1:11)] # extraer, dentro de la lista vectores_var, en la lista interna 'se', los valores de posición 1 a 11
  # test_tc_ce: se utiliza en inc_cut_r11
  tc_ce_list <- vectores_var[["tc"]][c(1:88)]
  # crear tests
  data <- data %>% dplyr::mutate(
    test_se1   = rowSums(dplyr::across(all_of(se1_list))), # sumar la cantidad de opciones seleccionadas entre se1_1 y se1_11
    test_tc_ce = rowSums(!is.na(across(all_of(tc_ce_list)))),
    test_tc_ce_psdf =  rowSums(!is.na(across(c(tc5_p_ds, tc6_p_ds))))
  )

  #* ######################################## RPH ##############################################################
  data <- data %>%
    #* --- Crear variables auxiliares para reglas desde inc_rph_r7 hasta adv_rph_r15 ---
    dplyr::group_by(folio) %>%
    dplyr::mutate(max_per_vivienda = max(n_linea_p)) %>% # Cantidad de personas en la vivienda
    dplyr::group_by(interview__key) %>%
    dplyr::mutate(
      max_per_hogar = max(n_linea_p), # Cantidad de personas en el hogar
      proveedor_hogar = any(rph3 == 1, na.rm = T), # Indica existencia de proveedor principal en el hogar
      proveedor_hogar_count = sum(rph3 == 1, na.rm = T), # Cantidad de proveedores principales en el hogar
      pareja_hogar_count = sum(rph3 == 2, na.rm = T), # Cantidad de parejas de proveedor principal
      padre_hogar_count = sum(rph3 == 5, na.rm = T), # Cantidad de madres/padres de proveedor principal
      suegro_hogar_count = sum(rph3 == 6, na.rm = T) # Cantidad de suegras/os de proveedor principal
    ) %>%
    dplyr::ungroup() %>%
    #* --- Crear variables auxiliares para reglas desde inc_rph_r16 hasta inc_rph_r20 ---
    varaux_dif_edad(., group = "interview__key", ptco1 = 1, ptco2 = 3, output = "dif_proveedor_hijos") %>% # Diferencia de edad proveedor principal (rph3 = 1) con hija/o (rph3 = 3)
    varaux_dif_edad(., group = "interview__key", ptco1 = 2, ptco2 = 4, output = "dif_pareja_hijos_pareja") %>% # Diferencia de edad pareja (rph3 = 2) con hija/o sólo de pareja (rph3 = 4)
    varaux_dif_edad(., group = "interview__key", ptco1 = 5, ptco2 = 1, output = "dif_padres_proveedor") %>% # Diferencia de edad madre/padre (rph3 = 5) con proveedor principal (rph3 = 1)
    varaux_dif_edad(., group = "interview__key", ptco1 = 6, ptco2 = 2, output = "dif_suegro_pareja") %>% # Diferencia de edad suegra/o (rph3 = 6) con pareja (rph3 = 2)
    varaux_dif_edad(., group = "interview__key", ptco1 = 5, ptco2 = 9, output = "dif_padre_hermano") # Diferencia de edad madre/padre (rph3 = 5) con hermana/o (rph3 = 9)

  #* ######################################## CH ##############################################################
  data <- data %>%
    #* --- Crear variable auxiliar para las varaux regla inc_ch_r15 e inc_ch_r16---
    dplyr::mutate(
      # ! PARCHE: Recientemente arreglé la variable c5 en mujer_rural/5_procesamiento#1, quedando en dos columnas (c5_1 y c5_2) en vez de una.
      # ! Esto implica que cualquier reporte de un tercer (o más) administrador del hogar está descartada desde la inegración.
      # ! Por ende, las reglas r15 y r16 quedan sin efecto. Sin embargo, para no afectar el flujo eliminandolas, voy a generar la variable c5.
      # ! Teoricamente, no deberían existir activaciones de estas dos reglas.
      c5 = case_when(
        c5_1 == 1 ~ 1,
        c5_2 == 1 ~ 2,
        TRUE ~ 0
      )
    ) %>%
    dplyr::group_by(interview__key) %>%
    #* --- Crear variables auxiliares para reglas inc_ch_r4 hasta inc_ch_r6 ---
    dplyr::mutate(
      conviviente_sa_hogar_count = sum(c1 == 2, na.rm = T), # Indica la cantidad de convivientes sin acuerdo de union civil
      casado_hogar_count =         sum(c1 == 1, na.rm = T), # Indica la cantidad de casadas/os
      conviviente_ca_hogar_count = sum(c1 == 3, na.rm = T) # Indica la cantidad de convivientes con acuerdo de union civil
    ) %>%
    #* --- Crear variables auxiliar para regla inc_ch_r9 ---
    dplyr::mutate(
      aux = ifelse(rph3 == 1, c1[rph3 == 1], NA), # Auxiliar: toma estado conyugal de proveedor principal
      aux2 = max(aux, na.rm = T), # Auxiliar: replica valor de aux en todas las filas del hogar
      test_econyugal_proveedor_pareja = ifelse(rph3 == 2, aux2 != c1, NA)
    ) %>% # Si existe pareja de proveedor (rph3 = 2) en el hogar, se compara su estado conyugal (c1) al del proveedor (aux)
    dplyr::select(-aux, -aux2) %>%
    dplyr::group_by(interview__key) %>%
    #* --- Crear variable auxiliar para regla adv_ch_r11 ---
    dplyr::mutate(
      aux = ifelse(rph3 == 5, c1[rph3 == 5], NA), # Auxiliar: extrae estado conyugal de cada madre/padre
      aux2 = max(aux, na.rm = T), # Auxiliar: trae el estado conyugal mayor entre ambos padres para todas las filas del hogar
      aux2 = ifelse(aux2 %in% -Inf, NA, aux), # OJO -- Si todos en el hogar tienen c1 = NA, se crea un -Inf, lo que afecta la creación de aux4 y a la larga termina dando un falso positivo ya que aux2 = -Inf y aux3 = Inf. Por eso se corrige en este ifelse
      aux3 = min(aux, na.rm = T), # Auxiliar: trae el estado conyugal de menor valor entre ambos padres para todas las filas del hogar (nota: en realidad no importa cual es mayor o menor, lo importante es diferenciarlos)
      aux3 = ifelse(aux3 %in% Inf, NA, aux), # OJO -- idem
      aux4 = ifelse(rph3 == 5, aux2 != aux3, NA), # Testea si los estados conyugales son los mismos
      # OJO: Hasta acá las auxiliares no testean realmente si es que hay más de dos padres en el hogar, sin embargo, si es que solo hay un padre en el hogar, aux2 y aux3 serán lo mismo y por ende aux4 será FALSE
      test_econyugal_padres = ifelse(padre_hogar_count == 2 & aux4 == TRUE | (padre_hogar_count == 2 & aux4 == FALSE & !aux2 %in% c(1:3)), TRUE, FALSE) # Inconsistencia si hay dos padres en el hogar y los estados conyugales son distintos o si hay dos padres en el hogar y los estados conyugales son los mismos, pero no son 1, 2 o 3
    ) %>%
    dplyr::select(-aux, -aux2, -aux3, -aux4) %>%
    #* --- Crear variable auxiliar para regla adv_ch_r13 ---
    dplyr::mutate(
      aux = ifelse(rph3 == 6, c1[rph3 == 6], NA), # Auxiliar: extrae estado conyugal de cada suegra/o
      aux2 = max(aux, na.rm = T), # Auxiliar: trae el estado conyugal mayor entre ambos suegros para todas las filas del hogar
      aux2 = ifelse(aux2 %in% -Inf, NA, aux), # OJO -- idem advertencia que test_econyugal_padres
      aux3 = min(aux, na.rm = T), # Auxiliar: trae el estado conyugal de menor valor entre ambos suegros para todas las filas del hogar (nota: en realidad no importa cual es mayor o menor, lo importante es diferenciarlos)
      aux3 = ifelse(aux3 %in% Inf, NA, aux), # OJO -- idem
      aux4 = ifelse(rph3 == 6, aux2 != aux3, NA), # Testea si los estados conyugales son los mismos
      # OJO: Hasta acá las auxiliares no testean realmente si es que hay más de dos padres en el hogar, sin embargo, si es que solo hay un padre en el hogar, aux2 y aux3 serán lo mismo y por ende aux4 será FALSE
      test_econyugal_suegros = ifelse(suegro_hogar_count == 2 & aux4 == TRUE | (suegro_hogar_count == 2 & aux4 == FALSE & !aux2 %in% c(1:3)), TRUE, FALSE) # Inconsistencia si hay dos suegros en el hogar y los estados conyugales son distintos o si hay dos padres en el hogar y los estados conyugales son los mismos, pero no son 1, 2 o 3
    ) %>%
    dplyr::select(-aux, -aux2, -aux3, -aux4) %>%
    #* --- Crear variable auxiliar para regla inc_ch_r15 ---
    dplyr::mutate(
      aux = max(c5, na.rm = T), # Auxilidar: identifica máximo valor de c5 en hogar
      test_max_c5 = ifelse(h1 > 1 & aux == 0, TRUE, FALSE)
    ) %>% # Total de administradores del hogar no puede ser cero
    dplyr::select(-aux) %>%
    #* --- Crear variable auxiliar para regla inc_ch_r16 ---
    dplyr::mutate(
      aux = ifelse(c5 != 0, 1, 0), # Auxiliar: toma valor 1 si es administrador de hogar
      aux2 = sum(aux, na.rm = T), # Auxiliar: suma de administradores del hogar
      test_sum_c5 = ifelse(aux2 > 2 & rph3 == 1, TRUE, FALSE) # TODO se usa rph3 == 1 como parche para que resultados sólo se alojen en una persona por hogar, mejorable
    ) %>% # Total de administradores del hogar no puede ser superior a 2
    dplyr::select(-aux, -aux2) %>%
    dplyr::ungroup() %>%
    #* --- Crear variable auxiliar para reglas desde adv_ch_r121 hasta adv_ch_r123 ---
    dplyr::mutate(
      test_ot1_exposicion = dplyr::select(., ot1a_1, ot1a_2, ot1a_3, ot1a_5, ot1a_7, ot1a_8) %>% rowSums(na.rm = TRUE),
      test_ot1_noexposicion = dplyr::select(., ot1a_4, ot1a_6) %>% rowSums(na.rm = TRUE)
    )

  #* ######################################## CUT ##############################################################
  data <- data %>%
    #* --- Crear variables auxiliares para reglas desde inc_cut_r2 hasta inc_cut_r6 ---
    dplyr::mutate(
      num_cd1_papel = day_to_num(cd1_papel),
      num_cd2_papel = day_to_num(cd2_papel),
      test_diaref = lubridate::ymd(dif_diaref(cd1, hdr_cut_fecha_visita)),
      cd2_ymd = as.Date(parse_date_time(cd2, "dmy")), # pasar cd2 a formato equivalente a test_diaref
      recolec_sm1 = interval(lubridate::ymd("2025-05-30"), lubridate::ymd("2025-06-20")), # crear fechas de recolección submuestra 1
      recolec_sm2 = interval(lubridate::ymd("2025-06-01"), lubridate::ymd("2025-07-05")), # crear fechas de recolección submuestra 2
      test_sm1 = ifelse(!is.na(hdr_cut_fecha_visita), ifelse(!hdr_cut_fecha_visita %within% recolec_sm1 & sm == 1, 1, 0), 0),
      test_sm2 = ifelse(!is.na(hdr_cut_fecha_visita), ifelse(!hdr_cut_fecha_visita %within% recolec_sm2 & sm == 2, 1, 0), 0)
    )

  data <- data %>%
    #* --- Crear variables auxiliares para reglas desde inc_cut_r20 hasta inc_cut_r27 ---
    varaux_identificar_sujetos(data = ., group = "interview__key", keep = "d1, d8, rph4", cond = "(rph4 < 5 | d1 == 1 | d8 == 1)", output = "sujetos_cuidado_4_o_psdf") %>%
    varaux_identificar_sujetos(data = ., group = "interview__key", keep = "d1, d8, rph4", cond = "(rph4 < 15 | d1 == 1 | d8 == 1)", output = "sujetos_cuidado_14_o_psdf") %>%
    varaux_identificar_sujetos(data = ., group = "interview__key", keep = "d1, d8", cond = "d1 == 1 | d8 == 1", output = "sujetos_cuidado_psdf") %>%
    varaux_identificar_sujetos(data = ., group = "interview__key", keep = "e4", cond = "e4 != 1", output = "sujetos_no_estudiando") %>%
    varaux_identificar_sujetos(data = ., group = "interview__key", keep = "o2d_2, o3, o4, o5", cond = "o3 != 1 | o4 != 1 | o5 != 1 | o2d_2 != 1 | o2d_2 != 2", output = "sujetos_no_ocupados") %>%
    varaux_sujetos_cuidado(., c("tc1_n1_ds", "tc1_n2_ds", "tc1_n3_ds", "tc1_n4_ds"), sujetos_cuidado_4_o_psdf, "test_sujetos_tc1") %>%
    varaux_sujetos_cuidado(., c("tc2_n1_ds", "tc2_n2_ds", "tc2_n3_ds", "tc2_n4_ds"), sujetos_cuidado_14_o_psdf, "test_sujetos_tc2") %>%
    varaux_sujetos_cuidado(., c("tc3_n1_ds", "tc3_n2_ds", "tc3_n3_ds", "tc3_n4_ds"), sujetos_cuidado_14_o_psdf, "test_sujetos_tc3") %>%
    varaux_sujetos_cuidado(., c("tc4_n1_ds", "tc4_n2_ds", "tc4_n3_ds", "tc4_n4_ds"), sujetos_cuidado_14_o_psdf, "test_sujetos_tc4") %>%
    varaux_sujetos_cuidado(., c("tc5_n1_ds", "tc5_n2_ds", "tc5_n3_ds", "tc5_n4_ds"), sujetos_cuidado_psdf, "test_sujetos_tc5") %>%
    varaux_sujetos_cuidado(., c("tc6_n1_ds", "tc6_n2_ds", "tc6_n3_ds", "tc6_n4_ds"), sujetos_cuidado_psdf, "test_sujetos_tc6") %>%
    varaux_sujetos_cuidado(., c("tc7_n1_ds", "tc7_n2_ds", "tc7_n3_ds", "tc7_n4_ds"), sujetos_cuidado_14_o_psdf, "test_sujetos_tc7") %>%
    varaux_sujetos_cuidado(., c("tc8_n1_ds", "tc8_n2_ds", "tc8_n3_ds", "tc8_n4_ds"), sujetos_cuidado_4_o_psdf, "test_sujetos_tc8")

  #* --- Crear variables axuliares para reglas desde inc_cut_r12 hasta inc_cut_r19 ---
  # Identificar casos de autorreporte de línea en módulo TC
  # El módulo TC es sólo para reportar cuidados de terceros, por lo que si se identifica el n_linea_kish entre los valores de receptores de cuidados, es un error
  for (i in c(1:8)) { # Se evalúa para cada pregunta entre tc1 a tc8
    cols <- paste0("tc", i, "_n", 1:4, "_ds") # columnas a evaluar: tcX_n1_ds a tcX_n4_ds
    new_var <- paste0("test_tc", i, "_autorreporte") # nombre de columna test

    # crear test: detectar si n_linea_kish fue reportado como sujeto de cuidados en tcX
    data <- data %>% dplyr::mutate(!!new_var := if_any(all_of(cols), ~ n_linea_kish == .x))
  }

  #* --- Crear variables auxiliares para reglas desde inc_cut_r28 hasta inc_cut_r33 ---
  data <- data %>%
    varaux_cuidados_chequeo(., sujetos = "sujetos_no_estudiando", var_tc = "tc11") %>%
    varaux_cuidados_chequeo(., sujetos = "sujetos_no_estudiando", var_tc = "tc14") %>%
    varaux_cuidados_chequeo(., sujetos = "sujetos_no_estudiando", var_tc = "tc16") %>%
    varaux_cuidados_chequeo(., sujetos = "sujetos_no_estudiando", var_tc = "tc17") %>%
    varaux_cuidados_chequeo(., sujetos = "sujetos_no_ocupados", var_tc = "tc30") %>%
    varaux_cuidados_chequeo(., sujetos = "sujetos_no_ocupados", var_tc = "tc33")

  # ! ################################### PARCHE FINAL #######################################################
  data <- data %>% dplyr::mutate_at(vars(starts_with("dif")), ~ (if_else(. == -Inf, NA, .))) # ! ¡¡¡¡¡¡¡¡¡¡MUY IMPORTANTE!!!!!!!!!!!
  # ! ¡OJO!: Para que la creación de las variables auxiliares funcione, es necesario aplciar el argumento de na.rm = T a la función max().
  # ! Sin embargo, cuando no hay nadie que cumpla las condiciones para ser evaluado en el hogar (que tenga valores en las variables para crear aux),
  # ! la función max() retornará un -Inf. Esto es porque se está calculando el máximo de una variable que es NA y se le pide a la funcion que saqué los NA,
  # ! es decir: max(NA, na.rm = T).

  # ! La solución para que no queden estos -Inf en la base será recodificarlas a NA. No es lo más limpio, pero cumple el objetivo.

  return(data)
}

#' Calculate age difference between household members
#'
#' Internal function to compute the age difference between two specific household members
#' (e.g., between parents and children, or grandparents and grandchildren).
#'
#' @param data Data frame with the household survey data.
#' @param group Grouping variable identifying the household (e.g., interview__key for households,
#'   folio for dwellings).
#' @param ptco1 Value of relationship code (rph3) corresponding to the *older* member in the comparison
#'   (e.g., parents, grandparents). If more than one person is in this category, the youngest of them
#'   is selected.
#' @param ptco2 Value of relationship code (rph3) corresponding to the *younger* member in the comparison
#'   (e.g., children, grandchildren).
#' @param output Name of the output variable where the computed age difference will be stored.
#'
#' @details The function helps detect implausible household structures, such as age differences
#' smaller than 12 years between parents and children.
#'
#' @return A data frame with an additional column (output) containing the age difference between
#'   the youngest ptco1 and each ptco2 member in the household.
#' @keywords internal
#' @noRd

varaux_dif_edad <- function(data, group, ptco1, ptco2, output) {
  data %>%
    dplyr::group_by(!!rlang::sym(group)) %>% # toma la bdd 'data' y la agrupa por la variable correspondiente (en encuestas de hogares corresponde al id del hogar)
    dplyr::mutate(
      aux = ifelse(rph3 == ptco1, rph4, NA), # extraer edad de cada fila que corresponda al ptco1
      edad_min = if (all(is.na(aux))) NA else min(aux, na.rm = TRUE), # selecciona la menor edad por hogar de ptco1 (si hay más de un padre/suegro, toma la menor edad; si hay solo uno, toma ese valor). Si nadie en el hogar pertenece a ptco1, queda como NA
      !!rlang::sym(output) := ifelse(rph3 == ptco2, edad_min - rph4, NA)
    ) %>% # ejecutar ifelse que toma edad de ptco2, resta edades min(ptco1) y ptco2, y resultado se guarda en columna 'output'
    dplyr::select(-aux, -edad_min) %>% # eliminar variables auxiliares
    dplyr::ungroup() %>%
    return(data)
}

#' Identify household members matching a condition
#'
#' Internal function to identify household members who satisfy specific conditions
#' (e.g., care, employment, schooling).
#'
#' @param data Data frame with the household survey data.
#' @param group Grouping variable identifying the household (e.g., interview__key, folio).
#' @param keep Variables required for evaluation.
#' @param cond Logical condition expressed as a string; rows satisfying the condition are selected.
#' @param output Name of the output variable. Stores, as a string, the line numbers (n_linea_p)
#'   of the members fulfilling the condition.
#'
#' @return A data frame with a new variable (output) listing household members that match the condition.
#' @keywords internal
#' @noRd

varaux_identificar_sujetos <- function(data, group, keep, cond, output) {
  keep_vars <- strsplit(keep, ",\\s*")[[1]] %>% rlang::syms() # Toma los valores de "keep", y los separa como elementos de una lista, con la coma como delimitador

  df <- data %>% # Crear df auxiliar
    dplyr::select(!!rlang::sym(group), n_linea_p, !!!keep_vars) %>% # Mantener variables 'group', listado 'keep' y 'n_linea_p' en df auxiliar
    dplyr::group_by(!!rlang::sym(group)) %>% # Agrupar por variable de interés dentro del hogar
    dplyr::filter(!!parse_expr(cond)) %>% # Mantener filas que cumplen con 'cond'
    dplyr::summarise(!!rlang::sym(output) := toString(n_linea_p)) %>% # Crear 'output', variable string que indica los números de fila por hogar que cumplen la condición
    dplyr::ungroup()

  data <- dplyr::left_join(data, df, by = group) # Unir df auxiliar a data

  return(data)
}

#' Check consistency of declared care recipients in activities
#'
#' Internal function to check whether individuals declared as care recipients in an activity
#' are indeed persons not studying/working.
#'
#' @param data Data frame with the household survey data.
#' @param sujetos Variable indicating subjects not studying/working
#'   (e.g., sujetos_no_estudiando, sujetos_no_ocupados).
#' @param var_tc Base name of the time-use variable (e.g., "tcX").
#'
#' @return A data frame with an additional column `test_<var_tc>` (0/1), indicating
#'   whether inconsistencies exist.
#' @keywords internal
#' @noRd

varaux_cuidados_chequeo <- function(data, sujetos, var_tc) {
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!rlang::sym(paste0("test_", var_tc)) := {
        # Obtener listado de sujetos no estudiando: obtener la variable character "sujetos_no_estudiando" como vector numérico
        rango <- strsplit(!!rlang::sym(sujetos), ",") %>%
          unlist() %>%
          as.numeric() %>%
          .[!is.na(.)]

        # si algunos de los números de línea (contenidos en 'rango') esta entre los valores de tc_nX_ds, hay un error
        revision <- any(sapply(1:4, function(j) {
          get(paste0(var_tc, "_n", j, "_ds")) %in% rango
        }))

        as.numeric(revision)
      }
    ) %>%
    dplyr::ungroup()

  return(data)
}

#' Validate care subjects against auxiliary variable
#'
#' Internal function to test whether household members declared as care recipients
#' match the group defined in an auxiliary variable.
#'
#' @param data Data frame with the household survey data.
#' @param vec_n Character vector with variable names containing line numbers
#'   of declared care subjects.
#' @param varaux Auxiliary variable containing the list of expected care subjects (string).
#' @param test_col_name Name of the output column where the test results will be stored.
#'
#' @return A data frame with a new boolean column (test_col_name) indicating
#'   whether inconsistencies are present in declared care subjects.
#' @keywords internal
#' @noRd

varaux_sujetos_cuidado <- function(data, vec_n, varaux, test_col_name) {
  # Crear inputs
  varaux_sym <- rlang::ensym(varaux) # Guardar la variable auxilar como simbolo
  test_name <- rlang::sym(test_col_name) # Crear el nombre que tendrá el test
  # Guardar el contenido de la variable auxiliar
  vec_sujetos <- data %>% dplyr::pull(!!varaux_sym)
  # Crear lista para guardar los vectores con los n linea de sujetos de cuidado
  lista_vectores_sujetos <- stringr::str_split(stringr::str_remove_all(vec_sujetos, " "), ",") %>%
    purrr::map(as.numeric)
  # Vector de las variables a evaluar
  vector_vars <- vec_n
  # Crear dinamicamente la expresión de la prueba
  condiciones <- purrr::map(
    vector_vars,
    ~ paste0("(!is.na(", .x, ") & !", .x, " %in% lista_vectores_sujetos)")
  ) %>%
    paste(collapse = " | ")
  # Aplicar la prueba
  results <- data %>% dplyr::mutate(!!test_name := !!parse_expr(condiciones))
  return(results)
}

#' Convert weekday names to numeric values
#'
#' Internal function to convert day names to numeric codes.
#'
#' @param dia_string Character vector of weekday names (e.g., "Lunes", "Martes").
#'
#' @return Numeric vector with weekday values (1 = Monday, …, 7 = Sunday).
#' @keywords internal
#' @noRd

day_to_num <- function(dia_string) {
  nums_dias <- c("Lunes" = 1, "Martes" = 2, "Miercoles" = 3, "Jueves" = 4, "Viernes" = 5, "Sabado" = 6, "Domingo" = 7)
  dia_numeric <- as.numeric(factor(dia_string, levels = names(nums_dias)))
  return(dia_numeric)
}

#' Validate coherence between reference day and interview date
#'
#' Internal function to check whether the reported reference day for activities
#' corresponds to the system-assigned interview day.
#'
#' @param dia Declared reference day (character, e.g., "Lunes").
#' @param fecha_aplicacion Interview application date.
#'
#' @return A Date object with the adjusted day corresponding to the assigned reference day.
#' @keywords internal
#' @noRd

dif_diaref <- function(dia, fecha_aplicacion) {
  fecha_aplicacion <- as.Date(fecha_aplicacion) # Pasar el input de la fecha a formato fecha

  # Crear un vector para tener la referencia de los números de días
  nums_semana_completa <- c("lunes" = 1, "martes" = 2, "mi\u00e9rcoles" = 3, "jueves" = 4, "viernes" = 5, "s\u00e1bado" = 6, "domingo" = 7)
  # Guardar los números del día de aplicación (semana)
  nums_dia_aplicacion <- as.numeric(factor(weekdays(as.Date(fecha_aplicacion)), levels = names(nums_semana_completa)))
  # Crear un vector nombrado de referencia para los días de semana
  nums_dia_ds <- c("Lunes" = 1, "Martes" = 2, "Miercoles" = 3, "Jueves" = 4, "Viernes" = 5, "Sabado" = 6, "Domingo" = 7)
  # Guardar los números del día de referencia
  nums_dia_asignado <- as.numeric(factor(dia, levels = names(nums_dia_ds)))
  # Obtener la diferencia entre dia aplicacion con dia asignado
  dif_aplicacion_asignado <- nums_dia_aplicacion - as.difftime(nums_dia_asignado, units = "days")
  dif_diaref <- ifelse(
    dif_aplicacion_asignado > 0, # Si la diferencia entre el el día asignado y el día de recolección es mayor a 0...
    paste0(as.Date(fecha_aplicacion - as.difftime(dif_aplicacion_asignado, units = "days"))), # Entonces restale a la fecha de aplicación la cantidad de días que existe entre el día de referencia y el día de aplicación y luego pasalo a fecha
    paste0(as.Date(fecha_aplicacion - (7 + (as.difftime(dif_aplicacion_asignado, units = "days"))))) # En caso contrario suma 7 previo a hacer esta resta
  )
  return(dif_diaref)
}

#' Standardize an estimation table (attach group variable and/or metadata)
#'
#' Attach standardized metadata and/or a standardized grouping column to a
#' single estimation table stored in a list of tables.
#'
#' The function expects a global object named analisis_insumo to exist and to
#' contain information about estimation identifiers (id_estimacion) and the
#' name of the grouping variable (group.var). Depending on include, the
#' function will standardize grouped estimations or attach metadata columns,
#' or both.
#'
#' @param list.with.tabs A named list of data frames / tibbles that
#'   contain estimation tables. Each element must be named with the
#'   corresponding estimation identificator contained in analisis_insumo.
#' @param id.est Character. Estimation identificator used to select the table
#'   from list.with.tabs. This value is also assigned to a new column
#'   id_estimacion inside the selected table before joins.
#' @param include Character scalar. Which elements to attach:
#'   - "group.var": attach only the standardized grouping column,
#'   - "metadata": attach only the requested metadata columns, or
#'   - "all": attach both group variable and metadata columns.
#'   Default: c("group.var", "metadata", "all").
#' @param vars.metadata Character vector. Names of metadata columns to attach
#'   from analisis_insumo. These columns will be joined by id_estimacion.
#'   Default: c("titulo", "stata_sort", "subpop").
#'
#' @details
#' - The function reads analisis_insumo from the **global environment**. Make
#'   sure that object exists and contains id_estimacion, group.var, and
#'   any names present in vars.metadata. If the object or required columns
#'   are missing, the function aborts with informative errors:
#'   - pmrural_error_missing_insumo when analisis_insumo is not found, and
#'   - pmrural_error_missing_cols when one or more metadata columns are
#'     missing.
#' - When a grouping variable is present (i.e. group.var identifies a column
#'   name that exists in the estimation table), the function renames that
#'   column to cats_group_var so downstream code can rely on a consistent
#'   grouping column name.
#'
#' @return A tibble (data.frame) corresponding to the selected estimation table
#'   with id_estimacion and the requested additional columns attached and
#'   ordered (id first).
#'
#' @section Side effects:
#' This function uses a global object (analisis_insumo) rather than receiving
#' it as an explicit argument. This design simplifies calls in scripts but
#' requires the global to be present and correct.
#'
#' @seealso add_group_var, add_metadata
#' @importFrom rlang abort
#' @importFrom dplyr mutate left_join relocate select rename any_of
#' @keywords internal
#' @noRd

standardize_est_tab <- function(
    list.with.tabs,
    id.est,
    include = c("group.var", "metadata", "all"),
    vars.metadata = c("titulo", "stata_sort", "subpop")) {
  # Evaluar si existe el insumo en el ambiente global
  if (!exists("analisis_insumo", envir = .GlobalEnv)) {
    rlang::abort(
      message = "Object 'analisis_insumo' not found in the global environment.
                 Please load it with the exact name 'analisis_insumo'.",
      class = "pmrural_error_missing_insumo"
    )
  }

  # Evaluar si el insumo tiene las variables solicitadas
  missing_cols <- setdiff(c("id_estimacion", "group.var", vars.metadata), names(analisis_insumo))
  if (length(missing_cols) > 0) {
    rlang::abort(
      message = paste0(
        "The following required columns are missing in 'analisis_insumo': ",
        paste(missing_cols, collapse = ", ")
      ),
      class = "pmrural_error_missing_cols"
    )
  }

  # Match args
  include <- match.arg(include)

  # Incluir id estimación
  stand_tab <- list.with.tabs[[id.est]] %>%
    mutate(id_estimacion = id.est)

  if (include == "group.var") {
    stand_tab <- stand_tab %>%
      add_group_var()
  } else if (include == "metadata") {
    stand_tab <- stand_tab %>%
      add_metadata(vars.metadata = vars.metadata)
  } else {
    stand_tab <- stand_tab %>%
      add_group_var() %>%
      add_metadata(vars.metadata = vars.metadata)

  }
}

#' Attach and standardize the grouping variable to an estimation table
#'
#' Internal helper that joins the group.var value from analisis_insumo
#' and, if present, renames the corresponding grouping column in tab to
#' cats_group_var. This ensures a consistent column name for grouping
#' categories across estimation tables.
#'
#' @param tab A data.frame / tibble representing a single estimation table that
#'   already contains an id_estimacion column.
#' @return The input table with group_var attached (relocated after
#'   variable), and, if a grouping column exists, renamed to
#'   cats_group_var.
#' @importFrom dplyr left_join relocate select rename
#' @keywords internal
#' @noRd

add_group_var <- function(tab) {
  # Guardar variable group
  var <- analisis_insumo %>%
    select(id_estimacion, group_var = group.var)

  # Pegar group_var
  tab <- tab %>%
    left_join(var, by = "id_estimacion") %>%
    relocate(group_var, .after = variable) %>%
    relocate(id_estimacion, everything())

  # Para estimaciones que si tienen una variable de agrupación, hay que cambiar el nombre de la columna para estandarizar el formato.
  # P.ej si la columna de agrupación es sexo (rph5), entonces hay que cambiarselo para que quede con un nombre general
  if (unique(tab$group_var) != "NULL") {
    name_group_var <- unique(tab$group_var)

    tab <- tab %>%
      rename(cats_group_var = all_of(name_group_var)) # Le dejamos el nombre de cats_group.var
  }

  return(tab)
}

#' Attach requested metadata columns to an estimation table
#'
#' Internal helper that left-joins the selected metadata columns from
#' analisis_insumo into tab by id_estimacion and relocates id_estimacion
#' and the metadata columns to the front of the table.
#'
#' @param tab A data.frame / tibble representing a single estimation table that
#'   already contains an id_estimacion column.
#' @param vars.metadata Character vector with names of metadata columns to
#'   attach (these must exist in analisis_insumo).
#' @return The input table with the requested metadata columns attached and
#'   relocated.
#' @importFrom dplyr left_join relocate select any_of
#' @keywords internal
#' @noRd


add_metadata <- function(tab, vars.metadata) {
  metadata <- analisis_insumo %>%
    select(id_estimacion, any_of(vars.metadata))

  tab <- tab %>%
    left_join(metadata, by = "id_estimacion") %>%
    relocate(id_estimacion, any_of(vars.metadata), everything())

  return(tab)
}
