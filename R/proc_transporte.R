#' Process transportation variables in ENUT data
#'
#' This function creates and renames transportation-related variables according to the
#' ENUT questionnaire structure, handling both participation and time-record variables.
#' It allows specifying a custom suffix (e.g., \code{_fds}, \code{_ds}, or others) depending on
#' the dataset version being processed.
#'
#' @param data A data frame containing the transportation module variables.
#' @param transport_sets A list of transportation specifications. Each element must include:
#' \describe{
#'   \item{preg_t}{The name of the time variable (e.g., \code{"tc12_t_fds"}).}
#'   \item{preg_p_raiz}{The name of the base participation variable (e.g., \code{"tc9_p_fds"}).}
#'   \item{preg_transp}{The name of the transportation mode variable (e.g., \code{"tc10_m_fds"}).}
#'   \item{prefijo_n}{The prefix of the variables identifying the transported persons (e.g., \code{"tc11"}).}
#' }
#' @param cod_no_p Integer. The code used to represent "not applicable" in the transportation question.
#' Default is \code{10L}.
#' @param sufijo Character. The suffix used in variable names (default is \code{"_ds"}).
#'
#' @return A data frame with transportation variables created and renamed according to the ENUT structure.
#'
#' @examples
#' \dontrun{
#' transport_sets <- list(
#'   list(
#'     preg_t = "tc12_t_fds",
#'     preg_p_raiz = "tc9_p_fds",
#'     preg_transp = "tc10_m_fds",
#'     prefijo_n = "tc11"
#'   )
#' )
#'
#' proc_transporte(enut, transport_sets, sufijo = "_fds")
#' }
#'
#' @export
procesar_transporte <- function(data, transport_sets, cod_no_p = 10L, sufijo = "_ds") {
  purrr::reduce(
    transport_sets,
    .init = data,
    .f = function(df, s) {
      df %>%
        crear_p_transp(
          preg_t      = s$preg_t,
          preg_p_raiz = s$preg_p_raiz,
          preg_transp = s$preg_transp,
          cod_no_p    = cod_no_p,
          sufijo      = sufijo
        ) %>%
        rename_t_per(
          prefijo_n = s$prefijo_n,
          preg_t    = s$preg_t,
          sufijo    = sufijo
        )
    }
  )
}
