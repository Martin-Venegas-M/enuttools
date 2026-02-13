#' Convert Decimal Time to HH:MM or Minutes Format
#'
#' This function converts time expressed in decimal hours (e.g., 1.5 = 1 hour 30 minutes)
#' into a more readable format (HH:MM), total minutes, or a detailed data frame.
#' It also handles negative times and user-defined missing codes.
#'
#' @param decimal_time Numeric vector. Time values expressed in decimal hours.
#' @param missing.code Numeric scalar. Value used to identify missing cases (default: \code{-777}).
#' @param output Character string specifying the type of output to return.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"hhmm"} — returns a character vector in "HH:MM" format.
#'     \item \code{"mm"} — returns a character vector with total minutes.
#'     \item \code{"df"} — returns a tibble with all intermediate variables.
#'   }
#'
#' @details
#' The function converts each decimal time value into hours and minutes,
#' rounding minutes to the nearest integer. If the rounded minutes equal 60,
#' the hour value is incremented by one and minutes are reset to zero.
#' Negative times are preserved with a leading minus sign.
#'
#' Missing codes are replaced with the value of \code{missing.code}
#' in the returned output.
#'
#' @return Depending on the value of \code{output}:
#'   \itemize{
#'     \item A character vector of formatted times ("HH:MM") if \code{output = "hhmm"}.
#'     \item A character vector of total minutes if \code{output = "mm"}.
#'     \item A tibble with all intermediate and final variables if \code{output = "df"}.
#'   }
#'
#' @examples
#' decimal_to_hhmm(c(1.5, 2.75, -3.25))
#' decimal_to_hhmm(c(1.5, -2.5, -777), output = "mm")
#' decimal_to_hhmm(c(1.5, 2.25), output = "df")
#'
#' @importFrom dplyr mutate across if_else
#' @importFrom tibble tibble
#' @export

decimal_to_hhmm <- function(
  decimal_time,
  missing.code = -777,
  output = c("hhmm", "mm", "df")
) {
  # Match args
  output <- match.arg(output)

  # Crear df con tiempos
  df <- tibble(
    time = if_else(decimal_time == missing.code, NA, decimal_time),
    hrs = floor(abs(time)),
    mins = round((abs(time) - hrs) * 60)
  )

  # Aplicar transformaciones
  df <- df %>%
    mutate(
      # Ajuste para minutos redondeados a 60
      hrs = if_else(mins == 60, hrs + 1, hrs),
      mins = if_else(mins == 60, 0, mins),
      # Guardar signo
      sign_t = if_else(time < 0, "-", ""),
      # Crear variables hhmm y mm absolutas
      hhmm_abs = sprintf("%02d:%02d", hrs, mins),
      mm_abs = sprintf("%02d", (hrs * 60) + mins),
      # Crear versiones con signo si coresponde
      hhmm = paste0(sign_t, hhmm_abs),
      mm = paste0(sign_t, mm_abs),
      # Tratar missing codes
      across(
        c(hhmm, hhmm_abs, mm, mm_abs),
        ~ if_else(decimal_time == missing.code, as.character(missing.code), .)
      )
    )

  # Return!
  if (output == "hhmm") {
    return(df$hhmm)
  } else if (output == "mm") {
    return(df$mm)
  } else if (output == "df") {
    return(df)
  }
}
