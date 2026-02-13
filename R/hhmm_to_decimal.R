#' Convert HH:MM time format to decimal hours
#'
#' This function converts time values in "HH:MM" format into decimal hour format.
#' It also handles missing or special codes (e.g., \code{-777}) to preserve data integrity
#' during numeric conversion.
#'
#' @param hhmm_time A vector of times in "HH:MM" format. Can be numeric, character, or factor.
#' @param missing.code A numeric value used to represent missing or invalid data. Defaults to \code{-777}.
#'
#' @return A numeric vector containing time values expressed in decimal hours.
#' Missing values (\code{NA}) or entries equal to \code{missing.code} are returned as such.
#'
#' @details
#' The function splits each time string at the colon (\code{:}) to separate hours and minutes,
#' converts them to numeric values, and then computes decimal hours as:
#' \deqn{decimal = hours + (minutes / 60)}
#' If both components are \code{NA}, the result is \code{NA}. If any component equals the
#' specified \code{missing.code}, the output is also set to \code{missing.code}.
#'
#' @examples
#' hhmm_to_decimal("02:30") # Returns 2.5
#' hhmm_to_decimal(c("01:45", "03:15"))
#' hhmm_to_decimal(c("NA", "12:00"))
#' hhmm_to_decimal("99:99", missing.code = -999)
#'
#' @importFrom stringr str_split_fixed
#' @importFrom dplyr case_when
#'
#' @export

hhmm_to_decimal <- function(hhmm_time, missing.code = -777) {
  # Manejar NA y transformar a carácter
  hhmm_time <- as.character(hhmm_time)

  # Separar horas y minutos
  parts <- hhmm_time %>%
    stringr::str_split_fixed(pattern = ":", n = 2)
  hrs <- as.numeric(parts[, 1])
  mins <- as.numeric(parts[, 2])

  # Calcular hora decimal con condiciones
  result <- dplyr::case_when(
    is.na(hrs) & is.na(mins) ~ NA_real_,
    hrs == missing.code | mins == missing.code ~ missing.code,
    TRUE ~ hrs + (mins / 60)
  )

  return(result)
}
