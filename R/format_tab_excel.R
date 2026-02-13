#' Format and (optionally) save a data table into an Excel worksheet
#'
#' Writes a \code{data.frame}/\code{tibble} into a new worksheet of an
#' \code{openxlsx::Workbook}, applies header styling, adds an auto-filter,
#' adjusts column widths, and draws a separating line below each group defined
#' by \code{var_col}. Optionally saves the workbook to disk.
#'
#' @param df A \code{data.frame} or \code{tibble}. The table to be written.
#' @param wb An \code{openxlsx::Workbook} object. The target workbook.
#'   Defaults to \code{openxlsx::createWorkbook()}.
#' @param sheet A single string specifying the name of the worksheet to create.
#' @param color_header A single string (hex color) defining the background color
#'   of the header row. Default is \code{"#478ec5"}.
#' @param col_width_auto Logical. If \code{TRUE} set auto width to all columns
#' in the table
#' @param mark_row Logical. If \code{TRUE} marks the bottom border of the row based
#'  on the value of var_col
#' @param var_col A single string giving the name of the column used to define
#'   grouping boundaries for the bottom border separator. Must exist in \code{df}.
#'   Default is \code{"variable"}.
#' @param sep_style A single string indicating the border style for the group
#'   separator (e.g., \code{"thick"}, \code{"thin"}, \code{"medium"}).
#'   Default is \code{"thick"}.
#' @param save Logical; if \code{TRUE}, saves the workbook to \code{path}.
#'   If \code{FALSE}, returns the \code{Workbook} object. Default is \code{FALSE}.
#' @param path A single string giving the path to the output \code{.xlsx} file.
#'   Required if \code{save = TRUE}.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Creates a worksheet named \code{sheet} and writes \code{df} to it (without Excel filters).
#'   \item Applies a formatted header style with background color, bold text, centered alignment,
#'         and a bottom border.
#'   \item Adds an auto-filter to the first row and automatically adjusts column widths.
#'   \item Draws a bottom border separator after each contiguous group of rows in \code{var_col}.
#'         Group boundaries are detected using \code{dplyr::lead()} on \code{df[[var_col]]}.
#' }
#'
#' @return
#' If \code{save = FALSE}, returns the updated \code{openxlsx::Workbook} object.
#' If \code{save = TRUE}, writes the workbook to the specified \code{path}
#' (with \code{overwrite = TRUE}) and returns \code{invisible(NULL)}.
#'
#' @section Recommendation:
#' When working with multiple tables, it is more efficient to create the
#' workbook once, call this function with \code{save = FALSE} for each table,
#' and finally save the workbook using \code{openxlsx::saveWorkbook()}.
#' If used within \code{purrr::reduce()}, remember that setting
#' \code{save = TRUE} will prevent the function from returning the workbook.
#'
#' @examples
#' \dontrun{
#' library(openxlsx)
#' library(purrr)
#'
#' # Example 1: Single table
#' wb <- openxlsx::createWorkbook()
#' wb <- format_tab_excel(
#'   df = iris,
#'   wb = wb,
#'   sheet = "Iris",
#'   var_col = "Species",
#'   save = FALSE
#' )
#' openxlsx::saveWorkbook(wb, "iris.xlsx", overwrite = TRUE)
#'
#' # Example 2: Iterate over a list of tables (save at the end)
#' tabs <- list(t1 = head(mtcars), t2 = head(iris))
#' wb2 <- openxlsx::createWorkbook()
#' iwalk(
#'   tabs,
#'   ~ format_tab_excel(.x, wb = wb2, sheet = .y, var_col = names(.x)[1], save = FALSE)
#' )
#' openxlsx::saveWorkbook(wb2, "multi.xlsx", overwrite = TRUE)
#'
#' # Example 3: Using purrr::reduce (with save = FALSE)
#' wb3 <- reduce(
#'   seq_along(tabs),
#'   \(w, i) format_tab_excel(
#'     df = tabs[[i]],
#'     wb = w,
#'     sheet = paste0("Sheet", i),
#'     var_col = names(tabs[[i]])[1],
#'     save = FALSE
#'   ),
#'   .init = openxlsx::createWorkbook()
#' )
#' openxlsx::saveWorkbook(wb3, "multi_reduce.xlsx", overwrite = TRUE)
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle
#' @importFrom openxlsx addFilter setColWidths saveWorkbook
#' @importFrom dplyr lead
#' @importFrom utils tail
#' @seealso \code{\link[openxlsx]{createWorkbook}},
#'   \code{\link[openxlsx]{addWorksheet}},
#'   \code{\link[openxlsx]{addStyle}},
#'   \code{\link[openxlsx]{saveWorkbook}}
#' @export

format_tab_excel <- function(
  df,
  wb = openxlsx::createWorkbook(),
  sheet,
  color_header = "#478ec5",
  col_width_auto = TRUE,
  mark_row = TRUE,
  var_col = "variable",
  sep_style = "thick",
  save = FALSE,
  path
) {
  # Añadir pestaña y escribr datos
  addWorksheet(wb, sheet)
  writeData(wb, sheet, x = df, withFilter = FALSE)
  n_cols <- ncol(df)

  # Generar y añadir estilo header
  header_style <- createStyle(
    fgFill = color_header,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "bottom",
    borderStyle = "thick"
  )
  addStyle(
    wb,
    sheet,
    header_style,
    rows = 1,
    cols = 1:n_cols,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Añadir filtros y setear anchos
  addFilter(wb, sheet, rows = 1, cols = 1:n_cols)
  if (col_width_auto) {
    setColWidths(wb, sheet, cols = 1:n_cols, widths = "auto")
  }

  if (mark_row) {
    # Añadir borde inferior grueso por cada "variable"
    ends <- which(
      df[[var_col]] !=
        dplyr::lead(df[[var_col]], default = tail(df[[var_col]], 1))
    )

    if (length(ends)) {
      group_border <- createStyle(border = "bottom", borderStyle = sep_style)

      addStyle(
        wb,
        sheet,
        style = group_border,
        rows = ends + 1,
        cols = 1:n_cols,
        gridExpand = TRUE,
        stack = TRUE # +1 porque los datos comienzan en la fila 2 (fila 1 = encabezado)
      )
    }
  }

  # Guardar si asi se solicita
  if (save) {
    saveWorkbook(wb, path, overwrite = TRUE)
  } else {
    return(wb)
  }
}
