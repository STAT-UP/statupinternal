#' Converts Character to Numeric for Numbers Using a Decimal Comma
#'
#' This function turns character variables containing numbers but using a
#' decimal comma, e.g. "1,23" into a numeric.
#'
#' @param .x Character vector. The first "," of each entry is turned into a "."
#'           and `as.numeric()` is applied to the result.
#' @param .group.separator Character value. This character is removed from the
#'                         input before conversion. Useful to remove "." used
#'                         as grouping character (e.g. "1.234,56")
#'
#' @return A numeric vector of same length as `.x`.
#'
#' @examples
#' as_numeric_comma(c("1,234", "4", "1.234", "1,2,3"))
#'
#' # .group.separator:
#' as_numeric_comma(c("1.234,56", "1.234"))
#' as_numeric_comma(c("1.234,56", "1.234"), .group.separator = ".")
#'
#' @import magrittr
#' @export
as_numeric_comma <-
  function(.x, .group.separator = ".")
  {
    ensure_that(.x, is.character)
    ensure_that(.group.separator,
                is.null(.) || (is.character(.) && length(.) == 1))

    if(!is.null(.group.separator))
      .x <- stringr::str_replace(.x, fixed("."), "")

    stringr::str_replace(.x, fixed(","), ".") %>%
      as.numeric
  }
