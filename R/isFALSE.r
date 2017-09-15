#' Opposite to isTRUE
#'
#' `isFALSE(.x)` is an abbreviation of `identical(FALSE, .x)`, and so is true if
#' and only if x is a length-one logical vector whose only element is `TRUE` and
#' which has no attributes (not even names).
#'
#' @param .x Just about anything (as that just means that `isFALSE` is `FALSE`)
#'
#' @examples
#' isFALSE(FALSE)
#' isFALSE(0)
#' isFALSE(c(FALSE, FALSE))
#' isFALSE(1 == 0)
#'
#' @export
isFALSE <- function(.x) {
  identical(FALSE, .x)
}
