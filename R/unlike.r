#' Negation of \%like\% Operator
#'
#' R (data.table) lacks a negation of the \%like\% operator. While you can negate the whole
#' expression, that solution results in less readable and clear code.
#'
#' @param .vector Either a character vector or a factor.
#' @param .pattern Gets passed on to `pattern` in `grepl`.

#' @return Logical vector of same length as `.vector`. `TRUE` if the input value
#' matches the pattern (via regular expression matching). `FALSE` otherwise.
#'
#' @examples
#' "ana" %like% c("Anabelle", "banana", "BanAna", "meh")
#'
#' @export
unlike <- function(.vector, .pattern)
{
  !data.table::like(.vector, .pattern)
}

#' @rdname unlike
#' @export
`%unlike%` <- function(.vector, .pattern)
{
  unlike(.vector, .pattern)
}

#' @rdname unlike
#' @export
`%notlike%` <- `%unlike%`
