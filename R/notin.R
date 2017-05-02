#' Negation of \%in\% Operator
#'
#' R lacks a negation of the \%in\% operator. While you can negate the whole
#' expression, that solution results in less readable and clear code.
#'
#' @param x Vector. The values to be matched
#' @param table Vector. The values to be matched against.
#'
#' @return A logical vector of same length as `x`. `TRUE` means the respective
#' value was found in `table`.
#'
#' @examples
#' c("a", "ab") %notin% letters
#'
#' @aliases notin
#' @export
`%notin%` <- function(x, table)
{
  match(x, table, nomatch = 0L) < 1L
}
