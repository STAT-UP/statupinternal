#' Negation of \%in\% Operator
#'
#' R lacks a negation of the `%in%`` operator. While you can negate the whole
#' expression, that solution results in less readable and clear code.
#'
#' @examples
#' c("a", "ab") %notin% letters
#'
#' @export
`%notin%` <- function(x, table)
{
  match(x, table, nomatch = 0L) < 1L
}
