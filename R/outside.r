#' Negation of \%between\% Operator
#'
#' R (data.table) lacks a negation of the \%between\% operator. While you can negate the whole
#' expression, that solution results in less readable and clear code.
#'
#' @param .x Numeric vector. Input data
#' @param .lower Numeric value or vector with the lower bound(s)
#' @param .upper Numeric value or vector with the upper bound(s)
#' @param .incbounds `TRUE` means inclusive bounds, i.e. `lower <= .x <= .upper`. `FALSE`
#'                   means exclusive bounds. For \%outside\%
#'                   `.incbounds = TRUE` is used.
#'
#' @return Logical vector of same length as `.x`. `TRUE` if the input value is
#' outside the bounds, `FALSE` otherwise.
#'
#' @examples
#' rnorm(10) %outside% c(-0.5, 0.5)
#'
#' @export
outside <- function(.x, .lower, .upper, .incbounds=TRUE)
{
  !data.table::between(.x, .lower, .upper, .incbounds)
}

#' @rdname outside
#' @export
`%outside%` <- function(.x, .bounds)
{
  outside(.x, .bounds[[1L]], .bounds[[2L]], .incbounds = TRUE)
}
