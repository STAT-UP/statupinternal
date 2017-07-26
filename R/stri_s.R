#' Export of stringi string concatenate
#'
#' @param e1 Character vector. LHS
#' @param e2 Chracter vector. RHS
#'
#' The operator concatenates the LHS and RHS just as if you'd typed
#' `stri_c(e1, e2)` or `str_c(e1, e2)` respectively.
#'
#' @examples
#' letters %s% LETTERS
#'
#' @export
`%s%` <- stringi::`%s+%`
