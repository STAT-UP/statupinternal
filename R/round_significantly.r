#' Round Numbers to Significant Digits
#'
#' This function is primarily geared towards use in labelling. By default it will
#' round a numeric vector to a given number of significant digits. E.g. rounding
#' `0.00412` with `.digits = 1` will result in `0.004`, while `234` will result
#' in `200`.
#'
#' @param .x Numeric vector. Values to be rounded.
#' @param .digits Numeric vector. Number of significant digits you want to keep.
#'
#' @return A rounded numeric vector.
#'
#' @examples
#' x <- c(-0.0002352, 1234)
#'
#' round_significantly(x)
#' round_significantly(x, 2)
#'
#' @import ensurer
#' @import magrittr
#' @export
round_significantly <- function(.x, .digits = 0)
{
  ensure_that(.x, is.numeric(.) || is.logical(.))
  ensure_that(.digits, is.numeric)

  order_of_magnitude <-
    .x %>%
    abs %>%
    log(10) %>%
    trunc %>%
    {. - (. < 0)}


  .x %>%
    {. / 10^order_of_magnitude} %>%
    round(.digits) %>%
    {. * 10^order_of_magnitude}
}
