#' Turns a Probability into a Percentage
#'
#' The function turns a probability or share (e.g. 0.49, 2.102) into a percentage
#' (49%, 210.2%).
#'
#' @param .x Numeric vector. Input data
#' @param .digits Integer value. To how many digits should the result be rounded.
#'
#' @return Vector with rounded percentages. The names of the vector contain the
#' values + "%" for use in graphical output.
#'
#' @examples
#' percentify(1:10 / 10)
#' percentify(rnorm(10), 2)
#'
#' @import magrittr
#' @export
percentify <-
  function(.x, .digits = 0)
  {
    round(.x * 100, .digits) %>%
      stats::setNames(., stringr::str_c(., "%"))
  }
