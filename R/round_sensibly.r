#' Round Numbers to an "Appropriate" Number of Digits
#'
#' This function is primarily geared towards use in labelling. By default it will
#' round a numeric vector to the lowest number of digits required to make sure
#' that no two values get rounded to the same value.
#' Alternatively it will round to significant digits, significant relative to
#' the `range` of the input vector.
#'
#' @param .x Numeric vector. Values to be rounded.
#' @param .digits Numeric vector. Number of digits that should be rounded to
#'                relative to the base value decided by `.separation=`.
#' @param .separation Logical scalar. If `TRUE`` `.digits=` is relative to the
#'                    number of digits required to discern **all** values. If
#'                    `FALSE` it's relative to the number of digits required to
#'                    discern the lowest and highest values.
#'
#' @details The usage of `.digits` is not quite consistent between `.separation = TRUE`
#' and `.separation = FALSE`. But it was chosen this way because it corresponds
#' in my experience with the way we think about rounding numbers for labels.
#'
#' Either you want "x significant digits" as in digits that make actually a
#' difference. It makes sense that 1 should be one significant digit and so on.
#'
#' Or you want enough digits so that there are no identical labels. In this case
#' it makes sense that 0 should be that number of digits.
#'
#' @return A rounded numeric vector.
#'
#' @examples
#' x <- c(1.123, 1.134, 2.23)
#'
#' # rounded to just enough digits to be able to separate one and 2
#' round_sensibly(x)
#'
#' # rounded to one more digit
#' round_sensibly(x, 1)
#'
#' # rounded to 1 significant digit of separation.
#' round_sensibly(x, 1, .separation = FALSE)
round_sensibly <- function(
  .x,
  .digits = 0,
  .separation = TRUE
)
{
  ensure_that(.x, is.numeric)
  ensure_that(.digits, is.numeric)
  type_logical_scalar(.separation)

  # if the vector is all NA or there is only one valid value return the input.
  if ( all(is.na(.x)) ||
       range(.x, na.rm = TRUE) %>% diff %>% {. == 0} )
    return(.x)

  # find the power of 10 that separates the values of the input
  sig.level <-
    range(.x, na.rm = TRUE) %>%
    diff %>%
    log(10) %>%
    trunc

  if(isTRUE(.separation))
  {
    # find the minimum number of significant digits that separates all
    # values.
    min.sep <-
      # of all the values in the input...
      .x %>%
      unique %>%
      # ... find the smallest difference...
      sort %>%
      diff %>%
      min %>%
      # ... and use its power of 10 as min.sep
      log(10) %>%
      trunc %>%
      {. - 1}

    sig.level <- min(sig.level, min.sep, na.rm = TRUE)
  }

  round(.x, .digits - sig.level)
}
