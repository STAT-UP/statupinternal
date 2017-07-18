#' Get Brightness of a Color
#'
#' This function tries to return how "bright" (i.e. perceived luminance) a given
#' color is based on psychological evaluations at the dawn of the color TV era.
#'
#' This takes into account that the human eye is most sensitive to green,
#' followed by red with blue colors appearing darker.
#'
#' Everything is stolen from stackoverflow so don't sue me.
#'
#' @param .colors Vector or list with colors.This is passed to `col2rgb` and
#'                so accepts color names ("black"), hexadecimal rgb strings
#'                ("#rrggbb" or "#rrggbbaa") or positive integers (`i` meaning
#'                `palette()[i]`).
#'
#' @return A vector of brightness values between 0 and 1. 0 meaning black and
#' 1 white.
#'
#' @export
get_brightness <-
  function(.color) {
    colSums(grDevices::col2rgb(.color) / 255 * c(0.299, 0.587, 0.114))
  }
