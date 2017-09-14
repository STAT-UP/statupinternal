#' Import of ggplot NULL-Avoiding OR
#'
#' @param a LHS. The operator returns this value unless it is `NULL`.
#' @param b RHS. If the LHS is `NULL` then this is return
#'
#' @return Either LHS or RHS
`%||%` <- ggplot2:::`%||%`

#' Bar Plot with Numbers at the Ends of the Bars. Options not mentioned here
#' work identical to `geom_col` or `geom_text`.
#'
#' @param mapping -
#' @param data -
#' @param position -
#' @param ... -
#' @param width -
#' @param center_text Logical value. If `TRUE` the text will be centered within
#'                    its respective bar.
#' @param just_mirror Logical value. If `TRUE` then the justification of the
#'                    text will be mirrored at the edge of the bar for the
#'                    largest and smallest values (see `just_mirror_cutoff`
#'                    below). E.g. if the text is outside the bars then for
#'                    the largest values it will be inside the bars.
#'                    This prevents text from being cut off by the
#'                    edges of the plot.
#' @param just_mirror_cutoff Two numeric values between 0 and 1. The default of
#'                           0.9 for the second value
#'                           means that if the text is supposed to appear
#'                           *outside* the bars then for values larger (in
#'                           absolute terms) than 90% of the data range this is
#'                           flipped to appear inside. And vice versa for text
#'                           inside bars and the first value of just_mirror_cutoff.
#'                           This avoids text getting cutoff by the edge of
#'                           the plot or text obscuring the bars for very
#'                           small values. Set it to 0 or 1 to switch off.
#' @param fontcolor Character vector of color specifications (names, hex-values,
#'                  etc.). For text appearing inside the bars the geom will
#'                  use the value "most unlike" the bar color. There are some
#'                  limits to this and bugs to be found.
#' @param parse -
#' @param na.rm -
#' @param show.legend -
#' @param inherit.aes -
#'
#' Relevant aes are
#' * **x**
#' * **y**
#' * label - `y` is used otherwise
#'
#' The following `aes` are passed to `geom_col`
#' * color
#' * fill
#' * size
#' * linetype
#' * alpha
#'
#' The following `aes` are used by `geom_text`
#' * fontsize - the size `aes` of geom_text
#' * angle
#' * **injust** - like `hjust` and `vjust` but it represents the "inward"
#'                justification. A value of -1 will be just outside and a
#'                value of 1 just inside the bar border.
#' * family
#' * fontface
#' * lineheight
#'
#' @examples
#' library(ggplot2)
#'
#' X <- data.frame(x = 1:5, y = sample(1:100, 5))
#'
#' ggplot(X, aes(x, y)) +
#'   geom_coltext(injust = -1.5)
#'
#' ggplot(X, aes(x, y)) +
#'   geom_coltext(injust = 1.5)
#'
#' ggplot(X, aes(x, y)) +
#'   geom_coltext(injust = 1.5) +
#'   coord_flip()
#'
#' ggplot(X, aes(x, y, label = percentify(y))) +
#'   geom_coltext(injust = -1.5) +
#'   coord_flip()
#'
#' @import ggplot2
#' @export
geom_coltext <- function(mapping = NULL,
                         data = NULL,
                         position = "dodge",
                         ...,
                         stat = "identity",
                         width = NULL,
                         center_text = FALSE,
                         just_mirror = TRUE,
                         just_mirror_cutoff = c(0.1, 0.9),
                         fontcolor = c(STAT.UP.Grey, STAT.UP.Lightgrey),
                         parse = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColtext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      center_text = center_text,
      just_mirror = just_mirror,
      just_mirror_cutoff = just_mirror_cutoff,
      fontcolor = fontcolor,
      parse = FALSE,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_coltext
#'
#' @export
geom_bartext <- geom_coltext

#' @rdname geom_coltext
#'
#' @import ggplot2
#' @export
GeomColtext <-
  ggplot2::ggproto(
    "GeomColtext",
    ggplot2::GeomRect,

    required_aes = c("x", "y"),

    default_aes = list(colour = NA,
                       fill = STAT.UP.Blue,
                       size = 0.5,
                       fontsize = 3.88,
                       linetype = 1,
                       alpha = NA,
                       angle = 0,
                       injust = 1.3,
                       family = "",
                       fontface = 1,
                       lineheight = 1.2),

    setup_data = function(self, data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

      data <-
        transform(
          data,
          ymin = pmin(y, 0), ymax = pmax(y, 0),
          xmin = x - width / 2, xmax = x + width / 2,
          width = NULL
        )

      data
    },

    draw_panel = function(self, data, params, coord,
                          stat = "identity",
                          width = NULL,
                          na.rm = FALSE,
                          parse = FALSE,
                          center_text = FALSE,
                          just_mirror = TRUE,
                          just_mirror_cutoff = c(0.1, 0.9),
                          fontcolor = c(STAT.UP.Grey, STAT.UP.Lightgrey))
    {
      ##### > Bar Grob #####
      col_grob <- GeomCol$draw_panel(data, params, coord, width)

      ##### > Text Grob #####

      ##### >> Small stuff #####

      # Add missing labels
      if(is.null(data$label))
        data$label <- as.character(data$y)

      # switch font size
      data$size <- data$fontsize

      ##### >> Adjust the injust #####

      ##### >>> React to coord_flip #####
      if("CoordFlip" %in% class(coord))
      {
        just <- "hjust"
        range <- "x.range"
      } else {
        just <- "vjust"
        range <- "y.range"
      }

      ##### >>> Mirror large and small values #####
      if (isTRUE(just_mirror))
      {
        ind <- (
          data$injust < 0.5 &
            data$y %outside% {just_mirror_cutoff[2] * params[[range]]}
        ) | (
          data$injust > 0.5 &
            !data$y %outside% {(just_mirror_cutoff[1]) * params[[range]]}
        )

        ind <- which(ind)
        data$injust[ind] <- - data$injust[ind]

        # Flip value for negative y
        data$injust <- 0.5 + sign(data$y) * (data$injust - 0.5)
      }

      data[[just]] <- data$injust / 2 + 0.5

      ##### >> Center Text #####

      if(isTRUE(center_text))
      {
        data$y <- (data$ymax + data$ymin) / 2
      }

      ##### >> Set text color #####

      data$colour <- fontcolor[1]

      # This will fail if fill is a variable and not a color
      try({
        ind <- which(sign(data[[just]] - 0.5) == sign(data$y))

        if(length(ind) > 0)
        {
          fills <- unique(data$fill[ind])
          bright.fills <- get_brightness(fills)
          bright.colors <- get_brightness(fontcolor)

          optimal.colors <-
            sapply(bright.fills,
                   function(.fill)
                     fontcolor[which.max(abs(bright.colors - .fill))])

          names(optimal.colors) <- fills

          data$colour[ind] <- optimal.colors[data$fill[ind]]
        }
      })

      ##### >> Create Text grob #####
      text_grob <- GeomText$draw_panel(data, params, coord, parse, na.rm)

      #  browser()
      ##### > Output #####
      ggplot2:::ggname("geom_coltext",
                       grid::grobTree(col_grob,
                                      text_grob))
    }
  )


