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
#' @param just_mirror_cutoff Numeric value between 0 and 1. The default of 0.9
#'                           means that if the text is supposed to appear
#'                           *outside* the bars then for values larger (in
#'                           absolute terms) than 90% of the data this is
#'                           flipped to appear inside. And vice versa (in that
#'                           case a value of e.g. 0.1 will make more sense).
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
#'                justification. A value of 0 will be just outside and a
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
#'   geom_bartext(injust = -0.5)
#'
#' ggplot(X, aes(x, y)) +
#'   geom_bartext(injust = 1.5)
#'
#' ggplot(X, aes(x, y)) +
#'   geom_bartext(injust = 1.5) +
#'   coord_flip()
#'
#' ggplot(X, aes(x, y, label = percentify(y))) +
#'   geom_bartext(injust = -0.5) +
#'   coord_flip()
#'
#' @import ggplot2
#' @export
geom_bartext <- function(mapping = NULL,
                         data = NULL,
                         position = "identity",
                         ...,
                         width = NULL,
                         just_mirror_cutoff = 0.9,
                         fontcolor = c("grey15", "grey85"),
                         parse = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomBarText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      just_mirror_cutoff = just_mirror_cutoff,
      fontcolor = fontcolor,
      parse = FALSE,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_bartext
#'
#' @import ggplot2
#' @export
GeomBarText <-
  ggplot2::ggproto(
    "GeomBarText",
    ggplot2::Geom,

    required_aes = c("x", "y"),

    default_aes = list(colour = NULL,
                       fill = "steelblue",
                       size = 0.5,
                       fontsize = 3.88,
                       linetype = 1,
                       alpha = NA,
                       angle = 0,
                       injust = -0.2,
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
                          width = NULL,
                          na.rm = FALSE,
                          parse = FALSE,
                          just_mirror_cutoff = 0.9,
                          check_overlap = FALSE,
                          fontcolor = c("grey15", "grey85"))
    {
      # browser()
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

      if("CoordFlip" %in% class(coord))
      {
        just <- "hjust"
        range <- "x.range"
      } else {
        just <- "vjust"
        range <- "y.range"
      }

      ind <- (
        data$injust < 0.5 &
          data$y %outside% {just_mirror_cutoff * params[[range]]}
      ) | (
        data$injust > 0.5 &
          !data$y %outside% {just_mirror_cutoff * params[[range]]}
      )

      ind <- which(ind)
      data$injust[ind] <- 1 - data$injust[ind]

      # Flip value for negative y
      data$injust <- 0.5 + sign(data$y) * (data$injust - 0.5)


      data <- setnames(data, "injust", just)


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
      text_grob <- GeomText$draw_panel(data, params, coord, parse, na.rm,
                                       check_overlap)

    #  browser()
      ##### > Output #####
      ggplot2:::ggname("geom_bartext",
                       grid::grobTree(col_grob,
                                text_grob))
    }
  )


