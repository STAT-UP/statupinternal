#' @import ggplot2
#' @export
STAT_UP_set_theme <- function(.font.size = 12) {
  extrafont::loadfonts()

  theme_set({
    cowplot::theme_cowplot(
      font_size = .font.size,
      font_family = "Calibri Light",
      line_size = 0.3
    ) +
      cowplot::background_grid(major = "y", minor = "y") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            legend.position = "bottom",
            # legend.box.margin = margin(),
            # legend.margin = margin(),
            legend.box.spacing = unit(0, "cm"),
            strip.text.y = element_text(angle = 0, hjust = 0),
            strip.background = element_blank())
  })
}
