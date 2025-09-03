#' SEACAR ggplot2 theme
#'
#' A custom `ggplot2` theme conforming to Florida SEACAR visualization standards.
#'
#' @return A `ggplot2` theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   SEACAR_plot_theme()
SEACAR_plot_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Arial"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12, color = "#314963"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "#314963"),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(hjust = 0),
      axis.title.x = ggplot2::element_text(size = 10, margin = ggplot2::margin(t = 5, b = 10)),
      axis.title.y = ggplot2::element_text(size = 10, margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = -45, hjust = 0)
    )
}
