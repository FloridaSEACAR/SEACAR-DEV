#' Subchunkify function for RMarkdown
#'
#' Function to allow dynamic figure resizing and proper caption implementation. Developed by Michael JW, via Stackoverflow user `jzadra`.
#'
#' @param g Knitr-based input to be parsed
#' @param fig_height Figure height (inches)
#' @param fig_width Figure width (inches)
#' @param fig_cap figure caption text
#' @return A deparsed knitr subchunk
#' @export
#'
subchunkify <- function(g, fig_height=9, fig_width=10, fig_cap) {
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')
  sub_chunk <- paste0("
  \n`","``{r sub_chunk_", floor(stats::runif(1) * 10000), ", fig.height=",
                      fig_height, ", fig.width=", fig_width, ", echo=FALSE, results='asis'}",
                      "\n(",
                      g_deparsed
                      , ")()",
                      "\n`","``
  ")
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}
