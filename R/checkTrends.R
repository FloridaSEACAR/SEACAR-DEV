#' checkTrends function for parsing statistical results
#'
#' Function to create text for stats results.
#'
#' @param p p-value
#' @param Slope LME Slope value
#' @param SufficientData boolean value (TRUE/FALSE)
#'
#' @return Character-based statement denoting trend
#' @export
#'
checkTrends <- function(p, Slope, SufficientData){
  if(SufficientData){
    if(is.na(Slope)){
      return("Model did not fit the available data")
    } else {
      increasing <- Slope > 0
      trendPresent <- p <= 0.05
      trendStatus <- "No significant trend"
      if(trendPresent){
        trendStatus <- ifelse(increasing, "Significantly increasing trend",
                              "Significantly decreasing trend")
      }
    }
  } else {
    trendStatus <- "Insufficient data to calculate trend"
  }
  return(trendStatus)
}
