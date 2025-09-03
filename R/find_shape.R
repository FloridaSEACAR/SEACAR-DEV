#' SEACAR find_shape function to locate individual Managed Area boundary
#'
#' @param rcp sf dataframe corresponding to the output from SEACAR::GeoData$rcp
#' @param ma Managed Area Name (full name)
#'
#' @return A single-line sf dataframe
#' @export
#'
#' @examples
#' ma_shape <- find_shape(rcp = SEACAR::GeoData$rcp, ma = "Estero Bay Aquatic Preserve")
find_shape <- function(rcp, ma){return(rcp %>% dplyr::filter(LONG_NAME==ma))}
