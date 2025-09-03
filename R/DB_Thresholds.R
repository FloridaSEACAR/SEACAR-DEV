#' DB_Thresholds
#'
#' Threshold and quantile values for every parameter in SEACAR database.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{ThresholdID}{numeric value assigned to each threshold}
#'   \item{ParameterID}{numeric value assigned to each parameter}
#'   \item{Habitat}{Name of SEACAR habitat}
#'   \item{IndicatorID}{numeric value assigned to each indicator}
#'   \item{IndicatorName}{Name of SEACAR indicator}
#'   \item{CombinedTable}{Refers to the internal database table from which the data derives}
#'   \item{ParameterName}{Name of SEACAR parameter}
#'   \item{Units}{Parameter units}
#'   \item{LowThreshold}{Value below which values are excluded}
#'   \item{HighThreshold}{Value above which values are excluded}
#'   \item{QuadSize_m2}{Quad Size in meters squared}
#'   \item{ExpectedValues}{Which values are expected for a given parameter}
#'   \item{Conversions}{Unit conversions}
#'   \item{LowQuantile}{Value below which values are flagged as low quantile}
#'   \item{HighQuantile}{Value above which values are flagged as high quantile}
#'   \item{Calculated}{Whether a parameter is calculated by SEACAR or not (TRUE or FALSE)}
#'   \item{IsSpeciesSpecific}{Whether a parameter is species-specific or not (TRUE or FALSE)}
#'   \item{AdditionalComments}{Additional comments}
#' }
#'
#' @source Created by Florida SEACAR
"DB_Thresholds"
