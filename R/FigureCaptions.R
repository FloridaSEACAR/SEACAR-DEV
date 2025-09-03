#' FigureCaptions
#'
#' Captions which accompany plots and figures on the SEACAR Atlas
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{HabitatName}{Name of SEACAR habitat}
#'   \item{IndicatorName}{Name of SEACAR indicator}
#'   \item{ParameterName}{Name of SEACAR parameter}
#'   \item{SamplingFrequency}{Frequency of WQ data collection (Discrete vs. Continuous)}
#'   \item{Website}{Whether a given ParameterName / ActivityType / RelativeDepth combination is included on the SEACAR Atlas}
#'   \item{ParameterVisId}{Internal Parameter Visualization ID for internal use on the SEACAR Atlas}
#'   \item{FigureCaptions}{Text of available figure captions, wrapped in HTML paragraph <p> tags}
#' }
#'
#' @source Created by Florida SEACAR
"FigureCaptions"
