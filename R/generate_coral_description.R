#' SEACAR generate_coral_description function to generate table descriptions for Coral habitat for a single ManagedArea, to be used within `generate_description()`.
#'
#' @param data a dataframe with statistical analysis results for Coral habitat.
#'
#' @import data.table
#' @importFrom glue glue
#' @importFrom stringi stri_replace_all_regex
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_to_sentence
#'
#' @return A text-based descriptive statement wrapped in HTML paragraph tags.
#' @export
#'
generate_coral_description <- function(data){
  # Function for coral percent cover
  generate_coral_description_pc <- function(data){
    trend <- data$StatisticalTrend
    min_year <- data$EarliestYear
    max_year <- data$LatestYear
    slope <- ifelse(round(data$LME_Slope, 2)==0.00, "less than 0.01", round(data$LME_Slope, 2))
    time_period <- ifelse(min_year==max_year, "", glue(" between {min_year} and {max_year}"))
    if(str_detect(trend, "Insufficient")){
      sentence <- glue("There was insufficient data to determine a trend{time_period}.")
    }
    if(str_detect(trend, "Model did not fit")){
      model_result <- glue("The model did not fit the available data{time_period}.")
    }
    if(str_detect(trend, "No significant")){
      sentence <- glue("Percent cover showed no detectable trend{time_period}.")
    }
    if(str_detect(trend, "Significantly increasing")){
      sentence <- glue("Annual average percent cover increased by {slope}%.")
    }
    if(str_detect(trend, "Significantly decreasing")){
      sentence <- glue("Annual average percent cover decreased by {slope}%{time_period}.")
    }
    return(sentence)
  }

  # Function for coral species richness
  generate_coral_description_sr <- function(data){
    data$N_Data <- formatC(data$N_Data, format="d", big.mark=",")

    median <- data$Median
    n_data <- data$N_Data
    min_year <- data$EarliestYear
    max_year <- data$LatestYear

    obs_wording <- ifelse(n_data>1, glue("{n_data} observations"), glue("{n_data} observation"))
    if(min_year==max_year){
      sentence <- glue("In the year {min_year}, {median} taxa were observed based on {obs_wording}.")
    } else {
      sentence <- glue("The median annual number of taxa was {median} based on {n_data} observations collected between {min_year} and {max_year}.")
    }

    return(sentence)
  }

  # Determine which indicator / parameter is being analzyed based on data input
  if(any(str_detect(names(data), "LME_"))){
    indicator <- "Percent Cover"
    parameter <- "Percent Cover"
    data <- data %>% distinct() %>% rowwise() %>%
      mutate(StatisticalTrend = SEACAR::checkTrends(p = LME_p, Slope = LME_Slope,
                                                    SufficientData = SufficientData))
  } else {
    indicator <- "Grazers and Reef Dependent Species"
    parameter <- "Presence/Absence"
  }

  if(indicator=="Percent Cover"){
    description <- paste0("<p>", generate_coral_description_pc(data), "</p>")
  } else {
    description <- paste0("<p>", generate_coral_description_sr(data), "</p>")
  }

  # Create output table
  descriptionText <- data.table(
    "ManagedAreaName" = ma,
    "HabitatName" = "Coral/Coral Reef",
    "IndicatorName"= indicator,
    "SamplingFrequency" = "None",
    "ParameterName" = parameter,
    "DescriptionHTML" = description,
    "DescriptionLatex" = stringi::stri_replace_all_regex(
      description,
      pattern = c("<i>", "</i>", "&#8805;"),
      replacement = c("*", "*", ">="),
      vectorize = FALSE
    )
  )
  return(descriptionText)
}
