#' SEACAR generate_cw_description function to generate table descriptions for Coastal Wetlands habitat for a single ManagedArea, to be used within `generate_description()`.
#'
#' @param data a dataframe with statistical analysis results for Coastal Wetland habitat.
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
generate_cw_description <- function(data){
  data$N_Data <- formatC(data$N_Data, format="d", big.mark=",")

  sentences <- c()
  for(i in seq_len(nrow(data))){
    sp <- data$Species[i]
    median <- data$Median[i]
    n_data <- data$N_Data[i]
    min_year <- data$EarliestYear[i]
    max_year <- data$LatestYear[i]
    obs_wording <- ifelse(n_data>1, glue("{n_data} observations"), glue("{n_data} observation"))
    if(min_year==max_year){
      sentence <- glue("In the year {min_year}, {median} species were observed for <i>{sp}</i> based on {obs_wording}.")
    } else {
      sentence <- glue("Between {min_year} and {max_year}, the median annual number of species for <i>{sp}</i> was {median} based on {obs_wording}.")
    }
    sentences <- c(sentences, sentence)
  }
  # Combine sentences into a single description
  description <- paste0("<p>", paste(str_to_sentence(sentences), collapse = " "), "</p>")

  # Create output table
  descriptionText <- data.table(
    "ManagedAreaName" = ma,
    "HabitatName" = "Coastal Wetlands",
    "IndicatorName"= "Species Composition",
    "SamplingFrequency" = "None",
    "ParameterName" = "Total/Canopy Percent Cover",
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
