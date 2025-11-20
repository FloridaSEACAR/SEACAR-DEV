#' SEACAR generate_nekton_description function to generate table descriptions for Nekton habitat for a single ManagedArea, to be used within `generate_description()`.
#'
#' @param data a dataframe with statistical analysis results for Nekton habitat.
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
generate_nekton_description <- function(data){
  data <- data %>%
    mutate(GearTypeSize = paste0(GearSize_m, "-meter ", GearType)) %>%
    group_by(ManagedAreaName, ParameterName, GearType, GearSize_m, GearTypeSize,
             EarliestYear, LatestYear, Median, N_Data) %>% reframe() %>% as.data.table()
  data$N_Data <- formatC(data$N_Data, format="d", big.mark=",")
  two_geartypes <- length(unique(data$GearTypeSize))>1
  sentences <- c()
  if(two_geartypes){
    split_df <- split(data, data$GearTypeSize)
    g1 <- split_df[[1]]
    g2 <- split_df[[2]]
    sentence <- sprintf("The median annual number of taxa was %.2f based on %s observations collected by %s between %i and %i, and the median annual number of taxa was %.2f based on %s observations collected by %s between %i and %i.",
                        g1$Median, formatC(g1$N_Data, format="d", big.mark = ","), g1$GearTypeSize, g1$EarliestYear, g1$LatestYear,
                        g2$Median, formatC(g2$N_Data, format="d", big.mark = ","), g2$GearTypeSize, g2$EarliestYear, g2$LatestYear)
    sentences <- c(sentences, sentence)
  } else {
    sentence <- sprintf("The median annual number of taxa was %.2f based on %s observations collected by %s between %i and %i.",
                        data$Median, formatC(data$N_Data, format="d", big.mark = ","), data$GearTypeSize, data$EarliestYear, data$LatestYear)
    sentences <- c(sentences, sentence)
  }
  # Combine sentences into a single description
  description <- paste0("<p>", paste(str_to_sentence(sentences), collapse = " "), "</p>")
  # Create output table
  descriptionText <- data.table(
    "ManagedAreaName" = ma,
    "HabitatName" = "Water Column",
    "IndicatorName"= "Nekton",
    "SamplingFrequency" = "None",
    "ParameterName" = "Presence/Absence",
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
