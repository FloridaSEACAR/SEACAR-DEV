#' SEACAR generate_nekton_description function to generate table descriptions for SAV habitat for a single ManagedArea, to be used within `generate_description()`.
#'
#' @param data a dataframe with statistical analysis results for SAV habitat.
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
generate_sav_description <- function(data){
  data$Species <- as.character(data$Species)

  increasing <- list()
  decreasing <- list()
  no_change <- list()
  insufficient <- c()
  model_not_fit <- c()

  min_years <- c()
  max_years <- c()

  # Loop through all rows and classify each species by trend type
  for(i in 1:nrow(data)){
    species <- data$Species[i]
    trend <- data$TrendText[i]
    record <- data$Period[i]

    if(str_detect(trend, "Insufficient")){
      insufficient <- c(insufficient, species)
      next
    }
    if(str_detect(trend, "Model did not fit")){
      model_not_fit <- c(model_not_fit, species)
      next
    }

    years <- str_split(record, " - ")[[1]]
    start_year <- as.integer(trimws(years[1]))
    end_year <- as.integer(trimws(years[2]))

    slope <- round(as.numeric(data$LME_Slope[i]), 2)

    if(str_detect(tolower(trend), "increasing")){
      increasing[[length(increasing)+1]] <- list(species = species, slope = slope)
    } else if(str_detect(tolower(trend), "decreasing")){
      decreasing[[length(decreasing)+1]] <- list(species = species, slope = slope)
    } else {
      no_change[[length(no_change)+1]] <- species
      min_years <- c(min_years, start_year)
      max_years <- c(max_years, end_year)
    }
  }

  sentences <- c()

  ## Increasing
  if(length(increasing) == 1){
    s <- increasing[[1]]
    sentences <- c(sentences, sprintf("An annual increase in percent cover was observed for %s (%.2f%%).", s$species, s$slope))
  } else if(length(increasing) == 2){
    parts <- sapply(increasing, function(s) sprintf("%s (%.2f%%)", s$species, s$slope))
    sentences <- c(sentences, sprintf("Annual increases in percent cover were observed for %s and %s.", parts[1], parts[2]))
  } else if(length(increasing) > 2){
    parts <- sapply(increasing, function(s) sprintf("%s (%.2f%%)", s$species, s$slope))
    species_list <- paste(parts[1:(length(parts)-1)], collapse = ", ")
    species_list <- paste0(species_list, ", and ", parts[[length(parts)]])
    sentences <- c(sentences, sprintf("Annual increases in percent cover were observed for %s.", species_list))
  }

  ## Decreasing
  if(length(decreasing) == 1){
    s <- decreasing[[1]]
    sentences <- c(sentences, sprintf("An annual decrease in percent cover was observed for %s (%.2f%%).", s$species, s$slope))
  } else if(length(decreasing) == 2){
    parts <- sapply(decreasing, function(s) sprintf("%s (%.2f%%)", s$species, s$slope))
    sentences <- c(sentences, sprintf("Annual decreases in percent cover were observed for %s and %s.", parts[1], parts[2]))
  } else if(length(decreasing) > 2){
    parts <- sapply(decreasing, function(s) sprintf("%s (%.2f%%)", s$species, s$slope))
    species_list <- paste(parts[1:(length(parts)-1)], collapse = ", ")
    species_list <- paste0(species_list, ", and ", parts[[length(parts)]])
    sentences <- c(sentences, sprintf("Annual decreases in percent cover were observed for %s.", species_list))
  }

  ## No change
  if(length(no_change) > 0){
    min_yr <- min(min_years)
    max_yr <- max(max_years)
    if(length(no_change) == 1){
      sentences <- c(sentences, sprintf("No detectable change in percent cover was observed for %s.", no_change[[1]]))
    } else if(length(no_change) == 2){
      sentences <- c(sentences,
                     sprintf("No detectable change in percent cover was observed for %s and %s.",
                             no_change[[1]], no_change[[2]]))
    } else {
      species_list <- paste(no_change[1:(length(no_change)-1)], collapse = ", ")
      species_list <- paste0(species_list, ", and ", no_change[[length(no_change)]])
      sentences <- c(sentences, sprintf("%s showed no detectable change in percent cover.", species_list))
    }
  }

  ## Insufficient and No Model Results
  # If both Insufficient and Model_not_fit have results, combine into single sentence
  if(length(insufficient) > 0){
    if(length(insufficient) == 1){
      insufficient_sentence <- sprintf("Trends in percent cover could not be evaluated for %s due to insufficient data", insufficient[1])
    } else if(length(insufficient) == 2){
      insufficient_sentence <- sprintf("Trends in percent cover could not be evaluated for %s and %s due to insufficient data", insufficient[1], insufficient[2])
    } else {
      species_list <- paste(insufficient[1:(length(insufficient)-1)], collapse = ", ")
      species_list <- paste0(species_list, ", and ", insufficient[length(insufficient)])
      insufficient_sentence <- sprintf("Trends in percent cover could not be evaluated for %s due to insufficient data", species_list)
    }
  }

  if(length(model_not_fit) > 0){
    if(length(model_not_fit) == 1){
      no_model_sentence <- sprintf("the model could not be fitted for %s.", model_not_fit[1])
    } else if(length(model_not_fit) == 2){
      no_model_sentence <- sprintf("a model could not be fitted for %s and %s.", model_not_fit[1], model_not_fit[2])
    } else {
      species_list <- paste(model_not_fit[1:(length(model_not_fit)-1)], collapse = ", ")
      species_list <- paste0(species_list, ", and ", model_not_fit[length(model_not_fit)])
      no_model_sentence <- sprintf("a model could not be fitted for %s.", species_list)
    }
  }

  if(length(insufficient)>0 & length(model_not_fit)>0){
    combined_sentence <- paste0(insufficient_sentence, ", and ", no_model_sentence)
    sentences <- c(sentences, combined_sentence)
  } else if(length(insufficient)>0){
    sentences <- c(sentences, paste0(insufficient_sentence,"."))
  } else if(length(model_not_fit)>0){
    sentences <- c(sentences, paste0(no_model_sentence))
  }
  # Compile full sentence(s) description
  description <- paste(str_to_sentence(sentences), collapse = " ")
  # Apply alterations to final display of species names (including italics)
  description <- stringi::stri_replace_all_regex(
    description,
    pattern = c("total sav", "Total sav", "halophila, unk.", "Halophila, unk.","halophila spp.", "Halophila spp.", "spp\\.\\."),
    replacement = c("total SAV", "Total SAV", "unknown <i>Halophila</i>", "unknown <i>Halophila</i>", "<i>Halophila</i> spp.", "<i>Halophila</i> spp.", "spp."),
    vectorize = FALSE
  )
  # Wrap with paragraph tags
  description <- paste0("<p>", description, "</p>")
  # Compile into table
  descriptionText <- data.table(
    "ManagedAreaName" = ma,
    "HabitatName" = "Submerged Aquatic Vegetation",
    "IndicatorName"= "Percent Cover (by species)",
    "SamplingFrequency" = "None",
    "ParameterName" = "Percent Cover",
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
