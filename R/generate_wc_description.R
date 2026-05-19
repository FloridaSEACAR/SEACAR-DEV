#' SEACAR generate_wc_description function to generate table descriptions for WC Discrete and Continuous habitat for a single ManagedArea, to be used within `generate_description()`.
#'
#' @param input_df a dataframe with statistical analysis results for WC Discrete and Continuous.
#'
#' @import data.table
#' @import dplyr
#' @importFrom glue glue
#' @importFrom stringi stri_replace_all_regex
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_to_sentence
#'
#' @return A text-based descriptive statement wrapped in HTML paragraph tags.
#' @export
#'
generate_wc_description <- function(input_df){
  # Declare discrete function
  generate_wc_description_discrete <- function(input_df){
    descriptionTable <- data.table()
    input_df$SenSlope <- ifelse(round(input_df$SenSlope,2)==0.00, "less than 0.01",
                                round(input_df$SenSlope,2))
    for(parameter in unique(input_df$ParameterName)){
      indicator <- WebsiteParameters[ParameterName==parameter, unique(IndicatorName)]

      p_subset <- input_df[ParameterName==parameter, ]
      units <- ifelse(parameter=="pH", "pH units", WebsiteParameters[ParameterName==parameter, unique(ParameterUnits)])

      increasing <- p_subset[str_detect(tolower(TrendText), "increasing"), ]
      decreasing <- p_subset[str_detect(tolower(TrendText), "decreasing"), ]
      no_change <- p_subset[str_detect(tolower(TrendText), "no detectable"), ]
      insufficient <- p_subset[str_detect(tolower(TrendText), "insufficient"), ]

      display_parameter <- ifelse(parameter=="pH", parameter, tolower(parameter))

      sentences <- c()

      if(nrow(increasing)>0){
        if(indicator=="Water Clarity"){
          wc_trend <- "a decrease"
          if(parameter=="Secchi Depth"){
            sentence <- sprintf("Monthly average %s became shallower by %s %s per year, indicating %s in water clarity.",
                                display_parameter, increasing$SenSlope, units, wc_trend)
          } else {
            sentence <- sprintf("Monthly average %s increased by %s %s per year, indicating %s in water clarity.",
                                display_parameter, increasing$SenSlope, units, wc_trend)
          }
          sentences <- c(sentences, sentence)
        } else {
          sentence <- sprintf("Monthly average %s increased by %s %s per year.",
                              display_parameter, increasing$SenSlope, units)
          sentences <- c(sentences, sentence)
        }
      }

      if(nrow(decreasing)>0){
        if(indicator=="Water Clarity"){
          wc_trend <- "an increase"
          if(parameter=="Secchi Depth"){
            sentence <- sprintf("Monthly average %s became deeper by %s %s per year, indicating %s in water clarity.",
                                display_parameter, decreasing$SenSlope, units, wc_trend)
          } else {
            sentence <- sprintf("Monthly average %s decreased by %s %s per year, indicating %s in water clarity.",
                                display_parameter, decreasing$SenSlope, units, wc_trend)
          }
          sentences <- c(sentences, sentence)
        } else {
          sentence <- sprintf("Monthly average %s decreased by %s %s per year.",
                              display_parameter, decreasing$SenSlope, units)
          sentences <- c(sentences, sentence)
        }
      }

      if(nrow(no_change)>0){
        sentence <- sprintf("%s showed no detectable trend between %s and %s.",
                            display_parameter, no_change$EarliestYear, no_change$LatestYear)
        sentences <- c(sentences, sentence)
      }

      if(nrow(insufficient)>0){
        sentence <- sprintf("There was insufficient data to fit a model for %s.",
                            display_parameter)
        sentences <- c(sentences, sentence)
      }

      # Combine all sentences together
      description <- paste(str_to_sentence(sentences), collapse = " ")

      # Apply alterations to final display of text
      description <- stringi::stri_replace_all_regex(
        description,
        pattern = c("Ph ", " ph ", "mg/l", "ntu ", " deg c ", " % per", "ug/l", "secchi", " pcu ", " ph\\.", "pheophytin "),
        replacement = c("pH ", " pH ", "mg/L", "NTU ", "&deg;C ", "% per", "&micro;g/L", "Secchi", " PCU ", " pH.", "pheophytin, "),
        vectorize = FALSE
      )

      # Add paragraph tags
      description <- paste0("<p>", description, "</p>")

      # Save description in excel workbook
      descriptionText <- data.table(
        "ManagedAreaName" = ma,
        "HabitatName" = "Water Column",
        "IndicatorName"= indicator,
        "SamplingFrequency" = "Discrete",
        "ParameterName" = parameter,
        "DescriptionHTML" = description,
        "DescriptionLatex" = stringi::stri_replace_all_regex(
          description,
          pattern = c("<i>", "</i>", "&#8805;"),
          replacement = c("*", "*", ">="),
          vectorize = FALSE
        )
      )
      descriptionTable <- bind_rows(descriptionTable, descriptionText)
    }
    return(descriptionTable)
  }

  # Declare Continuous function
  generate_wc_description_continuous <- function(input_df){
    descriptionTable <- data.table()
    for(parameter in unique(input_df$ParameterName)){
      indicator <- WebsiteParameters[ParameterName==parameter, unique(IndicatorName)]
      p_subset <- input_df[ParameterName==parameter, ]
      units <- WebsiteParameters[ParameterName==parameter, unique(ParameterUnits)]

      results_p_subset <- p_subset %>%
        group_by(ParameterName, TrendText) %>%
        summarise(n_stations = n(),
                  min_slope = min(SenSlope),
                  max_slope = max(SenSlope),
                  min_slope_pct = (min_slope * n_stations) / mean(SenSlope),
                  max_slope_pct = (max_slope * n_stations) / mean(SenSlope),
                  .groups = "keep") %>% as.data.table()

      results_p_subset$min_slope <- ifelse(is.na(results_p_subset$min_slope), NA, abs(results_p_subset$min_slope))
      results_p_subset$min_slope <- round(results_p_subset$min_slope, 2)
      results_p_subset$max_slope <- ifelse(is.na(results_p_subset$max_slope), NA, abs(results_p_subset$max_slope))
      results_p_subset$max_slope <- round(results_p_subset$max_slope, 2)

      # It is necessary to determine the ordering of the absolute value slopes for each subset beforehand (slope1, slope2)
      # Because once some are converted to character they cannot be compared
      increasing <- results_p_subset[str_detect(tolower(TrendText), "increasing"), ]
      increasing$slope1 <- min(increasing$min_slope, increasing$max_slope)
      # increasing$slope1 <- ifelse(increasing$slope1==0.00, "less than 0.01", increasing$slope1)
      increasing$slope2 <- max(increasing$min_slope, increasing$max_slope)
      # increasing$slope2 <- ifelse(increasing$slope2==0.00, "less than 0.01", increasing$slope2)

      decreasing <- results_p_subset[str_detect(tolower(TrendText), "decreasing"), ]
      decreasing$slope1 <- min(decreasing$min_slope, decreasing$max_slope)
      # decreasing$slope1 <- ifelse(decreasing$slope1==0.00, "less than 0.01", decreasing$slope1)
      decreasing$slope2 <- max(decreasing$min_slope, decreasing$max_slope)
      # decreasing$slope2 <- ifelse(decreasing$slope2==0.00, "less than 0.01", decreasing$slope2)

      no_change <- results_p_subset[str_detect(tolower(TrendText), "no detectable"), ]
      insufficient <- results_p_subset[str_detect(tolower(TrendText), "insufficient"), ]

      display_parameter <- ifelse(parameter=="pH", parameter, tolower(parameter))
      units <- ifelse(parameter=="pH", "pH units", units)

      sentences <- c()

      if(nrow(increasing)>0){
        singular <- increasing$n_stations==1
        if(singular){
          increasing$min_slope <- ifelse(increasing$min_slope==0.00, "less than 0.01", increasing$min_slope)
          if(class(increasing$min_slope)=="character"){
            sentence <- sprintf("At %s program location, monthly average %s increased by %s %s per year.",
                                english::english(increasing$n_stations), display_parameter, increasing$min_slope, units)
          } else {
            sentence <- sprintf("At %s program location, monthly average %s increased by %.2f %s per year.",
                                english::english(increasing$n_stations), display_parameter, increasing$min_slope, units)
          }
          sentences <- c(sentences, sentence)
        } else {
          # Make exception for when the slopes are the same and only 2 stations
          if(increasing$slope1 == increasing$slope2 & increasing$n_stations==2){
            sentence <- sprintf("At %s program locations, monthly average %s increased by %.2f %s per year.",
                                english::english(increasing$n_stations),
                                display_parameter,
                                increasing$slope1,
                                units)
          } else {
            increasing$slope1 <- ifelse(round(increasing$slope1,2)==0.00, "less than 0.01", round(increasing$slope1,2))
            increasing$slope2 <- ifelse(round(increasing$slope2,2)==0.00, "less than 0.01", round(increasing$slope2,2))
            if(increasing$n_stations==2){
              sentence <- sprintf("At %s program locations, monthly average %s increased by %s %s per year at one site and by %s %s per year at the other.",
                                  english::english(increasing$n_stations),
                                  display_parameter,
                                  increasing$slope1,
                                  units,
                                  increasing$slope2,
                                  units)
            } else {
              sentence <- sprintf("At %s program locations, monthly average %s increased between %s and %s %s per year.",
                                  english::english(increasing$n_stations),
                                  display_parameter,
                                  increasing$slope1,
                                  increasing$slope2,
                                  units)
            }
          }
          sentences <- c(sentences, sentence)
        }
      }

      if(nrow(decreasing)>0){
        singular <- decreasing$n_stations==1
        if(singular){
          decreasing$min_slope <- ifelse(decreasing$min_slope==0.00, "less than 0.01", decreasing$min_slope)
          if(class(decreasing$min_slope)=="character"){
            sentence <- sprintf("At %s program location, monthly average %s decreased by %s %s per year.",
                                english::english(decreasing$n_stations), display_parameter, decreasing$min_slope, units)
          } else {
            sentence <- sprintf("At %s program location, monthly average %s decreased by %.2f %s per year.",
                                english::english(decreasing$n_stations), display_parameter, decreasing$min_slope, units)
          }
          sentences <- c(sentences, sentence)
        } else {
          # Make exception for when the slopes are the same and only 2 stations
          if(decreasing$slope1 == decreasing$slope2 & decreasing$n_stations==2){
            sentence <- sprintf("At %s program locations, monthly average %s decreased by %.2f %s per year.",
                                english::english(decreasing$n_stations),
                                display_parameter,
                                decreasing$slope1,
                                units)
          } else {
            decreasing$slope1 <- ifelse(round(decreasing$slope1,2)==0.00, "less than 0.01", round(decreasing$slope1,2))
            decreasing$slope2 <- ifelse(round(decreasing$slope2,2)==0.00, "less than 0.01", round(decreasing$slope2,2))
            if(decreasing$n_stations==2){
              sentence <- sprintf("At %s program locations, monthly average %s decreased by %s %s per year at one site and by %s %s per year at the other.",
                                  english::english(decreasing$n_stations),
                                  display_parameter,
                                  decreasing$slope1,
                                  units,
                                  decreasing$slope2,
                                  units)
            } else {
              sentence <- sprintf("At %s program locations, monthly average %s decreased between %s and %s %s per year.",
                                  english::english(decreasing$n_stations),
                                  display_parameter,
                                  decreasing$slope1,
                                  decreasing$slope2,
                                  units)
            }
          }
          sentences <- c(sentences, sentence)
        }
      }

      if(nrow(no_change)>0){
        singular <- no_change$n_stations==1
        if(singular){
          sentence <- sprintf("No detectable change in monthly average %s was observed at %s location.",
                              display_parameter, english::english(no_change$n_stations))
        } else {
          sentence <- sprintf("No detectable change in monthly average %s was observed at %s locations.",
                              display_parameter, english::english(no_change$n_stations))
        }
        sentences <- c(sentences, sentence)
      }

      if(nrow(insufficient)>0){
        singular <- insufficient$n_stations==1
        if(singular){
          sentence <- sprintf("There was insufficient data to fit a model for %s location.",
                              english::english(insufficient$n_stations))
          sentences <- c(sentences, sentence)
        } else {
          sentence <- sprintf("There was insufficient data to fit a model for %s locations.",
                              english::english(insufficient$n_stations))
          sentences <- c(sentences, sentence)
        }
      }

      # Combine all sentences together
      description <- paste(str_to_sentence(sentences), collapse = " ")

      # Apply alterations to final display of text
      description <- stringi::stri_replace_all_regex(
        description,
        pattern = c(" ph", "mg/l", "ntu ", " deg c ", " % per", "ms/cm"),
        replacement = c(" pH", "mg/L", "NTU ", "&deg;C ", "% per", "mS/cm"),
        vectorize = FALSE
      )

      # Add paragraph tags
      description <- paste0("<p>", description, "</p>")

      # Save description in excel workbook
      descriptionText <- data.table(
        "ManagedAreaName" = ma,
        "HabitatName" = "Water Column",
        "IndicatorName"= indicator,
        "SamplingFrequency" = "Continuous",
        "ParameterName" = parameter,
        "DescriptionHTML" = description,
        "DescriptionLatex" = stringi::stri_replace_all_regex(
          description,
          pattern = c("<i>", "</i>", "&#8805;"),
          replacement = c("*", "*", ">="),
          vectorize = FALSE
        )
      )
      descriptionTable <- bind_rows(descriptionTable, descriptionText)
    }
    return(descriptionTable)
  }

  # Determine which function to use based on data input
  if(!any(str_detect(names(input_df), "ProgramID"))){
    descriptionText <- generate_wc_description_discrete(input_df)
  } else {
    descriptionText <- generate_wc_description_continuous(input_df)
  }
  return(descriptionText)
}
