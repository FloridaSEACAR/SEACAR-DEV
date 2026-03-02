#' SEACAR generate_osyter_description function to generate table descriptions for Oyster habitat for a single ManagedArea, to be used within `generate_description()`.
#'
#' @param data a dataframe with statistical analysis results for Oyster habitat.
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
generate_oyster_description <- function(data){
  # Oyster trend function to determine trend significance for Shell Height
  oyTrendText <- function(modelEstimate, lowConfidence, upConfidence){
    increasing <- modelEstimate > 0
    trendPresent <- (lowConfidence < 0 & upConfidence < 0) |
      (lowConfidence > 0 & upConfidence > 0)
    trendStatus <- "No significant change"
    if(isTRUE(trendPresent)){
      trendDirection <- ifelse(increasing, "increasing", "decreasing")
      trendStatus <- paste0("Significantly ", trendDirection, " trend")
    }
    return(trendStatus)
  }

  descriptionTable <- data.table()
  for(parameter in unique(data$ParameterName)){
    indicator <- ifelse(parameter=="Shell Height", "Size Class", parameter)
    filtered_oy <- data[ParameterName==parameter, ] %>% rowwise() %>%
      mutate("trend" = ifelse(!is.na(ModelEstimate), oyTrendText(ModelEstimate, LowerConfidence, UpperConfidence), NA)) %>%
      as.data.table()
    # Percent Live and Density statements
    if(parameter %in% c("Percent Live", "Density")){
      if(parameter=="Density"){
        units <- " oysters per square meter"
      } else if(parameter=="Percent Live"){
        units <- "%"
      }
      param_display <- ifelse(parameter=="Percent Live", "percent live cover", tolower(parameter))
      sentences <- c()
      for(hab_type in unique(filtered_oy$HabitatType)){
        if(filtered_oy[HabitatType==hab_type, SufficientData]==FALSE){
          sentence <- glue("For {tolower(hab_type)} reefs, there was insufficient data to calculate a trend for {param_display}.")
        } else {
          trend <- str_detect(filtered_oy[HabitatType==hab_type, trend], "increasing|decreasing")
          minYear <- filtered_oy[HabitatType==hab_type, EarliestLiveDate]
          maxYear <- filtered_oy[HabitatType==hab_type, LatestLiveDate]
          if(minYear==maxYear){
            yearStatement <- glue("for the year {minYear}")
          } else {
            yearStatement <- glue("between {minYear} and {maxYear}")
          }
          if(!trend){
            # No significant change
            sentence <- glue("For {tolower(hab_type)} reefs, {param_display} showed no detectable trend {yearStatement}.")
          } else {
            # Increase or decrease
            increase <- filtered_oy[HabitatType==hab_type, ModelEstimate]>0
            slope <- round(abs(filtered_oy[HabitatType==hab_type, ModelEstimate]),2)
            trendStatement <- ifelse(increase, "increased", "decreased")
            sentence <- glue("For {tolower(hab_type)} reefs, {param_display} {trendStatement} by an average of {slope}{units} per year.")
          }
        }
        sentences <- c(sentences, sentence)
      }
      description <- paste(sentences, collapse = " ")
    } else {
      # Shell Height statements
      habitat_order <- c("Natural", "Restored")
      trend_order <- c("trends", "deadShells")

      sentences <- list()

      # Loop through habitat types in desired order
      for(hab_type in habitat_order){
        if(!hab_type %in% unique(filtered_oy$HabitatType)) next
        filtered_oy_ht <- filtered_oy[HabitatType == hab_type & !SizeClass == "" & ShellType == "Live Oysters", ]
        filtered_oy_ht <- filtered_oy_ht %>% rowwise() %>%
          mutate("trend" = ifelse(!is.na(ModelEstimate), trendText(ModelEstimate, LowerConfidence, UpperConfidence), NA)) %>%
          as.data.table()

        num_inc <- nrow(filtered_oy_ht[ModelEstimate > 0 & !str_detect(trend, "No significant change")])
        num_dec <- nrow(filtered_oy_ht[ModelEstimate < 0 & !str_detect(trend, "No significant change")])
        num_no_model <- nrow(filtered_oy_ht[SufficientData==TRUE & is.na(ModelEstimate), ])
        num_insufficient <- nrow(filtered_oy_ht[SufficientData==FALSE, ])
        num_nonsig <- nrow(filtered_oy_ht[str_detect(trend, "No significant change")])
        # Diverging trend (one increase, one decrease)
        if(num_inc>0 & num_dec>0){
          inc_class <- filtered_oy_ht[ModelEstimate > 0, SizeClass]
          inc_slope <- round(abs(filtered_oy_ht[SizeClass==inc_class, ModelEstimate]),2)
          dec_class <- filtered_oy_ht[ModelEstimate < 0, SizeClass]
          dec_slope <- round(abs(filtered_oy_ht[SizeClass==dec_class, ModelEstimate]),2)
          sentence <- glue("For {tolower(hab_type)} reefs, annual average live oyster shell height in the {inc_class} size class increased by {inc_slope} mm per year, and it decreased by {dec_slope}mm per year in the {dec_class} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_inc==1 | num_dec==1){
          # Single direction
          direction <- ifelse(num_inc==1, "increased", "decreased")
          size_class <- ifelse(num_inc==1, filtered_oy_ht[ModelEstimate > 0, SizeClass], filtered_oy_ht[ModelEstimate < 0, SizeClass])
          slope <- round(abs(filtered_oy_ht[SizeClass==size_class, ModelEstimate]),2)
          first_part <- glue("For {tolower(hab_type)} reefs, annual average live oyster shell height in the {size_class} size class {direction} by {slope} mm per year,")
          # Second part
          if(num_no_model>0){
            size_class2 <- filtered_oy_ht[SufficientData==TRUE & is.na(ModelEstimate), unique(SizeClass)]
            second_part <- glue("and a model could not be fitted for live oysters in the {size_class2} size class.")
          }
          if(num_insufficient>0){
            size_class2 <- filtered_oy_ht[SufficientData==FALSE, unique(SizeClass)]
            second_part <- glue("and there was insufficient data to calculate a trend for live oysters in the {size_class2} size class.")
          }
          if(num_nonsig>0){
            size_class2 <- filtered_oy_ht[str_detect(trend, "No significant change"), unique(SizeClass)]
            second_part <- glue("and there was no detectable trend for live oysters in the {size_class2} size class.")
          }
          sentence <- paste(first_part, second_part)
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_inc==2 | num_dec==2) {
          # Same direction
          direction <- ifelse(nrow(filtered_oy_ht[ModelEstimate > 0])>0, "increased", "decreased")
          class1 <- sort(unique(filtered_oy_ht$SizeClass))[2]
          slope1 <- round(abs(filtered_oy_ht[SizeClass==class1, ModelEstimate]),2)
          class2 <- sort(unique(filtered_oy_ht$SizeClass))[1]
          slope2 <- round(abs(filtered_oy_ht[SizeClass==class2, ModelEstimate]),2)
          sentence <- glue("For {tolower(hab_type)} reefs, annual average live oyster shell height in the {class1} and {class2} size classes {direction} by {slope1} mm per year and {slope2} mm per year, respectively.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_no_model==2){
          class1 = sort(unique(filtered_oy_ht[SufficientData==TRUE & is.na(ModelEstimate), SizeClass]))[2]
          class2 = sort(unique(filtered_oy_ht[SufficientData==TRUE & is.na(ModelEstimate), SizeClass]))[1]
          sentence <- glue("For {tolower(hab_type)} reefs, a model could not be fitted for live oysters in either the {class1} or the {class2} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_no_model==1){
          size_class = sort(unique(filtered_oy_ht[SufficientData==TRUE & is.na(ModelEstimate), SizeClass]))
          sentence <- glue("For {tolower(hab_type)} reefs, a model could not be fitted for live oysters in the {size_class} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_insufficient==2){
          class1 = sort(unique(filtered_oy_ht[SufficientData==FALSE, SizeClass]))[2]
          class2 = sort(unique(filtered_oy_ht[SufficientData==FALSE, SizeClass]))[1]
          sentence <- glue("For {tolower(hab_type)} reefs, there was insufficient data to calculate a trend for live oysters in either the {class1} or the {class2} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_insufficient==1){
          size_class = sort(unique(filtered_oy_ht[SufficientData==FALSE, SizeClass]))
          sentence <- glue("For {tolower(hab_type)} reefs, there was insufficient data to calculate a trend for live oysters in the {size_class} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_nonsig==2){
          class1 = sort(unique(filtered_oy_ht[str_detect(trend, "No significant change"), SizeClass]))[2]
          class2 = sort(unique(filtered_oy_ht[str_detect(trend, "No significant change"), SizeClass]))[1]
          sentence <- glue("For {tolower(hab_type)} reefs, there was no detectable trend for live oysters in either the {class1} or the {class2} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        } else if(num_nonsig==1){
          size_class = sort(unique(filtered_oy_ht[str_detect(trend, "No significant change"), SizeClass]))
          sentence <- glue("For {tolower(hab_type)} reefs, there was no detectable trend for live oysters in the {size_class} size class.")
          sentences[[hab_type]][["trends"]] <- sentence
        }
      }

      # If dead shells are present, include statement about lack of models for Dead shells
      if(nrow(filtered_oy[ShellType=="Dead Oyster Shells" & SizeClass!=""])>0){
        dead_sentence <- glue("Models are not run on dead oyster shell measurements.")
        sentences[[hab_type]][["deadShells"]] <- dead_sentence
      }

      if(length(sentences)>0){
        # Build the final paragraph in the desired order
        completeStructure <- c()
        for(ht in habitat_order){
          for(trend_type in trend_order){
            if(!is.null(sentences[[ht]][[trend_type]])){
              completeStructure <- c(completeStructure, sentences[[ht]][[trend_type]])
            }
          }
        }
        description <- paste(completeStructure, collapse = " ")
      }
    }
    # Add paragraph tags
    description <- paste0("<p>", description, "</p>")
    # Make substitutions
    description <- stringi::stri_replace_all_regex(
      description,
      pattern = c(">75"),
      replacement = c("&#8805;75"),
      vectorize = FALSE
    )
    # Create output table
    descriptionText <- data.table(
      "ManagedAreaName" = ma,
      "HabitatName" = "Oyster/Oyster Reef",
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
    descriptionTable <- bind_rows(descriptionTable, descriptionText)
  }
  return(descriptionTable)
}
