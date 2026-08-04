#' SEACAR generate_description function to generate table descriptions for various habitats.
#'
#' @param data a dataframe with statistical analysis results for various habitats.
#' @param habitat a string containing the habitat to generate descriptions for. Options: "Coral", "CW", "Nekton", "Oyster", "SAV", "WC"
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
generate_description <- function(data, habitat, ...){
  habitat <- tolower(habitat)
  setDT(data)
  switch(habitat,
         coral = generate_coral_description(data),
         cw = generate_cw_description(data),
         nekton = generate_nekton_description(data),
         oyster = generate_oyster_description(data, ...),
         sav = generate_sav_description(data),
         wc = generate_wc_description(data),
         stop("Unknown habitat type: ", habitat))
}
