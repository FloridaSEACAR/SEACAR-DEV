#' Clean and split concatenated Managed Area names
#'
#' This function processes a dataframe containing a column named
#' `ManagedAreaName` where each entry is formatted as `"ID - Name"` and
#' some entries may contain multiple concatenated values separated by `"/"`.
#' Performs the same operations on `AreaID`, ensuring they are consistent with
#' the `AreaID` values derived from the `ManagedAreaName` concatenation.
#'
#' The function performs two operations:
#' \enumerate{
#'   \item Splits rows where multiple managed areas are concatenated using `"/"`,
#'         duplicating all other column values accordingly.
#'   \item Removes the leading `"ID - "` portion of each managed area string,
#'         returning only the cleaned area name.
#' }
#'
#' @param df A dataframe that contains a column named `ManagedAreaName`.
#'
#' @return A dataframe in which:
#'   \itemize{
#'     \item Rows with multiple managed areas have been split into separate rows.
#'     \item The `ManagedAreaName` column contains only the cleaned area names.
#'   }
#'
#' @examples
#' df <- data.frame(
#'   ManagedAreaName = c("1 - NameA/3 - NameB", "2 - NameC"),
#'   OtherColumn = c("X", "Y")
#' )
#'
#' clean_managed_areas(df)
#'
#' @import dplyr tidyr stringr
#' @export
clean_managed_areas <- function(df) {
  df <- df %>% mutate(.row_id = row_number())
  # Long form of AreaID
  area_long <- df %>%
    select(.row_id, AreaID) %>%
    separate_rows(AreaID, sep = "/", convert = FALSE) %>%
    mutate(AreaID = str_trim(AreaID))

  # Long form of ManagedAreaName, extracting IDs and names
  name_long <- df %>%
    select(.row_id, ManagedAreaName) %>%
    separate_rows(ManagedAreaName, sep = "/") %>%
    mutate(
      ManagedAreaName = str_trim(ManagedAreaName),
      ID_extracted = str_extract(ManagedAreaName, "^\\d+"),
      Name_clean   = str_replace(ManagedAreaName, "^\\d+\\s*-\\s*", "")
    )

  # Join both by row_id + numeric ID
  result <- area_long %>%
    inner_join(
      name_long,
      by = c(".row_id", "AreaID" = "ID_extracted")
    ) %>%
    select(-ManagedAreaName) %>%
    rename(ManagedAreaName = Name_clean) %>%
    left_join(df %>% select(-AreaID, -ManagedAreaName),
              by = ".row_id") %>%
    select(-.row_id) %>%
    mutate(AreaID = as.numeric(AreaID))

  result
}
