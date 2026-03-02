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
#' @param df A dataframe that contains the column to be modified, either `ManagedAreaName` or `ManagedAreaName_Buff`.
#' @param type A string value to indicate the name of the column to be modified:
#'  \itemize{
#'    \item `ma` to modify the `ManagedAreaName` and `AreaID` columns.
#'    \item `buff` to modify the `ManagedAreaName_Buff` and `AreaID_Buff` columns.
#'  }
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
clean_managed_areas <- function(df, type) {
  df <- df %>% mutate(.row_id = row_number())
  if(type %in% c("ma", "buff")){
    ma_col <- ifelse(type=="ma", "ManagedAreaName", "ManagedAreaName_Buff")
    a_id_col <- ifelse(type=="ma", "AreaID", "AreaID_Buff")
  } else {
    stop("Input `type` must be either 'ma' or 'buff'.")
  }
  # Long form of AreaID
  area_long <- df %>%
    dplyr::select(.row_id, a_id = dplyr::all_of(a_id_col)) %>%
    tidyr::separate_rows(a_id, sep = "/", convert = FALSE) %>%
    dplyr::mutate(a_id = str_trim(a_id))

  # Long form of ManagedAreaName, extracting IDs and names
  name_long <- df %>%
    dplyr::select(.row_id, ma = dplyr::all_of(ma_col)) %>%
    tidyr::separate_rows(ma, sep = "/") %>%
    dplyr::mutate(
      ma = stringr::str_trim(ma),
      ID_extracted = stringr::str_extract(ma, "^\\d+"),
      Name_clean   = stringr::str_replace(ma, "^\\d+\\s*-\\s*", "")
    )

  # Join both by row_id + numeric ID
  result <- area_long %>%
    dplyr::inner_join(
      name_long,
      by = c(".row_id", "a_id" = "ID_extracted")
    ) %>%
    dplyr::select(-ma) %>%
    dplyr::rename(
      !!ma_col := Name_clean,
      !!a_id_col := a_id
    ) %>%
    dplyr::left_join(
      df %>% dplyr::select(-dplyr::all_of(c(a_id_col, ma_col))),
      by = ".row_id") %>%
    dplyr::select(-.row_id) %>%
    dplyr::mutate(!!a_id_col := as.numeric(.data[[a_id_col]]))

  result
}
