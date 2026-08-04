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
#' clean_managed_areas(df, type = "ma")
#'
#' @import dplyr tidyr stringr polars data.table
#' @export
clean_managed_areas <- function(df, type, keep_na = TRUE){
  if(type %in% c("ma", "buff")){
    ma_col   <- if(type == "ma") "ManagedAreaName" else "ManagedAreaName_Buff"
    a_id_col <- if(type == "ma") "AreaID" else "AreaID_Buff"
  } else {
    stop("Input `type` must be either 'ma' or 'buff'.")
  }

  if(!is.logical(keep_na) ||
      length(keep_na) != 1L || is.na(keep_na)){
    stop("Input `keep_na` must be either TRUE or FALSE.")
  }

  # Save the original column order
  original_cols <- names(df)

  # Ensure splitting and regex operations use character columns
  df[[a_id_col]] <- as.character(df[[a_id_col]])
  df[[ma_col]]   <- as.character(df[[ma_col]])

  ldf <- as_polars_lf(df)$with_row_index(".row_id")

  # Only process rows that have values in both managed-area columns.
  # Rows where only one column is NA continue to be excluded.
  valid_ldf <- ldf$
    filter(
      pl$col(a_id_col)$is_not_null() & pl$col(ma_col)$is_not_null()
    )

  # Long form of AreaID
  area_long <- valid_ldf$
    select(
      ".row_id",
      pl$col(a_id_col)$alias("a_id")
    )$
    with_columns(
      pl$col("a_id")$
        str$split("/")$
        alias("a_id")
    )$
    explode("a_id")$
    with_columns(
      pl$col("a_id")$
        str$strip_chars()$
        alias("a_id")
    )

  # Long form of ManagedAreaName
  name_long <- valid_ldf$
    select(
      ".row_id",
      pl$col(ma_col)$alias("ma")
    )$
    with_columns(
      pl$col("ma")$
        str$split("/")$
        alias("ma")
    )$
    explode("ma")$
    with_columns(
      pl$col("ma")$
        str$strip_chars()$
        alias("ma"),

      pl$col("ma")$
        str$extract("^(\\d+)", 1)$
        alias("ID_extracted"),

      pl$col("ma")$
        str$replace("^\\d+\\s*-\\s*", "")$
        alias("Name_clean")
    )

  # All remaining original columns
  df_other <- valid_ldf$
    drop(c(a_id_col, ma_col))

  # Match AreaID values to IDs extracted from ManagedAreaName
  processed <- area_long$
    join(
      name_long,
      left_on  = c(".row_id", "a_id"),
      right_on = c(".row_id", "ID_extracted"),
      how = "inner"
    )$
    with_columns(
      pl$col("Name_clean")$
        alias(ma_col),

      pl$col("a_id")$
        cast(pl$Float64)$
        alias(a_id_col)
    )$
    drop(c("ma", "Name_clean", "a_id"))$
    join(
      df_other,
      on = ".row_id",
      how = "left"
    )$
    select(!!!c(".row_id", original_cols))

  if(keep_na){
    # Preserve rows where both corresponding fields are NA
    na_rows <- ldf$
      filter(
        pl$col(a_id_col)$is_null() &
          pl$col(ma_col)$is_null()
      )$
      with_columns(
        # Match the output datatype of processed AreaID values
        pl$col(a_id_col)$cast(pl$Float64)
      )$
      select(!!!c(".row_id", original_cols))

    result_lf <- pl$concat(
      processed,
      na_rows,
      how = "vertical_relaxed"
    )$
      sort(".row_id")

  } else {
    result_lf <- processed
  }

  result <- result_lf$
    drop(".row_id")$
    collect() |>
    as.data.table()

  result
}
