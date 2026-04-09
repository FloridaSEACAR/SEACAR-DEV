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
clean_managed_areas <- function(df, type) {
  if (type %in% c("ma", "buff")) {
    ma_col   <- if (type == "ma") "ManagedAreaName" else "ManagedAreaName_Buff"
    a_id_col <- if (type == "ma") "AreaID" else "AreaID_Buff"
  } else {
    stop("Input `type` must be either 'ma' or 'buff'.")
  }

  ldf <- as_polars_lf(df)$with_row_index(".row_id")

  area_long <- ldf$
    select(
      ".row_id",
      pl$col(a_id_col)$alias("a_id")
    )$
    with_columns(
      pl$col("a_id")$str$split("/")$alias("a_id")
    )$
    explode("a_id")$
    with_columns(
      pl$col("a_id")$str$strip_chars()$alias("a_id")
    )

  name_long <- ldf$
    select(
      ".row_id",
      pl$col(ma_col)$alias("ma")
    )$
    with_columns(
      pl$col("ma")$str$split("/")$alias("ma")
    )$
    explode("ma")$
    with_columns(
      pl$col("ma")$str$strip_chars()$alias("ma"),
      pl$col("ma")$str$extract("^(\\d+)", 1)$alias("ID_extracted"),
      pl$col("ma")$str$replace("^\\d+\\s*-\\s*", "")$alias("Name_clean")
    )

  df_other <- ldf$drop(c(a_id_col, ma_col))

  result <- area_long$
    join(
      name_long,
      left_on = c(".row_id", "a_id"),
      right_on = c(".row_id", "ID_extracted"),
      how = "inner"
    )$
    with_columns(
      pl$col("Name_clean")$alias(ma_col),
      pl$col("a_id")$alias(a_id_col)
    )$
    drop(c("ma", "Name_clean", "a_id"))$
    join(
      df_other,
      on = ".row_id",
      how = "left"
    )$
    drop(".row_id")$
    with_columns(
      pl$col(a_id_col)$cast(pl$Float64)
    )$
    collect() |>
    as.data.table()

  result
}
