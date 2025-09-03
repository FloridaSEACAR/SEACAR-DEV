library(roxygen2)
library(data.table)
library(dplyr)
library(ggplot2)
library(extrafont)

ManagedAreas <- fread("inst/extdata/ManagedArea.csv", na.strings = "")
usethis::use_data(ManagedAreas, overwrite = TRUE)

WebsiteParameters <- fread("inst/extdata/WebsiteParameters.csv", na.strings = "")
usethis::use_data(WebsiteParameters, overwrite = TRUE)

FigureCaptions <- openxlsx::read.xlsx("inst/extdata/AtlasFigureCaptions_Final.xlsx") %>%
  mutate(FigureCaptions = stringi::stri_replace_all_regex(
    FigureCaptions,
    pattern = c("<p>", "</p>"),
    replacement = c("", ""),
    vectorize = FALSE
  )) %>%
  as.data.table()
usethis::use_data(FigureCaptions, overwrite = TRUE)

TableDescriptions <- openxlsx::read.xlsx("inst/extdata/Atlas_Descriptions_2025-05-15_Final.xlsx")
usethis::use_data(TableDescriptions, overwrite = TRUE)

DB_Thresholds <- openxlsx::read.xlsx("inst/extdata/SEACAR_Metadata.xlsx", sheet = "Ref_QAThresholds", startRow = 7, check.names = F)
names(DB_Thresholds) <- gsub("\\.", "", names(DB_Thresholds))
DB_Thresholds %<>% rename("QuadSize_m2" = "QuadSizem2") %>% as.data.table()
usethis::use_data(DB_Thresholds, overwrite = TRUE)

##### load_shape_files.R
# Load and transform location-based .shp files from latest export
source("../SEACAR_Trend_Analyses/SEACAR_data_location.R")
shape_files <- list.files(seacar_shape_location, full=T)
library(sf)
# Modify GeoDBdate to match date of the latest geodatabase export (i.e., SampleLocationsddMMYYY)
GeoDBdate <- "2jun2025"
locs_pts <- st_read(paste0(seacar_shape_location, "/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Point.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)
locs_lns <- st_read(paste0(seacar_shape_location, "/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Line.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)
# Ensure we are using latest RCP shape file
rcp <- st_read(paste0(seacar_shape_location, "/RCP/Boundaries_2025_8_27/ORCP_Managed_Areas_Apr2025.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)

# Create "corners" crosswalk to provide min and max values
# Declare "coast" min/max values to create groupings (Atlantic, Gulf, Panhandle)
coasts <- data.frame(
  "Coast" = c("Atlantic", "Gulf", "Panhandle"),
  "xmin" = c(-83.200, -84.434, -87.487),
  "xmax" = c(-78.9500, -81.2825, -84.2880),
  "ymin" = c(24.30, 25.79, 29.55),
  "ymax" = c(30.7352, 30.5650, 30.6110)
)
# Function to assign coast status
assign_coast <- function(xmin, xmax, ymin, ymax){
  matched_coast <- coasts %>%
    filter(
      xmin <= !!xmin,
      xmax >= !!xmax,
      ymin <= !!ymin,
      ymax >= !!ymax
    ) %>%
    pull(Coast)
  if(length(matched_coast) == 1) {
    return(matched_coast)
  } else if (length(matched_coast) > 1) {
    return("Gulf") # Account for overlaps with Atlantic/Gulf
  } else {
    return(NA)
  }
}

corners <- data.frame()
for(ma in rcp$LONG_NAME){
  subset <- rcp %>% filter(LONG_NAME==ma)
  corner <- SEACAR::get_shape_coordinates(subset)
  corner$LONG_NAME <- ma
  corners <- bind_rows(corners, corner)
}

corners <- corners %>%
  rowwise() %>%
  mutate(Coast = assign_coast(xmin, xmax, ymin, ymax),
         xmax = xmax + (xmax-xmin)*0.25,
         ymax = ymax + (ymax-ymin)*0.1) %>%
  ungroup()

GeoData <- list("pointLocations" = locs_pts,
                "lineLocations" = locs_lns,
                "RCP Boundaries" = rcp,
                "corners" = corners)
usethis::use_data(GeoData, overwrite = TRUE)
