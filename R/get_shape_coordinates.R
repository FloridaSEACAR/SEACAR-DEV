#' SEACAR get_shape_coordinates function to provide coordinate min and max to allow for accurate setting of viewport on Leaflet maps
#'
#' @param ma_shape a single-line sf dataframe (output from SEACAR::find_shape())
#'
#' @return A data.frame with 4 values corresponding to min and max coordinate values: xmin, ymin, xmax, ymax
#' @export
#'
#' @examples
#' shape_coordinates <- get_shape_coordinates(ma_shape)
#' map <- leaflet() %>% addTiles() %>%
#'   fitBounds(lng1=shape_coordinates$xmin,
#'             lat1=shape_coordinates$ymin,
#'             lng2=shape_coordinates$xmax,
#'             lat2=shape_coordinates$ymax)
get_shape_coordinates <- function(ma_shape){
  bbox_list <- lapply(st_geometry(ma_shape), st_bbox)
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(ma_shape)))
  names(maxmin) <- names(bbox_list[[1]])
  return(maxmin)
}
