#' SEACAR addCircleLegend to maps
#'
#' A customized function to add Circle Legends to static SEACAR Atlas maps. Modified from GitHub user PaulC91:
#' https://gist.github.com/PaulC91/31c05f84b25975047092f13c2474507a
#'
#' @return A `leaflet`-based function
#' @export
#'
#' @examples
#' library(leaflet)
#'
#' calc_radius_sav <- function(n){sqrt(n)}
#'
#' map <- map %>%
#'   addCircleMarkers(data = pts,
#'                    lat=~Latitude_D, lng=~Longitude_,
#'                    weight=1, color = "#000000", stroke = TRUE,
#'                    fillColor = ~color,
#'                    radius=calc_radius_sav(pts$n_data),
#'                    fillOpacity=~alpha)
#'
#' map <- leaflet() %>% addTiles() %>%
#'   addCircleLegend(title = "Number of samples",
#'                   range = c(lns$n_data, pts$n_data),
#'                   scaling_fun = calc_radius_sav,
#'                   fillColor = "#b3b3b3",
#'                   fillOpacity = 0.8,
#'                   weight = 1,
#'                   color = "#000000",
#'                   position = "topright",
#'                   type = "sav")
addCircleLegend <- function(
    map, title = "", range, scaling_fun, ...,
    color, weight, fillColor, fillOpacity,
    position = c("topright", "bottomright", "bottomleft", "topleft"),
    data = leaflet::getMapData(map), layerId = NULL,
    type = "discrete") {

  # Function to determine the number of circles to display, account for different habitat types
  determine_range_size <- function(range, thresholds) {
    range_diff <- abs(min(range) - max(range))
    max_range <- max(range)
    min_range <- min(range)

    if (range_diff > thresholds$large_cutoff) {
      if (max_range <= thresholds$upper_limit | min_range >= max_range * thresholds$relative_threshold) {
        if (range_diff / max_range >= thresholds$tight_lower | range_diff / max_range <= thresholds$tight_upper) {
          return("small")
        } else {
          return("medium")
        }
      } else {
        return("large")
      }
    } else {
      return("small")
    }
  }

  # Define thresholds for continuous and discrete types
  thresholds_list <- list(
    continuous = list(large_cutoff = 3000, upper_limit = 70000, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    discrete = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    cw = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    coral = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    nekton = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    oyster = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    sav = list(large_cutoff = 60, upper_limit = 120, relative_threshold = 0.6, tight_lower = 0.95, tight_upper = 0.15)
  )

  # Apply function based on type
  if (type %in% names(thresholds_list)) {
    range_size <- determine_range_size(range, thresholds_list[[type]])
  }

  if(range_size %in% c("medium","large")){range <- base::pretty(sort(range), 20)}
  range <- range[range != 0]
  min_n <- ceiling(min(range, na.rm = TRUE))
  med_n <- round(median(range, na.rm = TRUE), 0)
  max_n <- round(max(range, na.rm = TRUE), 0)
  if(range_size=="small"){n_range<-max_n} else if(range_size=="medium"){
    n_range<-c(min_n, max_n)} else {n_range<-c(min_n, med_n, max_n)}
  radii <- scaling_fun(n_range, ...)
  n_range <- scales::label_number()(n_range)

  circle_style <- glue::glue(
    "border-radius:50%;
    border: {weight}px solid {color};
    background: {paste0(fillColor, round(fillOpacity*100, 0))};
    position: absolute;
    bottom:1px;
    right:25%;
    left:50%;"
  )

  text_style <- glue::glue(
    "text-align: right;
    font-size: 11px;
    position: absolute;
    bottom:3px;
    right:1px;"
  )

  buffer <-  max(radii)

  size_map <- list(
    large = c(3, 2, 1),
    medium = c(2, 1),
    small = c(1)
  )

  # Logic to account for different size combinations, write HTML
  sizes <- size_map[[range_size]]

  circle_elements <- glue::glue_collapse(
    glue::glue(
      '<div class="legendCircle" style="width: {radii[s] * 2}px; height: {radii[s] * 2}px; margin-left: {-radii[s]}px; {circle_style}"></div>',
      s = sizes
    ),
    sep = "\n"
  )

  value_elements <- glue::glue_collapse(
    glue::glue(
      '<div><p class="legendValue" style="margin-bottom: {radii[s] * 2 - 12}px; {text_style}">{n_range[s]}</p></div>',
      s = sizes
    ),
    sep = "\n"
  )

  min_width <- radii[max(sizes)] * 2 + buffer
  min_height <- radii[max(sizes)] * 2 + 12

  circle_legend <- htmltools::HTML(glue::glue(
    '<div class="bubble-legend">
    <div id="legendTitle" style="text-align: center; font-weight: bold;">{title}</div>
    <div class="symbolsContainer" style="min-width: {min_width}px; min-height: {min_height}px;">
      {circle_elements}
      {value_elements}
    </div>
  </div>'
  ))

  return(
    leaflet::addControl(map, html = circle_legend, position = position, layerId = layerId)
  )
}
