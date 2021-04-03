#' Distance to Coast
#'
#' @param latitude A decimal latitude WGS84.
#' @param longitude A decimal longitude WGS84.
#' @param detail Determines resolution of Coastline. 'medium' or 'large'. See rnaturalearth::ne_coastline for detail.
#' @param unit 'm' or 'km'.
#'
#' @description Wrapper for sf::st_distance to measure distance between input coordinates and Natural Earth Coastline rnaturalearth::ne_coastline.
#' For best results, use detail: 'large'. For faster processing time, use 'medium' or 'low'.
#' @return Distance to coast in km.
#' @export
#'
#' @examples
#' coast_distance(56.8,14.8)
coast_distance <- function(latitude,longitude, detail='large', unit='km'){
  world_coastline <- rnaturalearth::ne_coastline(scale=detail,'sf') %>% st_transform(4326)
  site_spatial_point_df <- data.frame(`lat`=latitude,`long`=longitude)
  site_spatial_point <- site_spatial_point_df %>% st_as_sf(coords=c("long","lat")) %>% st_set_crs(4326)

  dist <- min(st_distance(site_spatial_point, world_coastline))

  dist <- as.numeric(dist)

  if(unit=='km'){
    dist <- dist/1000
  }

  return(dist)
}
