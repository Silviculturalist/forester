#' Distance to Swedish Coast
#'
#' @param latitude A decimal latitude.
#' @param longitude A decimal longitude.
#' @param epsg The EPSG code of the projection, e.g. for WGS84: '4326' (default). Or for SWEREF99TM: '3006'.
#'
#'
#' @description Wrapper for sf::st_distance to measure distance between input coordinates and Natural Earth Coastline rnaturalearth::ne_coastline of medium resolution.
#' The world coastline set is very coarsely cut to include mainly Swedish coast.
#'
#'
#' @return Distance to coast, km.
#' @export
#'
#' @examples
#' coast_distance(56.8,14.8)
coast_distance <- function(latitude,longitude, epsg=4326){

  site_spatial_point_df <- data.frame(`lat`=latitude,`long`=longitude)
  site_spatial_point <- site_spatial_point_df %>% sf::st_as_sf(coords=c("long","lat")) %>% sf::st_set_crs(epsg)
  dist <- min(sf::st_distance(site_spatial_point, forester:::clipped))
  return((as.numeric(dist)/1000))
}
