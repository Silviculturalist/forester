#' Humidity according to polygons from the Swedish National Forest Inventory.
#'
#' @description Provides simple interface for an estimation of the humidity during the vegetation period for Swedish sites.
#'
#' The humidity during the vegetation period is 'the difference between precipitation amount and 'real' evapotranspiration.'.
#'
#' @source The polygons delineating contour lines were provided from the Swedish National Forest Inventory.
#' These polygons origins are unknown, but are very close to fig. 34 'Humidity during the vegetation period (mm)' from Eriksson, B. (1986) Nederbörds- och humiditetsklimatet i Sverige under vegetationsperiod. Sveriges Meterologiska och Hydrologiska Institut (SMHI), rapporter i meteorologi och klimatologi (RMK) 46. Available:
#' \url{https://www.smhi.se/polopoly_fs/1.166663!/RMK_46\%20Nederbörds-\%20och\%20humiditetsklimat\%20i\%20Sverige\%20under\%20vegetationsperioden..pdf}
#'
#' @param latitude Latitude degrees
#' @param longitude Longitude degrees
#' @param epsg Reference system, default 4326 (WGS84). For SWEREF99TM, 3006.
#'
#' @return Humidity during the vegetation period (mm).
#' @export
#'
#' @examples Eriksson_1986_humidity(latitude = 64.4,longitude = 17.8,epsg=4326)
Eriksson_1986_humidity <- function(latitude,
                                   longitude,
                                   epsg=4326){

  #Stop if lat lon are wrong for SWEREF99
  if(epsg==3006 & longitude>1E6){
    stop("Latitude and Longitude have been mixed up.")
  }

  point <- st_sfc(sf::st_point(x=c(longitude,latitude)))
  st_crs(point) <- epsg

  #Reproject point to WGS84 if epsg is not 4326.
  if(epsg!=4326){
    assign(x = "point",value = st_transform(point,crs=4326))
  }

  return(forester:::humiditet$humiditet[st_within(point,forester:::humiditet)[[1]]])

}
