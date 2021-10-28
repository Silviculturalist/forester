#' Sweden local climates
#'
#' @description Finds the local climate according to Ångström (1958)
#'
#' @param latitude Latitude, decimal
#' @param longitude Longitude, decimal
#' @param epsg Reference system. Default 'WGS84': 4326. For SWEREF99TM: '3006'.
#'
#' @return Local Swedish Climate Code.
#' @export
Angstrom_1958_local_climate_Sweden <- function(latitude, longitude,epsg){

  if(missing(latitude) | missing(longitude)) stop("Cannot calculate local climate, requires Latitude & Longitude.")

  #Stop if lat lon are wrong for SWEREF99TM.
  if(epsg==3006 & longitude>1E6){
    stop("Latitude and Longitude have been mixed up.")
  }

  point <- sf::st_sfc(sf::st_point(x=c(longitude,latitude)))
  sf::st_crs(point) <- epsg

  #Reproject to SWEREF99TM if epsg is not 3006.
  if(epsg!=3006){
    assign(x="point",value=sf::st_transform(point,crs=3006))
  }

  county_code <- as.character(forester:::sweden_local_climates$KLIMATZON[sf::st_within(point, forester:::sweden_local_climates)[[1]]])

  return(county_code)

}



