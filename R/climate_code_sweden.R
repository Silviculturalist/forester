#' Sweden local climates
#'
#' @description Finds the local climate according to Ångström (1958)
#' \strong{OBSERVE! Latitude and Longitude should be provided in WGS84 reference system.}
#'
#' @param latitude Latitude, decimal
#' @param longitude Longitude, decimal
#'
#' @return Local Swedish Climate Code.
#' @export
local_climate_sweden <- function(latitude, longitude){

  if(missing(latitude) | missing(longitude)) stop("Cannot calculate local climate, requires Latitude & Longitude.")

  warning("Local Climate is calculated from coordinates. Coordinates assumed WGS84. Reprojected to SWEREF99TM.")
  coord_list <- data.frame('latitude'=latitude,'longitude'=longitude)
  sp::coordinates(coord_list) <- ~longitude+latitude
  sp::proj4string(coord_list) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  coord_list <- sp::spTransform(coord_list,"+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  county_code <- sp::over(coord_list,forester::sweden_local_climates)$KLIMATZON

  return(county_code)



}



