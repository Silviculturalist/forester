#' siteData
#'
#' @description Initiates a siteData object, containing information about the plot site.
#'
#' @param altitude Metres above sea level
#' @param latitude Latitude decimal WGS84
#' @param longitude Longitude decimal WGS84
#' @param polygon Optional List of boundary coordinates.
#' @param county County in Sweden. If not supplied, will calculate through \link[forester]{county_sweden}
#' @param local_climate Swedish Local Climate Code. If not supplied, will calculate through \link[forester]{local_climate_sweden}
#' @param vegetation NFI Vegetation code (FsKod) 1-18
#' @param soil_texture 1-9
#' @param soil_moisture 1-5
#' @param temperature_sum.c Degrees celsius. If not supplied, will calculate from \link[forester]{odin_tsum_1983}
#'
#' @return
#' @export
#'
#' @examples
siteData <- function(altitude, latitude, longitude, polygon, county, local_climate, vegetation, soil_texture, soil_moisture, temperature_sum.c, aspect,incline){

  if(missing(altitude)) stop("altitude is required")
  if(missing(latitude)) stop("latitude is required")
  if(missing(vegetation)) stop("vegetation is required")
  if(missing(soil_texture)) stop("soil_texture is required")
  if(missing(soil_moisture)) stop("soil_moisture is required")

  if(missing(county)){
    county <- county_sweden(latitude = latitude,longitude = longitude)
  }

  if(missing(local_climate)){
    local_climate <- forester::local_climate_sweden(latitude = latitude,longitude = longitude)
  }

  temperature_sum.c <- if(!exists(temperature_sum.c)){
    forester::odin_tsum_1983(lat)
  }


  value <- list(main_species, altitude, latitude, longitude, polygon, county, local_climate, vegetation, soil_texture, soil_moister, temperature_sum.c, aspect, incline)

  attr(value, "class") <- "siteData"

  value

}
