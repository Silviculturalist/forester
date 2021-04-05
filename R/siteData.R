#' siteData
#'
#' @description Initiates a siteData object, containing information about the plot site.
#'
#' @param stand_id Stand ID
#' @param main_species Main species.
#' @param distance_to_coast if there is no coordinate information
#' @param aspect_main Main aspect (N,E,S,W or 0)
#' @param incline_percent degrees.
#' @param altitude Metres above sea level
#' @param latitude Latitude decimal WGS84
#' @param longitude Longitude decimal WGS84
#' @param polygon Optional List of boundary coordinates. Warning siteData does not check validity of polygon geometry.
#' @param county County in Sweden. If not supplied, will calculate through \link[forester]{county_sweden}
#' @param local_climate Swedish Local Climate Code. If not supplied, will calculate through \link[forester]{local_climate_sweden}
#' @param vegetation NFI Vegetation code (FsKod) 1-18
#' @param ground_layer Type 1="Lichen type", 2="Lichen-rich bogmoss type", 3="Lichen-rich",4="Bogmoss type" Sphagnum,5="Swamp moss type",6="Fresh moss type"
#' @param soil_texture 1-9
#' @param soil_moisture Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#' @param soil_depth Type 1="Deep, >70cm", 2 = "Rather shallow, 20-70 cm", 3 = "Shallow <20", 4 = "Varying"
#' @param lateral_water Type 1="Missing", 2="Seldom",3="shorter periods",4="longer periods",5="slope".
#' @param ditched TRUE/FALSE if affected by ditching.
#' @param temperature_sum.c Degrees celsius. If not supplied, will calculate from \link[forester]{odin_tsum_1983}
#'
#' @return
#' @export
#'
#' @examples
siteData <- function(stand_id, altitude, latitude, longitude, distance_to_coast, polygon, county, local_climate, vegetation, soil_texture, soil_moisture, temperature_sum.c, aspect,incline){

  if(missing(altitude)) stop("altitude is required")
  if(missing(latitude)) stop("latitude is required")
  if(missing(vegetation)) stop("vegetation is required")
  if(missing(soil_texture)) stop("soil_texture is required")
  if(missing(soil_moisture)) stop("soil_moisture is required")
  if(!missing(polygon)){
    message("Warning: Polygon geometry not validated by forester::siteData")
  } else if(missing(polygon)){
    polygon <- NULL
  }


  if(missing(county)){
    county <- county_sweden(latitude = latitude,longitude = longitude)
  }

  if(missing(local_climate)){
    local_climate <- forester::local_climate_sweden(latitude = latitude,longitude = longitude)
  }

  temperature_sum.c <- if(missing(temperature_sum.c)){
    forester::odin_tsum_1983(latitude = latitude, altitude = altitude)
  }

  if(missing(distance_to_coast)){
    distance_to_coast <- forester::coast_distance(latitude = latitude, longitude = longitude)
  }

  if(missing(incline)){
    incline <- 0
    message("No incline given, setting incline to 0.")
  }

  if(missing(aspect)){
    aspect <- 0
    message("No aspect given, setting aspect to 0.")
  }




  value <- list("stand_id"=stand_id,"altitude"=altitude, "latitude"=latitude, "longitude"=longitude, "polygon"=polygon, "county"=county, "local_climate"=local_climate, "vegetation"=vegetation, "soil_texture"=soil_texture, "soil_moisture"=soil_moisture, "temperature_sum.c"=temperature_sum.c, "aspect"=aspect, "incline"=incline)

  attr(value, "class") <- "siteData"

  value

}

