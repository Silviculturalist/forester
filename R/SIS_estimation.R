#' Site index estimation by site factors
#'
#' @description Corrected functions from Appendix II for estimation of site index by site factors according to Hägglund and Lundmark (1977), in
#' Hägglund, B. (1979) Ett system för bonitering av skogsmark - analys, kontroll och diskussion inför praktisk tillämpning. Rapport 14, Projekt HUGIN. Skogsvetenskapliga fakulteten, Sveriges Lantbruksuniversitet. Umeå. Sweden.
#'
#' @param species Spruce or Pine
#' @param vegetation Type
#' @param latitude Decimal degrees.
#' @param longitude Decimal degrees.
#' @param altitude Metres above sea level
#' @param soil_moisture Type
#' @param soil_texture Type
#' @param climate_code Optional, else from lat, lon.
#' @param county Optional, else from lat/lon
#'
#' @return Site index, in metres.
#' @export


SIS_estimate <- function(species, vegetation, latitude, longitude, altitude, aspect.main, incline.percent, soil_moisture, soil_texture, local_climate, county){

  assertive.numbers::assert_all_are_whole_numbers(c(vegetation, soil_moisture,soil_texture),tol = 0)

  if(!aspect.main %in% c("North","South","East","West",0)) stop("aspect.main must be 'North', 'South', 'East', 'West' or 0")



#County calc
if(!exists(county)){
  #county <- #calc county
}

#incline calculation
#NB IF ABOVE 2:20 , 5.71° , 10%.
#Applicable if Pine on North or East aspect above 350 MASL
if(incline.percent>10 & aspect.main %in% c("North","East")){
  ne_incline <- TRUE
} else {
  ne_incline <- FALSE
}

if(incline.percent<=10){
  no_incline <- TRUE
} else {
  no_incline <- FALSE
}

#Climate Code calc.
if(missing(local_climate)){
  local_climate <- forester::local_climate_sweden(latitude=latitude,longitude = longitude)
}


#D correction value.
D <-  altitude + 130*latitude - 8900

#G1, G2 correction coefficient
#DEVELOPMENT OBS: COEFFICIENT OR CONSTANT?
g1 <- if(D<=-60){
  1.05
  } else if(-60<=D & D<0){
    (1-(D/1200))
  } else if(D>=0){
    1
  }

g2 <- if(D<0){
  1
} else if(D <= 0 & D < 60){
  (1-(D/600))
} else if(D>=60){
  0.9
}


#Function choice



# ln(h100) in dm to metres.

h100 <- (exp(lnh100dm))/10


}


#testing gitkraken change
