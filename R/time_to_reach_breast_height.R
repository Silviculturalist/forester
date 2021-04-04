#' Time to reach breast height
#'
#' @description Number of years for basal area mean trees to reach breast height, 1.3metres, from a smoothed function to estimate a table by NFI.
#' For Oak (Quercus robur) and Beech (Fagus sylvatica), spruce will be used. For other broadleaves, e.g. (birch, aspen), time to breast height is computed 150/SIS.
#'
#' @param main_species Main species, Spruce or Pine.
#' @param SIS Site Index according to Site factors.
#' @param latitude Decimal degrees.
#'
#' @return Time to reach breast height, years.
#' @export
#'
#' @examples
#' time_to_reach_breast_height(main_species="Spruce",SIS=14,latitude=57)
time_to_reach_breast_height <- function(main_species, SIS, latitude){

  if(main_species%in%c("Betula pendula","Betula pubescens","Populus tremula")){
    return(150/SIS)
  } else if(main_species%in%c("Picea abies","Pinus sylvestris","Fagus sylvatica","Quercus robur")){

    if(main_species != "Pinus sylvestris"){
      spruce_indicator <- 1
    }

    37-0.605*latitude - 1121/SIS + 21.92*latitude/SIS + 29.5*spruce_indicator/SIS
  }

}
