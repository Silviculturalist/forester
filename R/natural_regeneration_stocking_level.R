#' Stocking in Natural regenerations
#'
#' @param altitude altitude masl
#' @param latitude latitude decimal
#' @param age age, years
#' @param proportion_cultivated percent
#' @param N_full
#' @param ground_water
#' @param county
#' @param scarification
#' @param burnt
#' @param no_treatment
#' @param uncleaned
#' @param number_seed_trees
#' @param SIH
#' @param SIS
#' @param area
#'
#' @return
#' @export
#'
#' @examples
regeneration_stocking <- function(altitude, latitude,age, proportion_cultivated, N_full, ground_water, county, scarification, burnt, no_treatment, uncleaned, number_seed_trees, SIH, SIS, area){
  #SI
  if(exists(SIH)){
    si <- SIH
  } else {
    si <- SIS
  }

  #Indicator variable
  if(latitude >= 60){
    northern_sweden <- 1
  } else {
    northern_sweden <- 0
  }

  swe_map <- (((geosphere::distHaversine(c(1,latitude),c(1,0))/1000)-6050)/50)




}
