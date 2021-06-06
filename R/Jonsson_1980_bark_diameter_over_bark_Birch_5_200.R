#' Bark function for Birch between 5-200 yr with diameter over bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 114.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.244
#' Number of trees: 4322
#' Coefficient of multiple correlation: 0.85
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height over bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
#'
#' @examples
Jonsson_1980_bark_diameter_over_bark_Birch_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Birch must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 14664E-4
  } else if(jonson_bonitet==4){
    constant <- 14937E-4
  }else if(jonson_bonitet==5){
    constant <- 15322E-4
  }else if(jonson_bonitet==6){
    constant <- 15573E-4
  }else if(jonson_bonitet>=7){
    constant <- 15934E-4
  }

  diameter_over_bark <- 80760E-7*(diameter_at_breast_height)
  diameter_over_bark_sq <- -87005E-10*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 67800E-7*age_at_breast_height
  tree_rings_breast_height_sq <- -16946E-9*age_at_breast_height^2

  lat <- -68541E-7*latitude
  alt <- 21082E-8*altitude
  lat_x_altitude <- -16573E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
