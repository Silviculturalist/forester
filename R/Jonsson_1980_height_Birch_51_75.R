#' Height function for Birch between 51-75 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 98.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1642
#' Number of trees: 960
#' Coefficient of multiple correlation: 0.91
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_51_75 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Birch must be at least 51 years.")
  }

  if(age_at_breast_height>75){
    stop("Birch must be at most 75 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 43441E-4
  } else if(jonson_bonitet==3){
    constant <- 43441E-4
  } else if(jonson_bonitet==4){
    constant <- 43219E-4
  }else if(jonson_bonitet==5){
    constant <- 42372E-4
  }else if(jonson_bonitet==6){
    constant <- 40727E-4
  }else if(jonson_bonitet==7){
    constant <- 39732E-4
  }

  diameter_over_bark <- -14313E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -38309E-2*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 99919E-8*age_at_breast_height

  relative_diameter <- 33084E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -31235E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -15880E-6*latitude
  lat_x_altitude <- -43920E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
