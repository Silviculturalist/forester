#' Height function for Norway Spruce between 101-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 92.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1340
#' Number of trees: 2050
#' Coefficient of multiple correlation: 0.89
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
#'
#' @examples
Jonsson_1980_height_Spruce_126_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<126){
    stop("Spruce must be at least 126 years.")
  }

  if(age_at_breast_height>200){
    stop("Spruce must be at most 200 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 47511E-4
  } else if(jonson_bonitet==3){
    constant <- 47511E-4
  } else if(jonson_bonitet==4){
    constant <- 46367E-4
  }else if(jonson_bonitet==5){
    constant <- 45815E-4
  }else if(jonson_bonitet==6){
    constant <- 45121E-4
  }else if(jonson_bonitet==7){
    constant <- 44052E-4
  }

  diameter_over_bark <- -25612E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 49560E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -81180E-8*age_at_breast_height

  relative_diameter <- 13814E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -63065E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -13037E-6*latitude
  lat_x_altitude <- -33171E-10*altitude*latitude

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
