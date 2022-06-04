#' Five years diameter increment under bark, mm for Norway Spruce between 51-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 60.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5678
#' Number of trees: 12196
#' Coefficient of multiple correlation: 0.58
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Spruce_51_125 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Spruce must be at least 51 years.")
  }

  if(age_at_breast_height>125){
    stop("Spruce must be at most 125 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 19384E-4
    diameter_under_bark <- 13907E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -18357E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 21253E-4
    diameter_under_bark <- 12479E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -16357E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 19763E-4
    diameter_under_bark <- 14570E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -23371E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 20111E-4
    diameter_under_bark <- 13878E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -21250E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 19281E-4
    diameter_under_bark <- 13958E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -23284E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -25085E-6*plot_basal_area_under_bark
    basal_area_sq <- 15953E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -23506E-6
    basal_area_sq <- 21414E-8
  }

  tree_rings_breast_height <- -19469E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 54430E-9*(age_at_breast_height^2)

  rel_diameter <- 10552E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -92733E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -49391E-7*latitude
  alt_meters <- -29436E-7*altitude

  Latitude_x_altitude <- 44066E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    rel_diameter+
    rel_diameter_sq+
    lat_values+
    alt_meters+
    Latitude_x_altitude

  return(exp(normal_growth_mm))

}
