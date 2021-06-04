#' Five years diameter increment under bark, mm for Scots Pine between 51-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 56.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5270
#' Number of trees: 12091
#' Coefficient of multiple correlation: 0.57
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
#'
#' @examples
Jonsson_1980_five_year_increment_under_bark_Pine_51_125 <- function(
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
    stop("Pine must be at least 51 years.")
  }

  if(age_at_breast_height>125){
    stop("Pine must be at most 125 years.")
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
    constant <- 40431E-4
    diameter_under_bark <- 12880E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -15512E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 37042E-4
    diameter_under_bark <- 17231E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -26288E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 37794E-4
    diameter_under_bark <- 16987E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -28357E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 38093E-4
    diameter_under_bark <- 16951E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -30162E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 41873E-4
    diameter_under_bark <- 12481E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -18713E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -44433E-6*plot_basal_area_under_bark
    basal_area_sq <- 48593-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -43693E-6
    basal_area_sq <- 56681E-8
  }

  tree_rings_breast_height <- -30491E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 12405E-8*(age_at_breast_height^2)

  rel_diameter <- 16031E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -13091E-4*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -32152E-6*latitude
  alt_meters <- -51186E-7*altitude

  Latitude_x_altitude <- 72810E-9*altitude*latitude

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
