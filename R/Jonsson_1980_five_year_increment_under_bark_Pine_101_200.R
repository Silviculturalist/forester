#' Five years diameter increment under bark, mm for Scots Pine between 101-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 58.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5754
#' Number of trees: 3802
#' Coefficient of multiple correlation: 0.46
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
Jonsson_1980_five_year_increment_under_bark_Pine_101_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<101){
    stop("Pine must be at least 101 years.")
  }

  if(age_at_breast_height>200){
    stop("Pine must be at most 200 years.")
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
    constant <- 29307E-4
    diameter_under_bark <- 11050E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -11558E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 32898E-4
    diameter_under_bark <- 10572E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -13283E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 32707E-4
    diameter_under_bark <- 10625E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -14815E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 35397E-4
    diameter_under_bark <- 79090E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -10288E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 32223E-4
    diameter_under_bark <- 83615E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -94219E-10*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -50156E-6*plot_basal_area_under_bark
    basal_area_sq <- 52911E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -46408E-6
    basal_area_sq <- 70015E-8
  }

  tree_rings_breast_height <- -10870E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 24836E-9*(age_at_breast_height^2)

  rel_diameter <- 21686E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -15957E-4*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -30385E-6*latitude
  alt_meters <- -80738E-8*altitude

  Latitude_x_altitude <- 38252E-10*altitude*latitude

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
