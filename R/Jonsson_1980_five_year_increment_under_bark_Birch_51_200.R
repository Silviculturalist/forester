#' Five years diameter increment under bark, mm for Birch between 51-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 68.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.7274
#' Number of trees: 2091
#' Coefficient of multiple correlation: 0.54
#'
#'
#' @param jonson_bonitet 1-6 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#'
#' @return mm, diameter increment during five years.
#' @export
#'
#' @examples
Jonsson_1980_five_year_increment_under_bark_Birch_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 51 years.")
  }

  if(age_at_breast_height>60){
    stop("Birch must be at most 200 years.")
  }

  if(jonson_bonitet>6){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VI")
    jonson_bonitet <- 6
  }

  if(jonson_bonitet%in%c(1,2,3,4)){
    constant <- 20841E-4
    diameter_under_bark <- 13231E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -20870E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 15763E-4
    diameter_under_bark <- 16923E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -29529E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 14897E-4
    diameter_under_bark <- 18975E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -40374E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -20432E-6*plot_basal_area_under_bark
    basal_area_sq <- 66314E-9*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -34342E-6
    basal_area_sq <- 50541E-8
  }

  tree_rings_breast_height <- -26058E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 71890E-9*(age_at_breast_height^2)

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq

  return(exp(normal_growth_mm))

}
