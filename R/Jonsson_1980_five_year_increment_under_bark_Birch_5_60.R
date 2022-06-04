#' Five years diameter increment under bark, mm for Birch between 5-60 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 66.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.6284
#' Number of trees: 1932
#' Coefficient of multiple correlation: 0.66
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
Jonsson_1980_five_year_increment_under_bark_Birch_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 5 years.")
  }

  if(age_at_breast_height>60){
    stop("Birch must be at most 60 years.")
  }

  if(jonson_bonitet>6){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VI")
    jonson_bonitet <- 6
  }

  if(jonson_bonitet%in%c(1,2,3,4)){
    constant <- 33052E-4
    diameter_under_bark <- 16514E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -27342E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 33322E-4
    diameter_under_bark <- 15521E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -27252E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 29699E-4
    diameter_under_bark <- 21563E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -53674E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -35016E-6*plot_basal_area_under_bark
    basal_area_sq <- 43131E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -20630E-6
    basal_area_sq <- -74479E-9
  }

  tree_rings_breast_height <- -83998E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 63884E-8*(age_at_breast_height^2)

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq

  return(exp(normal_growth_mm))

}
