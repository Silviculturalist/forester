#' Five years diameter increment under bark, mm for Scots Pine between 5-60 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 54.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.4719
#' Number of trees: 4411
#' Coefficient of multiple correlation: 0.73
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
Jonsson_1980_five_year_increment_under_bark_Pine_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Pine must be at least 5 years.")
  }

  if(age_at_breast_height>60){
    stop("Pine must be at most 60 years.")
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
      constant <- 41417E-4
      diameter_under_bark <- 17938E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -24344E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==4){
      constant <- 38920E-4
      diameter_under_bark <- 21910E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -37469E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==5){
      constant <- 39610E-4
      diameter_under_bark <- 21936E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -40864E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==6){
      constant <- 38478E-4
      diameter_under_bark <- 23420E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -46701E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==7){
      constant <- 39177E-4
      diameter_under_bark <- 23165E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -49942E-9*(diameter_at_breast_height^2)
    }

    basal_area <- 0
    basal_area_sq <- 0

    if(cutting_period=="a"){
      basal_area <- -65082E-6*plot_basal_area_under_bark
      basal_area_sq <- 95147E-8*(plot_basal_area_under_bark^2)
    }

    #Cutting during second half of five year period, see p. 16, Bengt Hansson
    #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
    #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
    #terrängförhållanden. Eng: "Description of some for logging operations
    #important characteristics of forest stands, trees and terrain in Sweden".
    # Royal College of Forestry. Printed by Esselte AB, Stockholm.
    if(cutting_period=="b"){
      basal_area <- -65895E-6
      basal_area_sq <- 11545E-7
    }

    tree_rings_breast_height <- -85475E-6*age_at_breast_height
    tree_rings_breast_height_sq <- 71672E-8*(age_at_breast_height^2)

    rel_diameter <- 54580E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
    rel_diameter_sq <- -78879E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

    lat_values <- -12677E-6*latitude
    alt_meters <- -62972E-7*altitude

    Latitude_x_altitude <- 94767E-9*altitude*latitude

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
